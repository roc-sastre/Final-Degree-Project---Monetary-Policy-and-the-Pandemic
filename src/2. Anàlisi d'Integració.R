# Paquets ----------------------------------------------------------------------

install.packages(c("urca", "tseries", "dplyr", "tidyverse","writexl","zoo",
                   "tibble"))
library(urca)
library(tseries)
library(dplyr)
library(tidyverse)
library(knitr)
library(writexl)
library(zoo)
library(tibble)

# ------------------------------------------------------------------------------
# Creació d'un data.frame amb totes les dades combinades -----------------------

load("1.RData")

df_names <- c("eonia", "estr", "eu_prices", "eu_output", 
              "us_prices", "us_output", "us_rate")

existing_dfs <- keep(mget(df_names, ifnotfound = list(NULL)), 
                     ~!is.null(.) && "month" %in% names(.))

df <- reduce(existing_dfs, ~full_join(.x, .y, by = "month")) %>% 
  arrange(month)

rm(existing_dfs,df_names)

# ------------------------------------------------------------------------------
# Transformacións vàries -------------------------------------------------------

for(i in c("eu_p_output", "us_p_output")) {
  df[[i]] <- as.numeric(df[[i]])
}

# Logaritmes
for(i in c("eu_output", "eu_p_output", "us_output", "us_p_output", "eu_prices",
           "us_prices")){
  df[[i]] <- log(df[[i]])
}

# Inflació = log(preus(+1)) - log(preus)
df$eu_inf <- c(NA, diff(df[["eu_prices"]]))
df$us_inf <- c(NA, diff(df[["us_prices"]]))

df <- df[-1, ]

# Output gap = log(Real GDP) - log(Potential GDP)
df$eu_gap <- df$eu_output - df$eu_p_output
df$us_gap <- df$us_output - df$us_p_output

# Eliminem variables innecessàries del data.frame
cols_to_remove <- c("eu_prices", "eu_output", "eu_p_output", "us_p_output",
                    "us_prices", "us_output")
df <- df[, !(names(df) %in% cols_to_remove)]

# ------------------------------------------------------------------------------
# Confecció de les 6 datasets --------------------------------------------------

df <- df %>%
  mutate(month_ym = as.yearmon(month, format = "%Y-%m"))

# Funció per filtrar per interval de yearmon (mesos)
filtra_months <- function(df, start_m, end_m) {
  start_ym <- as.yearmon(start_m, format = "%Y-%m")
  end_ym <- as.yearmon(end_m, format = "%Y-%m")
  
  df_filtered <- df %>%
    filter(month_ym >= start_ym & month_ym <= end_ym)
  
  return(df_filtered)
}

# Creem els data.frames filtrant per mesos

df1_eu <- filtra_months(df, "2014-02", "2019-12") %>%
  select(month, eonia, eu_inf, eu_gap)

df2_eu <- filtra_months(df, "2020-01", "2023-03") %>%
  select(month, estr, eu_inf, eu_gap)

df3_eu <- filtra_months(df, "2023-04", "2025-04") %>%
  select(month, estr, eu_inf, eu_gap)

df1_us <- filtra_months(df, "2014-02", "2019-12") %>%
  select(month, fed_funds, us_inf, us_gap)

df2_us <- filtra_months(df, "2020-01", "2023-03") %>%
  select(month, fed_funds, us_inf, us_gap)

df3_us <- filtra_months(df, "2023-04", "2025-05") %>%
  select(month, fed_funds, us_inf, us_gap)

# ------------------------------------------------------------------------------
# Contrastos d'integració individuals ------------------------------------------

# Llista de data.frames
llista_dfs <- list(df1_eu, df2_eu, df3_eu, df1_us, df2_us, df3_us)
noms_dfs <- c("df1_eu", "df2_eu", "df3_eu", "df1_us", "df2_us", "df3_us")

# Data frame per desar tots els resultats de tests d'estacionarietat
resultats_totals <- data.frame(
  DataFrame = character(),
  Variable = character(),
  DF_GLS_Estacionaria = logical(),
  PP_Estacionaria = logical(),
  KPSS_Estacionaria = logical(),
  stringsAsFactors = FALSE
)

# Data frame per desar resultats de cointegració
resultats_cointegracio <- data.frame(
  DataFrame = character(),
  Estacionari_Global = logical(),
  Cointegrat = logical(),
  stringsAsFactors = FALSE
)

# Iterar sobre cada data frame
for (i in seq_along(llista_dfs)) {
  df <- llista_dfs[[i]]
  nom_df <- noms_dfs[i]
  variables <- names(df)[names(df) != "month"]
  
  # Resultats estacionarietat per variable
  for (var in variables) {
    serie <- df[[var]]
    
    # (1) Test DF-GLS (amb gestió d'errors)
    estat_dfgls <- NA
    estacionaria_dfgls <- NA
    try({
      prova_dfgls <- ur.ers(serie, type = "DF-GLS", model = "constant", lag.max = 4)
      estat_dfgls <- prova_dfgls@teststat
      valor_critic_dfgls <- prova_dfgls@cval[,"5pct"]
      estacionaria_dfgls <- estat_dfgls < valor_critic_dfgls
    }, silent = TRUE)
    
    # (2) Test PP
    prova_pp <- ur.pp(serie, type = "Z-tau", model = "constant", lags = "short")
    estat_pp <- prova_pp@teststat
    valor_critic_pp <- prova_pp@cval[2]  # 5% level
    estacionaria_pp <- estat_pp < valor_critic_pp
    
    # (3) Test KPSS
    prova_kpss <- ur.kpss(serie, type = "mu", lags = "short")
    estat_kpss <- prova_kpss@teststat
    valor_critic_kpss <- as.numeric(prova_kpss@cval[2])
    estacionaria_kpss <- estat_kpss < valor_critic_kpss
    
    # Afegir fila
    resultats_totals <- rbind(resultats_totals, data.frame(
      DataFrame = nom_df,
      Variable = var,
      DF_GLS_Estacionaria = estacionaria_dfgls,
      PP_Estacionaria = estacionaria_pp,
      KPSS_Estacionaria = estacionaria_kpss
    ))
  }
  
  # Test de cointegració (Johansen)
  df_coin <- df[ , variables]
  df_coin <- na.omit(df_coin)
  if (ncol(df_coin) >= 2) {
    johansen_test <- ca.jo(df_coin, type = "trace", ecdet = "const", K = 2)
    estat <- johansen_test@teststat
    valors_crits <- johansen_test@cval[, "5pct"]
    num_cointegracions <- sum(estat > valors_crits)
    
    resultats_cointegracio <- rbind(resultats_cointegracio, data.frame(
      DataFrame = nom_df,
      Num_Vectors_Cointegracio = num_cointegracions
    ))
  } else {
    resultats_cointegracio <- rbind(resultats_cointegracio, data.frame(
      DataFrame = nom_df,
      Num_Vectors_Cointegracio = NA
    ))
  }
}

# Mostrar resultats
print("Resultats dels tests d'estacionarietat:")
print(resultats_totals)

print("\nResultats del test de cointegració de Johansen:")
print(resultats_cointegracio)

# ------------------------------------------------------------------------------
# Contrastos d'integració conjunts ---------------------------------------------

# Variables a analitzar
variables_interes <- c("eonia", "estr", "fed_funds", "eu_inf", "us_inf", "eu_gap", "us_gap")

# Inicialitzar data frame de resultats
resultats <- data.frame(
  Variable = character(),
  DF_GLS_Estacionaria = logical(),
  PP_Estacionaria = logical(),
  KPSS_Estacionaria = logical(),
  stringsAsFactors = FALSE
)

# Iterar sobre variables
for (var in variables_interes) {
  if (!var %in% names(df)) next  # Saltar si la variable no existeix
  serie <- df[[var]]
  
  # Eliminar NA si n'hi ha
  serie <- na.omit(serie)
  
  # Inicialitzar resultats per la variable
  dfgls_res <- NA
  pp_res <- NA
  kpss_res <- NA
  
  # Test DF-GLS
  try({
    test_dfgls <- ur.ers(serie, type = "DF-GLS", model = "constant", lag.max = 4)
    estat <- test_dfgls@teststat
    crit <- test_dfgls@cval[,"5pct"]
    dfgls_res <- estat < crit
  }, silent = TRUE)
  
  # Test PP
  try({
    test_pp <- ur.pp(serie, type = "Z-tau", model = "constant", lags = "short")
    estat <- test_pp@teststat
    crit <- test_pp@cval[2]
    pp_res <- estat < crit
  }, silent = TRUE)
  
  # Test KPSS
  try({
    test_kpss <- ur.kpss(serie, type = "mu", lags = "short")
    estat <- test_kpss@teststat
    crit <- as.numeric(test_kpss@cval[2])
    kpss_res <- estat < crit  # En KPSS, si el test < crítica -> estacionària
  }, silent = TRUE)
  
  # Afegir resultats a la taula
  resultats <- rbind(resultats, data.frame(
    Variable = var,
    DF_GLS_Estacionaria = dfgls_res,
    PP_Estacionaria = pp_res,
    KPSS_Estacionaria = kpss_res,
    stringsAsFactors = FALSE
  ))
}

# Mostrar resultats
print(resultats)

# ------------------------------------------------------------------------------
# Importació en .xlsx ----------------------------------------------------------

# Creació d'una llista amb els datasets
data_list <- list(
  df1_eu = df1_eu,
  df2_eu = df2_eu,
  df3_eu = df3_eu,
  df1_us = df1_us,
  df2_us = df2_us,
  df3_us = df3_us
)

# Bucle per a la creació dels fitxers .xlsx format BEAR toolbox
for (name in names(data_list)) {
  df <- data_list[[name]]
  
  # Canvi del format de "month"
  df$month <- paste0(format(df$month, "%Y"), "m", as.numeric(format(df$month, "%m")))
  
  # Reanomenem les columnes
  colnames(df)[2:4] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  
  # Sheet 1: data
  data_sheet <- cbind(" " = df$month, df[, 2:4])
  rownames(data_sheet) <- NULL
  
  # Sheet 2: sign rest values
  sign_sheet <- matrix("", nrow = 5, ncol = 5)
  sign_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  sign_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  sign_sheet[3, 2] <- "INTEREST_RATE"
  sign_sheet[4, 2] <- "INFLATION"
  sign_sheet[5, 2] <- "OUTPUT_GAP"
  sign_sheet[3, 3] <- "+"
  sign_sheet[4, 3] <- "-"
  sign_sheet[4, 4] <- "+"
  sign_sheet[5, 4] <- "+"
  sign_sheet[4, 5] <- "-"
  sign_sheet[5, 5] <- "+"
  
  # Sheet 3: sign rest periods
  period_sheet <- matrix("", nrow = 5, ncol = 5)
  period_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  period_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  period_sheet[3, 2] <- "INTEREST_RATE"
  period_sheet[4, 2] <- "INFLATION"
  period_sheet[5, 2] <- "OUTPUT_GAP"
  period_sheet[3, 3] <- "0 3"
  period_sheet[4, 3] <- "0 3"
  period_sheet[4, 4] <- "0 3"
  period_sheet[5, 4] <- "0 3"
  period_sheet[4, 5] <- "0 3"
  period_sheet[5, 5] <- "0 3"
  
  # Sheet 4: relmagn res values
  relmagn_sheet <- matrix("", nrow = 5, ncol = 5)
  relmagn_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  relmagn_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  relmagn_sheet[3, 2] <- "INTEREST_RATE"
  relmagn_sheet[4, 2] <- "INFLATION"
  relmagn_sheet[5, 2] <- "OUTPUT_GAP"
  
  # Sheet 5: relmagn res periods
  relmagn_period_sheet <- matrix("", nrow = 5, ncol = 5)
  relmagn_period_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  relmagn_period_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  relmagn_period_sheet[3, 2] <- "INTEREST_RATE"
  relmagn_period_sheet[4, 2] <- "INFLATION"
  relmagn_period_sheet[5, 2] <- "OUTPUT_GAP"
  
  # Sheet 6: FEVD res values
  FEVD_sheet <- matrix("", nrow = 5, ncol = 5)
  FEVD_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  FEVD_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  FEVD_sheet[3, 2] <- "INTEREST_RATE"
  FEVD_sheet[4, 2] <- "INFLATION"
  FEVD_sheet[5, 2] <- "OUTPUT_GAP"
  
  # Sheet 7: FEVD res periods
  FEVD_period_sheet <- matrix("", nrow = 5, ncol = 5)
  FEVD_period_sheet[1, 3:5] <- c("monetary policy", "demand", "supply")
  FEVD_period_sheet[2, 3:5] <- c("INTEREST_RATE", "INFLATION", "OUTPUT_GAP")
  FEVD_period_sheet[3, 2] <- "INTEREST_RATE"
  FEVD_period_sheet[4, 2] <- "INFLATION"
  FEVD_period_sheet[5, 2] <- "OUTPUT_GAP"
  
  # Convertim les matrius a data.frames i eliminem el nom de les columnes
  sign_sheet_df <- as.data.frame(sign_sheet)
  names(sign_sheet_df) <- NULL
  
  period_sheet_df <- as.data.frame(period_sheet)
  names(period_sheet_df) <- NULL
  
  relmagn_sheet_df <- as.data.frame(relmagn_sheet)
  names(relmagn_sheet_df) <- NULL
  
  relmagn_period_sheet_df <- as.data.frame(relmagn_period_sheet)
  names(relmagn_period_sheet_df) <- NULL
  
  FEVD_sheet_df <- as.data.frame(FEVD_sheet)
  names(FEVD_sheet_df) <- NULL
  
  FEVD_period_sheet_df <- as.data.frame(FEVD_period_sheet)
  names(FEVD_period_sheet_df) <- NULL
  
  # Guardem el fitxer .xlsx
  write_xlsx(
    list(
      `data` = as.data.frame(data_sheet),
      `sign res values` = sign_sheet_df,
      `sign res periods` = period_sheet_df,
      `relmagn res values` = relmagn_sheet_df,
      `relmagn res periods` = relmagn_period_sheet_df,
      `FEVD res values` = FEVD_sheet_df,
      `FEVD res periods` = FEVD_period_sheet_df
      
    ),
    path = paste0(name, ".xlsx")
  )
}

rm(data_list, period_sheet, sign_sheet, data_sheet, period_sheet_df,
   sign_sheet_df, relmagn_sheet_df, relmagn_period_sheet_df, FEVD_sheet_df,
   FEVD_period_sheet_df)

# ------------------------------------------------------------------------------