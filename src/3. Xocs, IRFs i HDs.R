# Paquets ----------------------------------------------------------------------

install.packages(c("R.matlab","ggplot2","gridExtra","tidyr","dplyr"))
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(zoo)
library(patchwork)

# ------------------------------------------------------------------------------
# Lectura dels fitxers .mat ----------------------------------------------------

file_names <- c(
  "results_1_eu.mat", "results_2_eu.mat", "results_3_eu.mat",
  "results_1_us.mat", "results_2_us.mat", "results_3_us.mat"
)

results_list <- lapply(file_names, readMat)
names(results_list) <- tools::file_path_sans_ext(file_names)

rm(file_names)

# ------------------------------------------------------------------------------
# Gràfics de xocs estimats -----------------------------------------------------

# Funció d'extracció (igual que la teva original)
extract_monetary_shocks <- function(shock_matrix, dates) {
  data.frame(
    Temps = as.Date(as.yearmon(dates)),
    Lower   = shock_matrix[1, ],
    Median  = shock_matrix[2, ],
    Upper   = shock_matrix[3, ]
  )
}

# Colors ajustats: fills una mica més intensos; línies segueixen vives
region_colors <- list(
  "us" = list(line = "#1f77b4", fill = "#6baed6"),  # blau mitjà per fill
  "eu" = list(line = "#ff7f0e", fill = "#fdae6b")   # taronja mitjà per fill
)

# Funció per crear el plot d'una regió amb ribbon més intens (alpha augmentat)
plot_shock_region <- function(shock_df, region_label,
                              line_color = "#1f77b4", fill_color = "#aec7e8",
                              y_limits = NULL, show_y = TRUE) {
  p <- ggplot(shock_df, aes(x = Temps)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = fill_color, alpha = 0.45) + # alpha augmentat
    geom_line(aes(y = Median), color = line_color, size = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_cartesian(ylim = y_limits) +
    labs(title = region_label, x = "Temps", y = ifelse(show_y, "Shock magnitude", "")) +
    # Usar Arial com a font base
    theme_minimal(base_size = 13, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_line(color = "gray92"),
      axis.text = element_text(color = "black", family = "Arial"),
      axis.title = element_text(color = "black", family = "Arial"),
      plot.title = element_text(color = "black", face = "bold", hjust = 0.02, family = "Arial"),
      legend.position = "none"
    )
  return(p)
}

# Directori per guardar (canvia-ho si cal)
save_dir <- "C:/Users/rocsa/Desktop/Universitat/TFG/Gràfics"
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

# Regions i períodes (assumim la mateixa convenció que abans: results_1_us, results_2_eu, ...)
regions <- c("us", "eu")
periods <- c("pre-COVID", "COVID", "post-COVID")

# Colors escollits (més neutres i accessibles)
region_colors <- list(
  "us" = list(line = "#1f77b4", fill = "#c6dbef"),
  "eu" = list(line = "#ff7f0e", fill = "#fdd0a2")
)

for (i in seq_along(periods)) {
  period_name <- periods[i]
  
  # Obtenir els resultats per ambdues regions per aquest període
  res_us <- results_list[[ paste0("results_", i, "_us") ]]
  res_eu <- results_list[[ paste0("results_", i, "_eu") ]]
  
  if (is.null(res_us) || is.null(res_eu)) {
    warning("Falta resultats per període ", i, " (", period_name, ") — saltant.")
    next
  }
  
  # Extreure matrius de xocs (assumim index 1 per monetari com a l'original)
  shock_mat_us <- res_us$strshocks.estimates[1,1][[1]][[1]]
  shock_mat_eu <- res_eu$strshocks.estimates[1,1][[1]][[1]]
  dates_us <- as.vector(res_us$decimaldates1)
  dates_eu <- as.vector(res_eu$decimaldates1)
  
  shock_df_us <- extract_monetary_shocks(shock_mat_us, dates_us)
  shock_df_eu <- extract_monetary_shocks(shock_mat_eu, dates_eu)
  
  # Calcular límits Y comuns (incloem 0) i afegim padding 5%
  ymin_all <- min(c(shock_df_us$Lower, shock_df_eu$Lower, 0), na.rm = TRUE)
  ymax_all <- max(c(shock_df_us$Upper, shock_df_eu$Upper, 0), na.rm = TRUE)
  if (!is.finite(ymin_all)) ymin_all <- 0
  if (!is.finite(ymax_all)) ymax_all <- 0
  range_val <- ymax_all - ymin_all
  if (range_val == 0) {
    pad <- ifelse(abs(ymax_all) > 0, 0.05 * abs(ymax_all), 0.1)
  } else {
    pad <- 0.05 * range_val
  }
  y_limits <- c(ymin_all - pad, ymax_all + pad)
  
  # Crear plots per regió amb colors definits
  p_us <- plot_shock_region(shock_df_us, region_label = paste("USA —", period_name),
                            line_color = region_colors$us$line, fill_color = region_colors$us$fill,
                            y_limits = y_limits, show_y = TRUE)
  p_eu <- plot_shock_region(shock_df_eu, region_label = paste("EU —", period_name),
                            line_color = region_colors$eu$line, fill_color = region_colors$eu$fill,
                            y_limits = y_limits, show_y = FALSE)
  
  # Combinar horitzontalment (2 panells). widths iguals per alineació perfecta
  combined <- p_us + p_eu + plot_layout(ncol = 2, widths = c(1, 1))
  
  # Guardar fitxer (nom sense espais)
  safe_period <- gsub("-", "", period_name)
  file_name <- paste0("XocsMonetaris_", safe_period, ".png")
  full_path <- file.path(save_dir, file_name)
  
  ggsave(filename = full_path, plot = combined, width = 12, height = 4.5, dpi = 300, units = "in")
  
  cat("Saved:", full_path, "\n")
}
# ------------------------------------------------------------------------------
# Obtenció de IRFs i gràfics ---------------------------------------------------

# Funció pel processament de les IRFs (la teva original)
extract_irf_data <- function(irf_matrix, variable_name) {
  data.frame(
    Horizon = 1:ncol(irf_matrix),
    Lower = irf_matrix[1, ],
    Median = irf_matrix[2, ],
    Upper = irf_matrix[3, ],
    Variable = variable_name
  )
}

# Funció per la generació de IRFs (versió: 3 horitzontals + mateixes y-limits)
draw_irfs_variable_economy <- function(results_list, economy, variable, save_dir) {
  # mapeig variables a índex i colors
  var_index <- list(
    "Interest Rate" = 1,
    "Inflation" = 2,
    "Output Gap" = 3
  )
  variable_colors <- list(
    "Interest Rate" = "gold",
    "Inflation" = "red",
    "Output Gap" = "blue"
  )
  idx <- var_index[[variable]]
  color <- variable_colors[[variable]]
  
  # Obtenir resultats en ordre explícit (results_1_econ, results_2_econ, results_3_econ)
  economy_results <- lapply(1:3, function(i) {
    results_list[[ paste0("results_", i, "_", economy) ]]
  })
  periods <- c("pre-COVID", "COVID", "post-COVID")
  
  # Extracció de dades garantint l'ordre
  irf_dfs <- lapply(seq_along(economy_results), function(i) {
    res <- economy_results[[i]]
    # extreure la matriu d'IRF (mateixa forma que tenies)
    irf_mat <- res$irf.estimates[idx,1][[1]][[1]]
    df <- extract_irf_data(irf_mat, variable)
    df$Period <- periods[i]
    df$Order <- i
    df
  })
  
  # Combinar
  irf_df <- do.call(rbind, irf_dfs)
  irf_df <- irf_df[order(irf_df$Order), ]
  
  # Límits Y comuns (assegurant incloure 0)
  ymin <- min(irf_df$Lower, 0, na.rm = TRUE)
  ymax <- max(irf_df$Upper, 0, na.rm = TRUE)
  # Si hi ha infinites/NA defensiu
  if (!is.finite(ymin)) ymin <- 0
  if (!is.finite(ymax)) ymax <- 0
  # petit padding perquè no quedin tocat els límits (5% del rang)
  range_val <- ymax - ymin
  if (range_val == 0) {
    pad <- ifelse(abs(ymax) > 0, 0.05 * abs(ymax), 0.1)
  } else {
    pad <- 0.05 * range_val
  }
  y_limits <- c(ymin - pad, ymax + pad)
  
  # Crear plots individuals, mantenint format i aplicant coord_cartesian per evitar clipping
  plots <- lapply(sort(unique(irf_df$Order)), function(ord) {
    df <- subset(irf_df, Order == ord)
    ggplot(df, aes(x = Horizon)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = color, alpha = 0.3) +
      geom_line(aes(y = Median), color = color, size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_cartesian(ylim = y_limits) +
      labs(title = df$Period[1], x = "Horizon (months)", y = "Response (percentage)") +
      theme_minimal(base_size = 14)
  })
  
  # Combinar horitzontalment amb patchwork (mateix aspect per a tots)
  combined <- (plots[[1]] + plots[[2]] + plots[[3]]) +
    plot_layout(ncol = 3, widths = c(1,1,1))
  
  # Guardar (ajusta width/height si vols un altre aspecte)
  file_name <- paste0("IRF_", gsub(" ", "", variable), "_", economy, ".png")
  full_path <- file.path(save_dir, file_name)
  ggsave(filename = full_path, plot = combined, width = 15, height = 5.5, dpi = 300, units = "in")
  
  invisible(combined)
}

# Bucle per generar plots per cada combinació variable x economia (igual que abans)
# Assegura't que save_dir existeix i results_list està carregat
save_dir <- "C:/Users/rocsa/Desktop/Universitat/TFG/Gràfics"
variables <- c("Interest Rate", "Inflation", "Output Gap")
economies <- c("eu", "us")

for (var in variables) {
  for (eco in economies) {
    cat("Processing:", var, eco, "\n")
    draw_irfs_variable_economy(results_list, economy = eco, variable = var, save_dir = save_dir)
  }
}
# ------------------------------------------------------------------------------
# Descomposició històrica + gràfics --------------------------------------------

# Funció per preparar el gràfic (NO canviem el tema ni posició de la llegenda aquí)
genera_grafic_hd <- function(result_data, var_index, nom_variable, periode, y_limits = NULL) {
  monetari <- as.numeric(result_data$hd.estimates[[1, var_index]][[1]][2, ])
  demanda  <- as.numeric(result_data$hd.estimates[[2, var_index]][[1]][2, ])
  oferta   <- as.numeric(result_data$hd.estimates[[3, var_index]][[1]][2, ])
  real     <- as.numeric(result_data$hd.estimates[[7, var_index]][[1]][2, ])
  
  decimal_dates <- as.vector(result_data$decimaldates1)
  Temps <- as.Date(as.yearmon(decimal_dates))
  
  df <- data.frame(
    Temps = Temps,
    Monetari = monetari,
    Demanda  = demanda,
    Oferta   = oferta,
    Total    = real
  ) %>%
    pivot_longer(cols = c("Monetari", "Demanda", "Oferta"),
                 names_to = "Xoc",
                 values_to = "Contribucio")
  
  p <- ggplot(df, aes(x = Temps)) +
    geom_bar(aes(y = Contribucio, fill = Xoc), stat = "identity", position = "stack") +
    geom_line(data = df %>% distinct(Temps, Total), aes(y = Total), color = "black", size = 1.0) +
    scale_fill_manual(values = c(
      "Demanda"  = "indianred2",
      "Monetari" = "khaki2",
      "Oferta"   = "steelblue2"
    )) +
    labs(
      title = paste0(nom_variable, " - ", periode),
      x = "Temps",
      y = "Valor (punts percentuals)",
      fill = "Tipus de xoc"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(y_limits) && length(y_limits) == 2) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# Mapeig
periode_map <- c("1" = "pre-COVID", "2" = "COVID", "3" = "post-COVID")
noms_vars <- c("Interest Rate", "Inflation", "Output Gap")

for (var_index in 1:3) {
  var_nom <- noms_vars[var_index]
  
  for (regio in c("eu", "us")) {
    cat("Saving plot for:", var_nom, "_", regio, "\n")
    
    # 1) Calcular límits comuns per als 3 períodes
    all_vals_min <- Inf
    all_vals_max <- -Inf
    
    for (i in 1:3) {
      name <- paste0("results_", i, "_", regio)
      resultat <- results_list[[name]]
      
      monetari <- as.numeric(resultat$hd.estimates[[1, var_index]][[1]][2, ])
      demanda  <- as.numeric(resultat$hd.estimates[[2, var_index]][[1]][2, ])
      oferta   <- as.numeric(resultat$hd.estimates[[3, var_index]][[1]][2, ])
      total    <- as.numeric(resultat$hd.estimates[[7, var_index]][[1]][2, ])
      
      stacked_sum <- monetari + demanda + oferta
      
      period_min <- min(stacked_sum, total, na.rm = TRUE)
      period_max <- max(stacked_sum, total, na.rm = TRUE)
      
      if (!is.finite(period_min)) period_min <- 0
      if (!is.finite(period_max)) period_max <- 0
      
      all_vals_min <- min(all_vals_min, period_min)
      all_vals_max <- max(all_vals_max, period_max)
    }
    
    range_val <- all_vals_max - all_vals_min
    if (range_val == 0) range_val <- abs(all_vals_max) + 1
    pad <- 0.05 * range_val
    y_limits <- c(all_vals_min - pad, all_vals_max + pad)
    
    # 2) Generar els 3 plots (cada un AMB llegenda visible - patchwork farà el "collect")
    plots <- vector("list", 3)
    for (i in 1:3) {
      name <- paste0("results_", i, "_", regio)
      resultat <- results_list[[name]]
      periode <- periode_map[as.character(i)]
      plots[[i]] <- genera_grafic_hd(resultat, var_index, var_nom, periode, y_limits = y_limits)
    }
    
    # 3) Combinar amb patchwork: guides = "collect" farà servir una sola llegenda; 
    #    i & theme(legend.position = "bottom") la col·loca centrada a baix.
    combined <- (plots[[1]] + plots[[2]] + plots[[3]]) +
      plot_layout(ncol = 3, guides = "collect", widths = c(1,1,1)) &
      theme(legend.position = "bottom",
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 9))
    
    # 4) Guardar (mantenint el mateix aspecte dels plots)
    safe_var_nom <- gsub(" ", "_", var_nom)
    file_name <- paste0("HD_", safe_var_nom, "_", regio, ".png")
    full_path <- file.path(save_dir, file_name)
    
    ggsave(filename = full_path, plot = combined, width = 15, height = 5.5, dpi = 300, units = "in")
  }
}
# ------------------------------------------------------------------------------