# Paquets ----------------------------------------------------------------------

install.packages(c("R.matlab","ggplot2","gridExtra","tidyr","dplyr"))
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(zoo)
library(patchwork)
library(grid)

# ------------------------------------------------------------------------------
# IRF ppt ----------------------------------------------------------------------

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

# Funció per la generació de IRFs combinades per variable
draw_combined_irfs_per_variable <- function(results_list, variable, save_dir) {
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
  
  economies <- c("eu", "us")
  periods <- c("pre-COVID", "COVID", "post-COVID")
  
  # Llista per emmagatzemar totes les dades
  all_data <- list()
  
  # Extraure dades per a cada economia i cada període
  for (eco in economies) {
    for (i in 1:3) {
      res_name <- paste0("results_", i, "_", eco)
      res <- results_list[[res_name]]
      irf_mat <- res$irf.estimates[idx,1][[1]][[1]]
      df <- extract_irf_data(irf_mat, variable)
      df$Period <- periods[i]
      df$Economy <- eco
      df$Order <- i
      all_data[[length(all_data) + 1]] <- df
    }
  }
  
  # Combinar totes les dades
  irf_df <- do.call(rbind, all_data)
  
  # Calcular els límits Y per a cada economia
  y_limits_by_economy <- list()
  for (eco in economies) {
    sub_df <- irf_df[irf_df$Economy == eco, ]
    ymin <- min(sub_df$Lower, 0, na.rm = TRUE)
    ymax <- max(sub_df$Upper, 0, na.rm = TRUE)
    if (!is.finite(ymin)) ymin <- 0
    if (!is.finite(ymax)) ymax <- 0
    range_val <- ymax - ymin
    if (range_val == 0) {
      pad <- ifelse(abs(ymax) > 0, 0.05 * abs(ymax), 0.1)
    } else {
      pad <- 0.05 * range_val
    }
    y_limits_by_economy[[eco]] <- c(ymin - pad, ymax + pad)
  }
  
  # Configurar el dispositiu gràfic amb transparència
  file_name <- paste0("IRF_", gsub(" ", "", variable), "_combined.png")
  full_path <- file.path(save_dir, file_name)
  
  # Obrir un nou dispositiu PNG amb fons transparent
  png(full_path, width = 15, height = 10, units = "in", res = 300, bg = "transparent")
  
  # Configurar layout 2x3
  par(mfrow = c(2, 3), mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0), bg = NA)
  
  # Crear cada subgràfic
  for (eco in economies) {
    for (i in 1:3) {
      df_sub <- irf_df[irf_df$Economy == eco & irf_df$Order == i, ]
      y_limits <- y_limits_by_economy[[eco]]
      
      # Crear plot buit
      plot(1, type = "n", 
           xlim = c(1, max(df_sub$Horizon)), 
           ylim = y_limits,
           xlab = "", ylab = "", 
           axes = FALSE,
           frame.plot = TRUE)
      
      # Afegir eixos només quan calgui
      if (i == 1) {  # Eix Y només a la primera columna
        axis(2, col = "white", col.axis = "white", col.ticks = "white")
      }
      if (eco == "us") {  # Eix X només a la fila inferior
        axis(1, col = "white", col.axis = "white", col.ticks = "white")
      }
      
      # Afegir línia a zero
      abline(h = 0, lty = 2, col = "white")
      
      # Afegir area de confiança
      polygon(c(df_sub$Horizon, rev(df_sub$Horizon)), 
              c(df_sub$Lower, rev(df_sub$Upper)), 
              col = adjustcolor(color, alpha.f = 0.3), border = NA)
      
      # Afegir línia mediana
      lines(df_sub$Horizon, df_sub$Median, col = color, lwd = 2)
      
      # Afegir caixa al voltant
      box(col = "white")
    }
  }
  
  # Tancar dispositiu
  dev.off()
  
  cat("Saved:", full_path, "\n")
}

# Bucle per generar els 3 arxius
save_dir <- "C:/Users/rocsa/Desktop/Universitat/TFG/Gràfics"
variables <- c("Interest Rate", "Inflation", "Output Gap")

for (var in variables) {
  cat("Processing:", var, "\n")
  draw_combined_irfs_per_variable(results_list, variable = var, save_dir = save_dir)
}

# ------------------------------------------------------------------------------
# HD ppt -----------------------------------------------------------------------

# Funció per generar gràfics HD combinats per variable (fons transparent)
draw_combined_hd_per_variable <- function(results_list, variable, save_dir) {
  # Mapeig variables
  var_index_map <- list(
    "Interest Rate" = 1,
    "Inflation" = 2, 
    "Output Gap" = 3
  )
  var_index <- var_index_map[[variable]]
  
  economies <- c("eu", "us")
  
  # Llista per emmagatzemar tots els plots
  plot_list <- list()
  y_limits_by_economy <- list()
  
  # Primer: calcular límits Y per a cada economia
  for (eco in economies) {
    all_vals_min <- Inf
    all_vals_max <- -Inf
    
    for (i in 1:3) {
      name <- paste0("results_", i, "_", eco)
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
    pad <- 0.1 * range_val
    y_limits_by_economy[[eco]] <- c(all_vals_min - pad, all_vals_max + pad)
  }
  
  # Segon: crear els plots amb l'estil IRF
  for (eco in economies) {
    for (i in 1:3) {
      name <- paste0("results_", i, "_", eco)
      resultat <- results_list[[name]]
      
      # Extraure dades
      monetari <- as.numeric(resultat$hd.estimates[[1, var_index]][[1]][2, ])
      demanda  <- as.numeric(resultat$hd.estimates[[2, var_index]][[1]][2, ])
      oferta   <- as.numeric(resultat$hd.estimates[[3, var_index]][[1]][2, ])
      real     <- as.numeric(resultat$hd.estimates[[7, var_index]][[1]][2, ])
      
      decimal_dates <- as.vector(resultat$decimaldates1)
      Temps <- as.Date(as.yearmon(decimal_dates))
      
      # Crear el dataframe - ORDRE ORIGINAL: Oferta, Monetari, Demanda
      df <- data.frame(
        Temps = Temps,
        Oferta = oferta,      # PRIMER: Supply
        Monetari = monetari,  # SEGON: Monetary  
        Demanda = demanda,    # TERCER: Demand
        Total = real
      )
      
      # Convertir a format llarg mantenint l'ordre ORIGINAL
      df_long <- df %>%
        pivot_longer(cols = c(Oferta, Monetari, Demanda),
                     names_to = "Xoc",
                     values_to = "Contribucio")
      
      # Assegurar l'ordre ORIGINAL dels xocs: Oferta, Monetari, Demanda
      df_long$Xoc <- factor(df_long$Xoc, levels = c("Oferta", "Monetari", "Demanda"))
      
      # Crear el plot amb l'estil IRF
      p <- ggplot(df_long, aes(x = Temps)) +
        geom_bar(aes(y = Contribucio, fill = Xoc), 
                 stat = "identity", position = "stack", width = 20) +
        geom_line(data = df, aes(x = Temps, y = Total), 
                  color = "white", size = 1.0) +
        # Colors corresponents a cada xoc
        scale_fill_manual(values = c(
          "Oferta" = "steelblue2",    # Supply - steelblue2
          "Monetari" = "khaki2",      # Monetary - khaki2  
          "Demanda" = "indianred2"    # Demand - indianred2
        )) +
        coord_cartesian(ylim = y_limits_by_economy[[eco]]) +
        labs(x = NULL, y = NULL) +
        # ESTIL IRF: tema minimal amb caixa blanca
        theme_minimal() +
        theme(
          # Fons completament transparent
          panel.background = element_rect(fill = "transparent", colour = "white", linewidth = 1),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid = element_blank(),
          # Caixa blanca al voltant (com IRF)
          panel.border = element_rect(colour = "white", fill = NA, linewidth = 1),
          # Eixos en blanc (estil IRF)
          axis.line = element_blank(),
          axis.ticks = element_line(color = "white"),
          axis.text = element_text(color = "white", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 5)),
          # Llegenda
          legend.position = "none",
          # Marges (com IRF: mar = c(3, 3, 1, 1))
          plot.margin = margin(3, 3, 1, 1, "mm")
        )
      
      # Controlar quins eixos es mostren (estil IRF)
      if (i != 1) {
        p <- p + theme(
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()
        )
      } else {
        # Mostrar eix Y només a la primera columna (com IRF)
        p <- p + theme(axis.ticks.y = element_line(color = "white"))
      }
      
      if (eco != "us") {
        p <- p + theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      } else {
        # Mostrar eix X només a la fila inferior (com IRF)
        p <- p + theme(axis.ticks.x = element_line(color = "white"))
      }
      
      plot_list[[paste(eco, i)]] <- p
    }
  }
  
  # Configurar el dispositiu gràfic amb transparència (estil IRF)
  file_name <- paste0("HD_", gsub(" ", "", variable), "_combined.png")
  full_path <- file.path(save_dir, file_name)
  
  # Obrir un nou dispositiu PNG amb fons transparent (mateixos paràmetres IRF)
  png(full_path, width = 15, height = 10, units = "in", res = 300, bg = "transparent")
  
  # Configurar layout 2x3 (mateix que IRF)
  library(gridExtra)
  combined_plot <- grid.arrange(
    plot_list[["eu 1"]], plot_list[["eu 2"]], plot_list[["eu 3"]],
    plot_list[["us 1"]], plot_list[["us 2"]], plot_list[["us 3"]],
    nrow = 2, ncol = 3,
    widths = c(1, 1, 1),
    heights = c(1, 1)
  )
  
  # Tancar dispositiu
  dev.off()
  
  cat("Saved HD (IRF style):", full_path, "\n")
}

# Bucle per generar els 3 arxius HD
save_dir <- "C:/Users/rocsa/Desktop/Universitat/TFG/Gràfics"
variables <- c("Interest Rate", "Inflation", "Output Gap")

# Assegurar que el directori existeix
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

for (var in variables) {
  cat("Processing HD:", var, "\n")
  draw_combined_hd_per_variable(results_list, variable = var, save_dir = save_dir)
}