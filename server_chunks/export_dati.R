# ============================
# ENRICHED CSV AND EXCEL DATA EXPORT
# ============================
prepara_dati_export <- function() {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(NULL)
  
  # 1. MODEL AND ALGORITHM INFO
  trasf_y_usata <- if (is.null(input$trasformazione_y) || input$trasformazione_y == "nessuna") "None (raw values)" else {
    scelte_trasf <- costruisci_scelte_trasformazione_y()
    nomi_scelte <- names(scelte_trasf)[scelte_trasf == input$trasformazione_y]
    if (length(nomi_scelte) > 0) nomi_scelte[1] else input$trasformazione_y
  }
  df_info <- data.frame(
    Property = c("Model Category", "Formula Used", "Y Pre-transformation", "Optimization Algorithm", "Loss Function", "Weighting", "Error Method",
                 "Max Iterations", "Convergence Tolerance",
                 "Monte Carlo Noise Type", "Monte Carlo Sigma Used", "Monte Carlo Successful Iterations", "Confidence Level (%)"),
    Value = c(input$tipo_modello, res$formula, trasf_y_usata, algoritmo_effettivo(input$algoritmo, input$tipo_loss), input$tipo_loss, descrizione_ponderazione(res), input$metodo_errore,
              as.character(input$maxiter_ottimizzazione),
              if (is.null(res$esponente_tolleranza)) "N/A" else paste0("1e", res$esponente_tolleranza),
              ifelse(is.null(res$tipo_rumore_mc) || is.na(res$tipo_rumore_mc), "N/A",
                     if (identical(res$tipo_rumore_mc, "bootstrap")) paste0("bootstrap (", if (isTRUE(res$residui_modificati)) "leverage-corrected, centred residuals; Davison & Hinkley, 1997" else "raw residuals", ")") else res$tipo_rumore_mc),
              ifelse(is.null(res$sigma_mc) || is.na(res$sigma_mc), "N/A", as.character(round(res$sigma_mc, 5))),
              ifelse(is.null(res$n_boot_totale) || is.na(res$n_boot_totale), "N/A", paste0(res$n_boot_successo, " / ", res$n_boot_totale)),
              as.character(input$livello_confidenza))
  )
  
  # 2. PARAMETERS
  crit_val <- valore_critico(res$df)  # Student's t (df=n-p), converges to Z for large samples
  df_params <- data.frame(Parameter = names(res$parametri), Estimate = as.numeric(res$parametri), Std_Error = as.numeric(res$errori))
  df_params$CI_Parametric_Lower <- df_params$Estimate - (crit_val * df_params$Std_Error)
  df_params$CI_Parametric_Upper <- df_params$Estimate + (crit_val * df_params$Std_Error)
  
  # With Monte Carlo, it also adds the EMPIRICAL interval (direct quantiles
  # of the bootstrap distribution), which unlike the parametric one
  # above does not assume symmetry.
  if (input$metodo_errore == "monte_carlo" && !is.null(res$mat_boot_params)) {
    p_inf <- (1 - input$livello_confidenza / 100) / 2
    p_sup <- 1 - p_inf
    df_params$CI_Empirical_Lower <- apply(res$mat_boot_params, 2, quantile, probs = p_inf, na.rm = TRUE)
    df_params$CI_Empirical_Upper <- apply(res$mat_boot_params, 2, quantile, probs = p_sup, na.rm = TRUE)
  }
  
  df_fissi <- if (!is.null(res$fissi) && length(res$fissi) > 0) {
    data.frame(Parameter = names(res$fissi), Fixed_Value = as.numeric(res$fissi))
  } else {
    data.frame(Parameter = character(0), Fixed_Value = numeric(0))
  }
  
  # 3. DIAGNOSTICS
  safe_extract <- function(x) { if (is.null(x) || length(x) == 0) NA else as.numeric(x[1]) }
  valori_diagnostici <- sapply(list(
    res$obs, res$df, res$r_corr, res$r2, res$r2_adj, res$rmse, 
    res$cv_rmse, res$sse, res$mse, res$aic, res$aicc, res$bic, res$iter
  ), safe_extract)
  
  df_diag <- data.frame(
    Metric = c("Observations (n)", "Degrees of Freedom", "Correlation (r)", "R-squared", "Adj R-squared",
               "RMSE", "CV_RMSE_perc", "SSE", "MSE", "AIC", "AICc", "BIC", "Iterations"),
    Value = valori_diagnostici
  )
  
  # 4. STANDARD PREDICTIONS (on the input points)
  df_pred <- data.frame(
    Obs = 1:res$obs, X_Value = res$x, Y_Observed = res$df_originale$y,
    Y_Predicted = res$y_pred, Residual = res$residui, Absolute_Error = abs(res$residui),
    Relative_Error_Perc = ifelse(res$df_originale$y != 0, (abs(res$residui) / abs(res$df_originale$y)) * 100, NA)
  )
  
  # 5. DENSE 1000-POINT CURVE
  x_dense <- seq(min(res$x), max(res$x), length.out = 1000)
  y_dense <- res$valuta(res$parametri, x_dense)
  df_dense <- data.frame(X_Grid_Value = x_dense, Y_Predicted_Curve = y_dense)
  
  # 6. ALL N BOOTSTRAP PARAMETER SETS (only if Monte Carlo was used)
  df_boot_params <- if (!is.null(res$mat_boot_params) && nrow(res$mat_boot_params) > 0) {
    mat_boot <- res$mat_boot_params
    colnames(mat_boot) <- names(res$parametri)
    cbind(data.frame(Iteration = seq_len(nrow(mat_boot))), as.data.frame(mat_boot))
  } else {
    data.frame(Iteration = integer(0))
  }
  
  # 7. BOOTSTRAP DISTRIBUTION SUMMARY & NORMALITY DIAGNOSTICS - Monte Carlo only.
  # Same function as the on-screen table (riepilogo_distribuzione_bootstrap
  # in rendering_output.R), so screen and export cannot diverge;
  # here the values remain numeric and complete.
  df_normalita <- if (!is.null(res$mat_boot_params) && nrow(res$mat_boot_params) > 0) {
    do.call(rbind, lapply(riepilogo_distribuzione_bootstrap(res), function(r) data.frame(
      Parameter = r$nome, Estimate = r$stima, Mean = r$media, Median = r$mediana, Bootstrap_SE = r$se_boot, Bias = r$bias,
      Abs_Bias_over_SE = r$rapporto,
      W_Statistic = if (is.null(r$test)) NA_real_ else unname(r$test$statistic), P_Value = if (is.null(r$test)) NA_real_ else r$test$p.value,
      Skewness = r$skew, Excess_Kurtosis = r$kurt, stringsAsFactors = FALSE)))
  } else {
    data.frame(Parameter = character(0))
  }
  
  # 8. PARAMETER CORRELATION MATRIX (Hessian for Linear method, bootstrap for Monte Carlo)
  mat_corr <- calcola_matrice_correlazione(res)
  df_correlazione <- if (!is.null(mat_corr)) {
    cbind(Parameter = rownames(mat_corr), as.data.frame(mat_corr))
  } else {
    data.frame(Parameter = character(0))
  }
  
  # 9. PARAMETER SENSITIVITY ANALYSIS (normalized, at every X of the dense curve)
  df_sensibilita <- tryCatch({
    n_par <- length(res$parametri)
    y_base <- res$valuta(res$parametri, x_dense)
    sens <- data.frame(X_Grid_Value = x_dense)
    for (j in seq_len(n_par)) {
      h_j <- passo_derivata(res$parametri[j])
      p_piu <- res$parametri; p_piu[j] <- p_piu[j] + h_j
      p_meno <- res$parametri; p_meno[j] <- p_meno[j] - h_j
      dydtheta <- (res$valuta(p_piu, x_dense) - res$valuta(p_meno, x_dense)) / (2 * h_j)
      sens[[paste0("Sensitivity_", names(res$parametri)[j])]] <- ifelse(abs(y_base) > 1e-10, dydtheta * (res$parametri[j] / y_base), NA)
    }
    sens
  }, error = function(e) data.frame(X_Grid_Value = numeric(0)))
  
  c(list(Model_Info = df_info, Parameters = df_params, Fixed_Parameters = df_fissi, Diagnostics = df_diag, 
       Simulated_Curve_1000pts = df_dense, Predictions = df_pred, Monte_Carlo_Bootstrap_Sets = df_boot_params,
       Bootstrap_Summary = df_normalita, Correlation_Matrix = df_correlazione, Sensitivity_Analysis = df_sensibilita),
    tabelle_diagnostica_residui(tryCatch(diagnostica_residui(res), error = function(e) NULL)))
}

# Data of the Loss Surface Explorer (grid search), exported separately
# because optional (they exist only if the user has actually launched
# at least one search in that section).
prepara_dati_grid_search_export <- function() {
  dati_grid <- tryCatch(griglia_attiva(), error = function(e) NULL)
  if (is.null(dati_grid) || !isTRUE(dati_grid$successo)) return(NULL)
  
  # "Long" format: one row for each combination of the two parameters
  griglia_lunga <- expand.grid(Par1_Value = dati_grid$seq1, Par2_Value = dati_grid$seq2)
  griglia_lunga$Loss_Fast <- as.vector(dati_grid$mat_loss)
  if (!is.null(dati_grid$mat_loss_rigorosa)) griglia_lunga$Loss_Rigorous <- as.vector(dati_grid$mat_loss_rigorosa)
  colnames(griglia_lunga)[1:2] <- c(dati_grid$par1, dati_grid$par2)
  
  df_info_grid <- data.frame(Property = c("Parameter 1", "Parameter 2"), Value = c(dati_grid$par1, dati_grid$par2))
  
  # CI comparison table (F-test/Classic/Monte Carlo) already computed
  # by the shared reactive - same source used by the on-screen table
  # and by the two bar charts, so the export is always consistent with
  # what is visible in the app. It automatically also includes any
  # other profiled parameters (if the checkbox "Also compute Model Comparison CI for
  # every other parameter" was ticked at the moment of Run Rigorous Profile),
  # since ci_marginali_grid_dati() now returns one row for each
  # available parameter, not only the 2 chosen for the grid.
  d_ci <- tryCatch(ci_marginali_grid_dati(), error = function(e) list(successo = FALSE))
  df_ci_confronto <- if (isTRUE(d_ci$successo)) {
    formatta_num <- function(v) if (is.na(v)) NA_real_ else round(v, 5)
    df_righe <- lapply(d_ci$righe, function(r) {
      riga <- data.frame(
        Parameter = r$par,
        Estimate = round(r$stima, 5),
        Profile_CI_Lower = formatta_num(r$profile_inf$valore),
        Profile_CI_Upper = formatta_num(r$profile_sup$valore),
        Classic_CI_Lower = formatta_num(r$classico_inf),
        Classic_CI_Upper = formatta_num(r$classico_sup),
        stringsAsFactors = FALSE
      )
      if (isTRUE(d_ci$ha_montecarlo)) {
        riga$MonteCarlo_CI_Lower <- formatta_num(r$mc_inf)
        riga$MonteCarlo_CI_Upper <- formatta_num(r$mc_sup)
      }
      riga
    })
    do.call(rbind, df_righe)
  } else {
    data.frame(Parameter = character(0))
  }
  
  list(Grid_Info = df_info_grid, Grid_Values = griglia_lunga, Grid_CI_Comparison = df_ci_confronto)
}

output$scarica_csv <- downloadHandler(
  filename = function() { paste0("regression_results_", Sys.Date(), ".csv") },
  content = function(file) {
    dati <- prepara_dati_export()
    if(is.null(dati)) return()
    cat("--- MODEL INFO ---\n", file = file)
    write.table(dati$Model_Info, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat("\n--- PARAMETER ESTIMATION RESULTS ---\n", file = file, append = TRUE)
    write.table(dati$Parameters, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    if (nrow(dati$Fixed_Parameters) > 0) {
      cat("\n--- FIXED PARAMETERS (EXCLUDED FROM OPTIMIZATION) ---\n", file = file, append = TRUE)
      write.table(dati$Fixed_Parameters, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    cat("\n--- GOODNESS-OF-FIT DIAGNOSTICS ---\n", file = file, append = TRUE)
    write.table(dati$Diagnostics, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat("\n--- PREDICTIONS & RESIDUALS (OBSERVED DATA) ---\n", file = file, append = TRUE)
    write.table(dati$Predictions, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat("\n--- DENSE SIMULATED CURVE (1000 POINTS) ---\n", file = file, append = TRUE)
    write.table(dati$Simulated_Curve_1000pts, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    if (nrow(dati$Monte_Carlo_Bootstrap_Sets) > 0) {
      cat("\n--- MONTE CARLO BOOTSTRAP PARAMETER SETS (ALL ITERATIONS) ---\n", file = file, append = TRUE)
      write.table(dati$Monte_Carlo_Bootstrap_Sets, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    if (nrow(dati$Bootstrap_Summary) > 0) {
      cat("\n--- BOOTSTRAP DISTRIBUTION SUMMARY & NORMALITY DIAGNOSTICS (MEAN, MEDIAN, BOOTSTRAP SE, BIAS, SHAPIRO-WILK, SKEWNESS, EXCESS KURTOSIS) ---\n", file = file, append = TRUE)
      write.table(dati$Bootstrap_Summary, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    if (nrow(dati$Correlation_Matrix) > 0) {
      cat("\n--- PARAMETER CORRELATION MATRIX ---\n", file = file, append = TRUE)
      write.table(dati$Correlation_Matrix, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    if (nrow(dati$Sensitivity_Analysis) > 0) {
      cat("\n--- PARAMETER SENSITIVITY ANALYSIS ---\n", file = file, append = TRUE)
      write.table(dati$Sensitivity_Analysis, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    for (sez in list(c("Residual_Autocorrelation", "RESIDUAL DIAGNOSTICS: LAG-1 AUTOCORRELATION"), c("Runs_Test", "RESIDUAL DIAGNOSTICS: RUNS TEST"), c("Lack_of_Fit", "REPLICATE DIAGNOSTICS: LACK-OF-FIT TEST"),
                     c("Variance_Homogeneity", "REPLICATE DIAGNOSTICS: VARIANCE HOMOGENEITY TESTS"), c("Replicates", "REPLICATE DIAGNOSTICS: REPLICATES PER X"))) {
      if (!is.null(dati[[sez[1]]]) && nrow(dati[[sez[1]]]) > 0) {
        cat(paste0("\n--- ", sez[2], " ---\n"), file = file, append = TRUE)
        write.table(dati[[sez[1]]], file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE, na = "")
      }
    }
    dati_grid_export <- prepara_dati_grid_search_export()
    if (!is.null(dati_grid_export)) {
      cat("\n--- LOSS SURFACE EXPLORER (GRID SEARCH) ---\n", file = file, append = TRUE)
      write.table(dati_grid_export$Grid_Info, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n", file = file, append = TRUE)
      write.table(dati_grid_export$Grid_Values, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      if (nrow(dati_grid_export$Grid_CI_Comparison) > 0) {
        cat("\n--- LOSS SURFACE EXPLORER: CONFIDENCE INTERVAL COMPARISON (PROFILE VS CLASSIC VS MONTE CARLO) ---\n", file = file, append = TRUE)
        write.table(dati_grid_export$Grid_CI_Comparison, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      }
    }
  }
)

output$scarica_excel <- downloadHandler(
  filename = function() { paste0("regression_results_", Sys.Date(), ".xlsx") },
  content = function(file) {
    dati <- prepara_dati_export()
    if(is.null(dati)) return()
    dati_grid_export <- prepara_dati_grid_search_export()
    if (!is.null(dati_grid_export)) {
      dati$Grid_Search_Info <- dati_grid_export$Grid_Info
      dati$Grid_Search_Values <- dati_grid_export$Grid_Values
      if (nrow(dati_grid_export$Grid_CI_Comparison) > 0) dati$Grid_CI_Comparison <- dati_grid_export$Grid_CI_Comparison
    }
    writexl::write_xlsx(per_excel(dati), path = file)
  }
)

# ---- ZIP download of every plot currently available in Single Fit Analysis ----
output$scarica_tutti_grafici_zip <- downloadHandler(
  filename = function() paste0("graphical_analysis_plots_", Sys.Date(), ".zip"),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    formato <- input$formato_esportazione
    
    cartella_tmp <- tempfile()
    dir.create(cartella_tmp)
    
    # Saves a single plot to file, in a safe way (does not block the whole
    # zip if a specific plot fails - it just skips it).
    salva_grafico <- function(nome_file, funzione_disegno) {
      percorso <- file.path(cartella_tmp, paste0(nome_file, ".", formato))
      tryCatch({
        if (formato == "pdf") pdf(percorso, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(percorso, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        funzione_disegno()
        dev.off()
      }, error = function(e) {
        if (!is.null(dev.list())) dev.off()
      })
    }
    
    if (!is.null(res) && isTRUE(res$successo)) {
      df_dati <- dati()
      salva_grafico("01_main_fit_plot", function() disegna_grafico_fit(df_dati, res))
      salva_grafico("02_residuals_plot", function() disegna_grafico_residui(res))
      salva_grafico("03_qq_plot", function() disegna_grafico_qq(res))
      salva_grafico("03b_lag_plot", function() disegna_grafico_lag(res))
      salva_grafico("03c_variance_structure_plot", function() disegna_grafico_sd_media(res))
      if (input$metodo_errore == "monte_carlo") {
        salva_grafico("04_parameter_distributions", function() disegna_grafico_distribuzioni(res))
        salva_grafico("05_bootstrap_parameter_cloud", function() disegna_pairs_bootstrap(res))
      }
      salva_grafico("06_sensitivity_analysis", function() disegna_grafico_sensibilita(res))
      
      dati_grid <- tryCatch(griglia_attiva(), error = function(e) NULL)
      if (!is.null(dati_grid) && isTRUE(dati_grid$successo)) {
        salva_grafico("07_loss_surface_3d", function() disegna_grid_3d(dati_grid))
        salva_grafico("08_loss_surface_contour", function() disegna_grid_contour(dati_grid, res))
        d_ci_zip <- tryCatch(ci_marginali_grid_dati(), error = function(e) list(successo = FALSE))
        if (isTRUE(d_ci_zip$successo)) {
          for (idx_ci in seq_along(d_ci_zip$righe)) {
            local({
              idx_locale <- idx_ci
              salva_grafico(sprintf("%02d_loss_surface_ci_comparison_%s", 8 + idx_locale, d_ci_zip$righe[[idx_locale]]$par),
                           function() disegna_barre_ci_grid(idx_locale))
            })
          }
        }
      }
    }
    
    file_generati <- list.files(cartella_tmp)
    if (length(file_generati) == 0) {
      writeLines("No plots available yet: run a successful optimization first.", file.path(cartella_tmp, "README.txt"))
      file_generati <- list.files(cartella_tmp)
    }
    
    cartella_corrente <- getwd()
    on.exit(setwd(cartella_corrente))
    setwd(cartella_tmp)
    zip::zip(zipfile = file, files = file_generati)
  }
)