# =========================================================================
# INVERSE PREDICTION (FIND X FROM Y) + ERROR ESTIMATION
# =========================================================================

# ---- Adapter: transforms a record saved in Model Comparison into an
# object compatible with what the rest of Inverse Prediction
# expects from elaborazione() (the current single fit) - allows reusing
# exactly the same computation logic on a fit chosen from the accumulated
# list, instead of only on the one active in the sidebar.
adatta_snapshot_a_res <- function(snapshot) {
  valuta_fn <- function(parametri, x) valuta_formula_generica(snapshot$formula, as.list(parametri), x)
  residui_calcolati <- snapshot$y_dati - valuta_fn(snapshot$parametri, snapshot$x_dati)
  
  # Rebuilds the Monte Carlo bands (curve_boot for the CI, pred_boot for
  # the PI) by evaluating the model, for EVERY row of the parameter bootstrap
  # already saved in the record, on the same x_griglia grid already saved -
  # without this, disegna_grafico_fit() (the same function used by the single
  # fit) finds neither x_griglia nor these matrices and draws neither
  # the curve nor the bands, showing only the raw points (the reported bug).
  curve_boot_ricostruito <- pred_boot_ricostruito <- NULL
  if (!is.null(snapshot$mat_boot) && !is.null(snapshot$x_griglia) && nrow(snapshot$mat_boot) >= 10) {
    curve_boot_ricostruito <- t(apply(snapshot$mat_boot, 1, function(riga_par) {
      valuta_fn(riga_par, snapshot$x_griglia)
    }))
    # Noise of a new observation generated as in the Single Fit: Gaussian with the sigma
    # used there, or the resampled residuals of its Monte Carlo (kept with the saved
    # fit). Fits saved by earlier versions, without this information: Gaussian with rmse.
    n_rumore <- length(curve_boot_ricostruito)
    valori_rumore <- if (identical(snapshot$tipo_rumore_mc, "gaussiano") && length(snapshot$sigma_mc) && is.finite(snapshot$sigma_mc)) {
      rnorm(n_rumore, mean = 0, sd = snapshot$sigma_mc)
    } else if (length(snapshot$residui_bootstrap)) {
      sample(snapshot$residui_bootstrap, n_rumore, replace = TRUE)
    } else if (!is.null(snapshot$rmse) && is.finite(snapshot$rmse)) {
      rnorm(n_rumore, mean = 0, sd = snapshot$rmse)
    } else NULL
    if (!is.null(valori_rumore)) {
      rumore_pred <- matrix(valori_rumore, nrow = nrow(curve_boot_ricostruito))
      # Weighted fit: noise brought back to the scale of each point of the curve.
      if (isTRUE(snapshot$ponderazione$attiva) && !is.null(snapshot$pesi))
        rumore_pred <- rumore_pred * sqrt(ifelse(curve_boot_ricostruito > 0, curve_boot_ricostruito^snapshot$ponderazione$zeta, NA_real_))
      pred_boot_ricostruito <- curve_boot_ricostruito + rumore_pred
    }
  }
  
  list(
    successo = TRUE,
    df_originale = data.frame(x = snapshot$x_dati, y = snapshot$y_dati),
    valuta = valuta_fn,
    parametri = snapshot$parametri,
    errori = snapshot$errori,
    df = snapshot$df,
    x_griglia = snapshot$x_griglia,
    mat_boot_params = snapshot$mat_boot,
    curve_boot = curve_boot_ricostruito,
    pred_boot = pred_boot_ricostruito,
    residui = residui_calcolati,
    residui_bootstrap = snapshot$residui_bootstrap,
    cov = snapshot$cov,
    rmse = snapshot$rmse,
    ponderazione = snapshot$ponderazione,
    pesi = snapshot$pesi
  )
}

# "Which fit to use" selector for Inverse Prediction: the current fit in the
# sidebar (default, behaviour unchanged with respect to before) or one
# of the records accumulated in Model Comparison.
output$ip_selettore_modello_ui <- renderUI({
  lista <- confronto_modelli()
  scelte <- c("Current Sidebar Fit" = "current")
  if (length(lista) > 0) {
    valori_indice <- as.character(seq_along(lista))
    names(valori_indice) <- sapply(lista, function(s) s$label)
    scelte <- c(scelte, valori_indice)
  }
  selectInput("ip_scelta_modello", "Run Inverse Prediction on:", choices = scelte, selected = "current")
})

res_scelto_ip <- reactive({
  if (is.null(input$ip_scelta_modello) || input$ip_scelta_modello == "current") {
    tryCatch(elaborazione(), error = function(e) NULL)
  } else {
    idx <- suppressWarnings(as.integer(input$ip_scelta_modello))
    lista <- confronto_modelli()
    if (is.na(idx) || idx < 1 || idx > length(lista)) return(NULL)
    tryCatch(adatta_snapshot_a_res(lista[[idx]]), error = function(e) NULL)
  }
})

# Intro paragraph and section headers reflect the custom X/Y axis labels
# (sidebar, "2. Axis Labels & Style") instead of a hardcoded "X"/"Y".
output$intro_inversa <- renderUI({
  p(style = "color: #2c3e50; font-size: 14px;",
    "Given a target ", tags$strong(input$label_y), " value, this tool numerically solves the fitted model for the corresponding ", 
    tags$strong(input$label_x), " (root-finding), and estimates the uncertainty on that ", input$label_x, " value. Requires a successful optimization (Run Optimization Calculation).")
})

output$titolo_grafico_inversa <- renderUI({
  tags$h4(paste0("Fit Plot with Predicted ", input$label_x, ":"))
})

output$titolo_grafico_distx <- renderUI({
  tags$h4(paste0("Distribution of ", input$label_x, " (Monte Carlo):"))
})

# Updates the numeric field / button labels to reflect the custom axis
# names, WITHOUT resetting whatever value the user already typed (using
# update*Input() instead of rebuilding the widgets via renderUI, which
# would reset them to their default value on every keystroke).
observeEvent(c(input$label_x, input$label_y), {
  updateNumericInput(session, "y_target_inversa", label = paste0("Target ", input$label_y, " value:"))
  updateNumericInput(session, "x_search_min_inversa", label = paste0(input$label_x, " Search Range Min:"))
  updateNumericInput(session, "x_search_max_inversa", label = paste0(input$label_x, " Search Range Max:"))
  updateActionButton(session, "calcola_inversa", label = paste0("Calculate ", input$label_x))
}, ignoreInit = FALSE)

output$sezione_risultato_inversa_ui <- renderUI({
  risultato <- tryCatch(calcolo_inversa(), error = function(e) NULL)
  if (is.null(risultato)) return(NULL)
  tagList(
    br(), br(),
    tags$h4("Result:"),
    verbatimTextOutput("risultato_inversa"),
    p(style = "font-size: 11px; color: #e67e22; line-height: 1.4;",
      tags$strong("Note: "), "the search range is scanned for every crossing of the target Y. If there is more than one, the smallest X is returned and the others are listed in the result; narrow the search range to select another one."),
    br(),
    uiOutput("titolo_grafico_inversa"),
    p(style = "font-size: 13px; color: #7f8c8d;",
      "Same plot as \"Single Fit Analysis\" when using the current sidebar fit; when a saved model is chosen instead, its own data and curve. The predicted X is highlighted by a crosshair, with its confidence interval (if available) shown as a horizontal error bar. If the predicted X falls outside the current Min X/Max X display range (sidebar), the crosshair won't be visible \u2014 widen that range to see it."),
    plotOutput("grafico_inversa", height = "420px"),
    div(style = "margin-top: 8px; margin-bottom: 15px;", downloadButton("scarica_grafico_inversa", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_fit_inv", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
    conditionalPanel(
      condition = "input.metodo_errore == 'monte_carlo'",
      br(),
      uiOutput("titolo_grafico_distx"),
      plotOutput("grafico_distribuzione_x", height = "350px"),
      div(style = "margin-top: 8px;", downloadButton("scarica_grafico_dist_x", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_distx", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
      br(),
      verbatimTextOutput("test_normalita_inversa")
    )
  )
})

calcolo_inversa <- eventReactive(input$calcola_inversa, {
  res <- res_scelto_ip()
  if (is.null(res) || is.null(res$successo) || !res$successo) {
    return(list(successo = FALSE, messaggio = "No successful fit available for the selection above — run the sidebar's optimization, or choose a different saved model."))
  }
  
  y_target <- input$y_target_inversa
  if (is.null(y_target) || is.na(y_target)) {
    return(list(successo = FALSE, messaggio = "Please enter a valid target Y value."))
  }
  
  # ---- Search range: the X Search Range fields, pre-filled with the X range of
  # the data whenever the dataset changes. If they are empty or not valid the
  # calculation does not start (previously a range extended by 50% on each side
  # was used, which could include meaningless values, e.g. negative times).
  if (!range_personalizzato_attivo(input$x_search_min_inversa, input$x_search_max_inversa)) {
    return(list(successo = FALSE, messaggio = "Please enter a valid X search range (Min smaller than Max). The fields are pre-filled with the X range of your data."))
  }
  intervallo_ricerca <- c(input$x_search_min_inversa, input$x_search_max_inversa)
  
  funzione_obiettivo <- function(x) res$valuta(res$parametri, x) - y_target
  ricerca <- trova_soluzioni_x(funzione_obiettivo, intervallo_ricerca[1], intervallo_ricerca[2])
  if (length(ricerca$soluzioni) == 0) {
    intervallo_curva <- ricerca$intervallo_obiettivo + y_target
    return(list(successo = FALSE, messaggio = paste0(
      "The model does not reach Y = ", signif(y_target, 6), " within the X search range [", signif(intervallo_ricerca[1], 6), ", ", signif(intervallo_ricerca[2], 6), "]",
      if (all(is.finite(intervallo_curva))) paste0(": in this range the predicted Y goes from ", signif(intervallo_curva[1], 6), " to ", signif(intervallo_curva[2], 6)) else "",
      ". Check the target Y, or widen the search range (solutions outside the range of the data are extrapolations).")))
  }
  # With more than one solution, the smallest X is returned and the others are reported.
  x0 <- ricerca$soluzioni[1]
  altre_soluzioni <- ricerca$soluzioni[-1]
  
  # ---- Error estimation on x0 ----
  z_val <- valore_critico(res$df)  # Student's t (df=n-p), converges to Z for large samples
  errore_x <- NA
  ci_inf <- NA
  ci_sup <- NA
  metodo_errore_usato <- ""
  
  if (input$metodo_errore == "monte_carlo" && !is.null(res$mat_boot_params) && nrow(res$mat_boot_params) > 1) {
    
    # ---- Monte Carlo method: solve for x0 for each bootstrap parameter set ----
    n_boot_tot <- nrow(res$mat_boot_params)
    x0_boot <- rep(NA_real_, n_boot_tot)
    
    for (i in 1:n_boot_tot) {
      p_boot <- res$mat_boot_params[i, ]
      if (any(is.na(p_boot))) next
      names(p_boot) <- names(res$parametri)
      
      y_target_iter <- if (input$tipo_intervallo_inversa == "predizione") {
        # With weighted regression: weighted residual brought back to the scale of the
        # sought value (variance sigma^2 y_target^zeta); unchanged without weights.
        # Same residuals as the prediction band (leverage-corrected, kept by the
        # Monte Carlo of the fit); previously the raw residuals, which are smaller
        # than the errors, giving a slightly too narrow interval.
        y_target + sample(if (length(res$residui_bootstrap)) res$residui_bootstrap else residui_diagnostici(res), 1) * sqrt(varianza_relativa(res, y_target))
      } else {
        y_target
      }
      
      # Solution of each bootstrap curve: the one closest to the main solution,
      # so that all iterations refer to the same crossing of the target.
      obiettivo_boot <- function(x) res$valuta(p_boot, x) - y_target_iter
      x0_boot[i] <- soluzione_piu_vicina(trova_soluzioni_x(obiettivo_boot, intervallo_ricerca[1], intervallo_ricerca[2])$soluzioni, x0)
    }
    
    x0_boot <- x0_boot[is.finite(x0_boot)]
    
    if (length(x0_boot) >= 10) {
      p_inf <- (1 - input$livello_confidenza / 100) / 2
      p_sup <- 1 - p_inf
      ci_inf <- as.numeric(quantile(x0_boot, probs = p_inf, na.rm = TRUE))
      ci_sup <- as.numeric(quantile(x0_boot, probs = p_sup, na.rm = TRUE))
      errore_x <- sd(x0_boot, na.rm = TRUE)
      metodo_errore_usato <- sprintf("Monte Carlo (Bootstrap, %d/%d successful iterations)", length(x0_boot), n_boot_tot)
    } else {
      metodo_errore_usato <- sprintf("Monte Carlo bootstrap produced too few valid solutions (%d/%d); error not available. Try widening the X Search Range.", length(x0_boot), n_boot_tot)
    }
    
  } else {
    
    # ---- Delta method (linear approximation via error propagation) ----
    if (!is.null(res$cov) && !anyNA(res$cov)) {
      h_x <- passo_derivata(x0)
      
      df_dx <- (res$valuta(res$parametri, x0 + h_x) - res$valuta(res$parametri, x0 - h_x)) / (2 * h_x)
      
      gradiente_par <- as.numeric(gradiente_parametri(res, x0))
      
      var_param <- as.numeric(t(gradiente_par) %*% res$cov %*% gradiente_par)
      var_y <- if (input$tipo_intervallo_inversa == "predizione") res$rmse^2 * varianza_relativa(res, y_target) else 0
      
      if (!is.na(df_dx) && abs(df_dx) > 1e-12) {
        var_x0 <- (var_y + var_param) / (df_dx^2)
        errore_x <- sqrt(var_x0)
        ci_inf <- x0 - z_val * errore_x
        ci_sup <- x0 + z_val * errore_x
        metodo_errore_usato <- "Linear Approximation (Delta Method)"
      } else {
        metodo_errore_usato <- "The model's derivative is nearly zero at this X (flat curve); the Delta Method is unreliable here. Try the Monte Carlo error method instead."
      }
    } else {
      metodo_errore_usato <- "Standard errors are not available for this fit; cannot estimate uncertainty on X."
    }
  }
  
  list(successo = TRUE, x0 = x0, altre_soluzioni = altre_soluzioni, y_target = y_target, errore_x = errore_x,
       ci_inf = ci_inf, ci_sup = ci_sup, metodo = metodo_errore_usato,
       intervallo_ricerca = intervallo_ricerca,
       x0_boot = if (exists("x0_boot", inherits = FALSE)) x0_boot else NULL)
})

output$risultato_inversa <- renderPrint({
  risultato <- tryCatch(calcolo_inversa(), error = function(e) NULL)
  if (is.null(risultato)) {
    cat("Enter a target Y value and click 'Calculate X'.")
    return(invisible(NULL))
  }
  if (!risultato$successo) {
    cat("ERROR:", risultato$messaggio)
    return(invisible(NULL))
  }
  
  cat(sprintf("Target %s value: %.5f\n", input$label_y, risultato$y_target))
  cat(sprintf("Search Range Used: [%.5f, %.5f]\n\n", risultato$intervallo_ricerca[1], risultato$intervallo_ricerca[2]))
  if (length(risultato$altre_soluzioni) > 0)
    cat(sprintf("NOTE: the target Y is reached %d times in the search range (X = %s); the result refers to X = %s. Narrow the search range to select another solution.\n\n",
                length(risultato$altre_soluzioni) + 1, paste(signif(c(risultato$x0, risultato$altre_soluzioni), 6), collapse = ", "), signif(risultato$x0, 6)))
  cat(sprintf("Estimated %s value: %.5f\n", input$label_x, risultato$x0))
  
  if (!is.na(risultato$errore_x)) {
    cat(sprintf("Standard Error of %s: %.5f\n", input$label_x, risultato$errore_x))
    cat(sprintf("%s%% Confidence Interval: [%.5f, %.5f]\n", input$livello_confidenza, risultato$ci_inf, risultato$ci_sup))
  }
  
  cat(sprintf("\nError Estimation Method: %s\n", risultato$metodo))
})

disegna_grafico_inversa <- function() {
  res <- res_scelto_ip()
  if (is.null(res) || !isTRUE(res$successo)) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "No successful fit available for the selection above.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  df <- res$df_originale
  disegna_grafico_fit(df, res)
  
  risultato <- tryCatch(calcolo_inversa(), error = function(e) NULL)
  if (!is.null(risultato) && isTRUE(risultato$successo)) {
    col_evid <- "#8e44ad"
    
    abline(v = risultato$x0, col = col_evid, lwd = 2, lty = 2)
    abline(h = risultato$y_target, col = col_evid, lwd = 2, lty = 2)
    
    if (!is.na(risultato$errore_x) && !is.na(risultato$ci_inf) && !is.na(risultato$ci_sup)) {
      suppressWarnings(arrows(risultato$ci_inf, risultato$y_target, risultato$ci_sup, risultato$y_target,
                              angle = 90, code = 3, length = 0.08, col = col_evid, lwd = 2.5))
    }
    
    points(risultato$x0, risultato$y_target, pch = 18, col = col_evid, cex = 2.2)
    
    cex_legenda_inv <- if (is.null(input$inv_cex_legenda)) 0.9 else input$inv_cex_legenda
    pos_legenda_inv <- if (is.null(input$inv_pos_legenda)) "bottomleft" else input$inv_pos_legenda
    legend(pos_legenda_inv, legend = sprintf("Predicted %s = %.4f", input$label_x, risultato$x0),
           pch = 18, col = col_evid, bty = "n", cex = cex_legenda_inv)
  }
}

output$grafico_inversa <- renderPlot({
  disegna_grafico_inversa()
})

output$scarica_grafico_inversa <- downloadHandler(
  filename = function() paste0("inverse_prediction_plot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_inversa()
    dev.off()
  }
)

disegna_grafico_distribuzione_x <- function() {
  risultato <- tryCatch(calcolo_inversa(), error = function(e) NULL)
  
  if (is.null(risultato) || !risultato$successo || is.null(risultato$x0_boot) || length(risultato$x0_boot) < 10) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "Distribution not available (run a Monte Carlo calculation with at least 10 valid iterations).", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  bar_col <- if(is.null(input$bar_col_dist)) "#f39c12" else input$bar_col_dist
  dns_col <- if(is.null(input$dens_col_dist)) "black" else input$dens_col_dist
  vln_col <- if(is.null(input$vln_col_dist)) "black" else input$vln_col_dist
  fnt_cex <- if(is.null(input$fnt_cex_dist)) 1.0 else input$fnt_cex_dist
  
  titolo_locale <- if(input$mostra_titolo) paste0("Bootstrap Distribution of Predicted ", input$label_x) else ""
  
  limiti_x_distx <- if (range_personalizzato_attivo(input$range_x_min_distx, input$range_x_max_distx)) {
    c(input$range_x_min_distx, input$range_x_max_distx)
  } else {
    range(risultato$x0_boot, na.rm = TRUE)
  }
  limiti_y_distx <- if (range_personalizzato_attivo(input$range_y_min_distx, input$range_y_max_distx)) c(input$range_y_min_distx, input$range_y_max_distx) else NULL
  
  limiti_x_label <- interpreta_etichetta(input$label_x)
  xlab_distx <- if (is.character(limiti_x_label)) paste(limiti_x_label, "(predicted)") else bquote(.(limiti_x_label) ~ "(predicted)")
  
  par(mar = c(5, 5, if(input$mostra_titolo) 4 else 2, 2) + 0.1)
  n_bins_distx <- if (is.null(input$n_bins_dist)) 30 else input$n_bins_dist
  hist(risultato$x0_boot, breaks = n_bins_distx, probability = TRUE, main = titolo_locale,
       xlab = xlab_distx, col = bar_col, border = "white", 
       las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, xlim = limiti_x_distx, ylim = limiti_y_distx)
  # Same controls as the aesthetic panel shared with Parameter
  # Distributions (Single Fit): show/hide, type and width of the line
  # of the estimate. The dashed lines of the confidence interval remain.
  vln_lty_dist <- as.numeric(if (is.null(input$vln_lty_dist)) "2" else input$vln_lty_dist)
  vln_lwd_dist <- if (is.null(input$vln_lwd_dist)) 2 else input$vln_lwd_dist
  if (is.null(input$mostra_densita_dist) || isTRUE(input$mostra_densita_dist)) lines(density(risultato$x0_boot, na.rm = TRUE), col = dns_col, lwd = 2)
  if (is.null(input$mostra_vln_dist) || isTRUE(input$mostra_vln_dist)) abline(v = risultato$x0, col = vln_col, lwd = vln_lwd_dist, lty = vln_lty_dist)
  abline(v = c(risultato$ci_inf, risultato$ci_sup), col = vln_col, lwd = 1.5, lty = 3)
}

output$test_normalita_inversa <- renderPrint({
  risultato <- tryCatch(calcolo_inversa(), error = function(e) NULL)
  if (is.null(risultato) || !isTRUE(risultato$successo) || is.null(risultato$x0_boot) || length(risultato$x0_boot) < 3) {
    cat("Not available (run a Monte Carlo calculation with at least 3 valid iterations).")
    return(invisible(NULL))
  }
  valori_completi <- risultato$x0_boot
  skew <- calcola_skewness(valori_completi)
  kurt <- calcola_kurtosi_eccesso(valori_completi)
  valori <- valori_completi
  if (length(valori) > 5000) valori <- sample(valori, 5000)  # shapiro.test() accepts at most 5000 observations
  test <- tryCatch(shapiro.test(valori), error = function(e) NULL)
  if (is.null(test)) {
    cat("Normality test could not be computed for this distribution.")
    return(invisible(NULL))
  }
  cat(sprintf("Shapiro-Wilk Normality Test on Predicted %s: W = %.4f, p = %s\n", 
              input$label_x, unname(test$statistic), format.pval(test$p.value, digits = 3, eps = 0.001)))
  cat(sprintf("Skewness = %.3f, Excess Kurtosis = %.3f (both close to 0 = close to normal; unlike the p-value, these don't grow more \"significant\" with more bootstrap iterations)\n", skew, kurt))
  if (test$p.value < 0.05) {
    cat("The bootstrap distribution significantly deviates from normal (p < 0.05).\n")
    cat("Note: the Confidence Interval reported above already uses the empirical (quantile-based) bounds, not a symmetric approximation, so it remains valid despite this.")
  } else {
    cat("Consistent with a normal distribution (p >= 0.05).")
  }
})

output$grafico_distribuzione_x <- renderPlot({
  disegna_grafico_distribuzione_x()
})

output$scarica_grafico_dist_x <- downloadHandler(
  filename = function() paste0("x_distribution_plot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_distribuzione_x()
    dev.off()
  }
)