# ============================
# GRAPHICAL FUNCTIONS
# ============================
disegna_grafico_simulazione <- function(res) {
  titolo_main <- if(input$mostra_titolo) "Interactive Modeler: Initial Guess Curve Preview" else ""
  if (is.null(res)) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = if(input$mostra_titolo) "Click 'Simulate...' to preview" else "", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  if (!res$successo) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = if(input$mostra_titolo) "Simulation halted due to errors." else "", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  pt_col  <- if(is.null(input$pt_col_sim)) "#bdc3c7" else input$pt_col_sim
  ln_col  <- if(is.null(input$ln_col_sim)) "#e74c3c" else input$ln_col_sim
  pt_cex  <- if(is.null(input$pt_cex_sim)) 1.4 else input$pt_cex_sim
  ln_lwd  <- if(is.null(input$ln_lwd_sim)) 3.5 else input$ln_lwd_sim
  grid_on <- if(is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  pt_pch  <- as.numeric(if(is.null(input$pt_pch_sim)) "16" else input$pt_pch_sim)
  fnt_cex <- if(is.null(input$fnt_cex_sim)) 1.0 else input$fnt_cex_sim
  mostra_legenda_sim <- if(is.null(input$sim_mostra_legenda)) TRUE else input$sim_mostra_legenda
  
  par(mar = c(5, 5, if(input$mostra_titolo) 4 else 2, 2) + 0.1)
  limiti_x <- range(res$x)
  mostra_punti_dati <- if (is.null(input$sim_mostra_punti_dati)) TRUE else input$sim_mostra_punti_dati
  mostra_punti <- mostra_punti_dati && !is.null(res$punti_exp) && nrow(res$punti_exp) > 0
  
  if (range_personalizzato_attivo(input$range_y_min_sim, input$range_y_max_sim)) {
    limiti_y <- c(input$range_y_min_sim, input$range_y_max_sim)
  } else {
    limiti_y <- range(res$y[is.finite(res$y)])
    if (mostra_punti) limiti_y <- range(c(limiti_y, res$punti_exp$y[is.finite(res$punti_exp$y)]), na.rm = TRUE)
  }
  
  plot(1, type = "n", xlim = limiti_x, ylim = limiti_y, xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y),
       main = titolo_main, cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1)
  if(grid_on) grid()
  
  # History of the previous curves (if the "Keep previous curves" option is
  # active) - drawn BEFORE the current curve, in a lighter
  # colour, so they remain visible as a "trace" under the current curve
  # without covering it.
  storico <- tryCatch(storico_curve_simulate(), error = function(e) NULL)
  if (!is.null(storico) && length(storico) > 0) {
    for (curva_vecchia in storico) {
      lines(curva_vecchia$x, curva_vecchia$y, col = adjustcolor(ln_col, alpha.f = 0.25), lwd = max(1, ln_lwd - 1))
    }
  }
  
  cex_legenda_sim <- if (is.null(input$sim_cex_legenda)) 1.0 else input$sim_cex_legenda
  pos_legenda_sim <- if (is.null(input$sim_pos_legenda)) "topright" else input$sim_pos_legenda
  if (mostra_punti) {
    points(res$punti_exp$x, res$punti_exp$y, col = pt_col, pch = pt_pch, cex = pt_cex)
    if (mostra_legenda_sim) {
      legend(pos_legenda_sim, legend = c("Simulated Curve", "Experimental Points"), 
             col = c(ln_col, pt_col), lty = c(1, NA), pch = c(NA, pt_pch), lwd = c(ln_lwd, NA), bty = "o", bg = "white", cex = cex_legenda_sim)
    }
  } else if (mostra_legenda_sim) {
    legend(pos_legenda_sim, legend = "Simulated Curve", col = ln_col, lty = 1, lwd = ln_lwd, bty = "o", bg = "white", cex = cex_legenda_sim)
  }
  lines(res$x, res$y, col = ln_col, lwd = ln_lwd, lty = 1)
}

# ---- Confidence and prediction bands of the main plot ----
# Returns the x grid, the curve and the requested bands (NULL if not
# requested or not available). Monte Carlo method: percentiles of the bootstrap curves
# (confidence) and of the curves with added noise (prediction).
# Linear method: delta method with gradient by central differences; the confidence
# band uses moltiplicatore_banda_confidenza() (pointwise or simultaneous,
# Bates & Watts 1988), the prediction band always the pointwise t.
calcola_bande_grafico <- function(res) {
  if (is.null(res) || !is.list(res) || !isTRUE(res$successo)) return(NULL)
  x_seq <- res$x_griglia; y_seq <- res$valuta(res$parametri, x_seq)
  p_inf <- (1 - input$livello_confidenza / 100) / 2; p_sup <- 1 - p_inf
  mc <- identical(input$metodo_errore, "monte_carlo")
  out <- list(x = x_seq, y = y_seq)
  se_curva <- function() {
    G <- sapply(seq_along(res$parametri), function(j) {
      h_j <- passo_derivata(res$parametri[j])
      pp <- res$parametri; pp[j] <- pp[j] + h_j; pm <- res$parametri; pm[j] <- pm[j] - h_j
      (res$valuta(pp, x_seq) - res$valuta(pm, x_seq)) / (2 * h_j)
    })
    if (is.null(dim(G))) G <- matrix(G, ncol = length(res$parametri))
    sqrt(rowSums((G %*% res$cov) * G))
  }
  lineare_ok <- !is.null(res$cov) && all(res$errori > 0, na.rm = TRUE)
  # Reason why a requested band is not available, shown in the plot
  # (previously the band was omitted without any explanation).
  motivo_assente <- if (mc && is.null(res$curve_boot) && !lineare_ok) "Monte Carlo results not available"
                    else if (!mc && !lineare_ok) (if (isTRUE(res$criteri_non_disponibili)) "loss other than MSE (use Monte Carlo)" else "standard errors not available")
                    else NULL
  errore_banda <- NULL
  if (isTRUE(input$mostra_pi)) tryCatch({
    if (mc && !is.null(res$pred_boot)) {
      out$pi_inf <- apply(res$pred_boot, 2, quantile, probs = p_inf, na.rm = TRUE)
      out$pi_sup <- apply(res$pred_boot, 2, quantile, probs = p_sup, na.rm = TRUE)
    } else if (lineare_ok) {
      se_tot <- sqrt(se_curva()^2 + res$rmse^2 * varianza_relativa(res, y_seq)); z <- valore_critico(res$df)
      out$pi_inf <- y_seq - z * se_tot; out$pi_sup <- y_seq + z * se_tot
    }
  }, error = function(e) errore_banda <<- conditionMessage(e))
  if (isTRUE(input$mostra_ci)) tryCatch({
    if (mc && !is.null(res$curve_boot)) {
      out$ci_inf <- apply(res$curve_boot, 2, quantile, probs = p_inf, na.rm = TRUE)
      out$ci_sup <- apply(res$curve_boot, 2, quantile, probs = p_sup, na.rm = TRUE)
      out$ci_tipo <- "monte_carlo"
    } else if (lineare_ok) {
      m <- moltiplicatore_banda_confidenza(res); se <- se_curva()
      out$ci_inf <- y_seq - m * se; out$ci_sup <- y_seq + m * se
      out$ci_tipo <- "lineare"
    }
  }, error = function(e) errore_banda <<- conditionMessage(e))
  richieste <- c(if (isTRUE(input$mostra_ci) && is.null(out$ci_inf)) "confidence", if (isTRUE(input$mostra_pi) && is.null(out$pi_inf)) "prediction")
  if (length(richieste)) out$nota <- paste0("No ", paste(richieste, collapse = "/"), " band: ",
                                           if (!is.null(motivo_assente)) motivo_assente
                                           else if (!is.null(errore_banda)) paste0("computation error (", substr(errore_banda, 1, 45), ")") else "not available")
  out
}

disegna_grafico_fit <- function(df, res) {
  titolo_main <- if(input$mostra_titolo) "Model Visualization" else ""
  
  pt_col   <- if(is.null(input$pt_col_fit)) "#005088" else input$pt_col_fit
  ln_col   <- if(is.null(input$ln_col_fit)) "#e74c3c" else input$ln_col_fit
  pt_cex   <- if(is.null(input$pt_cex_fit)) 1.5 else input$pt_cex_fit
  ln_lwd   <- if(is.null(input$ln_lwd_fit)) 3 else input$ln_lwd_fit
  grid_on  <- if(is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  pt_pch   <- as.numeric(if(is.null(input$pt_pch_fit)) "16" else input$pt_pch_fit)
  fnt_cex  <- if(is.null(input$fnt_cex_fit)) 1.0 else input$fnt_cex_fit
  ci_alpha <- if(is.null(input$ci_alpha_fit)) 0.3 else input$ci_alpha_fit
  ln_lty   <- as.numeric(if(is.null(input$ln_lty_fit)) "1" else input$ln_lty_fit)
  
  par(mar = c(5, 5, if(input$mostra_titolo) 4 else 2, 2) + 0.1)
  limiti_x <- calcola_limiti(input$range_x_min, input$range_x_max, df$x)
  z_val <- valore_critico(res$df)  # Student's t (df=n-p), converges to Z for large samples
  
  # Bands computed only once, before the plot: they are needed both for the automatic
  # limits of the y axis and for the drawing. Previously the limits considered
  # only the data (plus an approximate margin for the prediction band), and
  # the confidence band could go outside the plot.
  bande <- calcola_bande_grafico(res)
  if (range_personalizzato_attivo(input$range_y_min, input$range_y_max)) {
    limiti_y <- c(input$range_y_min, input$range_y_max)
  } else {
    tutti_y <- df$y[is.finite(df$y)]
    if (!is.null(bande)) {
      dentro <- bande$x >= limiti_x[1] & bande$x <= limiti_x[2]
      tutti_y <- c(tutti_y, unlist(lapply(bande[c("ci_inf", "ci_sup", "pi_inf", "pi_sup")], function(v) if (is.null(v)) NULL else v[dentro])))
      tutti_y <- tutti_y[is.finite(tutti_y)]
    }
    limiti_y <- if (length(tutti_y) > 0) range(tutti_y) else c(0, 1)
  }
  
  plot(df$x, df$y, main = titolo_main, col = pt_col, pch = pt_pch, cex = pt_cex, 
       cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1,
       xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y), xlim = limiti_x, ylim = limiti_y)
  if(grid_on) grid()
  
  if (!is.null(res) && is.list(res) && res$successo) {
    x_seq <- res$x_griglia
    y_seq <- res$valuta(res$parametri, x_seq)
    p_inf <- (1 - input$livello_confidenza / 100) / 2
    p_sup <- 1 - p_inf
    
    if (!is.null(bande$nota)) mtext(bande$nota, side = 3, line = 0.2, cex = 0.75 * fnt_cex, col = "#d35400")
    if (!is.null(bande$pi_inf)) {
      colore_pi <- rgb(0.95, 0.76, 0.76, ci_alpha)
      polygon(c(x_seq, rev(x_seq)), c(bande$pi_sup, rev(bande$pi_inf)), col = colore_pi, border = NA)
      lines(x_seq, bande$pi_sup, col = ln_col, lty = 4, lwd = 1.2)
      lines(x_seq, bande$pi_inf, col = ln_col, lty = 4, lwd = 1.2)
    }
    if (!is.null(bande$ci_inf)) {
      if (identical(bande$ci_tipo, "monte_carlo")) {
        polygon(c(x_seq, rev(x_seq)), c(bande$ci_sup, rev(bande$ci_inf)), col = rgb(0.1, 0.7, 0.1, ci_alpha / 2), border = NA)
        lines(x_seq, bande$ci_sup, col = "darkgreen", lty = 3); lines(x_seq, bande$ci_inf, col = "darkgreen", lty = 3)
      } else {
        polygon(c(x_seq, rev(x_seq)), c(bande$ci_sup, rev(bande$ci_inf)), col = rgb(0.7, 0.7, 0.7, ci_alpha), border = NA)
        lines(x_seq, bande$ci_sup, col = "gray50", lty = 3); lines(x_seq, bande$ci_inf, col = "gray50", lty = 3)
      }
    }
    lines(x_seq[is.finite(y_seq)], y_seq[is.finite(y_seq)], col = ln_col, lwd = ln_lwd, lty = ln_lty)
  }
}

# titolo_forzato: title to be always shown (Batch, combined images);
# limiti_personalizzati = FALSE ignores the axis limits set for
# Single Fit. With the default values the plot is the usual one.
disegna_grafico_residui <- function(res, titolo_forzato = NULL, limiti_personalizzati = TRUE) {
  titolo_main <- if (!is.null(titolo_forzato)) titolo_forzato else if(input$mostra_titolo) "Residual Analysis Diagnostics" else ""
  if (is.null(res) || !res$successo) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type="n", main=if(input$mostra_titolo) "Waiting for optimization..." else "", axes=FALSE, xlab="", ylab="")
    return(invisible(NULL))
  }
  
  pt_col  <- if(is.null(input$pt_col_res)) "#2c3e50" else input$pt_col_res
  ln_col  <- if(is.null(input$ln_col_res)) "#e74c3c" else input$ln_col_res
  pt_cex  <- if(is.null(input$pt_cex_res)) 1.3 else input$pt_cex_res
  grid_on <- if(is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  pt_pch  <- as.numeric(if(is.null(input$pt_pch_res)) "17" else input$pt_pch_res)
  fnt_cex <- if(is.null(input$fnt_cex_res)) 1.0 else input$fnt_cex_res
  ln_lty_res <- as.numeric(if(is.null(input$ln_lty_res)) "2" else input$ln_lty_res)
  ln_lwd_res <- if(is.null(input$ln_lwd_res)) 2 else input$ln_lwd_res
  
  df <- res$df_originale
  # With weighted regression: weighted residuals sqrt(w)(y - yhat).
  dg <- tryCatch(diagnostica_residui(res), error = function(e) NULL)
  pesato <- ponderazione_attiva(res)
  res$residui <- residui_diagnostici(res)
  par(mar = c(5, 5, if(input$mostra_titolo || !is.null(titolo_forzato)) 4 else 2, 2) + 0.1)
  limiti_x <- if (limiti_personalizzati) calcola_limiti(input$range_x_min, input$range_x_max, df$x) else calcola_limiti(NA, NA, df$x)
  
  if (limiti_personalizzati && range_personalizzato_attivo(input$range_y_min_res, input$range_y_max_res)) {
    limiti_y_res <- c(input$range_y_min_res, input$range_y_max_res)
  } else {
    limiti_y_res <- NULL
  }
  
  # Points always; optional connecting line, always in the order of x
  # (previously type = "b" connected the residuals in the order of the data, with vertical
  # zigzags between replicates and jumps back and forth with unsorted data).
  # With replicates: by default no line between the single residuals, and the mean
  # of the residuals per x (systematic trend, i.e. lack of fit).
  repliche <- isTRUE(dg$ha_repliche)
  collega <- if (is.null(input$res_collega_punti)) !repliche else isTRUE(input$res_collega_punti)
  mostra_medie <- repliche && (is.null(input$res_mostra_medie) || isTRUE(input$res_mostra_medie))
  plot(res$x, res$residui, type = "p", main = titolo_main, col = pt_col, pch = pt_pch, cex = pt_cex, 
       cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1,
       xlab = interpreta_etichetta(input$label_x), ylab = if (pesato) "Weighted residual \u221aw (Observed - Predicted)" else "Residual (Observed - Predicted)", xlim = limiti_x, ylim = limiti_y_res)
  if (collega) { o <- order(res$x); lines(res$x[o], res$residui[o], col = pt_col) }
  abline(h = 0, col = ln_col, lty = ln_lty_res, lwd = ln_lwd_res)
  if (mostra_medie) {
    lines(dg$medie_per_x$x, dg$medie_per_x$media, col = ln_col, lwd = 1.5)
    points(dg$medie_per_x$x, dg$medie_per_x$media, pch = 23, col = ln_col, bg = ln_col, cex = pt_cex)
    legend("topright", legend = "Mean residual per x", pch = 23, pt.bg = ln_col, col = ln_col, lwd = 1.5, bty = "n", cex = 0.8 * fnt_cex)
  }
  if(grid_on) grid()
}

disegna_grafico_qq <- function(res, titolo_forzato = NULL, limiti_personalizzati = TRUE) {
  titolo_main <- if (!is.null(titolo_forzato)) titolo_forzato else if(input$mostra_titolo) "Normal Q-Q Plot of Residuals" else ""
  if (is.null(res) || !res$successo) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type="n", main=if(input$mostra_titolo) "Waiting for optimization..." else "", axes=FALSE, xlab="", ylab="")
    return(invisible(NULL))
  }
  
  pt_col  <- if(is.null(input$pt_col_qq)) "#2c3e50" else input$pt_col_qq
  ln_col  <- if(is.null(input$ln_col_qq)) "#e74c3c" else input$ln_col_qq
  pt_cex  <- if(is.null(input$pt_cex_qq)) 1.3 else input$pt_cex_qq
  grid_on <- if(is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  pt_pch  <- as.numeric(if(is.null(input$pt_pch_qq)) "17" else input$pt_pch_qq)
  fnt_cex <- if(is.null(input$fnt_cex_qq)) 1.0 else input$fnt_cex_qq
  ln_lty_qq <- as.numeric(if(is.null(input$ln_lty_qq)) "2" else input$ln_lty_qq)
  ln_lwd_qq <- if(is.null(input$ln_lwd_qq)) 2 else input$ln_lwd_qq
  
  par(mar = c(5, 5, if(input$mostra_titolo || !is.null(titolo_forzato)) 4 else 2, 2) + 0.1)
  residui_qq <- residui_diagnostici(res)
  residui_validi <- residui_qq[is.finite(residui_qq)]
  
  if (length(residui_validi) < 3) {
    plot(1, type="n", main="Insufficient residuals for Q-Q plot.", axes=FALSE, xlab="", ylab="")
    return(invisible(NULL))
  }
  
  qq_dati <- qqnorm(residui_validi, plot.it = FALSE)
  
  limiti_x_qq <- if (limiti_personalizzati && range_personalizzato_attivo(input$range_x_min_qq, input$range_x_max_qq)) c(input$range_x_min_qq, input$range_x_max_qq) else NULL
  limiti_y_qq <- if (limiti_personalizzati && range_personalizzato_attivo(input$range_y_min_qq, input$range_y_max_qq)) c(input$range_y_min_qq, input$range_y_max_qq) else NULL
  
  plot(qq_dati$x, qq_dati$y, main = titolo_main, col = pt_col, pch = pt_pch, cex = pt_cex,
       cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1,
       xlab = "Theoretical Quantiles", ylab = if (ponderazione_attiva(res)) "Sample Quantiles (Weighted residuals)" else "Sample Quantiles (Residuals)", xlim = limiti_x_qq, ylim = limiti_y_qq)
  if(grid_on) grid()
  qqline(residui_validi, col = ln_col, lwd = ln_lwd_qq, lty = ln_lty_qq)
}

# ---- Parameter Distributions (Monte Carlo), one panel per parameter ----
# Number of available panels (0 = section hidden: no fit, or fit
# without Monte Carlo).
n_pannelli_distribuzioni_sf <- function() {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !isTRUE(res$successo) || is.null(res$mat_boot_params)) 0L else ncol(res$mat_boot_params)
}

# Draws the bootstrap distribution of parameter i (histogram, density
# and vertical line at the point estimate). Used by the single plots, by
# their downloads, by the ZIP, by the combined image and by the single figure
# of "Download All Plots (ZIP)" - a single place in which the drawing is
# defined.
disegna_distribuzione_pannello <- function(i, res = tryCatch(elaborazione(), error = function(e) NULL)) {
  par(mar = c(4.5, 4.5, if (isTRUE(input$mostra_titolo)) 3 else 1, 1))
  if (is.null(res) || !isTRUE(res$successo) || is.null(res$mat_boot_params) || i > ncol(res$mat_boot_params)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Available only with Monte Carlo method.")
    return(invisible(NULL))
  }
  bar_col <- if (is.null(input$bar_col_dist)) "#f39c12" else input$bar_col_dist
  dns_col <- if (is.null(input$dens_col_dist)) "black" else input$dens_col_dist
  vln_col <- if (is.null(input$vln_col_dist)) "black" else input$vln_col_dist
  vln_lty_dist <- as.numeric(if (is.null(input$vln_lty_dist)) "2" else input$vln_lty_dist)
  vln_lwd_dist <- if (is.null(input$vln_lwd_dist)) 2 else input$vln_lwd_dist
  fnt_cex <- if (is.null(input$fnt_cex_dist)) 1.0 else input$fnt_cex_dist
  n_bins_dist <- if (is.null(input$n_bins_dist)) 30 else input$n_bins_dist
  
  dati_param <- res$mat_boot_params[, i]
  simbolo <- interpreta_etichetta(simbolo_visualizzazione(names(res$parametri)[i]))
  if (all(is.na(dati_param))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No valid bootstrap values for this parameter.")
    return(invisible(NULL))
  }
  titolo_locale <- if (isTRUE(input$mostra_titolo)) {
    if (is.character(simbolo)) paste("Distribution of", simbolo) else bquote("Distribution of" ~ .(simbolo))
  } else ""
  xlab_locale <- if (is.character(simbolo)) paste("Parameter", simbolo, "Value") else bquote("Parameter" ~ .(simbolo) ~ "Value")
  hist(dati_param, breaks = n_bins_dist, probability = TRUE, main = titolo_locale,
       xlab = xlab_locale,
       col = bar_col, border = "white", las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1)
  if (is.null(input$mostra_densita_dist) || isTRUE(input$mostra_densita_dist)) lines(density(dati_param, na.rm = TRUE), col = dns_col, lwd = 2)
  if (is.null(input$mostra_vln_dist) || isTRUE(input$mostra_vln_dist)) abline(v = res$parametri[i], col = vln_col, lwd = vln_lwd_dist, lty = vln_lty_dist)
}

# Single figure with all parameters (2 columns), used only by "Download
# All Plots (ZIP)" of Single Fit (export_dati.R), which saves one figure per
# type of plot. Reuses disegna_distribuzione_pannello() for each panel.
disegna_grafico_distribuzioni <- function(res) {
  if (is.null(res) || !res$successo) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type="n", main=if(input$mostra_titolo) "Waiting for optimization..." else "", axes=FALSE, xlab="", ylab="")
    return(invisible(NULL))
  }
  if (is.null(res$mat_boot_params)) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type="n", main=if(input$mostra_titolo) "Available only with Monte Carlo method." else "", axes=FALSE, xlab="", ylab="")
    return(invisible(NULL))
  }
  n_params <- ncol(res$mat_boot_params)
  cols <- min(2, n_params)
  rows <- ceiling(n_params / cols)
  par(mfrow = c(rows, cols))
  for (i in seq_len(n_params)) disegna_distribuzione_pannello(i, res)
  par(mfrow = c(1, 1))
}

# ============================
# PARAMETER SENSITIVITY ANALYSIS
# ============================
# Normalized (dimensionless) local sensitivity around the optimal estimate:
# S_j(x) = (dy/dtheta_j) * (theta_j / y(x)), interpretable as "percentage
# change of y for a 1% change in parameter j", at each x.
# Reuses the same helper (passo_derivata) already used for the CI/PI bands and the
# Delta method - no new computation, only a different presentation.
disegna_grafico_sensibilita <- function(res) {
  if (is.null(res) || !res$successo) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "Run a successful optimization first.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  x_seq <- res$x_griglia
  n_par <- length(res$parametri)
  nomi_par <- names(res$parametri)
  y_base <- res$valuta(res$parametri, x_seq)
  
  sensibilita <- matrix(NA_real_, nrow = length(x_seq), ncol = n_par)
  for (j in seq_len(n_par)) {
    h_j <- passo_derivata(res$parametri[j])
    p_piu <- res$parametri; p_piu[j] <- p_piu[j] + h_j
    p_meno <- res$parametri; p_meno[j] <- p_meno[j] - h_j
    dydtheta <- (res$valuta(p_piu, x_seq) - res$valuta(p_meno, x_seq)) / (2 * h_j)
    # Protection against division by y_base ~ 0 (points where the model passes through zero)
    sensibilita[, j] <- ifelse(abs(y_base) > 1e-10, dydtheta * (res$parametri[j] / y_base), NA)
  }
  
  tipo_palette_sens <- if (is.null(input$sens_palette)) "fissa" else input$sens_palette
  colori_usati <- switch(tipo_palette_sens,
    "rainbow" = rainbow(n_par),
    "viridis" = hcl.colors(n_par, palette = "viridis"),
    "plasma" = hcl.colors(n_par, palette = "plasma"),
    "cividis" = hcl.colors(n_par, palette = "cividis"),
    "spectral" = hcl.colors(n_par, palette = "spectral"),
    "heat" = hcl.colors(n_par, palette = "heat"),
    rep_len(c("#e74c3c", "#2980b9", "#27ae60", "#f39c12", "#8e44ad", "#16a085", "#c0392b", "#34495e"), n_par)
  )
  
  fnt_cex   <- if (is.null(input$fnt_cex_sens)) 1.0 else input$fnt_cex_sens
  grid_on   <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  ln_lwd    <- if (is.null(input$ln_lwd_sens)) 2.5 else input$ln_lwd_sens
  mostra_curva <- if (is.null(input$mostra_curva_sens)) TRUE else input$mostra_curva_sens
  mostra_legenda_sens <- if (is.null(input$mostra_legenda_sens)) TRUE else input$mostra_legenda_sens
  
  titolo <- if (input$mostra_titolo) "Local Parameter Sensitivity Analysis" else ""
  # Enlarged right margin to make room for the second axis (predicted curve)
  par(mar = c(5, 5, if (input$mostra_titolo) 4 else 2, if (mostra_curva) 5 else 2) + 0.1)
  plot(1, type = "n", xlim = range(x_seq), ylim = range(sensibilita, na.rm = TRUE, finite = TRUE),
       main = titolo, xlab = interpreta_etichetta(input$label_x), ylab = "Normalized Sensitivity (% change in y per 1% change in parameter)", 
       las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1)
  if (grid_on) grid(col = "gray90", lty = "dotted")
  abline(h = 0, col = "gray50", lty = 1)
  for (j in seq_len(n_par)) {
    lines(x_seq, sensibilita[, j], col = colori_usati[j], lwd = ln_lwd)
  }
  cex_legenda_sens <- if (is.null(input$sens_cex_legenda)) 0.85 else input$sens_cex_legenda
  pos_legenda_sens <- if (is.null(input$sens_pos_legenda)) "topright" else input$sens_pos_legenda
  if (mostra_legenda_sens) legend(pos_legenda_sens, legend = costruisci_legenda_simboli(nomi_par), col = colori_usati, lwd = ln_lwd, bty = "o", cex = cex_legenda_sens, bg = "white")
  
  # Predicted curve overlaid as a visual reference (dashed grey,
  # secondary axis on the right: units and scale different from the sensitivity, a
  # shared axis would make no sense).
  if (mostra_curva) {
    par(new = TRUE)
    plot(x_seq, y_base, type = "l", col = "gray50", lty = 2, lwd = 1.5, axes = FALSE, xlab = "", ylab = "",
         xlim = range(x_seq))
    axis(4, las = 1, col.axis = "gray50", col = "gray50", cex.axis = fnt_cex * 0.9)
    mtext("Predicted Y (reference curve)", side = 4, line = 3, col = "gray50", cex = fnt_cex * 0.85)
  }
}

# ============================
# PARAMETER CORRELATION MATRIX (numeric table)
# ============================
# From the Hessian (Linear method, cov2cor on res$cov) or from the bootstrap (Monte
# Carlo, cor() on the matrix of sampled parameters) - whichever is
# available depending on the chosen error method.

# Shared helper (used by the numeric table):
# returns the correlation matrix, or NULL if it cannot be computed.
calcola_matrice_correlazione <- function(res) {
  if (input$metodo_errore == "monte_carlo" && !is.null(res$mat_boot_params) && nrow(res$mat_boot_params) >= 10) {
    mat_corr <- tryCatch(cor(res$mat_boot_params, use = "pairwise.complete.obs"), error = function(e) NULL)
  } else if (!is.null(res$cov) && !anyNA(res$cov)) {
    mat_corr <- tryCatch(cov2cor(res$cov), error = function(e) NULL)
  } else {
    return(NULL)
  }
  if (is.null(mat_corr) || anyNA(mat_corr)) return(NULL)
  colnames(mat_corr) <- rownames(mat_corr) <- names(res$parametri)
  mat_corr
}

# ============================
# PAIRWISE BOOTSTRAP SCATTER PLOT MATRIX
# ============================
# Cloud of bootstrap points for each pair of parameters, using pairs() of
# base R: automatically shows ALL pairwise combinations in a single
# grid, without needing to choose which pair to see.
disegna_pairs_bootstrap <- function(res) {
  if (is.null(res) || !res$successo || input$metodo_errore != "monte_carlo" || is.null(res$mat_boot_params)) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "Available only after a successful Monte Carlo optimization.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  mat_boot <- res$mat_boot_params
  colnames(mat_boot) <- names(res$parametri)
  mat_boot <- mat_boot[complete.cases(mat_boot), , drop = FALSE]
  if (nrow(mat_boot) < 10 || ncol(mat_boot) < 2) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "Not enough valid bootstrap draws, or only one free parameter.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  titolo <- if (input$mostra_titolo) "Bootstrap Parameter Cloud (Pairwise)" else ""
  pt_col   <- if (is.null(input$pt_col_pairs)) "#2980b9" else input$pt_col_pairs
  pt_cex   <- if (is.null(input$pt_cex_pairs)) 0.8 else input$pt_cex_pairs
  pt_alpha <- if (is.null(input$pt_alpha_pairs)) 0.4 else input$pt_alpha_pairs
  mostra_contour <- if (is.null(input$mostra_contour_pairs)) TRUE else input$mostra_contour_pairs
  
  # Custom panel: draws the points as before, and if requested overlays
  # the density contour lines (bivariate Gaussian kernel estimate,
  # base R only - see kde2d_base()).
  col_contour <- if (is.null(input$col_contour_pairs)) "#2c3e50" else input$col_contour_pairs
  lty_contour <- if (is.null(input$lty_contour_pairs)) 1 else as.numeric(input$lty_contour_pairs)
  lwd_contour <- if (is.null(input$lwd_contour_pairs)) 1 else input$lwd_contour_pairs
  
  pannello_punti_contour <- function(x, y, ...) {
    points(x, y, pch = 16, cex = pt_cex, col = adjustcolor(pt_col, alpha.f = pt_alpha))
    if (mostra_contour) {
      dens <- kde2d_base(x, y)
      if (!is.null(dens)) {
        contour(dens$x, dens$y, dens$z, add = TRUE, drawlabels = FALSE, col = col_contour, lty = lty_contour, lwd = lwd_contour)
      }
    }
  }
  
  pairs(mat_boot, labels = costruisci_legenda_simboli(colnames(mat_boot)),
        panel = pannello_punti_contour, main = titolo, gap = 0.5)
}

# ============================
# ENRICHED MODEL FORMULA DISPLAY
# ============================
# Draws the model formula centred, using plotmath (Greek letters,
# superscripts/subscripts) when the Parameter Display Symbols have been set.
# No axis: it is just text centred on an empty canvas.
# Base R has no native function for rectangles with rounded corners
# (rect() draws only sharp corners) - I build it by approximating each
# corner with a circular arc of radius "raggio" (25 points each),
# connected into a single polygon. Shared by the three "boxed" equations
# of the app (main fit, Shelf Life Batch, Shelf Life Model Comparison).
disegna_rettangolo_arrotondato <- function(x0, y0, x1, y1, raggio, ...) {
  n <- 25
  t_bl <- seq(pi, 3 * pi / 2, length.out = n)
  bl <- cbind(x0 + raggio + raggio * cos(t_bl), y0 + raggio + raggio * sin(t_bl))
  t_br <- seq(3 * pi / 2, 2 * pi, length.out = n)
  br <- cbind(x1 - raggio + raggio * cos(t_br), y0 + raggio + raggio * sin(t_br))
  t_tr <- seq(0, pi / 2, length.out = n)
  tr <- cbind(x1 - raggio + raggio * cos(t_tr), y1 - raggio + raggio * sin(t_tr))
  t_tl <- seq(pi / 2, pi, length.out = n)
  tl <- cbind(x0 + raggio + raggio * cos(t_tl), y1 - raggio + raggio * sin(t_tl))
  punti <- rbind(bl, br, tr, tl)
  polygon(punti[, 1], punti[, 2], ...)
}

disegna_formula_modello <- function(res) {
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  plot.new()
  if (is.null(res) || !isTRUE(res$successo)) {
    disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f8f9fa", border = "#dee2e6", lwd = 1.5)
    text(0.5, 0.5, "Run a successful optimization to see the model equation here.", cex = 1.0, col = "#7f8c8d")
    return(invisible(NULL))
  }
  # "Card" background consistent with the style already used for the "Active
  # function" box in the sidebar (same light green/teal border), instead of the
  # plain white background of before - gives the equation a distinct visual
  # frame, instead of floating on a bare background.
  disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f0faf8", border = "#18bc9c", lwd = 2)
  colore_testo <- "black"
  
  testo_sostituito <- costruisci_testo_formula_sostituita(res)
  testo_base <- if (!is.null(testo_sostituito)) testo_sostituito else res$formula
  divisione <- dividi_formula_lunga(testo_base)
  
  # Text size adaptive based on the LONGEST resulting line
  # (no longer on the total length) - a formula split into two
  # balanced lines needs less shrinking than would be needed
  # to fit entirely on a single line.
  lunghezza_riferimento <- max(nchar(divisione$riga1), if (!is.null(divisione$riga2)) nchar(divisione$riga2) else 0)
  # Reference threshold for resizing the font - the
  # container is 8/12 of the page wide (almost the whole main
  # column, not a narrow space), so even a single line
  # of 100-130 characters fits comfortably at full size. A single
  # constant for 1 and 2 lines, since even the longest line resulting
  # from a split into 2 (e.g. Integrated Baranyi Model, longest line 117
  # characters) is of the same order as a whole 1-line formula
  # (e.g. Reparameterized Baranyi, 117 characters) - there is no reason to
  # treat them differently.
  riferimento_cex <- 130
  cex_adattivo <- if (lunghezza_riferimento <= riferimento_cex) 1.7 else max(0.55, 1.7 * (riferimento_cex / lunghezza_riferimento)^0.6)
  
  if (is.null(divisione$riga2)) {
    espr1 <- tryCatch(str2expression(paste0("y == ", divisione$riga1)), error = function(e) NULL)
    if (!is.null(espr1) && length(espr1) == 1) {
      tryCatch({
        text(0.5, 0.5, espr1[[1]], cex = cex_adattivo, font = 3, col = colore_testo)
      }, error = function(e) {
        text(0.5, 0.5, paste("y =", res$formula), cex = min(cex_adattivo, 1.2), family = "mono", col = colore_testo)
      })
    } else {
      text(0.5, 0.5, paste("y =", res$formula), cex = min(cex_adattivo, 1.2), family = "mono", col = colore_testo)
    }
  } else {
    # Line 1: "y = [first part]" - line 2: continues from the operator
    # ("+"/"-") that caused the split, without repeating "y =".
    espr1 <- tryCatch(str2expression(paste0("y == ", divisione$riga1)), error = function(e) NULL)
    espr2 <- tryCatch(str2expression(divisione$riga2), error = function(e) NULL)
    if (!is.null(espr1) && length(espr1) == 1 && !is.null(espr2) && length(espr2) == 1) {
      tryCatch({
        text(0.5, 0.64, espr1[[1]], cex = cex_adattivo, font = 3, col = colore_testo)
        text(0.5, 0.36, espr2[[1]], cex = cex_adattivo, font = 3, col = colore_testo)
      }, error = function(e) {
        text(0.5, 0.5, paste("y =", res$formula), cex = min(cex_adattivo, 1.2), family = "mono", col = colore_testo)
      })
    } else {
      text(0.5, 0.5, paste("y =", res$formula), cex = min(cex_adattivo, 1.2), family = "mono", col = colore_testo)
    }
  }
}