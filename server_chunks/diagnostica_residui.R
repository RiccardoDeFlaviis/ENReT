# =============================================================================
# RESIDUAL AND REPLICATE DIAGNOSTICS (Single Fit)
# =============================================================================
# 2x2 grid below the model plot: Residual Analysis, Q-Q plot (already
# existing), Lag plot and plot of the standard deviation against the mean
# (new), and below the grid the tests on the replicates. Everything computed from the
# result of the fit (elaborazione()), without modifying it.
#
# Sources of the procedures:
#  - pure error and lack-of-fit test: van Boekel
#    (2008), ch. 7 and section 7.4.3 (SS_pe with n - f degrees of freedom, f = number
#    of distinct values of x; SS_lof = SSr - SS_pe with f - p; F = ratio of the
#    mean squares, in an ANOVA table); approximate in nonlinear
#    models like the other F-based procedures (Bates & Watts, 1988);
#  - homogeneity of variance: Bartlett and Brown-Forsythe tests (absolute
#    deviations from the median). Levene (deviations from the mean) was excluded:
#    with 3-4 replicates per x it gives 15-20% false positives instead of 5%;
#  - standard deviation - mean relationship: Carroll & Ruppert (1988), section
#    3.3.6, p. 89 (Rodbard's method: regression of log(SD) on log(mean)
#    in the "power-of-the-mean" model, with Raab's correction for the number
#    of replicates); without enough replicates, the plot of the studentized
#    residuals |r|^(2/3) against log(predicted values), section 2.7, pp. 29-31;
#  - lag plot and first-order autocorrelation: NIST/SEMATECH e-Handbook
#    of Statistical Methods; approximate bands +/- 1.96/sqrt(m).
# =============================================================================

# ---- Runs test on the signs of the residuals (van Boekel, 2008, section 7.8) ----
# e: residuals already ordered (by x; means per x if there are replicates). Zero
# residuals excluded. Expected runs R_r = 2 n_p n_n / (n_p + n_n) + 1; variance
# 2 n_p n_n (2 n_p n_n - n_p - n_n) / [(n_p + n_n)^2 (n_p + n_n - 1)]: in the
# book the denominator is printed with (n_p - n_n - 1), a misprint (negative
# or wrong variances; the standard formula agrees with simulation).
# Z_tf = (n_R - R_r + 0.5)/sigma (too few runs if strongly negative),
# Z_tm = (n_R - R_r - 0.5)/sigma (too many if strongly positive); van Boekel
# reports their absolute values and suggests 3 standard deviations as threshold,
# unreachable with fewer than about 14 points. For this reason the judgement is based
# on the exact p-value (Swed & Eisenhart distribution), one-sided.
runs_test_residui <- function(e) {
  e <- e[is.finite(e) & e != 0]; segni <- sign(e)
  np <- sum(segni > 0); nn <- sum(segni < 0); N <- np + nn
  if (np == 0 || nn == 0) return(list(disponibile = FALSE, motivo = "all residuals have the same sign", np = np, nn = nn))
  nR <- 1 + sum(segni[-1] != segni[-N])
  Rr <- 2 * np * nn / N + 1
  s <- sqrt(2 * np * nn * (2 * np * nn - np - nn) / (N^2 * (N - 1)))
  # Exact distribution of the number of runs, given n_p and n_n.
  prob_r <- function(r) { k <- r %/% 2
    if (r %% 2 == 0) 2 * choose(np - 1, k - 1) * choose(nn - 1, k - 1)
    else choose(np - 1, k - 1) * choose(nn - 1, k) + choose(np - 1, k) * choose(nn - 1, k - 1) }
  rr <- 2:N; pr <- sapply(rr, prob_r) / choose(N, np)
  list(disponibile = TRUE, np = np, nn = nn, n_runs = nR, attese = Rr, sigma = s,
       z_tf = (nR - Rr + 0.5) / s, z_tm = (nR - Rr - 0.5) / s,
       p_poche = sum(pr[rr <= nR]), p_troppe = sum(pr[rr >= nR]), p_minimo = min(pr[1], 1))
}

# Residuals for the runs test and for the lag plot: ordered by x; with replicates, the
# mean of the residuals per x (the order of the replicates at the same x is unknown).
# Also used in Model Comparison, to treat all models in the same way.
residui_ordinati_per_x <- function(x, e) {
  ok <- is.finite(x) & is.finite(e); x <- x[ok]; e <- e[ok]; ch <- signif(x, 12); u <- sort(unique(ch))
  if (length(u) < length(ch)) sapply(u, function(v) mean(e[ch == v])) else e[order(x)]
}

# ---- Exponent of the variance function from the replicates ------------------
# Used by the SD vs mean plot and by the engine (zeta = 2b for the weights
# 1/yhat^zeta): same code, therefore same estimate. repl = rows of the
# table per x with at least 2 replicates.
calcola_sd_media <- function(repl) {
  correzione_raab <- function(m) 0.5 * (digamma((m - 1) / 2) - log((m - 1) / 2))
  ok_sd <- repl$SD > 0 & repl$Mean > 0 & is.finite(repl$SD)
  if (sum(ok_sd) < 3) list(disponibile = FALSE, motivo = if (nrow(repl) < 3) "at least three x values with replicates are needed" else "at least three groups with positive mean and SD are needed",
                                          esclusi = nrow(repl) - sum(ok_sd))
    else {
      d <- repl[ok_sd, ]; d$log_SD_corr <- log(d$SD) - correzione_raab(d$n); d$SD_corr <- exp(d$log_SD_corr)
      fit <- lm(log_SD_corr ~ log(Mean), data = d); ci <- suppressWarnings(confint(fit)[2, ])
      b <- unname(coef(fit)[2])
      riferimenti <- c("0 (constant variance)" = 0, "0.5 (variance proportional to the mean)" = 0.5, "1 (constant coefficient of variation)" = 1)
      dentro <- names(riferimenti)[riferimenti >= ci[1] & riferimenti <= ci[2]]
      list(disponibile = TRUE, dati = d, a = exp(unname(coef(fit)[1])), b = b, ci = unname(ci), gruppi = nrow(d), esclusi = nrow(repl) - nrow(d), compatibili = dentro,
           repliche_diverse = length(unique(d$n)) > 1)
    }
}
stima_potenza_varianza <- function(x, y) {
  g <- raggruppa_per_x(x, y, rep(0, length(x)), y)
  calcola_sd_media(g$tab[g$tab$n >= 2, , drop = FALSE])
}

# ---- Grouping of the observations by value of x -----------------------------
# x compared after rounding to 12 significant digits, so as not to separate
# equal values that differ only by representation errors.
raggruppa_per_x <- function(x, y, residui, previsti) {
  ok <- is.finite(x) & is.finite(y) & is.finite(residui)
  x <- x[ok]; y <- y[ok]; residui <- residui[ok]; previsti <- previsti[ok]
  chiave <- signif(x, 12)
  livelli <- sort(unique(chiave))
  tab <- do.call(rbind, lapply(livelli, function(v) {
    i <- which(chiave == v)
    data.frame(x = x[i[1]], n = length(i), Mean = mean(y[i]), SD = if (length(i) > 1) sd(y[i]) else NA_real_,
               CV_percent = if (length(i) > 1 && mean(y[i]) != 0) 100 * sd(y[i]) / abs(mean(y[i])) else NA_real_,
               Predicted = mean(previsti[i]), Mean_Residual = mean(residui[i]))
  }))
  list(tab = tab, x = x, y = y, residui = residui, chiave = chiave, n = length(x), m = length(livelli))
}

# ---- Computation of all the diagnostics -------------------------------------
diagnostica_residui <- function(res) {
  if (is.null(res) || !isTRUE(res$successo) || is.null(res$df_originale)) return(NULL)
  df <- res$df_originale
  previsti <- df$y - res$residui
  g <- raggruppa_per_x(df$x, df$y, res$residui, previsti)
  tab <- g$tab; p <- length(res$parametri)
  repl <- tab[tab$n >= 2, , drop = FALSE]
  out <- list(gruppi = g, tabella = tab, n = g$n, m = g$m, p = p, n_x_replicati = nrow(repl),
              n_oss_replicate = sum(repl$n), ha_repliche = nrow(repl) >= 1)
  # Weighted regression: the diagnostics based on the residuals use the weighted
  # residuals sqrt(w)(y - yhat), with w = 1/yhat^zeta; homogeneity of variance
  # and the SD vs mean plot remain on the observed data, because they serve to
  # choose the weights. Without weights e_usati coincides with the usual residuals.
  pesato <- ponderazione_attiva(res)
  w_g <- if (pesato) 1 / (g$y - g$residui)^res$ponderazione$zeta else NULL
  e_usati <- if (pesato) g$residui * sqrt(w_g) else g$residui
  out$pesato <- pesato
  out$medie_per_x <- if (pesato) data.frame(x = tab$x, media = sapply(sort(unique(g$chiave)), function(v) mean(e_usati[g$chiave == v])))
                     else data.frame(x = tab$x, media = tab$Mean_Residual)
  
  # Pure error: dispersion of the replicates around their own mean (with
  # weights, weighted sum: the weight is constant within each value of x).
  out$ss_pe <- if (pesato) sum(sapply(split(seq_along(g$y), g$chiave), function(i) w_g[i[1]] * sum((g$y[i] - mean(g$y[i]))^2)))
               else sum(sapply(split(g$y, g$chiave), function(v) sum((v - mean(v))^2)))
  out$df_pe <- g$n - g$m
  
  # Lack-of-fit test: SSE = SS_pe + SS_lof, F = (SS_lof/(m-p)) /
  # (SS_pe/(n-m)). Requires replicates (n > m), m > p and the MSE loss (with the
  # other losses the SSE is not the minimized quantity).
  out$lof <- if (!out$ha_repliche || out$df_pe < 1) list(disponibile = FALSE, motivo = "no replicates")
    else if (g$m <= p) list(disponibile = FALSE, motivo = sprintf("not enough distinct x values (%d) for %d parameters", g$m, p))
    else if (isTRUE(res$criteri_non_disponibili)) list(disponibile = FALSE, motivo = "it requires the MSE loss")
    else {
      sse <- sum(e_usati^2); ss_lof <- max(sse - out$ss_pe, 0); df_lof <- g$m - p
      F <- (ss_lof / df_lof) / (out$ss_pe / out$df_pe)
      list(disponibile = TRUE, sse = sse, ss_lof = ss_lof, df_lof = df_lof, ss_pe = out$ss_pe, df_pe = out$df_pe,
           F = F, p = pf(F, df_lof, out$df_pe, lower.tail = FALSE), s_pe = sqrt(out$ss_pe / out$df_pe), rmse = res$rmse)
    }
  
  # Homogeneity of variance among the groups with at least 2 replicates (at least
  # 2 groups are needed): Bartlett and Brown-Forsythe (one-way ANOVA on the absolute
  # deviations from the median of each group).
  i_rep <- g$chiave %in% signif(repl$x, 12)
  out$omogeneita <- if (nrow(repl) < 2) list(disponibile = FALSE, motivo = if (nrow(repl) == 1) "only one x value has replicates" else "no replicates")
    else tryCatch({
      yy <- g$y[i_rep]; gg <- factor(g$chiave[i_rep])
      bt <- bartlett.test(yy, gg)
      # Brown-Forsythe: one-way analysis of variance on the absolute deviations
      # from the median, only on the groups with at least 4 replicates. With 2 replicates
      # the median coincides with the mean of the pair and the two deviations are
      # identical (within-group variability zero, infinite F, p = 0); with 3
      # the median coincides with one value, which has zero deviation, and the test
      # never rejects (0% rejections in simulation). With 4 replicates: 7%.
      g4 <- names(which(table(gg) >= 4))
      bf <- if (length(g4) >= 2) {
        i4 <- gg %in% g4; y4 <- yy[i4]; f4 <- droplevels(gg[i4])
        z <- abs(y4 - ave(y4, f4, FUN = median)); a <- suppressWarnings(anova(lm(z ~ f4)))
        c(statistica = a$`F value`[1], df1 = a$Df[1], df2 = a$Df[2], p = a$`Pr(>F)`[1], gruppi = length(g4))
      } else NULL
      list(disponibile = TRUE, gruppi = nlevels(gg),
           bartlett = c(statistica = unname(bt$statistic), df = unname(bt$parameter), p = bt$p.value),
           brown_forsythe = bf)
    }, error = function(e) list(disponibile = FALSE, motivo = conditionMessage(e)))
  
  # Standard deviation - mean relationship (Carroll & Ruppert, 1988, section
  # 3.3.6, p. 89): log(SD) = log(a) + b log(mean), on the groups with replicates,
  # SD > 0 and mean > 0 (at least 3 groups are needed). Raab's correction: with normal
  # data E[log s] = log(sigma) + c(m), with c(m) = [digamma((m-1)/2) -
  # log((m-1)/2)]/2 (-0.635, -0.289, -0.184 for m = 2, 3, 4, as in the book);
  # c(m) is subtracted from each log(SD). With the same number of replicates in all
  # groups only the intercept changes; with different numbers the slope b
  # would otherwise be biased.
  out$sd_media <- calcola_sd_media(repl)
  
  # Residual plot for the variance structure without replicates
  # (Carroll & Ruppert, 1988, section 2.7, pp. 29-31, Figure 2.1-2.3):
  # |r|^(2/3) of the studentized residuals r = e / (s sqrt(1 - h)), with the leverages h
  # from the Jacobian, against the logarithm of the predicted values (only predicted > 0).
  # |r|^(2/3) is the cube root of the square: Wilson-Hilferty
  # transformation, which with constant variance and normal data makes a
  # chi-square variable nearly normal.
  out$spread <- tryCatch({
    prev <- g$y - g$residui; s <- sqrt(sum(e_usati^2) / max(g$n - p, 1))
    J <- tryCatch(crea_jacobiano_modello(res$formula, names(res$parametri))(res$parametri, g$x, res$fissi), error = function(e) NULL)
    if (pesato && !is.null(J)) J <- J * sqrt(w_g)
    h <- if (!is.null(J) && nrow(J) == g$n) tryCatch(pmin(pmax(rowSums((J %*% inverti_hessiana(crossprod(J))) * J), 0), 0.99), error = function(e) NULL) else NULL
    studentizzati <- !is.null(h) && all(is.finite(h))
    r <- e_usati / (s * if (studentizzati) sqrt(1 - h) else 1)
    ok <- prev > 0 & is.finite(r)
    list(disponibile = sum(ok) >= 3, x = log(prev[ok]), y = abs(r[ok])^(2/3), studentizzati = studentizzati, esclusi = sum(!ok))
  }, error = function(e) list(disponibile = FALSE))
  
  # First-order autocorrelation of the residuals ordered by x; with replicates,
  # of the mean residuals per x (the order of the replicates at the same x is unknown).
  e <- if (out$ha_repliche) out$medie_per_x$media else e_usati[order(g$x)]
  out$runs <- runs_test_residui(e)
  out$lag <- list(e = e, su_medie = out$ha_repliche, m = length(e),
                  r1 = if (length(e) >= 3 && var(e) > 0) acf(e, lag.max = 1, plot = FALSE)$acf[2] else NA_real_,
                  banda = if (length(e) >= 3) 1.96 / sqrt(length(e)) else NA_real_)
  out
}

diagnostica_corrente <- reactive({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  tryCatch(diagnostica_residui(res), error = function(e) NULL)
})
ha_repliche_correnti <- function() isTRUE(tryCatch(diagnostica_corrente()$ha_repliche, error = function(e) FALSE))

# Subtitle that shrinks just enough to fit within the width of the
# figure (narrow panels of the combined images); if there is room it stays
# at the requested size, so the single plots do not change.
mtext_adattato <- function(testo, cex, col) {
  larghezza <- strwidth(testo, units = "inches", cex = cex); disponibile <- 0.97 * par("fin")[1]
  if (is.finite(larghezza) && larghezza > disponibile) cex <- cex * disponibile / larghezza
  mtext(testo, side = 3, line = 0.2, cex = cex, col = col)
}

# ---- Empty plot with message ------------------------------------------------
grafico_messaggio <- function(titolo, testo, mostra = isTRUE(input$mostra_titolo)) {
  par(mar = c(5, 5, if (mostra) 4 else 2, 2) + 0.1)
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = if (mostra) titolo else "")
  text(1, 1, testo, cex = 0.95, col = "#7f8c8d")
}

# ---- Lag plot (style of the residual plot) ----------------------------------
disegna_grafico_lag <- function(res, titolo_forzato = NULL) {
  titolo <- if (is.null(titolo_forzato)) "Lag Plot of Residuals" else titolo_forzato
  mostra_t <- !is.null(titolo_forzato) || isTRUE(input$mostra_titolo)
  if (is.null(res) || !isTRUE(res$successo)) return(grafico_messaggio(titolo, "Waiting for optimization...", mostra_t))
  dg <- diagnostica_residui(res); L <- dg$lag
  if (L$m < 3) return(grafico_messaggio(titolo, "At least three residuals are needed.", mostra_t))
  pt_col  <- if (is.null(input$pt_col_res)) "#2c3e50" else input$pt_col_res
  ln_col  <- if (is.null(input$ln_col_res)) "#e74c3c" else input$ln_col_res
  pt_cex  <- if (is.null(input$pt_cex_res)) 1.3 else input$pt_cex_res
  pt_pch  <- as.numeric(if (is.null(input$pt_pch_res)) "17" else input$pt_pch_res)
  fnt_cex <- if (is.null(input$fnt_cex_res)) 1.0 else input$fnt_cex_res
  ln_lty  <- as.numeric(if (is.null(input$ln_lty_res)) "2" else input$ln_lty_res)
  grid_on <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  e <- L$e; m <- length(e)
  etichetta <- if (L$su_medie) "mean residual per x" else "residual"
  par(mar = c(5, 5, if (mostra_t) 4 else 2, 2) + 0.1)
  lim <- range(e) + c(-1, 1) * 0.05 * diff(range(e))
  plot(e[-m], e[-1], main = if (mostra_t) titolo else "", col = pt_col, pch = pt_pch, cex = pt_cex,
       cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1, xlim = lim, ylim = lim,
       xlab = paste0("Previous ", etichetta, " (i - 1)"), ylab = paste0("Current ", etichetta, " (i)"))
  abline(h = 0, v = 0, col = ln_col, lty = ln_lty)
  if (grid_on) grid()
  testo <- sprintf("Lag-1 autocorrelation r1 = %.3f (approx. 95%% bounds \u00b1%.3f)%s", L$r1, L$banda, if (m < 10) " \u2014 fewer than 10 values: little information" else "")
  if (!isFALSE(input$res_mostra_sottotitolo)) mtext_adattato(testo, cex = 0.75 * fnt_cex, col = if (is.finite(L$r1) && abs(L$r1) > L$banda) "#d35400" else "#566573")
}

# ---- Variance structure (style of the Q-Q plot) -----------------------------
# With enough replicates: SD (corrected for the number of replicates) against
# mean, on a logarithmic scale, with the estimated line. Otherwise: |r|^(2/3) of the
# studentized residuals against log(predicted values), with a lowess curve.
disegna_grafico_sd_media <- function(res, titolo_forzato = NULL) {
  mostra_t <- !is.null(titolo_forzato) || isTRUE(input$mostra_titolo)
  if (is.null(res) || !isTRUE(res$successo)) return(grafico_messaggio(if (is.null(titolo_forzato)) "Variance Structure" else titolo_forzato, "Waiting for optimization...", mostra_t))
  dg <- diagnostica_residui(res)
  pt_col  <- if (is.null(input$pt_col_qq)) "#2c3e50" else input$pt_col_qq
  ln_col  <- if (is.null(input$ln_col_qq)) "#e74c3c" else input$ln_col_qq
  pt_cex  <- if (is.null(input$pt_cex_qq)) 1.3 else input$pt_cex_qq
  pt_pch  <- as.numeric(if (is.null(input$pt_pch_qq)) "17" else input$pt_pch_qq)
  fnt_cex <- if (is.null(input$fnt_cex_qq)) 1.0 else input$fnt_cex_qq
  ln_lty  <- as.numeric(if (is.null(input$ln_lty_qq)) "2" else input$ln_lty_qq)
  ln_lwd  <- if (is.null(input$ln_lwd_qq)) 2 else input$ln_lwd_qq
  grid_on <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  s <- dg$sd_media; sp <- dg$spread
  if (isTRUE(s$disponibile)) {
    titolo <- if (is.null(titolo_forzato)) "Replicate SD vs Mean" else titolo_forzato
    par(mar = c(5, 5, if (mostra_t) 4 else 2, 2) + 0.1)
    plot(s$dati$Mean, s$dati$SD_corr, log = "xy", main = if (mostra_t) titolo else "", col = pt_col, pch = pt_pch, cex = pt_cex,
         cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1, xlab = "Mean of replicates (log scale)", ylab = "SD of replicates, bias-corrected (log scale)")
    xs <- exp(seq(log(min(s$dati$Mean)), log(max(s$dati$Mean)), length.out = 100))
    lines(xs, s$a * xs^s$b, col = ln_col, lty = ln_lty, lwd = ln_lwd)
    if (grid_on) grid(equilogs = FALSE)
    if (!isFALSE(input$qq_mostra_sottotitolo)) mtext_adattato(sprintf("SD = a \u00b7 mean^b,  b = %.2f (95%% CI %.2f to %.2f);  \u03b6 = 2b = %.2f (%.2f to %.2f)", s$b, s$ci[1], s$ci[2], 2 * s$b, 2 * s$ci[1], 2 * s$ci[2]), cex = 0.75 * fnt_cex, col = "#566573")
  } else if (isTRUE(sp$disponibile)) {
    titolo <- if (is.null(titolo_forzato)) "Variance Structure (residuals)" else titolo_forzato
    par(mar = c(5, 5, if (mostra_t) 4 else 2, 2) + 0.1)
    plot(sp$x, sp$y, main = if (mostra_t) titolo else "", col = pt_col, pch = pt_pch, cex = pt_cex,
         cex.axis = fnt_cex, cex.lab = fnt_cex * 1.1, las = 1, xlab = "ln(predicted value)",
         ylab = if (sp$studentizzati) expression("|studentized residual|"^{2/3}) else expression("|residual / s|"^{2/3}))
    if (length(unique(sp$x)) >= 4) { lw <- lowess(sp$x, sp$y); lines(lw, col = ln_col, lty = ln_lty, lwd = ln_lwd) }
    if (grid_on) grid()
    if (!isFALSE(input$qq_mostra_sottotitolo)) mtext_adattato("No replicates: a flat trend indicates constant variance, a rising trend variance increasing with the mean", cex = 0.72 * fnt_cex, col = "#566573")
  } else {
    grafico_messaggio(if (is.null(titolo_forzato)) "Variance Structure" else titolo_forzato, if (dg$ha_repliche && !isTRUE(sp$disponibile)) paste0("Not available: ", s$motivo, ".") else "Not available: at least three positive predicted values are needed.")
  }
}

output$grafico_lag <- renderPlot({ par(family = "sans"); disegna_grafico_lag(tryCatch(elaborazione(), error = function(e) NULL)) })
output$grafico_sd_media <- renderPlot({ par(family = "sans"); disegna_grafico_sd_media(tryCatch(elaborazione(), error = function(e) NULL)) })

scarica_grafico_diagnostico <- function(nome, funzione) downloadHandler(
  filename = function() paste0(nome, ".", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    par(family = "sans"); funzione(res); dev.off()
  })
output$scarica_grafico_lag <- scarica_grafico_diagnostico("lag_plot", disegna_grafico_lag)
output$scarica_grafico_sd_media <- scarica_grafico_diagnostico("variance_structure_plot", disegna_grafico_sd_media)

# ---- Tables (interface and exports) -----------------------------------------
tabelle_diagnostica_residui <- function(dg) {
  vuoto <- data.frame(Note = character(0))
  if (is.null(dg)) return(list(Residual_Autocorrelation = vuoto, Runs_Test = vuoto, Replicates = vuoto, Lack_of_Fit = vuoto, Variance_Homogeneity = vuoto))
  L <- dg$lag
  autoc <- data.frame(Residuals_Used = if (L$su_medie) "Mean residual per x (replicates)" else "Residuals ordered by x", Values = L$m,
                      Lag1_Autocorrelation = signif(L$r1, 4), Approx_95_Bound = signif(L$banda, 4),
                      Outside_Bounds = if (is.finite(L$r1)) abs(L$r1) > L$banda else NA, stringsAsFactors = FALSE)
  repl <- if (dg$ha_repliche) { t <- dg$tabella; t[] <- lapply(t, function(v) if (is.numeric(v)) signif(v, 6) else v); names(t)[names(t) == "CV_percent"] <- "CV_%"; t } else vuoto
  lof <- if (isTRUE(dg$lof$disponibile)) with(dg$lof, data.frame(
      Source = c("Lack of fit", "Pure error", "Residual (total)"), SS = signif(c(ss_lof, ss_pe, sse), 6), DF = c(df_lof, df_pe, df_lof + df_pe),
      Mean_Square = signif(c(ss_lof / df_lof, ss_pe / df_pe, sse / (df_lof + df_pe)), 6), F = c(signif(F, 4), NA, NA), P_Value = c(signif(p, 4), NA, NA)))
    else data.frame(Note = paste0("Lack-of-fit test not available: ", dg$lof$motivo, "."))
  om <- if (isTRUE(dg$omogeneita$disponibile)) with(dg$omogeneita, {
      righe <- data.frame(Test = "Bartlett", Statistic = signif(bartlett[["statistica"]], 4), DF = sprintf("%g", bartlett[["df"]]),
                          P_Value = signif(bartlett[["p"]], 4), Groups = gruppi, stringsAsFactors = FALSE)
      riga_scarti <- function(r, nome) if (!is.null(r)) data.frame(Test = nome, Statistic = signif(r[["statistica"]], 4),
                          DF = sprintf("%g, %g", r[["df1"]], r[["df2"]]), P_Value = signif(r[["p"]], 4), Groups = r[["gruppi"]], stringsAsFactors = FALSE)
        else data.frame(Test = nome, Statistic = NA, DF = "", P_Value = NA, Groups = NA, stringsAsFactors = FALSE)
      rbind(righe, riga_scarti(brown_forsythe, "Brown-Forsythe (deviations from the median)")) })
    else data.frame(Note = paste0("Variance homogeneity tests not available: ", dg$omogeneita$motivo, "."))
  R <- dg$runs
  runs <- if (isTRUE(R$disponibile)) data.frame(Residuals_Used = autoc$Residuals_Used, Positive = R$np, Negative = R$nn, Observed_Runs = R$n_runs,
                  Expected_Runs = signif(R$attese, 4), SD_Runs = signif(R$sigma, 4), Z_Too_Few = signif(R$z_tf, 4), Z_Too_Many = signif(R$z_tm, 4),
                  P_Too_Few_Exact = signif(R$p_poche, 4), P_Too_Many_Exact = signif(R$p_troppe, 4), stringsAsFactors = FALSE)
    else data.frame(Note = paste0("Runs test not available: ", R$motivo, "."))
  list(Residual_Autocorrelation = autoc, Runs_Test = runs, Replicates = repl, Lack_of_Fit = lof, Variance_Homogeneity = om)
}

# Robust even if the table is missing (no valid fit): returns NULL
# instead of failing (previously: "colnames" error after an unsuccessful fit).
senza_underscore <- function(df) { if (!is.data.frame(df)) return(NULL); colnames(df) <- gsub("_", " ", colnames(df)); df }
output$tabella_runs <- renderTable({ senza_underscore(tabelle_diagnostica_residui(diagnostica_corrente())$Runs_Test) }, na = "")
output$tabella_lof <- renderTable({ senza_underscore(tabelle_diagnostica_residui(diagnostica_corrente())$Lack_of_Fit) }, na = "")
output$tabella_omogeneita <- renderTable({ senza_underscore(tabelle_diagnostica_residui(diagnostica_corrente())$Variance_Homogeneity) }, na = "")
output$tabella_repliche <- renderTable({ senza_underscore(tabelle_diagnostica_residui(diagnostica_corrente())$Replicates) }, na = "", digits = 6)

# ---- Section below the grid -------------------------------------------------
nota <- function(...) p(style = "font-size: 12px; color: #566573; margin: 4px 0 10px 0;", ...)
# Title of a table with the "Copy to Clipboard" button, as in the other
# tables of the app (JavaScript function copiaTabellaClipboard in ui.R).
titolo_con_copia <- function(titolo, id_tabella, livello = tags$h5) {
  id_bottone <- paste0("btn_copia_", id_tabella)
  tags$div(style = "display: flex; align-items: center; gap: 10px; margin-top: 10px; margin-bottom: 4px;",
    livello(style = "margin: 0;", titolo),
    tags$button(id = id_bottone, "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm", style = "padding-top: 2px; padding-bottom: 2px;",
                onclick = sprintf("copiaTabellaClipboard('%s', '%s')", id_tabella, id_bottone)))
}

# id: names of the tables (those of Single Fit by default; Batch passes its own).
ID_TABELLE_SINGLE_FIT <- list(runs = "tabella_runs", lof = "tabella_lof", omogeneita = "tabella_omogeneita", repliche = "tabella_repliche")
sezione_runs <- function(dg, id = ID_TABELLE_SINGLE_FIT) {
  R <- dg$runs
  tagList(
    titolo_con_copia("Residual Randomness: Runs Test", id$runs, tags$h4),
    tableOutput(id$runs),
    if (isTRUE(R$disponibile)) nota(
      sprintf("%d runs of residuals with the same sign, %.2f expected if the residuals are randomly distributed%s. ", R$n_runs, R$attese, if (dg$lag$su_medie) " (mean residual per x, since the data contain replicates)" else ""),
      if (R$p_poche < 0.05) "Too few runs (exact p < 0.05): the residuals form systematic groups of the same sign, which suggests that the model does not follow the shape of the data. "
      else if (R$p_troppe < 0.05) "Too many runs (exact p < 0.05): the residuals alternate in sign more than expected by chance. "
      else "No evidence of non-random residuals (exact p \u2265 0.05). ",
      if (R$p_minimo > 0.05) sprintf("With these %d residuals even the most extreme pattern would give p = %.3f, so the test cannot detect a systematic pattern. ", R$np + R$nn, R$p_minimo) else "",
      "Test as described by van Boekel (2008, section 7.8), who suggests treating |Z| larger than about 3 as a signal; this cut-off cannot be reached with fewer than about 14 residuals, so the exact one-sided p-values are given as well.")
  )
}
output$diagnostica_repliche_ui <- renderUI({
  dg <- diagnostica_corrente()
  if (is.null(dg)) return(NULL)
  tagList(sezione_runs(dg), sezione_repliche(dg))
})
sezione_repliche <- function(dg, id = ID_TABELLE_SINGLE_FIT) {
  if (!dg$ha_repliche) return(nota(tags$strong("Replicate diagnostics: "), "not available, no repeated x values in the data. ",
    "The fourth plot shows the variance structure from the studentized residuals instead (|r|^(2/3) against the logarithm of the predicted values; Carroll & Ruppert, 1988, section 2.7)."))
  sm <- dg$sd_media
  tagList(
    tags$h4("Replicate Diagnostics"),
    nota(sprintf("%d of %d distinct x values have replicates (%d of %d observations).", dg$n_x_replicati, dg$m, dg$n_oss_replicate, dg$n)),
    titolo_con_copia("Lack-of-Fit Test", id$lof),
    tableOutput(id$lof),
    if (isTRUE(dg$lof$disponibile)) nota(
      sprintf("Pure-error standard deviation %.4g (from the replicates) vs residual standard error %.4g. ", dg$lof$s_pe, dg$lof$rmse),
      if (dg$lof$p < 0.05) "The residuals are larger than the scatter of the replicates explains (p < 0.05): the model does not describe the data adequately."
      else "No evidence of lack of fit: the residuals are consistent with the scatter of the replicates (p \u2265 0.05).",
      " The test assumes constant variance: with clearly non-constant variance (see below) it can signal lack of fit even when the model is correct. Test as described by van Boekel (2008, section 7.4.3); for nonlinear models it is approximate, as are the other F-based procedures (Bates & Watts, 1988)."),
    titolo_con_copia("Variance Homogeneity", id$omogeneita),
    tableOutput(id$omogeneita),
    if (isTRUE(dg$omogeneita$disponibile)) nota(
      "Bartlett's test assumes normally distributed data; the Brown-Forsythe test is robust to non-normality",
      if (is.null(dg$omogeneita$brown_forsythe)) " but needs at least two x values with four or more replicates (with two or three replicates the deviations from the median are degenerate and the test cannot detect differences)" else "",
      ". With few replicates per x both tests have low power: a non-significant result does not prove that the variance is constant, so read them together with the SD vs Mean plot."),
    if (isTRUE(sm$disponibile)) nota(tags$strong("SD vs Mean: "),
      sprintf("b = %.2f (95%% CI %.2f to %.2f) from %d groups%s, regressing ln(SD) on ln(mean) with ln(SD) corrected for the number of replicates (Carroll & Ruppert, 1988, section 3.3.6). ", sm$b, sm$ci[1], sm$ci[2], sm$gruppi, if (sm$esclusi > 0) sprintf(" (%d excluded: non-positive mean or SD)", sm$esclusi) else ""),
      if (max(sm$dati$n) <= 3) "With two or three replicates per x the estimate of b is imprecise. " else "",
      if (length(sm$compatibili)) paste0("The interval includes: ", paste(sm$compatibili, collapse = "; "), ".") else "The interval includes none of the reference values 0, 0.5 and 1.",
      sprintf(" Equivalently, \u03b6 = 2b = %.2f (95%% CI %.2f to %.2f) in the variance model var(y) = \u03c3\u00b2\u00b7\u0177^\u03b6, which can be used for the weighting in section 5 (option \u201c\u03b6 = 2b from the replicates\u201d).", 2 * sm$b, 2 * sm$ci[1], 2 * sm$ci[2]),
      " Reference values: b = 0, ordinary least squares are appropriate; b = 0.5, variance proportional to the mean; b = 1, constant coefficient of variation (consider the ln(y) pre-transformation).")
    else nota(tags$strong("SD vs Mean: "), paste0("not available, ", sm$motivo, ".")),
    titolo_con_copia("Replicates", id$repliche),
    tableOutput(id$repliche)
  )
}
