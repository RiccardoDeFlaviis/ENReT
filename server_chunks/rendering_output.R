# ============================
# OUTPUT RENDERING
# ============================
dpi_attivo <- function() {
  if (is.null(input$jpeg_dpi) || is.na(input$jpeg_dpi) || input$jpeg_dpi <= 0) 200 else input$jpeg_dpi
}

# Shared helper: reads the global "Export Width/Height (px)" fields (sidebar)
# and returns both the dimensions in pixels (used directly by jpeg(), whose
# default is units="px") and in inches (required by pdf(), which only
# accepts inches) using the current DPI as the conversion factor. If the
# fields aren't valid, it falls back to a 2000x1400px default.
dimensioni_esportazione <- function() {
  dpi <- dpi_attivo()
  larghezza_px <- if (is.null(input$export_width_px) || is.na(input$export_width_px) || input$export_width_px <= 0) 2000 else input$export_width_px
  altezza_px <- if (is.null(input$export_height_px) || is.na(input$export_height_px) || input$export_height_px <= 0) 1400 else input$export_height_px
  # Server-side safety limit: the min/max attributes of numericInput in the
  # UI are only HTML hints, they don't prevent manually typing an extreme
  # value. A user could type e.g. 1 million pixels, risking a huge memory
  # allocation inside the WASM sandbox (limited memory).
  larghezza_px <- min(larghezza_px, 10000)
  altezza_px <- min(altezza_px, 10000)
  list(width_px = larghezza_px, height_px = altezza_px,
       width_in = larghezza_px / dpi, height_in = altezza_px / dpi, dpi = dpi)
}

# ---- Combined image (several plots in a single grid figure) ----
# Palette shared by the overlay plots and by the panels of the single fits
# (Batch and Model Comparison): same list in all 4 places, so a
# sample has the same colour in the overlay plot and in its panel.
# The three "single color" options give the same colour to all series:
# designed for the combined image, where each series has its own panel and
# there is no need to distinguish them by colour (in the overlay plot they would
# instead be indistinguishable).
PALETTE_FISSA_SERIE <- c("#e74c3c", "#2980b9", "#27ae60", "#f39c12", "#8e44ad", "#16a085", "#c0392b", "#34495e")
COLORI_SINGOLI_SERIE <- c(nero = "#000000", grigio_scuro = "#404040", rosso = "#c0392b")
colori_da_palette <- function(tipo, n) {
  if (is.null(tipo)) tipo <- "fissa"
  if (tipo %in% names(COLORI_SINGOLI_SERIE)) return(rep(COLORI_SINGOLI_SERIE[[tipo]], n))
  switch(tipo,
    "rainbow" = rainbow(n),
    "viridis" = hcl.colors(n, palette = "viridis"),
    "plasma" = hcl.colors(n, palette = "plasma"),
    "cividis" = hcl.colors(n, palette = "cividis"),
    "spectral" = hcl.colors(n, palette = "spectral"),
    "heat" = hcl.colors(n, palette = "heat"),
    rep_len(PALETTE_FISSA_SERIE, n)
  )
}
SCELTE_PALETTE_SERIE <- c(
  "Fixed Palette (best for few series)" = "fissa", "Rainbow" = "rainbow", "Viridis" = "viridis", "Plasma" = "plasma",
  "Cividis" = "cividis", "Spectral" = "spectral", "Heat" = "heat",
  "Black (single color \u2014 for combined images)" = "nero",
  "Dark Grey (single color \u2014 for combined images)" = "grigio_scuro",
  "Red (single color \u2014 for combined images)" = "rosso"
)
# Colour of the symbols: equal to that of the curve (historical behaviour) or
# a fixed colour, independent of the palette of the curves.
SCELTE_COLORE_PUNTI <- c("Same as line" = "uguale", "Black" = "#000000", "Dark Grey" = "#404040", "Red" = "#c0392b")
colori_punti_serie <- function(scelta, colori_curve) {
  if (is.null(scelta) || identical(scelta, "uguale")) colori_curve else rep(scelta, length(colori_curve))
}

# Style of the panel letters (A / a / (a), bold / normal /
# italic) - returns text and font (1 normal, 2 bold, 3 italic).
etichetta_pannello <- function(i) {
  lettera <- if (i <= 26) letters[i] else paste0(letters[((i - 1) %/% 26)], letters[((i - 1) %% 26) + 1])
  stile <- if (is.null(input$comp_stile_lettere)) "maiusc_grassetto" else input$comp_stile_lettere
  switch(stile,
    "maiusc_normale" = list(testo = toupper(lettera), font = 1),
    "minusc_corsivo" = list(testo = lettera, font = 3),
    "parentesi_corsivo" = list(testo = paste0("(", lettera, ")"), font = 3),
    "parentesi_normale" = list(testo = paste0("(", lettera, ")"), font = 1),
    "parentesi_dx_corsivo" = list(testo = paste0(lettera, ")"), font = 3),
    "parentesi_dx_normale" = list(testo = paste0(lettera, ")"), font = 1),
    list(testo = toupper(lettera), font = 2)
  )
}
# Computes rows/columns of the grid according to the global settings in
# "3. Plot Export": Auto (as square as possible), fixed number of
# columns, or fixed number of rows.
griglia_composita <- function(n_pannelli) {
  n_pannelli <- max(1, n_pannelli)
  modo <- if (is.null(input$comp_modo_layout)) "auto" else input$comp_modo_layout
  valore_intero <- function(v, predefinito) if (is.null(v) || is.na(v) || v < 1) predefinito else as.integer(round(v))
  if (modo == "colonne") {
    n_col <- min(valore_intero(input$comp_n_colonne, 3), n_pannelli)
    n_righe <- ceiling(n_pannelli / n_col)
  } else if (modo == "righe") {
    n_righe <- min(valore_intero(input$comp_n_righe, 2), n_pannelli)
    n_col <- ceiling(n_pannelli / n_righe)
  } else {
    n_col <- ceiling(sqrt(n_pannelli))
    n_righe <- ceiling(n_pannelli / n_col)
  }
  # Avoids completely empty rows: filling by rows, the rows actually
  # used are ceiling(n / columns) (e.g. 4 plots with 3 fixed rows ->
  # 2 columns -> 2 rows are enough, the third would remain empty); by
  # columns, the same applies to the columns. Only rows/columns
  # that would remain completely empty are removed, never panels or occupied places.
  if (identical(input$comp_riempimento, "colonne")) n_col <- ceiling(n_pannelli / n_righe)
  else n_righe <- ceiling(n_pannelli / n_col)
  c(righe = n_righe, colonne = n_col)
}

# Draws n_pannelli plots (funzione_disegno(i), i = 1..n_pannelli) in
# a single grid image and writes it to 'file', in the format (PDF/JPEG)
# and DPI already chosen in the sidebar. Each panel has the size given in
# "Panel size" - the total image is columns x width by rows x
# height, reduced proportionally if it exceeds the safety limit of
# 10000 px per side (the same already applied to every other download).
crea_immagine_composita <- function(file, n_pannelli, funzione_disegno) {
  griglia <- griglia_composita(n_pannelli)
  dpi <- dpi_attivo()
  pannello_w <- if (is.null(input$comp_pannello_w_px) || is.na(input$comp_pannello_w_px) || input$comp_pannello_w_px < 100) 1000 else input$comp_pannello_w_px
  pannello_h <- if (is.null(input$comp_pannello_h_px) || is.na(input$comp_pannello_h_px) || input$comp_pannello_h_px < 100) 800 else input$comp_pannello_h_px
  totale_w <- griglia[["colonne"]] * pannello_w
  totale_h <- griglia[["righe"]] * pannello_h
  fattore <- min(1, 10000 / totale_w, 10000 / totale_h)
  totale_w <- round(totale_w * fattore)
  totale_h <- round(totale_h * fattore)
  
  if (identical(input$formato_esportazione, "pdf")) pdf(file, width = totale_w / dpi, height = totale_h / dpi)
  else jpeg(file, width = totale_w, height = totale_h, res = dpi, quality = 95)
  on.exit(dev.off(), add = TRUE)
  
  # Grid of HALF cells: each panel occupies 2 half columns (filling
  # by rows) or 2 half rows (by columns). It serves to centre the last
  # incomplete row: with m panels on c places, it shifts by (c - m) half
  # cells, leaving the same empty space on the left and on the right. E.g. 7
  # plots on 3 columns: the seventh goes to the centre of the third row instead
  # of to the left (with mfrow/mfcol this is not possible, they always fill
  # from the start).
  n_righe <- griglia[["righe"]]; n_col <- griglia[["colonne"]]
  per_colonne <- identical(input$comp_riempimento, "colonne")
  if (!per_colonne) {
    matrice <- matrix(0L, nrow = n_righe, ncol = 2 * n_col)
    for (i in seq_len(n_pannelli)) {
      riga <- (i - 1) %/% n_col + 1
      pos <- (i - 1) %% n_col
      nella_riga <- if (riga < n_righe) n_col else n_pannelli - (n_righe - 1) * n_col
      spostamento <- n_col - nella_riga
      matrice[riga, spostamento + 2 * pos + 1:2] <- i
    }
  } else {
    matrice <- matrix(0L, nrow = 2 * n_righe, ncol = n_col)
    for (i in seq_len(n_pannelli)) {
      colonna <- (i - 1) %/% n_righe + 1
      pos <- (i - 1) %% n_righe
      nella_colonna <- if (colonna < n_col) n_righe else n_pannelli - (n_col - 1) * n_righe
      spostamento <- n_righe - nella_colonna
      matrice[spostamento + 2 * pos + 1:2, colonna] <- i
    }
  }
  layout(matrice)
  # layout() can shrink the text like mfrow - here it is not needed, because
  # the image grows with the number of panels: each one has the same space
  # as a single plot, therefore the same text.
  par(cex = 1)
  mostra_lettere <- isTRUE(input$comp_lettere)
  pos_lettere <- if (is.null(input$comp_pos_lettere)) "fuori" else input$comp_pos_lettere
  cex_lettere <- if (is.null(input$comp_cex_lettere) || is.na(input$comp_cex_lettere)) 1.5 else input$comp_cex_lettere
  
  for (i in seq_len(n_pannelli)) {
    tryCatch(funzione_disegno(i), error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = paste("Panel", i, "could not be drawn"))
    })
    if (mostra_lettere) {
      et <- etichetta_pannello(i)
      vecchio_xpd <- par(xpd = NA)
      if (pos_lettere == "dentro_sx") {
        # Inside the plot area, top left corner
        text(grconvertX(0.03, "npc", "user"), grconvertY(0.97, "npc", "user"), et$testo, font = et$font, cex = cex_lettere, adj = c(0, 1))
      } else if (pos_lettere == "dentro_dx") {
        # Inside the plot area, top right corner
        text(grconvertX(0.97, "npc", "user"), grconvertY(0.97, "npc", "user"), et$testo, font = et$font, cex = cex_lettere, adj = c(1, 1))
      } else {
        # Outside the plot area: top left corner of the
        # PANEL, independent of the margins of each drawing function.
        text(grconvertX(0.02, "nfc", "user"), grconvertY(0.97, "nfc", "user"), et$testo, font = et$font, cex = cex_lettere, adj = c(0, 1))
      }
      par(vecchio_xpd)
    }
  }
  invisible(NULL)
}

# File name of the combined download, with the same format chosen in the sidebar.
nome_file_composito <- function(prefisso) paste0(prefisso, "_combined.", if (identical(input$formato_esportazione, "pdf")) "pdf" else "jpg")

# "Same axis ranges in every panel" control for the panels of the single
# fits (Batch, Model Comparison).
assi_condivisi_composito <- function() if (is.null(input$comp_assi_condivisi)) TRUE else isTRUE(input$comp_assi_condivisi)

# Swaps the x and y columns of a data.frame: "Swap X and Y" option in the
# Data Input of Single Fit and Batch, for data entered or loaded in reverse
# order (y, x instead of x, y). The other columns, if present, remain
# unchanged. It must be applied to the raw data, BEFORE any transformation
# (Y pre-transformation, Distribution Density), which act on y.
# ---- Numerically stable inversion of the Hessian matrix (or JtJ) ----
# The covariance matrix is derived from it, and hence the standard errors.
# With parameters on very different scales (e.g. NIST Bennett5: b1 ~ -2500, b3 ~
# 0.9) the matrix is ill-conditioned and solve() rejects it as
# "numerically singular" (reciprocal condition number 1.1e-17 for
# Bennett5, below R's threshold of ~2.2e-16): the standard errors remained
# empty. Here the matrix is first scaled to a unit diagonal, inverted,
# and brought back to the original scale: mathematically identical result,
# numerically stable (diagonal equilibration, a standard technique of
# numerical linear algebra). Verified: on Bennett5 it agrees to 8 digits with
# the independent method via QR decomposition of the Jacobian (the one of
# nls() in R); on Rat43, well conditioned, it is identical digit by digit to
# the direct solve(). If the diagonal is not positive or there are non-finite
# values, it falls back to the direct solve() (previous behaviour).
# ---- Levenberg-Marquardt settings (minpack.lm::nls.lm) ----
# Used by the main fit, by the Monte Carlo bootstrap and by Batch, so
# they always remain consistent. Three corrections arising from the validation on the 26
# NIST StRD datasets:
#  - maxfev: previously it was not set and minpack.lm used its default,
#    100 x (parameters + 1) function evaluations (400 with 3 parameters).
#    Each iteration uses about (parameters + 1) of them for the numerical
#    Jacobian, so the fit stopped after ~100 iterations whatever
#    "Max Iterations" was: MGH10, Bennett5, MGH09 and MGH17 did not converge. Now
#    maxfev is high enough never to be the actual limit.
#  - maxiter: nls.lm reduces it to 1024 anyway (limit of the library),
#    warning only in the R console; here the ceiling is explicit and
#    stated in the interface.
#  - ftol/ptol: the default (~1.5e-8) stops the fit short of the precision
#    of the certified values (ENSO, Thurber, MGH09 at 3-4 correct digits);
#    with 1e-12 it reaches 5-7 digits.
MASSIMO_ITERAZIONI_LM <- 1024
controllo_lm <- function(maxiter, n_parametri, tolleranza = 1e-12) {
  iterazioni <- min(maxiter, MASSIMO_ITERAZIONI_LM)
  nls.lm.control(maxiter = iterazioni, maxfev = max(100000, iterazioni * (n_parametri + 1) * 10), ftol = tolleranza, ptol = tolleranza)
}

# ---- Residuals to be resampled in the residual bootstrap ----
# Davison & Hinkley (1997): modified residuals r_j = e_j / (1 - h_j)^(1/2)
# (section 6.2.1, eq. 6.9, p. 259), which have the same variance as the errors,
# then centred, r_j - mean(r) (section 6.2.3, Algorithm 6.1, pp. 261-262); for
# nonlinear models the leverages h_j come from the linear approximation, with
# the derivatives of the model at the estimates in place of the explanatory variables, and
# the adjustment of the mean is in general necessary (section 7.4, p. 355).
# The same quantities are needed for the noise of the new observations (prediction
# band; section 6.3.3, Algorithm 6.4, p. 284). Leverages from the Jacobian J:
# diagonal of J (J^T J)^-1 J^T. The modification with the leverages applies to least
# squares (J = NULL with the other losses: centring only). With 'modifica'
# disabled the raw residuals are returned, as in the previous
# versions.
residui_per_bootstrap <- function(residui, J = NULL, modifica = TRUE) {
  if (!isTRUE(modifica)) return(residui)
  if (!is.null(J) && nrow(J) == length(residui)) {
    h <- tryCatch(rowSums((J %*% inverti_hessiana(crossprod(J))) * J), error = function(e) NULL)
    # Leverages close to 1 (points that the model reproduces almost exactly):
    # upper limit to avoid divisions by values close to zero.
    if (!is.null(h) && all(is.finite(h))) residui <- residui / sqrt(1 - pmin(pmax(h, 0), 0.99))
  }
  residui - mean(residui)
}

# ---- Weighted regression: support functions ----
# Weights w_i = 1/yhat_i^zeta ("power of the mean" variance model:
# var(y) = sigma^2 f^zeta), computed from the fit and saved in res$pesi; res$rmse
# is then sigma. Without weights these functions return the usual values.
ponderazione_attiva <- function(res) isTRUE(res$ponderazione$attiva) && !is.null(res$pesi)
# Residuals for the diagnostics: with weights, the weighted residuals sqrt(w)(y - yhat),
# which have constant variance if the weights are correct.
residui_diagnostici <- function(res) if (ponderazione_attiva(res)) res$residui * sqrt(res$pesi) else res$residui
# Description of the weights for summary, tables and exports.
descrizione_ponderazione <- function(res) {
  if (!ponderazione_attiva(res)) return("None")
  pd <- res$ponderazione
  paste0("w = 1/\u0177^", signif(pd$zeta, 4),
         switch(pd$schema, zeta1 = " (variance proportional to the mean)", zeta2 = " (constant coefficient of variation)",
                repliche = sprintf(" (\u03b6 = 2b, b = %.3f, 95%% CI %.3f to %.3f, from the replicates)", pd$b, pd$b_ci[1], pd$b_ci[2]), ""),
         if (!is.null(pd$iterazioni_irls)) sprintf("; %d reweighting cycles", pd$iterazioni_irls) else "")
}
# Relative variance of a new observation where the model equals y_previsti:
# y_previsti^zeta with weights (NA where y_previsti <= 0), 1 without weights.
varianza_relativa <- function(res, y_previsti) {
  if (!ponderazione_attiva(res)) return(rep(1, length(y_previsti)))
  ifelse(is.finite(y_previsti) & y_previsti > 0, abs(y_previsti)^res$ponderazione$zeta, NA_real_)
}

# ---- Multiplier of the confidence band (linear method) ----
# Pointwise: t(1 - alpha/2; n - p). Simultaneous, if requested: the "1 - alpha
# approximate inference band" of Bates & Watts (1988, ch. 2),
#   f(x, theta) +/- s ||v^T R^-1|| sqrt(p F(p, n - p; alpha)),
# where s ||v^T R^-1|| = sqrt(v^T s^2 (V^T V)^-1 v) is the same standard
# error of the curve used for the pointwise band (delta method): only
# the multiplier changes. F(p, n - p; alpha) is the upper alpha quantile,
# i.e. qf(1 - alpha, p, n - p). The prediction band remains pointwise.
# General version, also used by the secondary models (Shelf-Life and
# Non-Isothermal): df = residual degrees of freedom, p = estimated parameters,
# level in percent. NA if the degrees of freedom are not valid.
moltiplicatore_banda <- function(df, p, livello, simultanea = FALSE) {
  if (is.null(df) || length(df) != 1 || !is.finite(df) || df <= 0 || !is.finite(livello)) return(NA_real_)
  a <- 1 - livello / 100
  if (isTRUE(simultanea) && p >= 1) sqrt(p * qf(1 - a, p, df)) else qt(1 - a / 2, df)
}
moltiplicatore_banda_confidenza <- function(res) {
  puntuale <- valore_critico(res$df)
  if (!isTRUE(input$banda_simultanea)) return(puntuale)
  m <- moltiplicatore_banda(res$df, length(res$parametri), input$livello_confidenza, TRUE)
  if (is.finite(m)) m else puntuale
}

# ---- Information criteria: AIC, AICc, BIC (Burnham & Anderson, 1998) ----
# Form for least squares with Gaussian errors: K = p + 1 estimated
# parameters (the p of the model plus the error variance),
#   AIC  = n ln(SSE/n) + 2K
#   AICc = AIC + 2K(K + 1)/(n - K - 1)
#   BIC  = n ln(SSE/n) + K ln(n)
# The form omits the constant n(1 + ln 2pi): the absolute values differ
# from those of R (AIC() on an nls fit), the differences between models do not. Previously
# K = p was counted, which weakened the correction of AICc with few points
# (n = 8: penalty of the third parameter 5.60 instead of 9.33). The indices
# presuppose maximum-likelihood estimates, i.e. least squares
# with Gaussian errors: with the other losses they are not computed.
# With weighted least squares (weights w_i, variance sigma^2/w_i) the
# Gaussian log-likelihood contains in addition (1/2) sum(ln w_i): sse is the
# weighted sum of squares and somma_log_pesi = sum(ln w_i) is subtracted (zero
# without weights, hence no change). In this way AIC remains comparable
# also between fits with different variance structures.
criteri_informazione <- function(sse, n, p, tipo_loss = "mse", somma_log_pesi = 0) {
  vuoto <- list(aic = NA_real_, aicc = NA_real_, bic = NA_real_)
  if (!identical(tipo_loss, "mse") || !is.finite(sse) || sse <= 0 || !is.finite(n) || n <= 0) return(vuoto)
  K <- p + 1
  base <- n * log(sse / n) - somma_log_pesi
  aic <- base + 2 * K
  list(aic = aic, aicc = if (n - K - 1 > 0) aic + 2 * K * (K + 1) / (n - K - 1) else NA_real_, bic = base + K * log(n))
}
NOTA_CRITERI_LOSS <- "Not shown with losses other than MSE: AIC, AICc and BIC assume least-squares (maximum-likelihood) estimates."

# ---- Names without underscores in the Excel files ----
# In the Excel files, which are read directly, the names of sheets and columns
# appear without underscores, as on screen; the CSV files keep the original
# names. Used by all the Excel exports of the app.
per_excel <- function(fogli) {
  if (is.data.frame(fogli)) fogli <- list(Sheet1 = fogli)
  fogli <- lapply(fogli, function(df) { if (is.data.frame(df)) colnames(df) <- gsub("_", " ", colnames(df)); df })
  names(fogli) <- make.unique(trimws(substr(gsub("_", " ", names(fogli)), 1, 31)), sep = " ")
  fogli
}

# ---- Correction for multiple comparisons (post-hoc) ----
# Same three choices in all post-hoc analyses (parameters in Batch and Model
# Comparison, shelf life in Batch and Model Comparison). Holm and Bonferroni
# control the probability of even a single false positive in the family
# of comparisons (FWER); Benjamini-Hochberg the expected proportion of false
# positives among the comparisons declared significant (FDR), less severe and more
# powerful with many comparisons. The correction is applied separately for
# each parameter.
SCELTE_CORREZIONE_MULTIPLA <- c("Holm (recommended)" = "holm", "Bonferroni (more conservative)" = "bonferroni",
                                "Benjamini-Hochberg (FDR, less conservative)" = "BH")
etichetta_correzione <- function(metodo) switch(metodo, holm = "Holm", bonferroni = "Bonferroni", BH = "Benjamini-Hochberg (false discovery rate)", metodo)
descrizione_correzione <- function(metodo) if (identical(metodo, "BH")) {
  " It controls the expected proportion of false positives among the comparisons declared significant (FDR), rather than the probability of any false positive."
} else ""

# ---- Minimum number of successful Monte Carlo iterations ----
# Below this threshold the Monte Carlo results (empirical intervals, bootstrap
# SE, distribution summary, bands) are not shown: on
# very few values they have no statistical meaning. Same threshold in
# Single Fit and Batch (where it already existed); previously Single Fit computed
# everything even with 2 successful iterations out of 40.
MINIMO_ITERAZIONI_MC <- 10

# ---- Algorithm actually used ----
# Levenberg-Marquardt minimizes only sums of squares: with a loss other
# than MSE Nelder-Mead is used, as the interface does (and states), which changes
# the selector by itself. It is needed by the engine to protect itself when the
# combination arrives anyway (e.g. from a loaded configuration): previously
# Single Fit failed with an incomprehensible error ('arg' should be one
# of ...) and Batch instead fell back on BFGS, inconsistently.
algoritmo_effettivo <- function(algoritmo, tipo_loss) if (identical(algoritmo, "LM") && !identical(tipo_loss, "mse")) "Nelder-Mead" else algoritmo

# ---- Settings of BFGS, L-BFGS-B and Nelder-Mead (optim) ----
# Used by the main fit, by the Monte Carlo bootstrap and by Batch. Previously
# only maxit was passed: without scaling, with parameters on very
# different scales the optimizers stopped before the optimum (NIST Misra1a,
# b1 ~ 239 and b2 ~ 0.00055: BFGS at 1.8 correct digits, L-BFGS-B at 1.3).
# Now: parscale = absolute value of the starting point (1 if zero) and
# relative tolerance 1e-12 for BFGS and Nelder-Mead; on Misra1a BFGS and
# L-BFGS-B reach 4.3 digits, Nelder-Mead from 4.9 to 7.8. For L-BFGS-B
# (which ignores reltol) R's default tolerance (factr) is left:
# with factr = 10, tried previously, almost half of the fits ended with
# code 52 (ABNORMAL_TERMINATION_IN_LNSRCH) and were discarded as non-
# converging, without any gain in accuracy (4.3 digits in both
# cases on 200 simulated fits; the improvement comes from the scaling).
# Nelder-Mead: in optim() maxit counts the function evaluations (one or
# two per step of the simplex), not the iterations as for Levenberg-Marquardt
# and BFGS; with 200 evaluations and tolerance 1e-12 it did not converge in 34 models
# of the registry out of 85. Therefore 10 times Max Iterations is used (2000 with the
# default value): 79/85 converging with the same precision; beyond that
# nothing is gained (10000: 79/85).
FATTORE_VALUTAZIONI_NELDER_MEAD <- 10

# Nelder-Mead restarts: the simplex can get stuck far from the minimum
# and declare convergence when the changes become small. The search
# restarts from the point found with a new simplex (parscale on the new point)
# until the loss no longer improves (relative change < 1e-10), at most
# 10 times. On the 52 NIST fits: accepted fits with at least 4 correct digits from 24 to
# 39, accepted but inaccurate from 16 to 4. Optional (checkbox in section 5,
# on by default). For the other methods, or without restarts, the call is
# exactly optim() as before.
MAX_RIPARTENZE_NELDER_MEAD <- 10
optim_con_ripartenze <- function(par, fn, ..., method, control, hessian = FALSE, ripartenze = FALSE) {
  if (!identical(method, "Nelder-Mead") || !isTRUE(ripartenze))
    return(optim(par = par, fn = fn, ..., method = method, hessian = hessian, control = control))
  f <- optim(par = par, fn = fn, ..., method = method, control = control)
  conteggi <- f$counts; n <- 0; ctrl <- control
  while (n < MAX_RIPARTENZE_NELDER_MEAD) {
    ctrl$parscale <- ifelse(is.finite(f$par) & abs(f$par) > 1e-8, abs(f$par), 1)
    g <- optim(par = f$par, fn = fn, ..., method = method, control = ctrl); n <- n + 1
    migliorato <- is.finite(g$value) && (f$value - g$value) > 1e-10 * max(abs(f$value), 1e-300)
    conteggi["function"] <- conteggi["function"] + g$counts["function"]
    if (is.finite(g$value) && g$value <= f$value) f <- g
    if (!migliorato) break
  }
  if (hessian) f$hessian <- tryCatch(optimHess(f$par, fn, ..., control = ctrl[intersect(names(ctrl), c("fnscale", "parscale", "ndeps"))]), error = function(e) NULL)
  f$counts <- conteggi; f$ripartenze <- n
  f
}
controllo_optim <- function(metodo, maxiter, partenza, tolleranza = NULL) {
  scala <- ifelse(is.finite(partenza) & abs(partenza) > 1e-8, abs(partenza), 1)
  ctrl <- list(maxit = if (identical(metodo, "Nelder-Mead")) maxiter * FATTORE_VALUTAZIONI_NELDER_MEAD else maxiter, parscale = scala)
  if (identical(metodo, "L-BFGS-B")) {
    # L-BFGS-B expresses the tolerance as a multiple of the machine
    # precision (factr); without an explicit value R's one (1e7) remains.
    if (!is.null(tolleranza)) ctrl$factr <- tolleranza / .Machine$double.eps
  } else {
    ctrl$reltol <- if (is.null(tolleranza)) 1e-12 else tolleranza
  }
  ctrl
}

# ---- Convergence tolerance settable by the user ----
# Field "Convergence Tolerance (exponent, 10^x)" of section 5: applies to the
# main fit, Monte Carlo and Batch. Profiles and secondary models stay
# at the fixed (tight) tolerances, because their intervals depend on the
# precision of the re-optimization. Default values and limits by
# algorithm: for L-BFGS-B the default -9 (1e-9) is close to R's factr (1e7,
# ~2.2e-9; same NIST and Monte Carlo results) and the limit -11 avoids tolerances at which about half of the fits
# would end with code 52 (abnormal line search).
TOLLERANZA_ESPONENTI <- list(
  "LM"          = list(predefinito = -12, minimo = -15, massimo = -4),
  "BFGS"        = list(predefinito = -12, minimo = -15, massimo = -4),
  "Nelder-Mead" = list(predefinito = -12, minimo = -15, massimo = -4),
  "L-BFGS-B"    = list(predefinito = -9,  minimo = -11, massimo = -4)
)
limiti_tolleranza <- function(algoritmo) {
  l <- TOLLERANZA_ESPONENTI[[algoritmo]]
  if (is.null(l)) TOLLERANZA_ESPONENTI[["LM"]] else l
}
# Valid exponent for the algorithm (default if empty, brought within the limits).
esponente_tolleranza <- function(valore, algoritmo) {
  l <- limiti_tolleranza(algoritmo)
  if (is.null(valore) || length(valore) != 1 || !is.finite(valore)) return(l$predefinito)
  min(max(round(valore), l$minimo), l$massimo)
}

# ---- Analytical Jacobian of the model (symbolic derivatives, deriv() of base R) ----
# Returns a function (parameters, x, fixed) -> n x p matrix, columns
# in the order of nomi_liberi. The derivatives are computed symbolically by
# deriv(), hence exact (up to machine rounding). If the
# formula contains functions that deriv() does not know (e.g. ifelse, abs, min,
# max) or the result is not finite, it falls back on the numerical Jacobian
# (jacobiano_numerico, central differences). The formula is evaluated as in
# valuta_formula_generica(): same environment (parameters, fixed, x).
crea_jacobiano_modello <- function(formula_stringa, nomi_liberi) {
  espressione <- tryCatch(str2lang(formula_stringa), error = function(e) NULL)
  gradiente <- if (is.null(espressione) || length(nomi_liberi) == 0) NULL else tryCatch(deriv(espressione, nomi_liberi), error = function(e) NULL)
  function(parametri, x, fissi = list()) {
    valori <- c(as.list(setNames(as.numeric(parametri), nomi_liberi)), as.list(fissi))
    if (!is.null(gradiente)) {
      risultato <- tryCatch(eval(gradiente, c(valori, list(x = x))), error = function(e) NULL)
      J <- if (is.null(risultato)) NULL else attr(risultato, "gradient")
      if (!is.null(J)) {
        # Formula that does not depend on x: deriv() returns a single row.
        if (nrow(J) == 1 && length(x) > 1) J <- matrix(rep(J, each = length(x)), nrow = length(x))
        if (all(is.finite(J))) { colnames(J) <- nomi_liberi; return(J) }
      }
    }
    f <- function(p) eval(espressione, c(setNames(as.list(p), nomi_liberi), as.list(fissi), list(x = x)))
    jacobiano_numerico(f, setNames(as.numeric(parametri), nomi_liberi))
  }
}

# ---- Numerical Jacobian of the model (central differences) ----
# Used for the least-squares covariance, s^2 (J^T J)^-1, with the
# algorithms other than Levenberg-Marquardt. Previously the numerical Hessian
# returned by optim was used (finite differences of a gradient that is itself
# numerical, i.e. approximate second derivatives): with correlated
# parameters its errors are amplified in the inversion, and the
# standard errors turned out very wrong even on easy datasets (NIST
# Misra1a with BFGS: about 20 times too small). The Jacobian requires
# only first derivatives, is much more accurate and is the standard method
# (Bates & Watts, 1988; it is what nls() and minpack.lm do). Step
# relative to the magnitude of each parameter (cube root of the
# machine precision), so it also works with very small
# or very large parameters.
jacobiano_numerico <- function(f, parametri) {
  passo_base <- .Machine$double.eps^(1/3)
  J <- sapply(seq_along(parametri), function(j) {
    h <- passo_base * max(abs(parametri[j]), 1e-8)
    p_piu <- parametri; p_meno <- parametri
    p_piu[j] <- parametri[j] + h; p_meno[j] <- parametri[j] - h
    (f(p_piu) - f(p_meno)) / (2 * h)
  })
  if (is.null(dim(J))) J <- matrix(J, ncol = length(parametri))
  colnames(J) <- names(parametri)
  J
}

# ---- Fit of the secondary models (Shelf-Life, Non-Isothermal) ----
# Least squares with Levenberg-Marquardt (minpack.lm), with the same
# settings as the main fit (controllo_lm) and, if provided, the analytical
# Jacobian of the model (crea_jacobiano_modello); otherwise nls.lm uses its
# internal numerical derivatives. Used for the point estimate and for the bootstrap
# fits. Previously: a single Nelder-Mead without scaling, which with
# parameters on very different scales (Arrhenius: Ea ~ 1e4-1e5, q ~ 10-30)
# often stopped far from the optimum. On 6 synthetic cases (Arrhenius,
# Eyring, Q10; 3-4 temperatures; days and minutes), starting from the default
# initial values, 5 cases reach 8.6-10.1 correct digits with respect
# to a reference fit; the sixth requires initial values of the right order
# of magnitude. modello(p) returns the predictions, y the observed
# values. Returns par, value (SSE) and convergence (0 if nls.lm
# ended by convergence, info codes 1-4), like optim.
fit_modello_secondario <- function(modello, y, partenza, jacobiano = NULL) {
  previsioni <- function(p) { v <- tryCatch(modello(p), error = function(e) NULL); if (is.null(v) || any(!is.finite(v))) NULL else v }
  residui <- function(p) { v <- previsioni(p); if (is.null(v)) rep(1e5, length(y)) else y - v }
  argomenti <- list(par = partenza, fn = residui, control = controllo_lm(1024, length(partenza)))
  if (!is.null(jacobiano)) argomenti$jac <- function(p) { J <- -jacobiano(p); J[!is.finite(J)] <- 0; J }
  fit <- tryCatch(do.call(nls.lm, argomenti), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  v <- previsioni(fit$par)
  if (is.null(v)) return(NULL)
  list(par = fit$par, value = sum((y - v)^2), convergence = if (fit$info %in% 1:4) 0L else as.integer(fit$info))
}

# ---- Covariance of a least-squares model, from the Jacobian ----
# s^2 (J^T J)^-1 with s^2 = SSE / degrees of freedom. Returns NULL (never
# an invalid matrix) if it cannot be computed or with non-positive variances.
covarianza_minimi_quadrati <- function(f_modello, parametri, sse, df_residuo, jacobiano = NULL) {
  if (!is.finite(sse) || df_residuo <= 0) return(NULL)
  J <- tryCatch(if (!is.null(jacobiano)) jacobiano(parametri) else jacobiano_numerico(f_modello, parametri), error = function(e) NULL)
  cov <- if (is.null(J)) NULL else tryCatch((sse / df_residuo) * inverti_hessiana(crossprod(J)), error = function(e) NULL)
  if (is.null(cov) || !all(is.finite(diag(cov)) & diag(cov) > 0)) return(NULL)
  dimnames(cov) <- list(names(parametri), names(parametri))
  cov
}

inverti_hessiana <- function(H) {
  H <- as.matrix(H)
  d <- diag(H)
  if (all(is.finite(H)) && all(d > 0)) {
    s <- sqrt(d)
    return(solve(H / outer(s, s)) / outer(s, s))
  }
  solve(H)
}

inverti_colonne_xy <- function(df) {
  if (is.null(df) || !all(c("x", "y") %in% names(df))) return(df)
  x_originale <- df$x
  df$x <- df$y
  df$y <- x_originale
  df
}

# ---- y-axis limits for bar charts with error bars ----
# barplot() draws the bars starting from zero, so zero must always be
# included; the margin (10% of the extreme value) must be added on the side OPPOSITE
# to zero: above for positive values, below for negative ones. Previously
# min * 0.9 / max * 1.1 was used, which with negative values narrowed
# the axis instead of widening it (e.g. NIST Bennett5, b1 ~ -2500: the lower part
# of the error bars was cut off) or left the error bars
# exactly on the edge. With positive values the result is identical to before.
limiti_asse_barre <- function(valori) {
  estremi <- range(c(0, valori), na.rm = TRUE, finite = TRUE)
  inferiore <- if (estremi[1] < 0) estremi[1] * 1.1 else 0
  superiore <- if (estremi[2] > 0) estremi[2] * 1.1 else 0
  if (inferiore == superiore) c(-1, 1) else c(inferiore, superiore)
}
# Same idea for an axis that must not include zero: widens by 10%
# outwards whatever the sign (min * 0.9 with a negative minimum
# would bring it closer to zero, cutting the plot).
allarga_estremo <- function(v, verso) if (verso == "giu") (if (v >= 0) v * 0.9 else v * 1.1) else (if (v >= 0) v * 1.1 else v * 0.9)

# ---- Title of the x axis under category names rotated by 90 degrees ----
# In category plots (bars, boxplots, violins) the names on the x axis are
# rotated (las = 2): a title in the standard position (line 3) would end up
# over long names. The longest name is measured, the lower margin is
# widened as needed and the title goes right below the names. Usage: call
# prepara_margine_nomi_ruotati() BEFORE drawing, then
# titolo_asse_x_ruotato() AFTERWARDS, with the value returned by the first.
# With con_titolo = FALSE (title hidden by the user) the margin
# shrinks to the names only, without leaving empty space below.
prepara_margine_nomi_ruotati <- function(nomi, cex_nomi = 0.8, mar_base = c(6, 4, 3, 1), con_titolo = TRUE) {
  righe_nomi <- max(c(0, strwidth(as.character(nomi), units = "inches", cex = cex_nomi)), na.rm = TRUE) / (par("csi") * par("mex"))
  mar <- mar_base
  mar[1] <- if (con_titolo) max(mar_base[1], righe_nomi + 3.2) else righe_nomi + 1.8
  par(mar = mar)
  righe_nomi
}
titolo_asse_x_ruotato <- function(testo, righe_nomi, mostra = TRUE) if (mostra) title(xlab = testo, line = righe_nomi + 1.8)
# "Show x-axis title" checkbox of the aesthetics panels: active if not yet created.
titolo_x_attivo <- function(valore) is.null(valore) || isTRUE(valore)

# ---- "One plot per parameter" sections (single scheme for the whole app) ----
# Same scheme as the Grid Search sections: each parameter has its own
# plot and its own "Download Plot", and next to the title there are "Download
# All (ZIP)" and "Download Combined Image". Used by Parameter Distributions
# (Single Fit), parameter histograms and boxplots (Batch), parameter
# boxplots (Model Comparison).
MASSIMO_PANNELLI_PARAMETRI <- 15

estensione_esportazione <- function() if (identical(input$formato_esportazione, "pdf")) "pdf" else "jpg"

# Server side: registers the pre-generated outputs (plot + download for
# each possible panel), the ZIP and the combined image. 'n_pannelli'
# and 'disegna' are functions (read at drawing time, not now):
# n_pannelli() returns how many panels there are (0 = nothing to
# show), disegna(k) draws panel k on the current device.
registra_sezione_pannelli <- function(id, n_pannelli, disegna, prefisso_file) {
  for (idx in seq_len(MASSIMO_PANNELLI_PARAMETRI)) local({
    k <- idx
    output[[paste0("grafico_", id, "_", k)]] <- renderPlot({
      if (k > n_pannelli()) return(invisible(NULL))
      par(family = "sans")
      disegna(k)
    }, res = 100)
    output[[paste0("scarica_", id, "_", k)]] <- downloadHandler(
      filename = function() paste0(prefisso_file, "_", k, ".", estensione_esportazione()),
      content = function(file) {
        dim_exp <- dimensioni_esportazione()
        if (identical(input$formato_esportazione, "pdf")) pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        on.exit(dev.off(), add = TRUE)
        disegna(k)
      }
    )
  })
  output[[paste0("scarica_tutti_", id)]] <- downloadHandler(
    filename = function() paste0(prefisso_file, "_all.zip"),
    content = function(file) crea_zip_grafici(seq_len(max(1, n_pannelli())), prefisso_file, disegna, file),
    contentType = "application/zip"
  )
  output[[paste0("scarica_composito_", id)]] <- downloadHandler(
    filename = function() nome_file_composito(prefisso_file),
    content = function(file) crea_immagine_composita(file, max(1, n_pannelli()), disegna)
  )
  invisible(NULL)
}

# UI side: title with ZIP and combined image next to it, optional
# description, then the plots 3 per row with "Download Plot" under each one and
# the 🎨 button under the first (the settings apply to all of them).
ui_sezione_pannelli <- function(id, n, titolo, descrizione, id_estetica, altezza = "380px") {
  n <- min(n, MASSIMO_PANNELLI_PARAMETRI)
  blocchi <- lapply(seq_len(n), function(k) {
    column(4,
      plotOutput(paste0("grafico_", id, "_", k), height = altezza),
      div(style = "margin-top: 8px; margin-bottom: 16px;",
          downloadButton(paste0("scarica_", id, "_", k), "Download Plot", class = "btn-outline-secondary btn-sm"),
          if (k == 1 && !is.null(id_estetica)) actionButton(id_estetica, "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize these plots"))
    )
  })
  righe <- lapply(seq(1, n, by = 3), function(i) do.call(fluidRow, blocchi[i:min(i + 2, n)]))
  tagList(
    tags$h4(style = "display: inline-block; margin-right: 10px;", titolo),
    downloadButton(paste0("scarica_tutti_", id), "Download All (ZIP)", class = "btn-outline-primary btn-sm"),
    downloadButton(paste0("scarica_composito_", id), "Download Combined Image", class = "btn-outline-primary btn-sm"),
    if (!is.null(descrizione)) p(style = "font-size: 12px; color: #566573; margin-top: 6px;", descrizione),
    righe
  )
}

output$grafico_simulazione <- renderPlot({
  par(family = "sans")
  res <- tryCatch(simulazione_dati(), error = function(e) NULL)
  disegna_grafico_simulazione(res)
})

output$scarica_grafico_simulazione <- downloadHandler(
  filename = function() paste0("simulation_plot.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(simulazione_dati(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_simulazione(res)
    dev.off()
  }
)

output$log_errore_simulazione <- renderPrint({
  res <- tryCatch(simulazione_dati(), error = function(e) NULL)
  if (is.null(res)) return(cat("Simulation engine ready. Modify initial guesses and trigger calculation."))
  if (!res$successo) {
    cat("STATUS: ERROR DETECTED\n---------------------------------------\n", res$messaggio)
  } else {
    cat("STATUS: SUCCESS\n---------------------------------------\nFormula evaluated successfully across the grid range.\nUsed active string:", res$formula)
  }
})

output$grafico_fit <- renderPlot({
  par(family = "sans")
  df <- tryCatch(dati(), error = function(e) NULL)
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(1, type = "n", main = "Insert data to display the plot", axes = FALSE, xlab = "", ylab = "")
    return()
  }
  disegna_grafico_fit(df, res)
})

output$scarica_grafico_fit <- downloadHandler(
  filename = function() paste0("model_plot.", input$formato_esportazione),
  content = function(file) {
    df <- tryCatch(dati(), error = function(e) NULL)
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return()
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_fit(df, res)
    dev.off()
  }
)

output$grafico_residui <- renderPlot({
  par(family = "sans")
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_grafico_residui(res)
})

output$scarica_grafico_residui <- downloadHandler(
  filename = function() paste0("residuals_plot.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_residui(res)
    dev.off()
  }
)

output$grafico_qq <- renderPlot({
  par(family = "sans")
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_grafico_qq(res)
})

output$scarica_grafico_qq <- downloadHandler(
  filename = function() paste0("qq_plot.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_qq(res)
    dev.off()
  }
)

# ---- Parameter Distributions (Monte Carlo): one plot per parameter ----
registra_sezione_pannelli("dist_sf",
                          n_pannelli = function() n_pannelli_distribuzioni_sf(),
                          disegna = function(k) disegna_distribuzione_pannello(k),
                          prefisso_file = "parameter_distribution")

output$sezione_dist_parametri_ui <- renderUI({
  n <- n_pannelli_distribuzioni_sf()
  if (n == 0) return(NULL)
  ui_sezione_pannelli("dist_sf", n, "Parameter Distributions (Monte Carlo):", NULL, "btn_estetica_dist")
})

# ---- Enriched Model Formula (plotmath, centered) ----
output$grafico_formula_modello_contenitore <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  altezza <- 80
  if (!is.null(res) && isTRUE(res$successo)) {
    testo_sostituito <- costruisci_testo_formula_sostituita(res)
    testo_base <- if (!is.null(testo_sostituito)) testo_sostituito else res$formula
    divisione <- dividi_formula_lunga(testo_base)
    if (!is.null(divisione$riga2)) altezza <- 140
  }
  plotOutput("grafico_formula_modello", height = paste0(altezza, "px"))
})

output$grafico_formula_modello <- renderPlot({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_formula_modello(res)
})

# ---- Fit info (text: no special characters needed here) ----
output$info_risultati <- renderPrint({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(cat("Waiting for data input and optimization execution..."))
  
  if (!isTRUE(res$convergenza_vera)) {
    cat("\u26a0\ufe0f\u26a0\ufe0f WARNING: THIS FIT DID NOT FULLY CONVERGE \u26a0\ufe0f\u26a0\ufe0f\n")
    cat("Results are shown only because \"Accept non-converged results\" is enabled.\n")
    cat("These parameter estimates may NOT represent a true minimum of the loss function \u2014 interpret with caution.\n\n")
  }
  
  cat("Minimized Loss Function (final value):", round(res$loss_valore, 5), "[Type:", input$tipo_loss, "]\n")
  if (ponderazione_attiva(res)) cat("Weighting:", descrizione_ponderazione(res), "\n")
  if (!is.null(res$ripartenze_nm)) cat("Nelder-Mead restarts:", if (isTRUE(res$ripartenze_nm$attive)) sprintf("on (%s used in the main fit)", if (is.null(res$ripartenze_nm$usate)) "none" else res$ripartenze_nm$usate) else "off", "\n")
  metodo_testo <- if (input$metodo_errore == "monte_carlo") {
    if (!is.null(res$tipo_rumore_mc) && !is.na(res$tipo_rumore_mc) && res$tipo_rumore_mc == "gaussiano") {
      sprintf("Monte Carlo (Parametric Gaussian Noise, sigma = %.5f)", res$sigma_mc)
    } else {
      "Monte Carlo (Non-parametric Residual Bootstrap)"
    }
  } else {
    "Linear Approximation (Jacobian Z-scores)"
  }
  cat(sprintf("Error Estimation Method: %s\n", metodo_testo))
  if (input$metodo_errore == "monte_carlo" && !is.na(res$n_boot_totale)) {
    perc_successo <- 100 * res$n_boot_successo / res$n_boot_totale
    if (identical(res$tipo_rumore_mc, "bootstrap")) cat(if (isTRUE(res$residui_modificati)) "Resampled residuals: leverage-corrected and centred (Davison & Hinkley, 1997)\n" else "Resampled residuals: raw\n")
    cat(sprintf("Successful Bootstrap Iterations: %d / %d (%.1f%%)\n", res$n_boot_successo, res$n_boot_totale, perc_successo))
    if (!is.na(res$n_boot_successo) && res$n_boot_successo < MINIMO_ITERAZIONI_MC) {
      cat(sprintf("Monte Carlo results are NOT shown: at least %d successful iterations are needed. The standard errors shown are the classic ones.\n", MINIMO_ITERAZIONI_MC))
    } else if (perc_successo < 50) {
      cat("Warning: fewer than half of the bootstrap iterations converged \u2014 the reported uncertainty (SE, CI) may be unreliable. Consider increasing Max Iterations, adjusting initial guesses, or adding boundaries.\n")
    }
  }
  if (!is.null(res$esponente_tolleranza)) cat(sprintf("Convergence Tolerance: 1e%d\n", res$esponente_tolleranza))
  cat(sprintf("Confidence Level: %s%%\n", input$livello_confidenza))
  if (isTRUE(input$banda_simultanea) && !identical(input$metodo_errore, "monte_carlo")) cat(sprintf("Confidence band: simultaneous (Bates & Watts, 1988), multiplier %.3f instead of %.3f\n", moltiplicatore_banda_confidenza(res), valore_critico(res$df)))
})

# ---- Parameter Estimation Results (real HTML table, symbols rendered) ----
output$tabella_parametri_stimati <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(data.frame(Message = "Waiting for data input and optimization execution..."))
  
  tab <- data.frame(Parameter = names(res$parametri), Estimate = as.numeric(res$parametri), Std_Error = as.numeric(res$errori))
  crit_val <- valore_critico(res$df)  # Student's t (df=n-p), converges to Z for large samples
  tab$CI_Param_Lower <- tab$Estimate - (crit_val * tab$Std_Error)
  tab$CI_Param_Upper <- tab$Estimate + (crit_val * tab$Std_Error)
  colnames(tab)[(ncol(tab)-1):ncol(tab)] <- c(paste0("CI_Parametric_", input$livello_confidenza, "_Lower"), paste0("CI_Parametric_", input$livello_confidenza, "_Upper"))
  
  # If the method is Monte Carlo, it also adds the EMPIRICAL interval (direct
  # quantiles of the bootstrap distribution): unlike the parametric one
  # above (Estimate +/- t*SE, which assumes symmetry), this captures any
  # real asymmetries of the bootstrap distribution - see the Shapiro-Wilk
  # normality test further below to understand when the two diverge.
  if (input$metodo_errore == "monte_carlo" && !is.null(res$mat_boot_params)) {
    p_inf <- (1 - input$livello_confidenza / 100) / 2
    p_sup <- 1 - p_inf
    tab$CI_Emp_Lower <- apply(res$mat_boot_params, 2, quantile, probs = p_inf, na.rm = TRUE)
    tab$CI_Emp_Upper <- apply(res$mat_boot_params, 2, quantile, probs = p_sup, na.rm = TRUE)
    colnames(tab)[(ncol(tab)-1):ncol(tab)] <- c(paste0("CI_Empirical_", input$livello_confidenza, "_Lower"), paste0("CI_Empirical_", input$livello_confidenza, "_Upper"))
  }
  
  # renderTable()/xtable apply their own default rounding to the
  # DISPLAY, independent of how the value has already been rounded
  # with round() - for guaranteed control over the decimal digits shown,
  # each numeric column is converted here to text with sprintf().
  for (col in setdiff(colnames(tab), "Parameter")) {
    tab[[col]] <- formatta_cifre_significative(tab[[col]])
  }
  # The parameter name is translated into safe HTML (simbolo_a_html()
  # already escapes any text not recognized as special
  # syntax) BEFORE disabling the automatic sanitization of
  # renderTable below - so the only "real" HTML that reaches the browser
  # is the one we build ourselves in a controlled way, not free
  # text from the user.
  tab$Parameter <- sapply(tab$Parameter, function(n) simbolo_a_html(simbolo_visualizzazione(n)))
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

# ---- Fixed Parameters (real HTML table, shown only if any are set) ----
output$tabella_parametri_fissi <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo || is.null(res$fissi) || length(res$fissi) == 0) return(NULL)
  tab <- data.frame(
    Parameter = sapply(names(res$fissi), function(n) simbolo_a_html(simbolo_visualizzazione(n))),
    Fixed_Value = round(as.numeric(res$fissi), 5)
  )
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

# Numbers in the results tables: 6 significant digits (scientific
# notation only for very small or very large values). Previously
# 3 fixed decimals were used, which for small parameters showed only zeros (NIST
# Misra1a: b2 = 0.00055016 appeared as "0.001" and its standard error,
# 7.3e-06, as "0.000"). The CSV/Excel exports already contain the complete
# values and are not touched.
formatta_cifre_significative <- function(v) ifelse(is.finite(v), formatC(v, digits = 6, format = "g"), NA_character_)

# ---- Bootstrap Distribution Summary (Monte Carlo method) ----
# For each parameter: point estimate, mean, median and standard deviation
# (= bootstrap standard error) of the bootstrap distribution, bias (mean -
# estimate) and |bias|/SE, then the normality checks already present
# (Shapiro-Wilk, skewness, excess kurtosis). Rule of thumb for the
# bias (Efron & Tibshirani, 1993, ch. 10, Estimates of bias): if |bias| < 0.25 SE it is negligible.
riepilogo_distribuzione_bootstrap <- function(res) {
  mat_boot <- res$mat_boot_params
  nomi <- names(res$parametri)
  righe <- lapply(seq_len(ncol(mat_boot)), function(i) {
    valori <- mat_boot[, i]
    valori <- valori[is.finite(valori)]
    stima <- as.numeric(res$parametri[i])
    media <- if (length(valori)) mean(valori) else NA_real_
    se_boot <- if (length(valori) > 1) sd(valori) else NA_real_
    bias <- media - stima
    rapporto <- if (is.finite(se_boot) && se_boot > 0) abs(bias) / se_boot else NA_real_
    test <- if (length(valori) >= 3 && length(valori) <= 5000 && length(unique(valori)) >= 3) tryCatch(shapiro.test(valori), error = function(e) NULL) else NULL
    list(nome = nomi[i], stima = stima, media = media, mediana = if (length(valori)) median(valori) else NA_real_,
         se_boot = se_boot, bias = bias, rapporto = rapporto, test = test,
         skew = calcola_skewness(valori), kurt = calcola_kurtosi_eccesso(valori), n = length(valori))
  })
  righe
}

output$tabella_normalita <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo || is.null(res$mat_boot_params)) {
    return(data.frame(Message = "Available only after a successful Monte Carlo optimization."))
  }
  f <- formatta_cifre_significative
  righe <- lapply(riepilogo_distribuzione_bootstrap(res), function(r) {
    normalita <- if (r$n < 3 || r$n > 5000) "N/A (not enough valid bootstrap draws)" else if (is.null(r$test)) "N/A (test failed)" else if (r$test$p.value < 0.05) "Deviates from normal (p < 0.05)" else "Consistent with normal"
    data.frame(Parameter = simbolo_a_html(simbolo_visualizzazione(r$nome)),
               Estimate = f(r$stima), Mean = f(r$media), Median = f(r$mediana), Bootstrap_SE = f(r$se_boot), Bias = f(r$bias),
               `|Bias| / SE` = if (is.na(r$rapporto)) NA_character_ else paste0(sprintf("%.2f", r$rapporto), if (r$rapporto >= 0.25) " \u26a0\ufe0f" else ""),
               W_Statistic = if (is.null(r$test)) NA_character_ else sprintf("%.4f", unname(r$test$statistic)),
               P_Value = if (is.null(r$test)) NA_character_ else format.pval(r$test$p.value, digits = 3, eps = 0.001),
               Skewness = if (is.na(r$skew)) NA_character_ else sprintf("%.3f", r$skew),
               Excess_Kurtosis = if (is.na(r$kurt)) NA_character_ else sprintf("%.3f", r$kurt),
               Normality = normalita, check.names = FALSE, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, righe)
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

# ---- Parameter Sensitivity Analysis ----
output$grafico_sensibilita <- renderPlot({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_grafico_sensibilita(res)
})

output$scarica_grafico_sensibilita <- downloadHandler(
  filename = function() paste0("parameter_sensitivity.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_sensibilita(res)
    dev.off()
  }
)

# ---- Parameter Correlation Matrix (table only, no heatmap) ----
output$tabella_correlazione <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(data.frame(Message = "Run a successful optimization first."))
  mat_corr <- calcola_matrice_correlazione(res)
  if (is.null(mat_corr)) return(data.frame(Message = "Correlation matrix not available for this fit."))
  df_corr <- as.data.frame(apply(mat_corr, 2, function(col) sprintf("%.3f", col)), stringsAsFactors = FALSE)
  # Both the column headers and the "Parameter" column use safe
  # HTML: since the automatic sanitization was disabled for the whole
  # table (below), every piece of text that ends up in the HTML must pass
  # through simbolo_a_html() - never raw unsanitized text, not even in the
  # headers.
  colnames(df_corr) <- sapply(colnames(df_corr), function(n) simbolo_a_html(simbolo_visualizzazione(n)))
  cbind(Parameter = sapply(rownames(mat_corr), function(n) simbolo_a_html(simbolo_visualizzazione(n))), df_corr)
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", rownames = FALSE, sanitize.text.function = function(x) x)

# ---- Pairwise Bootstrap Scatter Plot Matrix ----
output$grafico_pairs_bootstrap <- renderPlot({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_pairs_bootstrap(res)
})

output$scarica_grafico_pairs <- downloadHandler(
  filename = function() paste0("parameter_pairs_bootstrap.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$width_in)  # square, pairs plots render better with 1:1 proportions
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$width_px, res = dim_exp$dpi)
    }
    disegna_pairs_bootstrap(res)
    dev.off()
  }
)

output$tabella_diagnostica <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(data.frame(Message = "No diagnostic calculations available."))
  # Forces each value to be a true scalar: protection against
  # a possible NULL (which c() would silently discard, shortening the
  # vector and causing a row mismatch with Metric) or a value with a
  # leftover "names" attribute (e.g. fit$counts["function"] for the algorithms
  # via optim(), which carries with it the name "function").
  valore_sicuro <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    as.numeric(unname(x[1]))
  }
  # renderTable()/xtable apply their own default rounding to the
  # DISPLAY, independent of how the value has already been rounded
  # above - for guaranteed control over the decimal digits shown, each
  # value is pre-formatted here as TEXT with sprintf() instead of
  # letting xtable decide its format.
  formatta <- function(x, decimali = 3) {
    v <- valore_sicuro(x)
    if (is.na(v)) return("N/A")
    sprintf(paste0("%.", decimali, "f"), v)
  }
  formatta_intero <- function(x) {
    v <- valore_sicuro(x)
    if (is.na(v)) return("N/A")
    as.character(round(v))
  }
  tab <- data.frame(
    Metric = c("Observations (n)", "DF (Degrees of Freedom)", "Correlation Index (r)", "Pseudo R-squared (R\u00b2)",
               "Adjusted R-squared (R\u00b2 adj)", "Residual Std Error (RMSE)", "CV(RMSE)%", "SSE (Sum of Squared Errors)",
               "MSE (Mean Squared Error)", "AIC (Akaike Info Criterion)", "AICc (AIC Small Sample Corr.)", 
               "BIC (Bayesian Info Criterion)", "Iterations"),
    Value = c(formatta_intero(res$obs), formatta_intero(res$df), formatta(res$r_corr), formatta(res$r2), 
              formatta(res$r2_adj), formatta(res$rmse), formatta(res$cv_rmse), formatta(res$sse), formatta(res$mse), 
              formatta(res$aic), formatta(res$aicc), formatta(res$bic), formatta_intero(res$iter)),
    stringsAsFactors = FALSE
  )
  # With weighted regression: SSE, sigma and R^2 are the weighted ones.
  if (ponderazione_attiva(res)) {
    rinomina <- c("Pseudo R-squared (R\u00b2)" = "Weighted R-squared (R\u00b2)", "Adjusted R-squared (R\u00b2 adj)" = "Weighted adjusted R-squared",
                  "Residual Std Error (RMSE)" = "\u03c3 of the variance model (weighted residual SE)", "SSE (Sum of Squared Errors)" = "Weighted SSE (\u03a3 w\u00b7residual\u00b2)",
                  "MSE (Mean Squared Error)" = "Weighted MSE")
    for (k in names(rinomina)) tab$Metric[tab$Metric == k] <- rinomina[[k]]
    tab <- rbind(tab, data.frame(Metric = "Weighting", Value = descrizione_ponderazione(res), stringsAsFactors = FALSE))
  }
  # With losses other than MSE the three rows of the criteria are replaced by a note.
  if (isTRUE(res$criteri_non_disponibili)) {
    i <- which(tab$Metric == "AIC (Akaike Info Criterion)")
    tab <- rbind(tab[seq_len(i - 1), ], data.frame(Metric = "AIC, AICc, BIC", Value = NOTA_CRITERI_LOSS, stringsAsFactors = FALSE), tab[(i + 3):nrow(tab), ])
  }
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", colnames = TRUE)

output$tabella_pred_residui <- renderTable({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(data.frame(Status = "Please run optimization to generate prediction tables."))
  df <- res$df_originale
  tab <- data.frame(
    Obs = 1:res$obs, X_Value = res$x, Y_Observed = df$y, Y_Predicted = res$y_pred,
    Residual = res$residui, Absolute_Error = abs(res$residui),
    Relative_Error_Perc = ifelse(df$y != 0, (abs(res$residui) / abs(df$y)) * 100, NA)
  )
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, digits = 4)

output$messaggi_errore <- renderPrint({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (!is.null(res) && !res$successo) cat("Log:", res$messaggio)
})