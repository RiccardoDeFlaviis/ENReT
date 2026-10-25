# =========================================================================
# LOSS SURFACE EXPLORER (GRID SEARCH ON 2 PARAMETERS)
# =========================================================================
# Two independent computations, both keyed off the same chosen pair of
# parameters and the same grid:
#  - FAST version: holds every OTHER free parameter fixed at its optimal
#    value, and evaluates the loss function once per grid point (no new
#    optimization). Good for a quick look: local-minimum sanity check,
#    improving initial guesses.
#  - RIGOROUS version (separate button, explicit opt-in due to cost):
#    at each grid point, the two chosen parameters are held fixed at that
#    point's values and every OTHER free parameter is re-optimized to
#    minimize the loss - the classic "profile likelihood" surface. This is
#    what a statistically valid joint confidence region (F-test based) is
#    computed from; the fast version's surface is only a rough guide for
#    that purpose, since the other parameters aren't re-optimized there.

# ---- Re-optimization of the free parameters at each point of a profile ----
# Used by Model Comparison via F-test (1D profile, from which the
# confidence intervals are derived) and by the Rigorous Profile of Grid Search (2D).
# Previously there was a single BFGS with 100 iterations, without scaling: with
# ill-conditioned models (parameters on very different scales and strongly
# correlated, e.g. NIST Bennett5: b1 ~ -2500, b3 ~ 0.9) it stopped halfway
# along the narrow, curved valley of the loss, producing
# jagged profiles with false local minima (19 on Bennett5 against 1 true one) and
# values inflated up to 85 times - and therefore wrong confidence
# intervals. Now: BFGS with scaled parameters (parscale) and more iterations,
# then refinement with Nelder-Mead (which does not use the numerical gradient, the
# real weak point in those valleys) and a final BFGS; the best value
# is kept. Verified on Bennett5 against a reference profile
# computed independently (exact agreement at all 100 points)
# and on Rat43 (never worse than the previous method, actually slightly
# better). Returns NULL if no attempt produces a finite value.
# ---- Re-optimization in the profiles for least squares (MSE loss) ----
# Levenberg-Marquardt (minpack.lm, the same settings as the main
# fit, controllo_lm) with analytical Jacobian (crea_jacobiano_modello),
# on the remaining free parameters, with those fixed by the profile in
# valori_fissi. Replaces, for the MSE loss, the three-step procedure
# below (which remains as a fallback for the other losses): profiles are
# least-squares problems, and Levenberg-Marquardt is the standard tool, already
# validated on the NIST datasets. Without free parameters (model with a single
# parameter in the 1D profile) it directly returns the SSE. The returned
# value is the SSE, on the same scale as calcola_loss_generica("mse").
# pesi: weights of the weighted regression (res$pesi), NULL without weights; with weights
# the residuals are sqrt(w)(y - f) and the returned value is the weighted sum.
riottimizza_minimi_quadrati <- function(formula, valori_fissi, nomi_liberi, partenza, jacobiano_modello, x, y, pesi = NULL) {
  radice_pesi <- if (is.null(pesi)) NULL else sqrt(pesi)
  sse_in <- function(p) {
    valori <- c(valori_fissi, setNames(as.list(p), nomi_liberi))
    y_pred <- tryCatch(valuta_formula_generica(formula, valori, x), error = function(e) rep(NA_real_, length(x)))
    if (any(!is.finite(y_pred))) return(NULL)
    if (is.null(radice_pesi)) y - y_pred else radice_pesi * (y - y_pred)
  }
  if (length(nomi_liberi) == 0) {
    r <- sse_in(numeric(0))
    return(if (is.null(r)) NULL else list(par = numeric(0), value = sum(r^2)))
  }
  residui <- function(p) { r <- sse_in(p); if (is.null(r)) rep(1e5, length(y)) else r }
  jac <- function(p) { J <- -jacobiano_modello(p, x, valori_fissi); J[!is.finite(J)] <- 0; if (!is.null(radice_pesi)) J <- J * radice_pesi; J }
  # nls.lm warnings silenced: at the points where the model is not defined
  # (e.g. exponents with the wrong sign) the iteration limit is
  # reached and the warning would end up only in the console; those points still
  # correctly stay above the threshold.
  fit <- tryCatch(suppressWarnings(nls.lm(par = setNames(as.numeric(partenza), nomi_liberi), fn = residui, jac = jac,
                         control = controllo_lm(1024, length(partenza)))), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  r <- sse_in(fit$par)
  if (is.null(r)) return(NULL)
  list(par = fit$par, value = sum(r^2))
}

riottimizza_parametri_liberi <- function(obiettivo, partenza) {
  obiettivo_sicuro <- function(p) {
    v <- tryCatch(obiettivo(p), error = function(e) NA_real_)
    if (length(v) == 1 && is.finite(v)) v else 1e100
  }
  scala <- ifelse(is.finite(partenza) & abs(partenza) > 1e-8, abs(partenza), 1)
  bfgs <- function(inizio) tryCatch(optim(par = inizio, fn = obiettivo_sicuro, method = "BFGS",
                                          control = list(maxit = 1000, reltol = 1e-12, parscale = scala)),
                                    error = function(e) NULL)
  migliore <- bfgs(partenza)
  if (is.null(migliore)) return(NULL)
  nm <- tryCatch(suppressWarnings(optim(par = migliore$par, fn = obiettivo_sicuro, method = "Nelder-Mead",
                                        control = list(maxit = 2000, reltol = 1e-12))),
                 error = function(e) NULL)
  if (!is.null(nm) && nm$value < migliore$value) migliore <- nm
  finale <- bfgs(migliore$par)
  if (!is.null(finale) && finale$value < migliore$value) migliore <- finale
  if (!is.finite(migliore$value) || migliore$value >= 1e99) return(NULL)
  migliore
}

# Confidence level for the F-test, limited to a reasonable range - unlike
# the original slider (which physically constrained the value),
# the numeric field does not prevent it by itself if the user types a value
# out of range.
livello_grid_valido <- reactive({
  v <- input$grid_livello_confidenza
  if (is.null(v) || is.na(v)) v <- input$livello_confidenza
  min(max(v, 50), 99.9)
})

# Confidence level dedicated to the 1D analysis (Marginal CI / Run Profile
# CI), unlinked from that of the grid/joint region - same logic
# already applied to resolution and spread, since this section is by now
# completely independent and positioned BEFORE the Grid Search in the
# page (the shared control would otherwise live physically lower
# than where it is needed).
livello_profile_ci_valido <- reactive({
  v <- input$profile_ci_livello_confidenza
  if (is.null(v) || is.na(v)) v <- input$livello_confidenza
  min(max(v, 50), 99.9)
})

# Evaluates the model from a fully-specified named list of parameter values
# (fixed + free, all in one place) - does not depend on elaborazione()'s own
# internal closures (valuta_modello there always merges with a FROZEN copy
# of the original fissi, which isn't flexible enough for the grid search,
# where different parameters are held fixed at different times).
valuta_formula_generica <- function(formula_stringa, valori_completi, x) {
  env <- valori_completi
  env$x <- x
  eval(parse(text = formula_stringa), envir = env)
}

# Mirrors calcola_loss_personalizzata() in motore_calcolo.R exactly (same
# loss formulas), but as a standalone function usable outside elaborazione().
calcola_loss_generica <- function(y_pred, y, tipo_loss, pesi = NULL) {
  if (any(!is.finite(y_pred))) return(1e10)
  errori <- y - y_pred
  switch(tipo_loss,
         "mse" = if (is.null(pesi)) sum(errori^2, na.rm = TRUE) else sum(pesi * errori^2, na.rm = TRUE),
         "mae" = sum(abs(errori), na.rm = TRUE),
         "huber" = {
           delta <- 1.345 * mad(errori, na.rm = TRUE)
           if (is.na(delta) || delta == 0) delta <- 1.0
           condizione <- abs(errori) <= delta
           sum(ifelse(condizione, 0.5 * (errori^2), delta * (abs(errori) - 0.5 * delta)), na.rm = TRUE)
         },
         "log10cosh" = {
           log_cosh <- ifelse(abs(errori) < 20, log(cosh(errori)), abs(errori) - log(2))
           sum(log_cosh / log(10), na.rm = TRUE)
         },
         "rmsle" = {
           y_safe <- ifelse(y < 0, 0, y)
           y_pred_safe <- ifelse(y_pred < 0, 0, y_pred)
           sum((log1p(y_safe) - log1p(y_pred_safe))^2, na.rm = TRUE)
         }
  )
}

# SSE threshold for the joint confidence region on 2 parameters (profile
# likelihood method, Bates & Watts 1988): SSE(theta) <=
# SSE_opt * [1 + (2/(n-p)) * F(2, n-p, level)]. Valid only for the MSE loss
# (assumes Gaussian errors, like all the classic SSE/F-test theory).
calcola_soglia_confidenza_grid <- function(sse_ott, n, p_tot, livello_confidenza) {
  df_den <- n - p_tot
  if (df_den <= 0) return(NA_real_)
  f_crit <- qf(livello_confidenza / 100, df1 = 2, df2 = df_den)
  sse_ott * (1 + (2 / df_den) * f_crit)
}

output$selettore_grid_par1_ui <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a successful optimization first."))
  nomi <- names(res$parametri)
  if (length(nomi) < 2) return(p(style = "font-size: 12px; color: #7f8c8d;", "Fewer than 2 free parameters."))
  scelte <- setNames(nomi, sapply(nomi, function(n) simbolo_a_testo_semplice(simbolo_visualizzazione(n))))
  selectInput("grid_par1", "Parameter 1 (X-axis):", choices = scelte, selected = nomi[1])
})

output$selettore_grid_par2_ui <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) return(NULL)
  nomi <- names(res$parametri)
  if (length(nomi) < 2) return(NULL)
  scelte <- setNames(nomi, sapply(nomi, function(n) simbolo_a_testo_semplice(simbolo_visualizzazione(n))))
  selectInput("grid_par2", "Parameter 2 (Y-axis):", choices = scelte, selected = nomi[2])
})

# Search range for the two chosen parameters, shown as a
# DOUBLE-handle slider with the real ABSOLUTE VALUES (not a percentage around
# the optimal value as before) - same request and same logic for
# handling sign/zero already verified for the Initial Guess Simulator. The
# range of the slider itself is wider (\u00b1150%) than the one initially
# selected (\u00b150%, the previous default selection), so the user has
# room to widen it by dragging the handles, if desired, without having to
# retype anything.
costruisci_range_slider_grid <- function(id_input, etichetta, valore_ottimo) {
  if (valore_ottimo == 0) {
    sel_inf <- -1; sel_sup <- 1
  } else if (valore_ottimo > 0) {
    sel_inf <- valore_ottimo * 0.5; sel_sup <- valore_ottimo * 1.5
  } else {
    sel_inf <- valore_ottimo * 1.5; sel_sup <- valore_ottimo * 0.5
  }
  fluidRow(
    column(12, tags$strong(style = "font-size: 13px;", etichetta)),
    column(6, numericInput(paste0(id_input, "_min"), "Min:", value = signif(sel_inf, 3))),
    column(6, numericInput(paste0(id_input, "_max"), "Max:", value = signif(sel_sup, 3)))
  )
}

output$grid_range_par1_ui <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  p1 <- input$grid_par1; p2 <- input$grid_par2
  if (is.null(res) || !isTRUE(res$successo) || is.null(p1) || is.null(p2) || identical(p1, p2)) return(NULL)
  if (!(p1 %in% names(res$parametri))) return(NULL)
  etichetta1 <- paste0("Parameter 1 Range (", simbolo_a_testo_semplice(simbolo_visualizzazione(p1)), "):")
  costruisci_range_slider_grid("grid_range_par1", etichetta1, as.numeric(res$parametri[p1]))
})

output$grid_range_par2_ui <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  p1 <- input$grid_par1; p2 <- input$grid_par2
  if (is.null(res) || !isTRUE(res$successo) || is.null(p1) || is.null(p2) || identical(p1, p2)) return(NULL)
  if (!(p2 %in% names(res$parametri))) return(NULL)
  etichetta2 <- paste0("Parameter 2 Range (", simbolo_a_testo_semplice(simbolo_visualizzazione(p2)), "):")
  costruisci_range_slider_grid("grid_range_par2", etichetta2, as.numeric(res$parametri[p2]))
})

# Builds the grid points from the CURRENT inputs (read fresh at
# every call, never from a previously saved state) - used by
# BOTH buttons, so each always reflects the latest selection of
# parameters/range/resolution shown in the UI, regardless of which
# button was pressed last or in which order.
costruisci_griglia_base <- function(res) {
  p1 <- input$grid_par1; p2 <- input$grid_par2
  if (is.null(p1) || is.null(p2) || p1 == p2) return(NULL)
  
  val1_ott <- as.numeric(res$parametri[p1]); val2_ott <- as.numeric(res$parametri[p2])
  n_ris <- if (is.null(input$grid_risoluzione) || is.na(input$grid_risoluzione)) 25 else min(max(round(input$grid_risoluzione), 10), 50)
  
  # Range read directly from the two double-handle sliders (real absolute
  # values, no longer a percentage computed around the optimal
  # value) - if for some reason they are not yet populated (e.g. very first
  # render before grid_range_par1_ui/grid_range_par2_ui have appeared), it falls back to a
  # default ±50%, the same logic used to pre-fill the Min/Max cells.
  range1 <- if (!is.null(input$grid_range_par1_min) && !is.null(input$grid_range_par1_max)) {
    sort(c(input$grid_range_par1_min, input$grid_range_par1_max))
  } else sort(if (val1_ott != 0) val1_ott * c(0.5, 1.5) else c(-1, 1))
  range2 <- if (!is.null(input$grid_range_par2_min) && !is.null(input$grid_range_par2_max)) {
    sort(c(input$grid_range_par2_min, input$grid_range_par2_max))
  } else sort(if (val2_ott != 0) val2_ott * c(0.5, 1.5) else c(-1, 1))
  
  # Protection: for a parameter with a strictly positive optimal value
  # (e.g. a rate constant, an exponent), a too wide range
  # would make the lower limit fall to zero or below zero - many
  # formulas (log, sqrt, fractional powers) are not defined there, and
  # that portion of the grid would give silent NaN.
  # The range never crosses zero: it stops at 1% of the optimal value.
  if (val1_ott > 0) range1[1] <- max(range1[1], val1_ott * 0.01)
  if (val1_ott < 0) range1[2] <- min(range1[2], val1_ott * 0.01)
  if (val2_ott > 0) range2[1] <- max(range2[1], val2_ott * 0.01)
  if (val2_ott < 0) range2[2] <- min(range2[2], val2_ott * 0.01)
  
  list(p1 = p1, p2 = p2, 
       seq1 = seq(range1[1], range1[2], length.out = n_ris), 
       seq2 = seq(range2[1], range2[2], length.out = n_ris),
       val1_ott = val1_ott, val2_ott = val2_ott)
}

grid_stato <- reactiveVal(NULL)

observeEvent(input$esegui_grid_search, {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) {
    showNotification("Run a successful optimization first.", type = "error")
    return()
  }
  griglia <- costruisci_griglia_base(res)
  if (is.null(griglia)) {
    showNotification("Choose two different parameters.", type = "error")
    return()
  }
  p1 <- griglia$p1; p2 <- griglia$p2
  
  altri_nomi <- setdiff(names(res$parametri), c(p1, p2))
  altri_valori <- as.list(res$parametri[altri_nomi])
  base_valori <- c(as.list(res$fissi), altri_valori)
  x_dati <- res$df_originale$x; y_dati <- res$df_originale$y
  
  mat_loss <- matrix(NA_real_, length(griglia$seq1), length(griglia$seq2))
  withProgress(message = "Computing loss surface (fast)...", value = 0, {
    for (i in seq_along(griglia$seq1)) {
      for (j in seq_along(griglia$seq2)) {
        valori_completi <- base_valori
        valori_completi[[p1]] <- griglia$seq1[i]
        valori_completi[[p2]] <- griglia$seq2[j]
        y_pred <- tryCatch(valuta_formula_generica(res$formula, valori_completi, x_dati), error = function(e) rep(NA_real_, length(x_dati)))
        mat_loss[i, j] <- calcola_loss_generica(y_pred, y_dati, input$tipo_loss, res$pesi)
      }
      incProgress(1 / length(griglia$seq1))
    }
  })
  
  # EXPLICITLY clears any previous rigorous result: this is
  # a new "fast only" state, not an update of the old one.
  grid_stato(list(successo = TRUE, seq1 = griglia$seq1, seq2 = griglia$seq2, mat_loss = mat_loss, mat_loss_rigorosa = NULL,
                   par1 = p1, par2 = p2, val1_ott = griglia$val1_ott, val2_ott = griglia$val2_ott,
                   loss_ott = res$loss_valore, n = res$obs, p_tot = length(res$parametri)))
})

observeEvent(input$esegui_grid_rigoroso, {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !res$successo) {
    showNotification("Run a successful optimization first.", type = "error")
    return()
  }
  # Builds the grid from the CURRENT inputs, exactly like the Fast
  # button - it NO longer depends on a previous click on Fast: the two buttons
  # are completely independent, so they can never become misaligned
  # (e.g. changing the range after pressing Fast and then pressing Rigorous
  # now correctly uses the new range, not the old one).
  griglia <- costruisci_griglia_base(res)
  if (is.null(griglia)) {
    showNotification("Choose two different parameters.", type = "error")
    return()
  }
  p1 <- griglia$p1; p2 <- griglia$p2
  
  altri_nomi <- setdiff(names(res$parametri), c(p1, p2))
  altri_valori <- as.list(res$parametri[altri_nomi])
  base_valori <- c(as.list(res$fissi), altri_valori)
  x_dati <- res$df_originale$x; y_dati <- res$df_originale$y
  n_ris <- length(griglia$seq1)
  
  # Also computes the fast surface on the same grid (needed anyway
  # as a base/fallback, and guarantees that fast and rigorous are ALWAYS
  # on exactly the same grid, by construction).
  mat_loss <- matrix(NA_real_, n_ris, n_ris)
  for (i in seq_along(griglia$seq1)) {
    for (j in seq_along(griglia$seq2)) {
      valori_completi <- base_valori
      valori_completi[[p1]] <- griglia$seq1[i]
      valori_completi[[p2]] <- griglia$seq2[j]
      y_pred <- tryCatch(valuta_formula_generica(res$formula, valori_completi, x_dati), error = function(e) rep(NA_real_, length(x_dati)))
      mat_loss[i, j] <- calcola_loss_generica(y_pred, y_dati, input$tipo_loss, res$pesi)
    }
  }
  
  mat_loss_rig <- matrix(NA_real_, n_ris, n_ris)
  if (length(altri_nomi) == 0) {
    # No remaining parameter to re-optimize: with only 2 free
    # parameters in total, the rigorous version coincides exactly with the
    # fast one - no additional computation to do.
    mat_loss_rig <- mat_loss
  } else {
    valori_iniziali_altri <- as.numeric(res$parametri[altri_nomi])
    fissi_base <- as.list(res$fissi)
    jacobiano_rigoroso <- crea_jacobiano_modello(res$formula, altri_nomi)
    withProgress(message = "Computing rigorous profile (re-optimizing other parameters)...", value = 0, {
      for (i in seq_along(griglia$seq1)) {
        for (j in seq_along(griglia$seq2)) {
          obiettivo <- function(altri) {
            valori_completi <- fissi_base
            for (k in seq_along(altri_nomi)) valori_completi[[altri_nomi[k]]] <- altri[k]
            valori_completi[[p1]] <- griglia$seq1[i]
            valori_completi[[p2]] <- griglia$seq2[j]
            y_pred <- tryCatch(valuta_formula_generica(res$formula, valori_completi, x_dati), error = function(e) rep(NA_real_, length(x_dati)))
            calcola_loss_generica(y_pred, y_dati, input$tipo_loss, res$pesi)
          }
          fit_sub <- if (identical(input$tipo_loss, "mse")) {
            valori_fissi_sub <- fissi_base; valori_fissi_sub[[p1]] <- griglia$seq1[i]; valori_fissi_sub[[p2]] <- griglia$seq2[j]
            riottimizza_minimi_quadrati(res$formula, valori_fissi_sub, altri_nomi, valori_iniziali_altri, jacobiano_rigoroso, x_dati, y_dati, res$pesi)
          } else riottimizza_parametri_liberi(obiettivo, valori_iniziali_altri)
          mat_loss_rig[i, j] <- if (is.null(fit_sub)) NA_real_ else fit_sub$value
        }
        incProgress(1 / length(griglia$seq1))
      }
    })
  }
  
  grid_stato(list(successo = TRUE, seq1 = griglia$seq1, seq2 = griglia$seq2, mat_loss = mat_loss, mat_loss_rigorosa = mat_loss_rig,
                   par1 = p1, par2 = p2, val1_ott = griglia$val1_ott, val2_ott = griglia$val2_ott,
                   loss_ott = res$loss_valore, n = res$obs, p_tot = length(res$parametri)))
})

griglia_attiva <- reactive({ grid_stato() })

disegna_grid_3d <- function(dati_grid) {
  par(mar = c(1, 1, if (isTRUE(input$mostra_titolo)) 3 else 1, 1))
  if (is.null(dati_grid) || !isTRUE(dati_grid$successo)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the grid search first.")
    return(invisible(NULL))
  }
  mat <- if (!is.null(dati_grid$mat_loss_rigorosa)) dati_grid$mat_loss_rigorosa else dati_grid$mat_loss
  simb1 <- simbolo_a_testo_semplice(simbolo_visualizzazione(dati_grid$par1))
  simb2 <- simbolo_a_testo_semplice(simbolo_visualizzazione(dati_grid$par2))
  titolo <- if (isTRUE(input$mostra_titolo)) paste("Loss Surface:", simb1, "vs", simb2) else ""
  ombreggiatura <- if (is.null(input$ombra_grid)) 0.4 else input$ombra_grid
  theta_rot <- if (is.null(input$grid_3d_theta)) 30 else input$grid_3d_theta
  phi_rot <- if (is.null(input$grid_3d_phi)) 25 else input$grid_3d_phi
  
  # Gradient colouring based on height (standard technique to make
  # 3D surfaces readable): each FACET (not each vertex - persp()
  # draws (nrow-1)*(ncol-1) facets) takes a colour based on the mean
  # of the heights of its 4 corners, not a single flat uniform colour -
  # much more readable, and it allows the different chosen palettes
  # to be told apart immediately, which a flat pastel colour + shading did not
  # make clearly visible.
  palette_nome <- if (is.null(input$palette_superficie_grid)) "blu_rosso" else input$palette_superficie_grid
  colori_gradiente <- switch(palette_nome,
    "blu_rosso" = colorRampPalette(c("#2166ac", "#67a9cf", "#fddbc7", "#ef8a62", "#b2182b"))(100),
    "verde_giallo" = colorRampPalette(c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"))(100),
    "viola_arancio" = colorRampPalette(c("#542788", "#998ec3", "#f7f7f7", "#f1a340", "#b35806"))(100),
    "oceano" = colorRampPalette(c("#08306b", "#4292c6", "#9ecae1", "#deebf7", "#ffffff"))(100),
    colorRampPalette(c("#2166ac", "#67a9cf", "#fddbc7", "#ef8a62", "#b2182b"))(100)
  )
  z_facce <- (mat[-1, -1, drop = FALSE] + mat[-1, -ncol(mat), drop = FALSE] + 
              mat[-nrow(mat), -1, drop = FALSE] + mat[-nrow(mat), -ncol(mat), drop = FALSE]) / 4
  z_range <- range(z_facce, na.rm = TRUE)
  indice_colore <- if (diff(z_range) > 0) {
    cut(z_facce, breaks = seq(z_range[1], z_range[2], length.out = 101), include.lowest = TRUE, labels = FALSE)
  } else {
    rep(50, length(z_facce))
  }
  colori_facce <- colori_gradiente[indice_colore]
  
  persp_ritorno <- persp(dati_grid$seq1, dati_grid$seq2, mat, theta = theta_rot, phi = phi_rot, expand = 0.65,
        col = colori_facce, shade = ombreggiatura, ticktype = "detailed", border = NA,
        xlab = simb1, ylab = simb2, zlab = "Loss", main = titolo, cex.axis = 0.7)
  
  # Points actually sampled on the grid (where the loss was
  # really computed), overlaid on the interpolated surface - trans3d()
  # converts the 3D coordinates into the 2D coordinates of the plot already drawn,
  # using the same transformation matrix returned by persp().
  mostra_punti_grid <- if (is.null(input$mostra_punti_grid_3d)) TRUE else input$mostra_punti_grid_3d
  if (mostra_punti_grid) {
    col_punti <- if (is.null(input$col_punti_grid_3d)) "black" else input$col_punti_grid_3d
    cex_punti <- if (is.null(input$cex_punti_grid_3d)) 0.35 else input$cex_punti_grid_3d
    alpha_punti <- if (is.null(input$alpha_punti_grid_3d)) 0.5 else input$alpha_punti_grid_3d
    pch_punti_grid <- as.numeric(if (is.null(input$pch_punti_grid_3d)) "16" else input$pch_punti_grid_3d)
    griglia_punti <- expand.grid(x = dati_grid$seq1, y = dati_grid$seq2)
    z_punti <- as.vector(mat)
    punti_2d <- trans3d(griglia_punti$x, griglia_punti$y, z_punti, persp_ritorno)
    points(punti_2d, pch = pch_punti_grid, cex = cex_punti, col = adjustcolor(col_punti, alpha.f = alpha_punti))
  }
}

disegna_grid_contour <- function(dati_grid, res) {
  par(mar = c(5, 5, if (isTRUE(input$mostra_titolo)) 4 else 2, 2) + 0.1)
  if (is.null(dati_grid) || !isTRUE(dati_grid$successo)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the grid search first.")
    return(invisible(NULL))
  }
  mat <- if (!is.null(dati_grid$mat_loss_rigorosa)) dati_grid$mat_loss_rigorosa else dati_grid$mat_loss
  titolo <- if (isTRUE(input$mostra_titolo)) "Loss Surface (Contour View)" else ""
  col_linee <- if (is.null(input$col_linee_grid)) "black" else input$col_linee_grid
  n_livelli <- if (is.null(input$n_livelli_grid)) 15 else input$n_livelli_grid
  col_punto_ott <- if (is.null(input$col_punto_ott_grid)) "#e74c3c" else input$col_punto_ott_grid
  col_regione <- if (is.null(input$col_regione_grid)) "#8e44ad" else input$col_regione_grid
  lwd_linee_grid <- if (is.null(input$lwd_linee_grid)) 1 else input$lwd_linee_grid
  contour(dati_grid$seq1, dati_grid$seq2, mat, col = col_linee, nlevels = n_livelli, lwd = lwd_linee_grid,
          xlab = interpreta_etichetta(simbolo_visualizzazione(dati_grid$par1)), 
          ylab = interpreta_etichetta(simbolo_visualizzazione(dati_grid$par2)), 
          main = titolo, las = 1)
  points(dati_grid$val1_ott, dati_grid$val2_ott, pch = 4, col = col_punto_ott, cex = 2, lwd = 3)
  
  mostra_regione <- if (is.null(input$mostra_regione_confidenza)) TRUE else input$mostra_regione_confidenza
  if (mostra_regione && !is.null(dati_grid$mat_loss_rigorosa) && !is.null(res) && isTRUE(res$successo) && input$tipo_loss == "mse") {
    livello_grid <- livello_grid_valido()
    soglia <- calcola_soglia_confidenza_grid(res$sse, res$obs, length(res$parametri), livello_grid)
    if (!is.na(soglia) && soglia > min(mat, na.rm = TRUE) && soglia < max(mat, na.rm = TRUE)) {
      contour(dati_grid$seq1, dati_grid$seq2, mat, levels = soglia, add = TRUE, col = col_regione, lwd = 2.5, drawlabels = FALSE)
      cex_legenda_grid <- if (is.null(input$grid_cex_legenda)) 0.8 else input$grid_cex_legenda
      pos_legenda_grid <- if (is.null(input$grid_pos_legenda)) "topright" else input$grid_pos_legenda
      legend(pos_legenda_grid, legend = paste0(livello_grid, "% Confidence Region"), col = col_regione, lwd = 2.5, bty = "n", cex = cex_legenda_grid)
    }
  }
  cex_legenda_grid2 <- if (is.null(input$grid_cex_legenda)) 0.8 else input$grid_cex_legenda
  legend("bottomright", legend = "Optimal Fit", pch = 4, col = col_punto_ott, pt.cex = 1.5, pt.lwd = 3, bty = "n", cex = cex_legenda_grid2)
}

output$grafico_grid_3d <- renderPlot({
  disegna_grid_3d(griglia_attiva())
})

output$grafico_grid_contour <- renderPlot({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  disegna_grid_contour(griglia_attiva(), res)
})

output$scarica_grid_3d <- downloadHandler(
  filename = function() paste0("loss_surface_3d.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_grid_3d(griglia_attiva())
    dev.off()
  }
)

output$scarica_grid_contour <- downloadHandler(
  filename = function() paste0("loss_surface_contour.", input$formato_esportazione),
  content = function(file) {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_grid_contour(griglia_attiva(), res)
    dev.off()
  }
)

output$stato_grid_search <- renderText({
  dati <- griglia_attiva()
  if (is.null(dati) || !isTRUE(dati$successo)) return("")
  if (!is.null(dati$mat_loss_rigorosa)) {
    "Showing: Rigorous Profile (other parameters re-optimized at every grid point)"
  } else {
    "Showing: Fast Grid (other parameters held fixed at their optimal value)"
  }
})

# Explicitly shows the computation of the F threshold used for the confidence
# region (formula: SSE_opt * (1 + (2/(n-p)) * F(2, n-p, level))) - the
# same computation done internally in calcola_soglia_confidenza_grid(),
# made visible here with the real numbers used, not only inside the plot.
output$info_soglia_confidenza <- renderPrint({
  dati <- griglia_attiva()
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(dati) || !isTRUE(dati$successo) || is.null(dati$mat_loss_rigorosa)) {
    cat("Not computed: requires the Rigorous Profile to be run first.")
    return(invisible(NULL))
  }
  if (is.null(res) || !isTRUE(res$successo) || input$tipo_loss != "mse") {
    cat("Not computed: the confidence region threshold is only valid for MSE loss (current loss: ", input$tipo_loss, ").", sep = "")
    return(invisible(NULL))
  }
  n <- res$obs; p_tot <- length(res$parametri); df_den <- n - p_tot
  livello <- livello_grid_valido()
  f_crit <- qf(livello / 100, df1 = 2, df2 = df_den)
  soglia <- res$sse * (1 + (2 / df_den) * f_crit)
  cat(sprintf("F-distribution critical value: F(2, %d) at %s%% = %.4f\n", df_den, livello, f_crit))
  cat(sprintf("SSE threshold for the confidence region: %.5f * [1 + (2/%d) * %.4f] = %.5f\n", res$sse, df_den, f_crit, soglia))
  cat(sprintf("(Optimal fit's SSE = %.5f; the confidence region is the area where SSE stays below the threshold above.)", res$sse))
})

output$info_soglia_profile_ci <- renderPrint({
  d_profili <- profili_tutti_parametri_dati()
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(d_profili) || !isTRUE(d_profili$successo)) {
    cat("Not computed: click \"Run Profile Likelihood\" above.")
    return(invisible(NULL))
  }
  if (is.null(res) || !isTRUE(res$successo) || input$tipo_loss != "mse") {
    cat("Not computed: the confidence threshold is only valid for MSE loss (current loss: ", input$tipo_loss, ").", sep = "")
    return(invisible(NULL))
  }
  df_den <- res$obs - length(res$parametri)
  livello <- livello_profile_ci_valido()
  f_crit <- qf(livello / 100, df1 = 1, df2 = df_den)
  soglia <- res$sse * (1 + (1 / df_den) * f_crit)
  cat(sprintf("F-distribution critical value: F(1, %d) at %s%% = %.4f\n", df_den, livello, f_crit))
  cat(sprintf("SSE threshold for each parameter's marginal interval: %.5f * [1 + (1/%d) * %.4f] = %.5f\n", res$sse, df_den, f_crit, soglia))
  cat(sprintf("(Optimal fit's SSE = %.5f; each parameter's Profile CI bound is where its own profile curve crosses the threshold above.)", res$sse))
})

# ---- Marginal profile-likelihood confidence intervals (per parameter) ----
# Reuses the rigorous grid already computed (no additional
# optimization): for each value of a parameter, the MINIMUM of the loss along
# the corresponding row/column IS already the complete profile of
# that parameter alone (since the other parameter of the grid, besides
# all the others, has already been re-optimized point by point). The
# result is a confidence interval based on the real curvature
# of the loss surface - a method statistically different from Monte
# Carlo (which is based on the empirical resampling of the data), not the same
# computation under another name. Valid only for the MSE loss (assumes Gaussian
# errors, like all the classic SSE/F-test theory).
# ---- Model Comparison CI for the other parameters: SEPARATE ANALYSIS, independent
# of Run Rigorous Profile - it still requires that the latter has already
# been run at least once (it needs res$sse, the F-test threshold, and the
# structure of the model), but its trigger is its own button/checkbox,
# not linked to the click on Run Rigorous Profile. Exactly the same statistical
# technique (every other free parameter re-optimized continuously),
# search range ±50% of the estimate (same convention as the
# Min/Max cells of p1/p2) and same resolution (input$grid_risoluzione).
profili_tutti_parametri_dati <- reactiveVal(NULL)

# Clears the result when the single fit is relaunched (Run Optimization
# Calculation) - without this, an old result of Run Model Comparison
# would remain visible even after a completely different fit (data,
# guesses or model changed), giving the misleading impression that it is
# still valid. Separate observer, it does not touch the logic of elaborazione()
# itself.
observeEvent(input$esegui, {
  profili_tutti_parametri_dati(NULL)
})

# A width input (%) for EACH parameter of the model, instead of a
# single shared one - a 50% that works for one parameter may not work
# for another, since scales and sensitivities can differ
# a lot among different parameters of the same model.
output$profile_ci_spread_dinamica_ui <- renderUI({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !isTRUE(res$successo)) return(NULL)
  nomi_par <- names(res$parametri)
  blocchi <- lapply(nomi_par, function(nome) {
    etichetta <- simbolo_a_testo_semplice(simbolo_visualizzazione(nome))
    stima_par <- as.numeric(res$parametri[[nome]])
    # Pre-filled at +-50% around the estimate, same convention as
    # before - but now as directly editable point values,
    # instead of a percentage that recomputed them every time.
    if (stima_par == 0) {
      default_min <- -1; default_max <- 1
    } else if (stima_par > 0) {
      default_min <- stima_par * 0.5; default_max <- stima_par * 1.5
    } else {
      default_min <- stima_par * 1.5; default_max <- stima_par * 0.5
    }
    tagList(
      column(2, numericInput(paste0("profile_ci_min_", nome), paste0("Min ", etichetta, ":"), value = signif(default_min, 4))),
      column(2, numericInput(paste0("profile_ci_max_", nome), paste0("Max ", etichetta, ":"), value = signif(default_max, 4)))
    )
  })
  tagList(
    tags$strong(style = "font-size: 13px;", "Search Range (Min/Max), per parameter:"),
    fluidRow(blocchi)
  )
})

observeEvent(input$esegui_profile_ci, {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !isTRUE(res$successo)) { profili_tutti_parametri_dati(list(successo = FALSE, messaggio = "Run the single fit first.")); return() }
  if (input$tipo_loss != "mse") { profili_tutti_parametri_dati(list(successo = FALSE, messaggio = "Only valid for MSE loss.")); return() }
  
  df_den <- res$obs - length(res$parametri)
  if (df_den <= 0) { profili_tutti_parametri_dati(list(successo = FALSE, messaggio = "Not enough residual degrees of freedom.")); return() }
  
  x_dati <- res$df_originale$x; y_dati <- res$df_originale$y
  tutti_nomi <- names(res$parametri)
  
  f_crit_1 <- qf(livello_profile_ci_valido() / 100, df1 = 1, df2 = df_den)
  soglia <- res$sse * (1 + (1 / df_den) * f_crit_1)
  # Own resolution of this analysis (5-100, wider interval
  # than that of the 2D grid since here only one parameter is profiled
  # at a time - a 1D is much cheaper than a 2D, it makes sense to be able to
  # ask for more points), completely unlinked from "Grid Resolution"
  # (which serves only the 2D grid/joint confidence region above).
  risoluzione <- if (is.null(input$profile_ci_risoluzione) || is.na(input$profile_ci_risoluzione)) 100 else min(max(round(input$profile_ci_risoluzione), 5), 1000)
  
  profili <- vector("list", length(tutti_nomi))
  names(profili) <- tutti_nomi
  withProgress(message = "Running Profile Likelihood...", value = 0, {
    for (idx in seq_along(tutti_nomi)) {
      p_target <- tutti_nomi[idx]
      incProgress(1 / length(tutti_nomi), detail = paste0("Parameter ", idx, " of ", length(tutti_nomi), ": ", p_target))
      stima_t <- res$parametri[[p_target]]
      # Search range read directly from the Min/Max fields of
      # this parameter (pre-filled at +-50% but editable to point
      # values) - no longer recomputed from a percentage at every round.
      range_inf_t <- input[[paste0("profile_ci_min_", p_target)]]
      range_sup_t <- input[[paste0("profile_ci_max_", p_target)]]
      if (is.null(range_inf_t) || is.na(range_inf_t) || is.null(range_sup_t) || is.na(range_sup_t) || range_inf_t >= range_sup_t) {
        if (stima_t == 0) {
          range_inf_t <- -1; range_sup_t <- 1
        } else if (stima_t > 0) {
          range_inf_t <- stima_t * 0.5; range_sup_t <- stima_t * 1.5
        } else {
          range_inf_t <- stima_t * 1.5; range_sup_t <- stima_t * 0.5
        }
      }
      seq_t <- seq(range_inf_t, range_sup_t, length.out = risoluzione)
      
      altri_no_target <- setdiff(tutti_nomi, p_target)
      valori_iniziali_no_target <- as.numeric(res$parametri[altri_no_target])
      
      profilo_t <- rep(NA_real_, length(seq_t))
      # Trace of the OTHER parameters: for each value of the profiled
      # parameter, the optimal values that the others take to compensate
      # - a free by-product of the same loop (optim() computes them anyway),
      # they simply were not saved until now. It is the raw ingredient
      # of the "profile traces" of Bates & Watts (1988): it shows how the
      # parameters move together, not only how much the loss worsens.
      traccia_altri_t <- matrix(NA_real_, nrow = length(seq_t), ncol = length(altri_no_target),
                                dimnames = list(NULL, altri_no_target))
      jacobiano_profilo_t <- crea_jacobiano_modello(res$formula, altri_no_target)
      for (k in seq_along(seq_t)) {
        obiettivo_t <- function(altri) {
          valori_completi <- as.list(res$fissi)
          for (m in seq_along(altri_no_target)) valori_completi[[altri_no_target[m]]] <- altri[m]
          valori_completi[[p_target]] <- seq_t[k]
          y_pred <- tryCatch(valuta_formula_generica(res$formula, valori_completi, x_dati), error = function(e) rep(NA_real_, length(x_dati)))
          calcola_loss_generica(y_pred, y_dati, input$tipo_loss, res$pesi)
        }
        valori_fissi_t <- as.list(res$fissi); valori_fissi_t[[p_target]] <- seq_t[k]
        fit_t <- riottimizza_minimi_quadrati(res$formula, valori_fissi_t, altri_no_target, valori_iniziali_no_target,
                                             jacobiano_profilo_t, x_dati, y_dati, res$pesi)
        profilo_t[k] <- if (is.null(fit_t)) NA_real_ else fit_t$value
        if (!is.null(fit_t)) traccia_altri_t[k, ] <- fit_t$par
      }
      
      # ---- Limits of the interval: exact crossing of the threshold ----
      # Previously the limits were the first and the last grid point below
      # the threshold: with well-determined parameters the true interval could be
      # narrower than the grid step (100 points over +/-50% = 1% of the
      # estimate), giving an empty (NA) or zero-width interval, and in any
      # case limits approximated to the step; moreover separate zones below the threshold
      # (e.g. NIST ENSO, two periods that swap roles) were
      # merged in. Now, as the standard methods on profiles do (e.g.
      # confint() in R): we start from the estimate (always below the threshold), we
      # move outwards up to the first grid point above the threshold,
      # and between it and the nearest inner point the exact crossing is sought
      # with uniroot(), recomputing the profile where needed. The interval
      # is the continuous one containing the estimate; any other zones below the
      # threshold beyond the limits are flagged. If all the points of one side
      # are below the threshold, the limit is the edge of the explored interval.
      profilo_in <- function(valore) {
        vf <- as.list(res$fissi); vf[[p_target]] <- valore
        f <- riottimizza_minimi_quadrati(res$formula, vf, altri_no_target, valori_iniziali_no_target, jacobiano_profilo_t, x_dati, y_dati, res$pesi)
        if (is.null(f)) NA_real_ else f$value
      }
      sotto <- !is.na(profilo_t) & profilo_t <= soglia
      cerca_limite <- function(verso) {
        lato <- if (verso < 0) which(seq_t < stima_t) else which(seq_t > stima_t)
        lato <- lato[order(abs(seq_t[lato] - stima_t))]            # from the closest to the estimate outwards
        interno <- stima_t
        for (k in lato) {
          if (sotto[k]) { interno <- seq_t[k]; next }
          radice <- tryCatch(uniroot(function(v) profilo_in(v) - soglia, sort(c(interno, seq_t[k])),
                                     tol = 1e-10 * max(1, abs(stima_t)))$root, error = function(e) NULL)
          return(list(valore = if (is.null(radice)) interno else radice, al_bordo = FALSE,
                      altri_tratti = any(sotto[lato[seq_along(lato) > which(lato == k)]])))
        }
        list(valore = if (length(lato)) seq_t[lato[length(lato)]] else stima_t, al_bordo = TRUE, altri_tratti = FALSE)
      }
      limite_inf <- cerca_limite(-1); limite_sup <- cerca_limite(1)
      ci_inf_t <- limite_inf$valore; ci_sup_t <- limite_sup$valore
      al_bordo_inf_t <- limite_inf$al_bordo; al_bordo_sup_t <- limite_sup$al_bordo
      
      profili[[p_target]] <- list(stima = stima_t, ci_inf = ci_inf_t, ci_sup = ci_sup_t,
                                  al_bordo_inf = al_bordo_inf_t, al_bordo_sup = al_bordo_sup_t,
                                  altri_tratti = limite_inf$altri_tratti || limite_sup$altri_tratti,
                                  seq_curva = seq_t, profilo_curva = profilo_t, traccia_altri = traccia_altri_t)
    }
  })
  
  profili_tutti_parametri_dati(list(successo = TRUE, profili = profili))
})

ci_marginali_grid_dati <- reactive({
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  d_profili <- profili_tutti_parametri_dati()
  if (is.null(d_profili)) d_profili <- list(successo = FALSE, messaggio = "Click \"Run Profile Likelihood\" below to compute this.")
  if (is.null(res) || !isTRUE(res$successo)) {
    return(list(successo = FALSE, messaggio = "Run the single fit first."))
  }
  if (!isTRUE(d_profili$successo)) {
    return(list(successo = FALSE, messaggio = d_profili$messaggio))
  }
  
  livello <- livello_profile_ci_valido()
  df_den <- res$obs - length(res$parametri)
  
  # "Classic" CI (Wald: estimate +/- t*SE), at the SAME confidence level
  # chosen here - so the comparison among the three methods on the same row is
  # at the same level, not at two different levels (which would be misleading).
  # Computed locally instead of reusing the shared valore_critico():
  # that one always reads input$livello_confidenza (the general level
  # of the sidebar), not the specific level of this tab.
  t_crit_classico <- qt(1 - (1 - livello / 100) / 2, df = df_den)
  
  ha_montecarlo <- !is.null(res$mat_boot_params) && nrow(res$mat_boot_params) >= 10
  p_inf_mc <- (1 - livello / 100) / 2; p_sup_mc <- 1 - p_inf_mc
  
  righe <- lapply(names(d_profili$profili), function(nome_par) {
    info <- d_profili$profili[[nome_par]]
    stima <- info$stima
    se_p <- as.numeric(res$errori[nome_par])
    classico_inf <- if (is.finite(se_p)) stima - t_crit_classico * se_p else NA_real_
    classico_sup <- if (is.finite(se_p)) stima + t_crit_classico * se_p else NA_real_
    if (ha_montecarlo && nome_par %in% colnames(res$mat_boot_params)) {
      mc_inf <- as.numeric(quantile(res$mat_boot_params[, nome_par], p_inf_mc, na.rm = TRUE))
      mc_sup <- as.numeric(quantile(res$mat_boot_params[, nome_par], p_sup_mc, na.rm = TRUE))
    } else {
      mc_inf <- mc_sup <- NA_real_
    }
    list(par = nome_par, stima = stima,
        altri_tratti = isTRUE(info$altri_tratti),
        profile_inf = list(valore = info$ci_inf, al_bordo = info$al_bordo_inf),
        profile_sup = list(valore = info$ci_sup, al_bordo = info$al_bordo_sup),
        classico_inf = classico_inf, classico_sup = classico_sup,
        mc_inf = mc_inf, mc_sup = mc_sup,
        seq_curva = info$seq_curva, profilo_curva = info$profilo_curva, traccia_altri = info$traccia_altri)
  })
  
  list(successo = TRUE, righe = righe, ha_montecarlo = ha_montecarlo)
})

output$tabella_ci_marginali_grid <- renderTable({
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) return(data.frame(Message = d$messaggio))
  
  formatta <- function(info) {
    if (is.na(info$valore)) return("N/A (widen the grid range)")
    if (info$al_bordo) paste0(formatta_cifre_significative(info$valore), " (range edge \u2014 widen the Min/Max range for the exact bound)") else formatta_cifre_significative(info$valore)
  }
  formatta_semplice <- function(valore) if (is.na(valore)) "N/A" else formatta_cifre_significative(valore)
  
  df_righe <- lapply(d$righe, function(r) {
    riga <- data.frame(
      Parameter = r$par,
      Estimate = formatta_cifre_significative(r$stima),
      Profile_CI_Lower = formatta(r$profile_inf),
      Profile_CI_Upper = formatta(r$profile_sup),
      Classic_CI_Lower = formatta_semplice(r$classico_inf),
      Classic_CI_Upper = formatta_semplice(r$classico_sup),
      stringsAsFactors = FALSE
    )
    # Other zones below the threshold beyond the limits (non-monotone profile, e.g. two
    # periods that swap roles): the reported interval is the
    # continuous one containing the estimate, but the user must be warned.
    if (isTRUE(r$altri_tratti)) riga$Profile_CI_Upper <- paste0(riga$Profile_CI_Upper, " \u26a0\ufe0f other regions below the threshold beyond this interval (see the profile curve)")
    if (isTRUE(d$ha_montecarlo)) {
      riga$MonteCarlo_CI_Lower <- formatta_semplice(r$mc_inf)
      riga$MonteCarlo_CI_Upper <- formatta_semplice(r$mc_sup)
    }
    riga
  })
  tab <- do.call(rbind, df_righe)
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

# ---- Comparison bar charts: one plot per parameter, one group
# of bars per method (F-test/Classic/Monte Carlo), error bar from the
# actual limits of that method - allows seeing at a glance
# how much the three methods agree or diverge on the same parameter.
# ---- 2D plot of the profile: parameter on the x axis, loss function
# on the y axis - the very curve from which the profile CI is extracted,
# shown in full instead of reducing it to just two numbers (the limits).
# The two lines (vertical at the optimal value of the parameter, horizontal
# at the global minimum loss) cross exactly at the optimum
# point, by statistical construction: holding the parameter there and
# re-optimizing everything else always gives back the global optimum.
disegna_profilo_2d <- function(indice_parametro) {
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo) || indice_parametro > length(d$righe)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Not available.")
    return(invisible(NULL))
  }
  r <- d$righe[[indice_parametro]]
  if (is.null(r$seq_curva) || is.null(r$profilo_curva) || all(is.na(r$profilo_curva))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Profile curve not available for this parameter.")
    return(invisible(NULL))
  }
  
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  simbolo <- interpreta_etichetta(simbolo_visualizzazione(r$par))
  col_linea <- if (is.null(input$profilo2d_col_linea)) "black" else input$profilo2d_col_linea
  col_punto <- if (is.null(input$profilo2d_col_punto)) "#aed6f1" else input$profilo2d_col_punto
  fnt_cex <- if (is.null(input$profilo2d_fnt_cex)) 0.9 else input$profilo2d_fnt_cex
  lwd_linea <- if (is.null(input$profilo2d_lwd_linea)) 1.5 else input$profilo2d_lwd_linea
  pch_punto <- if (is.null(input$profilo2d_pch_punto)) 16 else as.numeric(input$profilo2d_pch_punto)
  cex_punto <- if (is.null(input$profilo2d_cex_punto)) 0.6 else input$profilo2d_cex_punto
  
  grid_on <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  par(mar = c(4, 4, if (input$mostra_titolo) 3 else 1.5, 1))
  plot(r$seq_curva, r$profilo_curva, type = "l", col = col_linea, lwd = lwd_linea,
       main = if (input$mostra_titolo) simbolo else "", xlab = "Parameter value", ylab = "Loss (SSE)", las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex,
       yaxt = "n")
  y_ticks <- axTicks(2)
  axis(2, at = y_ticks, labels = signif(y_ticks, 3), las = 1, cex.axis = fnt_cex)
  if (grid_on) grid()
  points(r$seq_curva, r$profilo_curva, pch = pch_punto, col = col_punto, cex = cex_punto)
  
  # Three reference lines, each with a clearly distinct colour/style and
  # explained by the legend below - no more need for a verbal
  # explanation, the plot explains itself:
  # - blue: where the optimal value of the parameter lies (vertical)
  # - green: the absolute minimum loss achievable (horizontal) - the two
  #   cross exactly at the optimum point, by statistical
  #   construction (fixing the parameter there and re-optimizing everything
  #   else always gives back the global optimum)
  # - orange: the F-test threshold - where the profile crosses it are
  #   exactly the limits of the CI already shown in the table above
  etichette_legenda <- character(0); colori_legenda <- character(0); lty_legenda <- numeric(0)
  if (!is.null(res) && isTRUE(res$successo)) {
    abline(v = r$stima, col = "#2980b9", lty = 2, lwd = 1.3)
    abline(h = res$sse, col = "#27ae60", lty = 2, lwd = 1.3)
    etichette_legenda <- c("Optimal parameter value", "Minimum loss (SSE)")
    colori_legenda <- c("#2980b9", "#27ae60")
    lty_legenda <- c(2, 2)
    df_den <- res$obs - length(res$parametri)
    if (df_den > 0) {
      f_crit_1 <- qf(livello_profile_ci_valido() / 100, df1 = 1, df2 = df_den)
      soglia <- res$sse * (1 + (1 / df_den) * f_crit_1)
      abline(h = soglia, col = "#e67e22", lty = 3, lwd = 1.3)
      etichette_legenda <- c(etichette_legenda, "CI threshold (F-based)")
      colori_legenda <- c(colori_legenda, "#e67e22")
      lty_legenda <- c(lty_legenda, 3)
    }
    if (isTRUE(if (is.null(input$profilo2d_mostra_legenda)) TRUE else input$profilo2d_mostra_legenda)) {
      pos_legenda_profilo2d <- if (is.null(input$profilo2d_pos_legenda)) "topright" else input$profilo2d_pos_legenda
      legend(pos_legenda_profilo2d, legend = etichette_legenda, col = colori_legenda, lty = lty_legenda, lwd = 1.3,
             bty = "o", cex = fnt_cex * 0.75, bg = "white")
    }
  }
}

disegna_barre_ci_grid <- function(indice_parametro) {
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = d$messaggio)
    return(invisible(NULL))
  }
  # par1/par2 are always the first two rows of the list, in this order -
  # built this way in ci_marginali_grid_dati() regardless of whether
  # other parameters are also present.
  r <- d$righe[[indice_parametro]]
  nome_par <- r$par; stima <- r$stima
  inf_vals <- c(r$profile_inf$valore, r$classico_inf, r$mc_inf)
  sup_vals <- c(r$profile_sup$valore, r$classico_sup, r$mc_sup)
  etichette_metodo <- c("Profile", "Classic", "Monte Carlo")
  # If this fit did not use Monte Carlo, that column is NA anyway
  # (already handled by ci_marginali_grid_dati()) - it is excluded here
  # simply by not drawing that bar, instead of showing it empty.
  disponibili <- is.finite(inf_vals) & is.finite(sup_vals)
  if (!any(disponibili)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No confidence interval available to compare for this parameter.")
    return(invisible(NULL))
  }
  etichette_metodo <- etichette_metodo[disponibili]
  inf_vals <- inf_vals[disponibili]; sup_vals <- sup_vals[disponibili]
  n_barre <- length(etichette_metodo)
  
  simbolo <- interpreta_etichetta(simbolo_visualizzazione(nome_par))
  col_barre <- if (is.null(input$grid_ci_col_barre)) "#f39c12" else input$grid_ci_col_barre
  col_bordo <- if (is.null(input$grid_ci_col_bordo)) "black" else input$grid_ci_col_bordo
  fnt_cex <- if (is.null(input$grid_ci_fnt_cex)) 0.9 else input$grid_ci_fnt_cex
  
  par(mar = c(5, 4, 3, 1))
  range_dati <- range(c(inf_vals, sup_vals, stima), na.rm = TRUE)
  # barplot() ALWAYS draws the bars starting from zero, regardless
  # of where range_dati starts - if this does not include zero (typical with
  # all-positive values, common after the boundaries added to the registry),
  # the part of the bar below the lower limit of the plot is cut
  # off, and the base (axis at y=0) becomes invisible. ylim must always include
  # zero for this reason.
  ylim_barre <- limiti_asse_barre(range_dati)
  posizioni <- barplot(rep(stima, n_barre), names.arg = etichette_metodo, main = simbolo, ylab = "Value", xlab = if (titolo_x_attivo(input$grid_ci_mostra_titolo_x)) "Method" else "",
                       col = col_barre, border = col_bordo, cex.names = fnt_cex, cex.axis = fnt_cex,
                       ylim = ylim_barre)
  arrows(posizioni, inf_vals, posizioni, sup_vals, angle = 90, code = 3, length = 0.08, col = col_bordo, lwd = 1.5)
}

output$sezione_profilo_2d_dinamica <- renderUI({
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    return(p(style = "font-size: 12px; color: #566573;", d$messaggio))
  }
  n_par <- length(d$righe)
  blocchi <- lapply(seq_len(n_par), function(idx) {
    column(6,
      plotOutput(paste0("grafico_profilo_2d_", idx), width = "440px", height = "440px"),
      div(style = "margin-top: 8px;",
          downloadButton(paste0("scarica_profilo_2d_", idx), "Download Plot", class = "btn-outline-secondary btn-sm"),
          if (idx == 1) actionButton("btn_estetica_profilo_2d", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  })
  righe_fluid <- lapply(seq(1, n_par, by = 2), function(i) {
    do.call(fluidRow, blocchi[i:min(i + 1, n_par)])
  })
  tagList(
    tags$h5(style = "margin-top: 15px; display: inline-block; margin-right: 10px;", "Profile Curve (Parameter vs Loss):"),
    downloadButton("scarica_tutti_profilo_2d", "Download All (ZIP)", class = "btn-outline-primary btn-sm"),
    downloadButton("scarica_composito_profilo_2d", "Download Combined Image", class = "btn-outline-primary btn-sm"),
    righe_fluid
  )
})

output$scarica_tutti_profilo_2d <- downloadHandler(
  filename = function() "grid_profile_curves_all.zip",
  content = function(file) {
    d <- ci_marginali_grid_dati()
    if (!isTRUE(d$successo)) return()
    crea_zip_grafici(seq_len(length(d$righe)), "profile_curve", disegna_profilo_2d, file)
  },
  contentType = "application/zip"
)

MASSIMO_PARAMETRI_PROFILO_2D <- 10

# Creates a zip with one file for each index in 'indici', drawn by the
# function passed, in the same format/size already chosen for the
# single downloads (dimensioni_esportazione()) - shared by all the
# "Download All" buttons of the sections with several plots.
#
# Uses zip::zip() (zip package, already used elsewhere in the app for the same
# purpose), NOT utils::zip() - the latter calls an external system
# program (the "zip" binary), which in the local test sandbox was
# available but in the real deployment environment of the app (presumably
# Shinylive/WebAssembly, where external processes cannot be launched)
# is not available at all, making the download fail silently - hence
# the reported bug. zip::zip() is a self-contained R package, without
# this external dependency.
crea_zip_grafici <- function(indici, prefisso_file, funzione_disegno, file_destinazione) {
  dir_temp <- tempfile("grafici_zip_")
  dir.create(dir_temp)
  dim_exp <- dimensioni_esportazione()
  estensione <- if (input$formato_esportazione == "pdf") "pdf" else "jpg"
  for (i in indici) {
    percorso <- file.path(dir_temp, paste0(prefisso_file, "_", i, ".", estensione))
    if (input$formato_esportazione == "pdf") pdf(percorso, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(percorso, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    tryCatch(funzione_disegno(i), error = function(e) NULL)
    dev.off()
  }
  file_generati <- list.files(dir_temp)
  cartella_corrente <- getwd()
  on.exit(setwd(cartella_corrente))
  setwd(dir_temp)
  zip::zip(zipfile = file_destinazione, files = file_generati)
}

# ---- t-plot (Bates & Watts, 1988): transforms the same curve of
# Model Comparison (loss vs parameter value, already computed, no
# new optimization needed) into the tau statistic:
#   tau(theta) = sign(theta - theta_hat) * sqrt((SSE(theta)-SSE_opt)/s^2)
# If the model behaved perfectly linearly near
# the optimum, tau(theta) would be EXACTLY a straight line with slope
# 1/SE_classic(theta) - that theoretical line is drawn for comparison.
# Where the real tau curve crosses +-t_critical (horizontal lines)
# EXACTLY the same limits as the Model Comparison CI already
# in the table are found (F(1,df) = t(df)^2, numerically verified) - here however one
# SEES at a glance how much the real curve departs from the theoretical
# line, i.e. how far the classic (symmetric) interval can be trusted
# instead of the profile (asymmetric) one.
disegna_t_plot <- function(indice_parametro) {
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo) || indice_parametro > length(d$righe)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Not available.")
    return(invisible(NULL))
  }
  r <- d$righe[[indice_parametro]]
  if (is.null(r$seq_curva) || is.null(r$profilo_curva) || all(is.na(r$profilo_curva))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Profile curve not available for this parameter.")
    return(invisible(NULL))
  }
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !isTRUE(res$successo)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the single fit first.")
    return(invisible(NULL))
  }
  df_den <- res$obs - length(res$parametri)
  # Safety check: if the model has changed after the last "Run
  # Model Comparison" (different formula, renamed parameters), the data
  # saved in ci_marginali_grid_dati() still refer to the OLD
  # model - r$par might no longer exist among the parameters of
  # res$errori, and [[ on an absent name gives "subscript out of bounds"
  # instead of NULL. Checked BEFORE using [[, not afterwards.
  if (!(r$par %in% names(res$errori))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Model has changed - re-run Profile Likelihood.")
    return(invisible(NULL))
  }
  se_classico <- res$errori[[r$par]]
  if (df_den <= 0 || is.null(se_classico) || is.na(se_classico) || se_classico <= 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Classic standard error not available for this parameter.")
    return(invisible(NULL))
  }
  
  # NOTE (self-correction): a previous attempt had "corrected" this
  # formula assuming that calcola_loss_generica(tipo_loss="mse") computes
  # a mean (SSE/n) - verified now by reading the function itself: the
  # "mse" branch actually returns sum(errori^2), i.e. the raw sum
  # (SSE), despite the name. r$profilo_curva and res$sse are therefore ALREADY
  # on the same scale (SSE) - the right formula is the original one,
  # without any additional factor.
  s2 <- res$sse / df_den
  tau <- sign(r$seq_curva - r$stima) * sqrt(pmax(0, (r$profilo_curva - res$sse) / s2))
  t_crit <- qt(1 - (1 - livello_profile_ci_valido() / 100) / 2, df = df_den)
  
  simbolo <- interpreta_etichetta(simbolo_visualizzazione(r$par))
  col_linea <- if (is.null(input$tplot_col_linea)) "black" else input$tplot_col_linea
  col_punto <- if (is.null(input$tplot_col_punto)) "#aed6f1" else input$tplot_col_punto
  col_retta <- if (is.null(input$tplot_col_retta)) "#7f8c8d" else input$tplot_col_retta
  fnt_cex <- if (is.null(input$tplot_fnt_cex)) 0.9 else input$tplot_fnt_cex
  lwd_linea <- if (is.null(input$tplot_lwd_linea)) 1.5 else input$tplot_lwd_linea
  pch_punto <- if (is.null(input$tplot_pch_punto)) 16 else as.numeric(input$tplot_pch_punto)
  cex_punto <- if (is.null(input$tplot_cex_punto)) 0.6 else input$tplot_cex_punto
  mostra_punti <- if (is.null(input$tplot_mostra_punti)) TRUE else input$tplot_mostra_punti
  mostra_soglia <- if (is.null(input$tplot_mostra_soglia)) TRUE else input$tplot_mostra_soglia
  zoom_soglia <- if (is.null(input$tplot_zoom_soglia)) FALSE else input$tplot_zoom_soglia
  zoom_margine_pct <- if (is.null(input$tplot_zoom_margine_pct)) 25 else input$tplot_zoom_margine_pct
  
  # Zoom on the Y axis (additional option, independent of the others): instead
  # of showing the whole range of the profiled curve, crops around
  # +-t critical with an extra margin - the zone that really decides
  # the confidence interval fills the plot, instead of being
  # squeezed by the distant ends of the curve.
  ylim_plot <- if (isTRUE(zoom_soglia)) c(-t_crit, t_crit) * (1 + zoom_margine_pct / 100) else range(tau, finite = TRUE)
  
  grid_on <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  # As in the profile t plots of Bates & Watts (1988, ch. 6, p. 213, isomerization
  # data): tau on the left axis, confidence level on the
  # right, studentized parameter delta(theta) = (theta - estimate)/SE
  # on the bottom axis and nominal value on the top one; the linear
  # approximation is the diagonal tau = delta; dotted lines for the
  # construction of the interval (at the level chosen in the app; the book
  # shows 99%).
  delta <- (r$seq_curva - r$stima) / se_classico
  livelli_dx <- c(0.80, 0.90, 0.95, 0.99); t_livelli <- qt(1 - (1 - livelli_dx) / 2, df = df_den)
  par(mar = c(4, 4, if (input$mostra_titolo) 5.2 else 3.6, 4.2))
  plot(delta, tau, type = "n", ylim = ylim_plot, main = "",
       xlab = expression(delta(theta) == (theta - hat(theta)) / SE), ylab = expression(tau ~ "(profile t statistic)"),
       las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex)
  if (input$mostra_titolo) title(main = "Profile t plot", line = 3.6, cex.main = fnt_cex * 1.1)
  usr <- par("usr")
  nominali <- pretty(r$stima + usr[1:2] * se_classico); pos_nom <- (nominali - r$stima) / se_classico
  tieni <- pos_nom >= usr[1] & pos_nom <= usr[2]
  axis(3, at = pos_nom[tieni], labels = format(nominali[tieni]), cex.axis = fnt_cex * 0.9)
  mtext(simbolo, side = 3, line = 2, cex = fnt_cex)
  # Confidence levels on the right axis: only those within the scale and with
  # enough space from the previous label (with very wide tau scales
  # the levels are close to zero and the labels would overlap).
  altezza_et <- 1.15 * strheight("0.99", cex = fnt_cex * 0.85)
  livelli_vis <- c(); ultimo <- -Inf
  for (k in seq_along(t_livelli)) if (t_livelli[k] - ultimo >= altezza_et) { livelli_vis <- c(livelli_vis, k); ultimo <- t_livelli[k] }
  sopra <- livelli_vis[t_livelli[livelli_vis] <= usr[4]]; sotto <- livelli_vis[-t_livelli[livelli_vis] >= usr[3]]
  if (length(sopra) + length(sotto) > 0) {
    axis(4, at = c(t_livelli[sopra], -t_livelli[sotto]), labels = c(livelli_dx[sopra], livelli_dx[sotto]), las = 1, cex.axis = fnt_cex * 0.85)
    mtext("Confidence level", side = 4, line = 3, cex = fnt_cex)
  }
  if (grid_on) grid()
  abline(a = 0, b = 1, col = col_retta, lty = 2, lwd = 1.3)
  lines(delta, tau, col = col_linea, lwd = lwd_linea)
  if (isTRUE(mostra_punti)) points(delta, tau, pch = pch_punto, col = col_punto, cex = cex_punto)
  etichette_legenda <- c("Profile t", "Linear approximation")
  colori_legenda <- c(col_linea, col_retta); lty_legenda <- c(1, 2)
  if (isTRUE(mostra_soglia)) {
    abline(h = c(-t_crit, t_crit), col = "#e67e22", lty = 3, lwd = 1.3)
    ok_d <- is.finite(delta) & is.finite(tau)
    estremi <- tryCatch(suppressWarnings(approx(tau[ok_d], delta[ok_d], xout = c(-t_crit, t_crit), ties = mean)$y), error = function(e) c(NA_real_, NA_real_))
    for (k in 1:2) if (is.finite(estremi[k])) segments(estremi[k], usr[3], estremi[k], c(-t_crit, t_crit)[k], col = "#e67e22", lty = 3, lwd = 1.3)
    etichette_legenda <- c(etichette_legenda, sprintf("Construction of the %g%% likelihood interval", livello_profile_ci_valido()))
    colori_legenda <- c(colori_legenda, "#e67e22"); lty_legenda <- c(lty_legenda, 3)
  }
  abline(v = 0, col = "#2980b9", lty = 2, lwd = 1.3)
  etichette_legenda <- c(etichette_legenda, "Optimal parameter value")
  colori_legenda <- c(colori_legenda, "#2980b9"); lty_legenda <- c(lty_legenda, 2)
  
  if (isTRUE(if (is.null(input$tplot_mostra_legenda)) TRUE else input$tplot_mostra_legenda)) {
    pos_legenda_tplot <- if (is.null(input$tplot_pos_legenda)) "topleft" else input$tplot_pos_legenda
    legend(pos_legenda_tplot,
           legend = etichette_legenda, col = colori_legenda, lty = lty_legenda, lwd = 1.3,
           bty = "o", cex = fnt_cex * 0.75, bg = "white")
  }
}

# Lists all possible pairs (i,j) with i<j among n parameters - e.g. for
# n=4 it gives (1,2),(1,3),(1,4),(2,3),(2,4),(3,4), the 6 possible pairs.
elenca_coppie_parametri <- function(n) {
  if (n < 2) return(list())
  coppie <- list()
  for (i in seq_len(n - 1)) for (j in (i + 1):n) coppie[[length(coppie) + 1]] <- c(i, j)
  coppie
}

# ---- Profile trace (Bates & Watts, 1988): for a pair of parameters
# (i,j), draws TWO overlaid traces on the same axes (tau_i, tau_j):
#  - the trace of j WHILE i is profiled (j readjusts to
#    compensate at each fixed value of i) - reuses exactly the
#    "other parameters" already computed and saved by Run Model Comparison,
#    no new optimization needed;
#  - symmetrically, the trace of i while j is profiled.
# If the two parameters were linked by a perfectly linear
# relationship, the two traces would be straight lines crossing at the origin with
# slope equal to the correlation (and its reciprocal) between the two - how much
# the real traces depart from straight lines reveals how far that relationship is
# really linear, how much their crossing angle departs from a
# symmetric "X" reveals how correlated the two parameters are.
disegna_profile_trace <- function(indice_coppia) {
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Not available.")
    return(invisible(NULL))
  }
  coppie <- elenca_coppie_parametri(length(d$righe))
  if (indice_coppia > length(coppie)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Not available.")
    return(invisible(NULL))
  }
  idx_i <- coppie[[indice_coppia]][1]; idx_j <- coppie[[indice_coppia]][2]
  r_i <- d$righe[[idx_i]]; r_j <- d$righe[[idx_j]]
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || !isTRUE(res$successo) || is.null(r_i$traccia_altri) || is.null(r_j$traccia_altri)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Trace data not available for this pair.")
    return(invisible(NULL))
  }
  df_den <- res$obs - length(res$parametri)
  # Same safety check as the t-plot: if the model has changed
  # after the last "Run Model Comparison", both the names in res$errori and the
  # column names of traccia_altri (saved with the OLD model) may
  # no longer correspond to the current parameters - checked before
  # any access with [[ or [,name], not afterwards.
  if (!(r_i$par %in% names(res$errori)) || !(r_j$par %in% names(res$errori)) ||
      !(r_j$par %in% colnames(r_i$traccia_altri)) || !(r_i$par %in% colnames(r_j$traccia_altri))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Model has changed - re-run Profile Likelihood.")
    return(invisible(NULL))
  }
  se_i <- res$errori[[r_i$par]]; se_j <- res$errori[[r_j$par]]
  if (df_den <= 0 || is.null(se_i) || is.null(se_j) || !is.finite(se_i) || !is.finite(se_j) || se_i <= 0 || se_j <= 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Classic standard errors not available for this pair.")
    return(invisible(NULL))
  }
  # r$profilo_curva and res$sse are both on the SSE scale (verified
  # by reading calcola_loss_generica: the "mse" branch actually returns
  # sum(errori^2), not a mean, despite the name) - same scale,
  # no conversion needed, s^2 = SSE_opt/(n-p) as usual.
  s2 <- res$sse / df_den
  # Nominal values by default, as in the profile traces of Bates & Watts
  # (1988, ch. 6, p. 213); the tau/delta scale is an option of the app.
  valori_nominali <- if (is.null(input$ptrace_valori_nominali)) TRUE else input$ptrace_valori_nominali
  
  if (isTRUE(valori_nominali)) {
    # Nominal values: the axes show the real value of the parameter
    # (in its unit of measurement), not the studentized version - exactly the same
    # raw data already computed, only without the transformation
    # into tau. The reference point moves accordingly from the
    # origin (0,0) to the combination of the two estimates (stima_i, stima_j).
    asse_x_1 <- r_i$seq_curva; asse_y_1 <- r_i$traccia_altri[, r_j$par]
    asse_y_2 <- r_j$seq_curva; asse_x_2 <- r_j$traccia_altri[, r_i$par]
    punto_riferimento <- c(r_i$stima, r_j$stima)
  } else {
    # Trace 1: i profiled, j readjusts - x axis = tau_i (profile),
    # y axis = tau_j_traccia (standardized with ITS classic SE, not
    # a new profiling: it is a deterministic adjustment, not a
    # new estimate with its own uncertainty).
    asse_x_1 <- sign(r_i$seq_curva - r_i$stima) * sqrt(pmax(0, (r_i$profilo_curva - res$sse) / s2))
    asse_y_1 <- (r_i$traccia_altri[, r_j$par] - r_j$stima) / se_j
    
    # Trace 2: j profiled, i readjusts - same axes, roles inverted.
    asse_y_2 <- sign(r_j$seq_curva - r_j$stima) * sqrt(pmax(0, (r_j$profilo_curva - res$sse) / s2))
    asse_x_2 <- (r_j$traccia_altri[, r_i$par] - r_i$stima) / se_i
    punto_riferimento <- c(0, 0)
  }
  tau_i_prof <- asse_x_1; tau_j_traccia1 <- asse_y_1
  tau_j_prof <- asse_y_2; tau_i_traccia2 <- asse_x_2
  
  simbolo_i <- interpreta_etichetta(simbolo_visualizzazione(r_i$par))
  simbolo_j <- interpreta_etichetta(simbolo_visualizzazione(r_j$par))
  col_traccia1 <- if (is.null(input$ptrace_col1)) "#2980b9" else input$ptrace_col1
  col_traccia2 <- if (is.null(input$ptrace_col2)) "#e74c3c" else input$ptrace_col2
  lwd_traccia <- if (is.null(input$ptrace_lwd)) 1.8 else input$ptrace_lwd
  fnt_cex <- if (is.null(input$ptrace_fnt_cex)) 0.9 else input$ptrace_fnt_cex
  mostra_riferimento <- if (is.null(input$ptrace_mostra_riferimento)) TRUE else input$ptrace_mostra_riferimento
  zoom_ottimo <- if (is.null(input$ptrace_zoom_ottimo)) FALSE else input$ptrace_zoom_ottimo
  zoom_ottimo_range <- if (is.null(input$ptrace_zoom_ottimo_range) || is.na(input$ptrace_zoom_ottimo_range)) 5 else input$ptrace_zoom_ottimo_range
  
  grid_on <- if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  par(mar = c(4, 4, if (input$mostra_titolo) 3 else 1.5, 1))
  # Title and axes: the symbols with subscripts (e.g. N[0] for the parameter "N0")
  # render correctly in plotmath WHEN used directly as
  # a single expression with bquote() - visually verified. The
  # problem (solved elsewhere in this same function) arose
  # only when passing them inside paste0() or inside a multi-entry legend(), where
  # they were treated as vectors of their internal parts instead of
  # as a single symbol - here, in main/xlab/ylab, there is no such
  # risk: they are always a single expression, never concatenated.
  titolo_trace <- if (input$mostra_titolo) bquote(.(simbolo_i) ~ "vs" ~ .(simbolo_j)) else ""
  xlab_trace <- if (isTRUE(valori_nominali)) simbolo_i else bquote(tau[.(simbolo_i)])
  ylab_trace <- if (isTRUE(valori_nominali)) simbolo_j else bquote(tau[.(simbolo_j)])
  # Zoom around the optimum (additional option, independent of the others):
  # instead of the whole range of the traces, crops to +-zoom_ottimo_range
  # on both axes - useful because the tails of the traces, far
  # from the origin, can extend a lot and squeeze the central zone
  # (the one that really matters for judging linearity and correlation).
  # It does not apply with nominal values: there each parameter has its own scale
  # and unit of measurement, a fixed range of +-5 would make no common sense.
  zoom_ottimo <- zoom_ottimo && !isTRUE(valori_nominali)
  lim_x <- if (isTRUE(zoom_ottimo)) c(-zoom_ottimo_range, zoom_ottimo_range) else range(c(tau_i_prof, tau_i_traccia2), finite = TRUE)
  lim_y <- if (isTRUE(zoom_ottimo)) c(-zoom_ottimo_range, zoom_ottimo_range) else range(c(tau_j_traccia1, tau_j_prof), finite = TRUE)
  plot(1, type = "n", xlim = lim_x, ylim = lim_y,
       main = titolo_trace, xlab = xlab_trace, ylab = ylab_trace,
       las = 1, cex.axis = fnt_cex, cex.lab = fnt_cex)
  if (grid_on) grid()
  if (isTRUE(mostra_riferimento)) {
    abline(h = punto_riferimento[2], v = punto_riferimento[1], col = "#bdc3c7", lty = 3, lwd = 1)
  }
  
  lines(tau_i_prof, tau_j_traccia1, col = col_traccia1, lwd = lwd_traccia)
  lines(tau_i_traccia2, tau_j_prof, col = col_traccia2, lwd = lwd_traccia)
  if (isTRUE(mostra_riferimento)) points(punto_riferimento[1], punto_riferimento[2], pch = 4, col = "black", cex = 1.2, lwd = 2)
  
  if (isTRUE(if (is.null(input$ptrace_mostra_legenda)) TRUE else input$ptrace_mostra_legenda)) {
    pos_legenda_trace <- if (is.null(input$ptrace_pos_legenda)) "topleft" else input$ptrace_pos_legenda
    # expression() as CONTAINER (not c() of simple calls): each entry
    # is built with bquote() (text and symbol juxtaposed with *, without
    # any space or visible multiplication sign in plotmath) then
    # wrapped individually in as.expression() - unlike c() on
    # raw calls, which legend() would end up confusing with several
    # entries (internal length of the expression instead of a single
    # element), expression() is designed precisely for this, verified
    # visually before writing it here.
    etichetta_traccia1 <- bquote("Trace of " * .(simbolo_j) * " (" * .(simbolo_i) * " profiled)")
    etichetta_traccia2 <- bquote("Trace of " * .(simbolo_i) * " (" * .(simbolo_j) * " profiled)")
    etichette_trace <- c(as.expression(etichetta_traccia1), as.expression(etichetta_traccia2))
    colori_trace <- c(col_traccia1, col_traccia2); lty_trace <- c(1, 1); pch_trace <- c(NA, NA)
    if (isTRUE(mostra_riferimento)) {
      etichette_trace <- c(etichette_trace, as.expression(quote("Optimum")))
      colori_trace <- c(colori_trace, "black"); lty_trace <- c(lty_trace, NA); pch_trace <- c(pch_trace, 4)
    }
    legend(pos_legenda_trace,
           legend = etichette_trace, col = colori_trace, lty = lty_trace, pch = pch_trace, lwd = lwd_traccia,
           bty = "o", cex = fnt_cex * 0.75, bg = "white")
  }
}

lapply(seq_len(MASSIMO_PARAMETRI_PROFILO_2D), function(idx) {
  local({
    indice_locale <- idx
    output[[paste0("grafico_profilo_2d_", indice_locale)]] <- renderPlot({
      d <- ci_marginali_grid_dati()
      if (!isTRUE(d$successo) || indice_locale > length(d$righe)) return(invisible(NULL))
      disegna_profilo_2d(indice_locale)
    }, res = 100)
    output[[paste0("scarica_profilo_2d_", indice_locale)]] <- downloadHandler(
      filename = function() paste0("grid_profile_curve_par", indice_locale, ".", input$formato_esportazione),
      content = function(file) {
        dim_exp <- dimensioni_esportazione()
        if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        disegna_profilo_2d(indice_locale)
        dev.off()
      }
    )
    output[[paste0("grafico_t_plot_", indice_locale)]] <- renderPlot({
      d <- ci_marginali_grid_dati()
      if (!isTRUE(d$successo) || indice_locale > length(d$righe)) return(invisible(NULL))
      disegna_t_plot(indice_locale)
    }, res = 100)
    output[[paste0("scarica_t_plot_", indice_locale)]] <- downloadHandler(
      filename = function() paste0("grid_t_plot_par", indice_locale, ".", input$formato_esportazione),
      content = function(file) {
        dim_exp <- dimensioni_esportazione()
        if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        disegna_t_plot(indice_locale)
        dev.off()
      }
    )
  })
})

# The possible pairs (n*(n-1)/2) can exceed the number of parameters
# itself already with models with 4-5+ free parameters - its own constant, larger
# than MASSIMO_PARAMETRI_PROFILO_2D, so as not to run short of slots
# pre-generated with the models that have more of them.
MASSIMO_COPPIE_PROFILE_TRACE <- 28

lapply(seq_len(MASSIMO_COPPIE_PROFILE_TRACE), function(idx) {
  local({
    indice_locale <- idx
    output[[paste0("grafico_profile_trace_", indice_locale)]] <- renderPlot({
      d <- ci_marginali_grid_dati()
      if (!isTRUE(d$successo) || indice_locale > length(elenca_coppie_parametri(length(d$righe)))) return(invisible(NULL))
      disegna_profile_trace(indice_locale)
    }, res = 100)
    output[[paste0("scarica_profile_trace_", indice_locale)]] <- downloadHandler(
      filename = function() paste0("grid_profile_trace_", indice_locale, ".", input$formato_esportazione),
      content = function(file) {
        dim_exp <- dimensioni_esportazione()
        if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        disegna_profile_trace(indice_locale)
        dev.off()
      }
    )
  })
})

output$sezione_t_plot_dinamica <- renderUI({
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    return(p(style = "font-size: 12px; color: #566573;", d$messaggio))
  }
  n_par <- length(d$righe)
  blocchi <- lapply(seq_len(n_par), function(idx) {
    column(6,
      plotOutput(paste0("grafico_t_plot_", idx), width = "440px", height = "440px"),
      div(style = "margin-top: 8px;",
          downloadButton(paste0("scarica_t_plot_", idx), "Download Plot", class = "btn-outline-secondary btn-sm"),
          if (idx == 1) actionButton("btn_estetica_t_plot", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  })
  righe_fluid <- lapply(seq(1, n_par, by = 2), function(i) {
    do.call(fluidRow, blocchi[i:min(i + 1, n_par)])
  })
  tagList(
    tags$h5(style = "margin-top: 15px; display: inline-block; margin-right: 10px;", "t-Plots (Bates & Watts, 1988) \u2014 actual profile vs. linear approximation:"),
    downloadButton("scarica_tutti_t_plot", "Download All (ZIP)", class = "btn-outline-primary btn-sm"),
    downloadButton("scarica_composito_t_plot", "Download Combined Image", class = "btn-outline-primary btn-sm"),
    righe_fluid
  )
})

output$scarica_tutti_t_plot <- downloadHandler(
  filename = function() "grid_t_plots_all.zip",
  content = function(file) {
    d <- ci_marginali_grid_dati()
    if (!isTRUE(d$successo)) return()
    crea_zip_grafici(seq_len(length(d$righe)), "t_plot", disegna_t_plot, file)
  },
  contentType = "application/zip"
)

output$sezione_profile_trace_dinamica <- renderUI({
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    return(p(style = "font-size: 12px; color: #566573;", d$messaggio))
  }
  n_coppie <- length(elenca_coppie_parametri(length(d$righe)))
  if (n_coppie == 0) {
    return(p(style = "font-size: 12px; color: #566573;", "Needs at least 2 free parameters."))
  }
  blocchi <- lapply(seq_len(n_coppie), function(idx) {
    column(6,
      plotOutput(paste0("grafico_profile_trace_", idx), width = "440px", height = "440px"),
      div(style = "margin-top: 8px;",
          downloadButton(paste0("scarica_profile_trace_", idx), "Download Plot", class = "btn-outline-secondary btn-sm"),
          if (idx == 1) actionButton("btn_estetica_profile_trace", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  })
  righe_fluid <- lapply(seq(1, n_coppie, by = 2), function(i) {
    do.call(fluidRow, blocchi[i:min(i + 1, n_coppie)])
  })
  tagList(
    tags$h5(style = "margin-top: 15px; display: inline-block; margin-right: 10px;", "Profile Traces (Bates & Watts, 1988) \u2014 how each pair of parameters moves together:"),
    downloadButton("scarica_tutti_profile_trace", "Download All (ZIP)", class = "btn-outline-primary btn-sm"),
    downloadButton("scarica_composito_profile_trace", "Download Combined Image", class = "btn-outline-primary btn-sm"),
    righe_fluid
  )
})

output$scarica_tutti_profile_trace <- downloadHandler(
  filename = function() "grid_profile_traces_all.zip",
  content = function(file) {
    d <- ci_marginali_grid_dati()
    if (!isTRUE(d$successo)) return()
    n_coppie <- length(elenca_coppie_parametri(length(d$righe)))
    if (n_coppie == 0) return()
    crea_zip_grafici(seq_len(n_coppie), "profile_trace", disegna_profile_trace, file)
  },
  contentType = "application/zip"
)

output$sezione_barre_ci_grid_dinamica <- renderUI({
  d <- ci_marginali_grid_dati()
  if (!isTRUE(d$successo)) {
    return(p(style = "font-size: 12px; color: #566573;", d$messaggio))
  }
  n_par <- length(d$righe)
  blocchi <- lapply(seq_len(n_par), function(idx) {
    column(4,
      plotOutput(paste0("grafico_barre_ci_grid_", idx), height = "340px"),
      div(style = "margin-top: 8px;",
          downloadButton(paste0("scarica_barre_ci_grid_", idx), "Download Plot", class = "btn-outline-secondary btn-sm"),
          if (idx == 1) actionButton("btn_estetica_barre_ci_grid", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  })
  # I group the blocks three per row (they were two, but with only 2-3 bars per
  # plot they turned out needlessly huge)
  righe_fluid <- lapply(seq(1, n_par, by = 3), function(i) {
    do.call(fluidRow, blocchi[i:min(i + 2, n_par)])
  })
  tagList(
    tags$h5(style = "margin-top: 15px; display: inline-block; margin-right: 10px;", "Confidence Interval Comparison (Profile vs Classic vs Monte Carlo):"),
    downloadButton("scarica_tutte_barre_ci_grid", "Download All (ZIP)", class = "btn-outline-primary btn-sm"),
    downloadButton("scarica_composito_barre_ci_grid", "Download Combined Image", class = "btn-outline-primary btn-sm"),
    righe_fluid
  )
})

output$scarica_tutte_barre_ci_grid <- downloadHandler(
  filename = function() "grid_ci_comparison_bars_all.zip",
  content = function(file) {
    d <- ci_marginali_grid_dati()
    if (!isTRUE(d$successo)) return()
    crea_zip_grafici(seq_len(length(d$righe)), "ci_comparison_bar", disegna_barre_ci_grid, file)
  },
  contentType = "application/zip"
)

# I pre-generate the plot/download outputs up to a reasonable ceiling (10,
# amply sufficient - no model in the registry exceeds a few
# parameters) instead of generating them dynamically with lapply+local for a
# variable number: safer, it avoids any risk of duplicated or badly scoped
# observers/outputs. Each single output checks whether its own
# index really exists in the current list of parameters - the UI above
# never references them beyond n_par anyway, so Shiny does not even compute them
# in that case.
MASSIMO_PARAMETRI_BARRE_CI <- 10
lapply(seq_len(MASSIMO_PARAMETRI_BARRE_CI), function(idx) {
  local({
    indice_locale <- idx
    output[[paste0("grafico_barre_ci_grid_", indice_locale)]] <- renderPlot({
      d <- ci_marginali_grid_dati()
      if (!isTRUE(d$successo) || indice_locale > length(d$righe)) return(invisible(NULL))
      disegna_barre_ci_grid(indice_locale)
    }, res = 100)
    output[[paste0("scarica_barre_ci_grid_", indice_locale)]] <- downloadHandler(
      filename = function() paste0("grid_ci_comparison_par", indice_locale, ".", input$formato_esportazione),
      content = function(file) {
        dim_exp <- dimensioni_esportazione()
        if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
        else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
        disegna_barre_ci_grid(indice_locale)
        dev.off()
      }
    )
  })
})

# ---- Download Combined Image (all the plots of a section in a single grid image) ----
output$scarica_composito_profilo_2d <- downloadHandler(
  filename = function() nome_file_composito("profile_curves"),
  content = function(file) {
    d <- ci_marginali_grid_dati()
    n_pannelli <- if (isTRUE(d$successo)) length(d$righe) else 0
    if (n_pannelli == 0) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run Profile Likelihood first."))
    } else {
      crea_immagine_composita(file, n_pannelli, disegna_profilo_2d)
    }
  }
)

output$scarica_composito_t_plot <- downloadHandler(
  filename = function() nome_file_composito("t_plots"),
  content = function(file) {
    d <- ci_marginali_grid_dati()
    n_pannelli <- if (isTRUE(d$successo)) length(d$righe) else 0
    if (n_pannelli == 0) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run Profile Likelihood first."))
    } else {
      crea_immagine_composita(file, n_pannelli, disegna_t_plot)
    }
  }
)

output$scarica_composito_profile_trace <- downloadHandler(
  filename = function() nome_file_composito("profile_traces"),
  content = function(file) {
    d <- ci_marginali_grid_dati()
    n_pannelli <- if (isTRUE(d$successo)) length(elenca_coppie_parametri(length(d$righe))) else 0
    if (n_pannelli == 0) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run Profile Likelihood first."))
    } else {
      crea_immagine_composita(file, n_pannelli, disegna_profile_trace)
    }
  }
)

output$scarica_composito_barre_ci_grid <- downloadHandler(
  filename = function() nome_file_composito("ci_comparison_bars"),
  content = function(file) {
    d <- ci_marginali_grid_dati()
    n_pannelli <- if (isTRUE(d$successo)) length(d$righe) else 0
    if (n_pannelli == 0) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run Profile Likelihood first."))
    } else {
      crea_immagine_composita(file, n_pannelli, disegna_barre_ci_grid)
    }
  }
)
