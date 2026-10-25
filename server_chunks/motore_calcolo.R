# ============================
# COMPUTATION ENGINE
# ============================
simulazione_dati <- reactive({
  # The dependency on valori_guess_correnti() (which reads the sliders of the
  # parameters) makes this reactive "live": it recomputes itself at every
  # movement of a slider, or at the click of "Load Current Sidebar Guesses",
  # without needing to press any separate "Simulate" button.
  valori_correnti <- valori_guess_correnti()
  if (is.null(valori_correnti)) {
    return(list(successo = FALSE, messaggio = "Click \u201cLoad Current Sidebar Guesses\u201d above first."))
  }
  
  formula_stringa <- costruisci_formula(input$tipo_modello, input$formula_custom)
  guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) {
    return(list(successo = FALSE, messaggio = paste("Parameter Error:", e$message)))
  })
  if (is.list(guesses_raw) && !is.null(guesses_raw$successo) && !guesses_raw$successo) return(guesses_raw)
  fissi <- guesses_raw$fissi
  if (!valida_formula(formula_stringa, guesses_raw$tutti_nomi)) {
    return(list(successo = FALSE, messaggio = "Security Error: Invalid formula or undeclared parameter names detected."))
  }
  
  df_punti <- tryCatch(dati(), error = function(e) NULL)
  valori_x_base <- if (!is.null(df_punti) && nrow(df_punti) > 0) df_punti$x else c(0, 10)
  limiti_x <- calcola_limiti(input$range_x_min_sim, input$range_x_max_sim, valori_x_base)
  x_griglia <- seq(limiti_x[1], limiti_x[2], length.out = 300)
  
  y_griglia <- tryCatch({
    env <- as.list(c(valori_correnti, fissi))
    env$x <- x_griglia
    res_y <- eval(parse(text = formula_stringa), envir = env)
    if (any(!is.finite(res_y))) stop("Mathematical calculation yielded non-finite values (Inf / NaN).")
    res_y
  }, error = function(e) {
    return(list(successo = FALSE, messaggio = paste("Mathematical Model Error:", e$message)))
  })
  if (is.list(y_griglia) && !is.null(y_griglia$successo) && !y_griglia$successo) return(y_griglia)
  
  return(list(successo = TRUE, x = x_griglia, y = y_griglia, formula = formula_stringa, punti_exp = df_punti))
})

elaborazione <- eventReactive(input$esegui, {
  # Does not run the fit if the button was pressed while on the
  # Batch Elaboration tab (where this same button makes no
  # sense, since that tab has its own "Run Batch Elaboration" on
  # completely different data) - it really prevents the execution, it does not only warn,
  # so a click there does not accidentally overwrite the results of the single
  # fit already computed with wrong data.
  if (identical(input$main_tabs, "tab_batch")) {
    return(list(successo = FALSE, messaggio = "This button doesn't run anything on the Batch Elaboration tab. Use 'Run Batch Elaboration' instead."))
  }
  df <- dati()
  # The RMSLE is defined only for non-negative values: the observed
  # (and predicted) values below zero are treated as 0 in the computation of the loss.
  # Previously this happened silently: now the user is warned.
  if (identical(input$tipo_loss, "rmsle") && !is.null(df) && any(df$y < 0, na.rm = TRUE)) {
    showNotification(sprintf("RMSLE is defined only for non-negative values: %d observed value(s) below zero are treated as 0 in the loss.", sum(df$y < 0, na.rm = TRUE)), type = "warning", duration = 10)
  }
  if (nrow(df) < 3) {
    showNotification("✗ Optimization Failed: Insufficient data for complete diagnostic calculations (at least 3 data points required).", type = "error", duration = 8)
    return(list(successo = FALSE, messaggio = "Insufficient data for complete diagnostic calculations."))
  }
  
  guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) e)
  if (inherits(guesses_raw, "error")) {
    showNotification(paste("✗ Optimization Failed: Error parsing initial guesses:", conditionMessage(guesses_raw)), type = "error", duration = 8)
    return(list(successo = FALSE, messaggio = paste("Error parsing initial guesses:", conditionMessage(guesses_raw))))
  }
  guesses <- guesses_raw$guess
  fissi <- guesses_raw$fissi
  limiti_inferiori <- guesses_raw$lower
  limiti_superiori <- guesses_raw$upper
  p <- length(guesses)
  if (nrow(df) <= p) {
    msg <- paste0("Not enough data points (", nrow(df), ") for the number of free parameters being estimated (", p,
                  "). At least ", p + 1, " data points are required so the residual degrees of freedom (n - p) stay positive.")
    showNotification(paste("✗ Optimization Failed:", msg), type = "error", duration = 8)
    return(list(successo = FALSE, messaggio = msg))
  }
  formula_stringa <- costruisci_formula(input$tipo_modello, input$formula_custom)
  if (!valida_formula(formula_stringa, guesses_raw$tutti_nomi)) {
    showNotification("✗ Optimization Failed: Invalid or unsafe formula.", type = "error", duration = 8)
    return(list(successo = FALSE, messaggio = "Invalid or unsafe formula."))
  }
  
  maxiter_scelto <- if (is.null(input$maxiter_ottimizzazione) || is.na(input$maxiter_ottimizzazione) || input$maxiter_ottimizzazione <= 0) 200 else input$maxiter_ottimizzazione
  # Server-side safety constraint: min/max of numericInput are only
  # HTML hints, they do not prevent manually typing a value
  # out of range. An extreme maxiter, especially combined with many Monte Carlo
  # iterations, would risk making the computation impractically slow.
  maxiter_scelto <- min(max(maxiter_scelto, 50), 2000)
  algoritmo <- algoritmo_effettivo(input$algoritmo, input$tipo_loss)
  esponente_tol <- esponente_tolleranza(input$esponente_tolleranza, algoritmo)
  tolleranza <- 10^esponente_tol
  accetta_non_conv <- isTRUE(input$accetta_non_convergenti)
  # Nelder-Mead restarts (checkbox in section 5, on by default).
  ripartenze_nm <- !isFALSE(input$nm_ripartenze)
  ripartenze_usate <- NULL
  
  # ---- Weighted regression (least squares only, MSE loss) ----
  # Weights w_i = 1/yhat_i^zeta, variance model var(y) = sigma^2 f^zeta,
  # estimated with iteratively reweighted least squares (further below). zeta
  # from the user (1, 2 or a value), or zeta = 2b from the replicates (same
  # computation as the SD vs mean plot). Without weights pesi_radice stays NULL and
  # the computation is identical to the usual one.
  schema_pesi <- if (is.null(input$ponderazione)) "nessuna" else input$ponderazione
  pond <- list(attiva = FALSE, schema = "nessuna", zeta = 0)
  pesi_radice <- NULL; pesi <- NULL
  if (!identical(schema_pesi, "nessuna")) {
    if (!identical(input$tipo_loss, "mse")) {
      showNotification(paste0("Weighting applies only to least squares (MSE loss): it is ignored with the ", toupper(input$tipo_loss), " loss."), type = "warning", duration = 8)
    } else {
      stima_b <- NULL
      zeta <- switch(schema_pesi, zeta1 = 1, zeta2 = 2,
                     personalizzato = suppressWarnings(as.numeric(input$ponderazione_zeta)),
                     repliche = { stima_b <- stima_potenza_varianza(df$x, df$y); if (isTRUE(stima_b$disponibile)) 2 * stima_b$b else NA_real_ },
                     NA_real_)
      if (length(zeta) != 1 || !is.finite(zeta)) {
        msg <- if (identical(schema_pesi, "repliche")) paste0("Weighting: zeta cannot be estimated from the replicates (", if (!is.null(stima_b$motivo)) stima_b$motivo else "not enough replicates", "). Choose a fixed zeta or no weighting.")
               else "Weighting: enter a valid value of zeta."
        showNotification(paste("\u2717 Optimization Failed:", msg), type = "error", duration = 10)
        return(list(successo = FALSE, messaggio = msg))
      }
      pond <- list(attiva = TRUE, schema = schema_pesi, zeta = zeta,
                   b = if (!is.null(stima_b)) stima_b$b else NA_real_, b_ci = if (!is.null(stima_b)) stima_b$ci else c(NA_real_, NA_real_))
    }
  }
  
  valuta_modello <- function(parametri, x) {
    env <- as.list(c(parametri, fissi))
    env$x <- x
    eval(parse(text = formula_stringa), envir = env)
  }
  
  calcola_loss_personalizzata <- function(parametri, x, y) {
    y_pred <- valuta_modello(parametri, x)
    if (any(!is.finite(y_pred))) return(1e10) 
    errori <- y - y_pred
    res_loss <- switch(input$tipo_loss,
                       "mse" = if (is.null(pesi_radice)) sum(errori^2, na.rm = TRUE) else sum((pesi_radice * errori)^2, na.rm = TRUE),
                       "mae" = sum(abs(errori), na.rm = TRUE),
                       "huber" = {
                         delta <- 1.345 * mad(errori, na.rm = TRUE) 
                         if(is.na(delta) || delta == 0) delta <- 1.0
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
    return(res_loss)
  }
  
  # Analytical Jacobian of the model (deriv(), numerical fallback if needed),
  # passed to Levenberg-Marquardt and used for the covariance. The residual is
  # y - f, so its Jacobian is -J; where the model is not finite the
  # residual equals a constant (1e5), so its Jacobian is zero.
  jacobiano_modello <- crea_jacobiano_modello(formula_stringa, names(guesses))
  jacobiano_residui_lm <- function(parametri, x, y) {
    J <- -jacobiano_modello(parametri, x, fissi)
    J[!is.finite(J)] <- 0
    if (!is.null(pesi_radice)) J <- J * pesi_radice
    J
  }
  
  residui_lm <- function(parametri, x, y) {
    y_pred <- valuta_modello(parametri, x)
    if (any(!is.finite(y_pred))) return(rep(1e5, length(y))) 
    if (is.null(pesi_radice)) y - y_pred else pesi_radice * (y - y_pred)
  }
  
  fit_successo <- FALSE
  parametri_ottimi <- NULL
  hessiano_finale <- NULL
  messaggio_stato <- ""
  iterazioni <- NA
  
  convergenza_vera <- TRUE
  tryCatch({
    if (algoritmo == "LM" && input$tipo_loss == "mse") {
      fit <- nls.lm(par = guesses, fn = residui_lm, jac = jacobiano_residui_lm, x = df$x, y = df$y,
                    lower = limiti_inferiori, upper = limiti_superiori,
                    control = controllo_lm(maxiter_scelto, length(guesses), tolleranza))
      parametri_ottimi <- fit$par
      hessiano_finale <- fit$hessian
      iterazioni <- fit$niter
      # info %in% 1:4 = true convergence reached (one of the ftol/ptol/gtol criteria
      # met); info == 5 or negative = iteration limit reached
      # without really converging (MINPACK still returns the best estimate
      # found, but it is not guaranteed to be the true minimum).
      convergenza_vera <- fit$info %in% 1:4
      if (convergenza_vera) {
        fit_successo <- TRUE
        messaggio_stato <- "Levenberg-Marquardt optimization successfully completed."
      } else if (accetta_non_conv) {
        fit_successo <- TRUE
        messaggio_stato <- paste0("Levenberg-Marquardt did NOT fully converge (info=", fit$info, "), but results are shown because \"Accept non-converged results\" is enabled. These estimates may not be a true minimum.")
      } else {
        messaggio_stato <- paste0("Levenberg-Marquardt did not fully converge within the iteration limit (info=", fit$info, "). Try different initial guesses, add boundaries, switch algorithm, increase Max Iterations, or enable \"Accept non-converged results\" if you understand the risk.")
      }
    } else if (algoritmo == "L-BFGS-B") {
      fit <- optim(par = guesses, fn = calcola_loss_personalizzata, x = df$x, y = df$y, 
                   method = "L-BFGS-B", lower = limiti_inferiori, upper = limiti_superiori, hessian = TRUE,
                   control = controllo_optim("L-BFGS-B", maxiter_scelto, guesses, tolleranza))
      parametri_ottimi <- fit$par
      hessiano_finale <- fit$hessian
      iterazioni <- fit$counts["function"]
      convergenza_vera <- fit$convergence == 0
      if (convergenza_vera) {
        fit_successo <- TRUE
        messaggio_stato <- "Optimization successfully completed via: L-BFGS-B"
      } else if (accetta_non_conv) {
        fit_successo <- TRUE
        messaggio_stato <- paste0("L-BFGS-B did NOT fully converge (code=", fit$convergence, "), but results are shown because \"Accept non-converged results\" is enabled. These estimates may not be a true minimum.")
      } else {
        messaggio_stato <- paste("Optimization failed to converge. Error code:", fit$convergence, "- try increasing Max Iterations or enabling \"Accept non-converged results\".")
      }
    } else {
      fit <- optim_con_ripartenze(par = guesses, fn = calcola_loss_personalizzata, x = df$x, y = df$y, method = algoritmo, hessian = TRUE,
                   control = controllo_optim(algoritmo, maxiter_scelto, guesses, tolleranza), ripartenze = ripartenze_nm)
      ripartenze_usate <- fit$ripartenze
      parametri_ottimi <- fit$par
      hessiano_finale <- fit$hessian
      iterazioni <- fit$counts["function"]
      convergenza_vera <- fit$convergence == 0
      if (convergenza_vera) {
        fit_successo <- TRUE
        messaggio_stato <- paste("Optimization successfully completed via:", algoritmo)
      } else if (accetta_non_conv) {
        fit_successo <- TRUE
        messaggio_stato <- paste0(algoritmo, " did NOT fully converge (code=", fit$convergence, "), but results are shown because \"Accept non-converged results\" is enabled. These estimates may not be a true minimum.")
      } else {
        messaggio_stato <- paste("Optimization failed to converge. Error code:", fit$convergence, "- try increasing Max Iterations or enabling \"Accept non-converged results\".")
      }
    }
  }, error = function(e) { messaggio_stato <<- paste("Error:", e$message) })
  
  if (!fit_successo || is.null(parametri_ottimi)) {
    showNotification(paste("✗ Optimization Failed:", messaggio_stato), type = "error", duration = 10)
    return(list(successo = FALSE, messaggio = messaggio_stato))
  }
  
  # Iteratively reweighted least squares: starting from the unweighted fit, the
  # weights 1/yhat^zeta are computed, the model is refitted with fixed weights and this is repeated until
  # the relative change of the parameters falls below 1e-9 (at most 30 cycles).
  # The final weights, computed from the final fit, remain fixed for inference.
  iterazioni_irls <- 0
  if (pond$attiva) {
    esegui_fit_pesato <- function(partenza) tryCatch({
      if (algoritmo == "LM") {
        f <- nls.lm(par = partenza, fn = residui_lm, jac = jacobiano_residui_lm, x = df$x, y = df$y, lower = limiti_inferiori, upper = limiti_superiori,
                    control = controllo_lm(maxiter_scelto, length(partenza), tolleranza))
        list(par = f$par, iter = f$niter, conv = f$info %in% 1:4)
      } else if (algoritmo == "L-BFGS-B") {
        f <- optim(par = partenza, fn = calcola_loss_personalizzata, x = df$x, y = df$y, method = "L-BFGS-B", lower = limiti_inferiori, upper = limiti_superiori,
                   control = controllo_optim("L-BFGS-B", maxiter_scelto, partenza, tolleranza))
        list(par = f$par, iter = f$counts["function"], conv = f$convergence == 0)
      } else {
        f <- optim_con_ripartenze(par = partenza, fn = calcola_loss_personalizzata, x = df$x, y = df$y, method = algoritmo,
                   control = controllo_optim(algoritmo, maxiter_scelto, partenza, tolleranza), ripartenze = ripartenze_nm)
        list(par = f$par, iter = f$counts["function"], conv = f$convergence == 0)
      }
    }, error = function(e) NULL)
    fallisci_pesi <- function(msg) {
      showNotification(paste("✗ Optimization Failed:", msg), type = "error", duration = 10)
      list(successo = FALSE, messaggio = msg)
    }
    msg_positivi <- "Weighting 1/\u0177^\u03b6 requires positive predicted values at all data points; this model gives zero or negative predictions. Use no weighting."
    variazione <- Inf
    for (ciclo in 1:30) {
      y_corrente <- valuta_modello(parametri_ottimi, df$x)
      if (any(!is.finite(y_corrente)) || any(y_corrente <= 0)) return(fallisci_pesi(msg_positivi))
      pesi_radice <- sqrt(1 / y_corrente^pond$zeta)
      f <- esegui_fit_pesato(parametri_ottimi)
      if (is.null(f)) return(fallisci_pesi("the weighted fit failed. Try different initial guesses or no weighting."))
      variazione <- max(abs(f$par - parametri_ottimi) / pmax(abs(parametri_ottimi), 1e-12))
      parametri_ottimi <- f$par; iterazioni <- f$iter; convergenza_vera <- isTRUE(f$conv); iterazioni_irls <- ciclo
      if (is.finite(variazione) && variazione < 1e-9) break
    }
    y_corrente <- valuta_modello(parametri_ottimi, df$x)
    if (any(!is.finite(y_corrente)) || any(y_corrente <= 0)) return(fallisci_pesi(msg_positivi))
    pesi <- 1 / y_corrente^pond$zeta; pesi_radice <- sqrt(pesi)
    if (!convergenza_vera && !accetta_non_conv) return(fallisci_pesi("the weighted fit did not fully converge. Try different initial guesses, increase Max Iterations, or enable \"Accept non-converged results\"."))
    if (!is.finite(variazione) || variazione >= 1e-6) showNotification(sprintf("Weighting: the weights did not fully stabilize after %d reweighting cycles (relative change %.1e).", iterazioni_irls, variazione), type = "warning", duration = 10)
    pond$iterazioni_irls <- iterazioni_irls
  }
  
  limiti_x_griglia <- calcola_limiti(input$range_x_min, input$range_x_max, df$x)
  x_griglia_estesa <- seq(limiti_x_griglia[1], limiti_x_griglia[2], length.out = 200)
  
  n <- length(df$x)
  df_residual <- n - p
  y_pred <- valuta_modello(parametri_ottimi, df$x)
  residui_vettore <- df$y - y_pred
  
  sse <- sum(residui_vettore^2, na.rm = TRUE)
  sst <- sum((df$y - mean(df$y, na.rm = TRUE))^2, na.rm = TRUE)
  mse <- sse / df_residual
  rmse <- sqrt(mse)
  
  media_y_osservata <- mean(df$y, na.rm = TRUE)
  cv_rmse <- ifelse(media_y_osservata != 0, (rmse / media_y_osservata) * 100, NA)
  
  r_corr <- tryCatch(cor(df$y, y_pred, use = "complete.obs"), error = function(e) NA)
  r2 <- ifelse(sst > 0, 1 - (sse / sst), NA)
  r2_adj <- ifelse(sst > 0 && df_residual > 0, 1 - ((1 - r2) * (n - 1) / df_residual), NA)
  
  ic <- criteri_informazione(sse, n, p, input$tipo_loss)
  aic <- ic$aic; aicc <- ic$aicc; bic <- ic$bic
  # With weights: weighted sum of squares (SSE_w), sigma = sqrt(SSE_w/(n-p)),
  # weighted R^2 with respect to the weighted mean, criteria with the term sum(ln w).
  # CV(RMSE) makes no sense with a relative sigma and is not reported.
  if (pond$attiva) {
    sse <- sum(pesi * residui_vettore^2, na.rm = TRUE)
    media_pesata <- sum(pesi * df$y) / sum(pesi)
    sst <- sum(pesi * (df$y - media_pesata)^2)
    mse <- sse / df_residual; rmse <- sqrt(mse); cv_rmse <- NA
    r2 <- ifelse(sst > 0, 1 - (sse / sst), NA)
    r2_adj <- ifelse(sst > 0 && df_residual > 0, 1 - ((1 - r2) * (n - 1) / df_residual), NA)
    ic <- criteri_informazione(sse, n, p, input$tipo_loss, sum(log(pesi)))
    aic <- ic$aic; aicc <- ic$aicc; bic <- ic$bic
  }
  
  errori_standard <- rep(NA, p)
  names(errori_standard) <- names(parametri_ottimi)
  errori_standard_bootstrap <- rep(NA, p)
  names(errori_standard_bootstrap) <- names(parametri_ottimi)
  matrice_cov <- matrix(0, nrow=p, ncol=p)
  matrice_curve_boot <- matrix(NA, nrow = 1, ncol = length(x_griglia_estesa))
  matrice_pred_boot <- matrix(NA, nrow = 1, ncol = length(x_griglia_estesa))
  matrice_boot_parametri <- NULL
  
  covarianza_problematica <- FALSE
  # "Classic" standard errors (Linear method): valid ONLY for least
  # squares. The formula s^2 (J^T J)^-1 derives from the SSE loss; with MAE,
  # Huber, log10cosh or RMSLE applying it (as was done before, on the Hessian
  # of the chosen loss) gave plausible but wrong numbers. With these losses
  # they are not computed: the Monte Carlo method is suggested, whose empirical
  # intervals are valid with any loss.
  errori_non_disponibili_per_loss <- !identical(input$tipo_loss, "mse")
  if (errori_non_disponibili_per_loss) {
    matrice_cov <- NULL
  } else if (!is.null(hessiano_finale)) {
    esito_cov <- try({
      if (algoritmo != "LM" || pond$attiva) {
        # Least squares with BFGS / L-BFGS-B / Nelder-Mead: covariance from the
        # Jacobian of the model, s^2 (J^T J)^-1, not from the numerical Hessian of
        # optim (see jacobiano_numerico in rendering_output.R). With weights,
        # for every algorithm: s^2 (J^T W J)^-1 with the weighted Jacobian.
        J <- jacobiano_modello(parametri_ottimi, df$x, fissi)
        if (pond$attiva) J <- J * pesi_radice
        matrice_cov <- mse * inverti_hessiana(crossprod(J))
      } else {
        # Levenberg-Marquardt: minpack.lm already returns J^T J.
        matrice_cov <- mse * inverti_hessiana(hessiano_finale)
      }
      errori_standard <- sqrt(diag(matrice_cov))
    }, silent = TRUE)
    # The message for the user is given together with the one on convergence,
    # further below, so as not to show two contradictory notifications.
    covarianza_problematica <- inherits(esito_cov, "try-error") || any(!is.finite(errori_standard))
  } else {
    matrice_cov <- NULL
  }
  # Never return an invalid covariance: previously the initial matrix of
  # zeros remained (or one with negative variances), and the downstream plots
  # used it, drawing confidence bands of zero width, null intervals
  # in Inverse Prediction and invalid correlations. NULL is handled by
  # all of them (bands, correlation, Inverse Prediction, Shelf-Life).
  if (covarianza_problematica || errori_non_disponibili_per_loss) {
    matrice_cov <- NULL
    errori_standard[] <- NA
  }
  
  sigma_mc_usato <- NA
  n_boot_successo <- NA
  n_boot_totale <- NA
  if (input$metodo_errore == "monte_carlo") {
    n_sim <- if (is.null(input$n_boot) || is.na(input$n_boot) || input$n_boot <= 0) 100 else input$n_boot
    # Same server-side safety constraint: the number of Monte Carlo
    # iterations is brought back within the range declared in the UI, even if
    # the user manually types an out-of-scale value.
    n_sim <- min(max(round(n_sim), 10), 5000)
    mat_boot <- matrix(NA, n_sim, p)
    matrice_curve_boot <- matrix(NA, nrow = n_sim, ncol = length(x_griglia_estesa))
    matrice_pred_boot <- matrix(NA, nrow = n_sim, ncol = length(x_griglia_estesa))
    
    if (input$tipo_rumore_mc == "gaussiano") {
      sigma_mc_usato <- if (is.null(input$sigma_gaussiano) || is.na(input$sigma_gaussiano) || input$sigma_gaussiano <= 0) rmse else input$sigma_gaussiano
    }
    # Residuals to be resampled (Davison & Hinkley, 1997; option on by default).
    residui_modificati_attivi <- is.null(input$residui_modificati) || isTRUE(input$residui_modificati)
    J_boot <- if (identical(input$tipo_loss, "mse")) tryCatch(jacobiano_modello(parametri_ottimi, df$x, fissi), error = function(e) NULL) else NULL
    # With weights: weighted residuals sqrt(w)(y - yhat) (constant variance) and leverages
    # from the weighted Jacobian; the extracted noise is then brought back to the scale
    # of each point by dividing by sqrt(w), i.e. multiplying by yhat^(zeta/2).
    if (pond$attiva && !is.null(J_boot)) J_boot <- J_boot * pesi_radice
    residui_boot <- residui_per_bootstrap(if (pond$attiva) residui_vettore * pesi_radice else residui_vettore, J_boot, residui_modificati_attivi)
    if (pond$attiva) { y_griglia_fit <- valuta_modello(parametri_ottimi, x_griglia_estesa); scala_griglia <- sqrt(ifelse(y_griglia_fit > 0, y_griglia_fit^pond$zeta, NA_real_)) }
    
    withProgress(message = 'Running Monte Carlo Simulations...', value = 0, {
      for(i in 1:n_sim) {
        rumore_campionato <- if (input$tipo_rumore_mc == "gaussiano") {
          rnorm(n, mean = 0, sd = sigma_mc_usato)
        } else {
          sample(residui_boot, replace=TRUE)
        }
        if (pond$attiva) rumore_campionato <- rumore_campionato / pesi_radice
        y_f <- y_pred + rumore_campionato
        try({
          if (algoritmo == "LM" && input$tipo_loss == "mse") {
            fit_boot <- nls.lm(par = parametri_ottimi, fn = residui_lm, jac = jacobiano_residui_lm, x = df$x, y = y_f,
                               lower = limiti_inferiori, upper = limiti_superiori,
                               control = controllo_lm(maxiter_scelto, length(parametri_ottimi), tolleranza))
            # Discards this iteration if it does not really converge, UNLESS
            # the user has explicitly chosen to accept non-converged
            # results (same choice applied to the main fit).
            if (!(fit_boot$info %in% 1:4) && !accetta_non_conv) stop("Bootstrap iteration did not converge within the iteration limit.")
            p_boot <- fit_boot$par
          } else if (algoritmo == "L-BFGS-B") {
            fit_boot <- optim(par = parametri_ottimi, fn = calcola_loss_personalizzata, x = df$x, y = y_f,
                              method = "L-BFGS-B", lower = limiti_inferiori, upper = limiti_superiori,
                              control = controllo_optim("L-BFGS-B", maxiter_scelto, parametri_ottimi, tolleranza))
            if (fit_boot$convergence != 0 && !accetta_non_conv) stop("Bootstrap iteration did not converge within the iteration limit.")
            p_boot <- fit_boot$par
          } else {
            fit_boot <- optim_con_ripartenze(par = parametri_ottimi, fn = calcola_loss_personalizzata, x = df$x, y = y_f, method = algoritmo,
                              control = controllo_optim(algoritmo, maxiter_scelto, parametri_ottimi, tolleranza), ripartenze = ripartenze_nm)
            if (fit_boot$convergence != 0 && !accetta_non_conv) stop("Bootstrap iteration did not converge within the iteration limit.")
            p_boot <- fit_boot$par
          }
          mat_boot[i,] <- p_boot
          y_linea_media <- valuta_modello(p_boot, x_griglia_estesa)
          matrice_curve_boot[i,] <- y_linea_media
          rumore_predittivo <- if (input$tipo_rumore_mc == "gaussiano") {
            rnorm(length(x_griglia_estesa), mean = 0, sd = sigma_mc_usato)
          } else {
            sample(residui_boot, length(x_griglia_estesa), replace=TRUE)
          }
          if (pond$attiva) rumore_predittivo <- rumore_predittivo * scala_griglia
          matrice_pred_boot[i,] <- y_linea_media + rumore_predittivo
        }, silent=TRUE)
        incProgress(1/n_sim)
      }
    })
    # BUG FIXED: previously this line overwrote errori_standard (the
    # same variable as the classic Hessian-based computation, lines
    # above), erasing its value every time Monte Carlo was the
    # chosen method - "Std Error"/"CI Parametric" in the results table,
    # the CSV/Excel export, and (more seriously) any
    # downstream use of res$errori in the Grid Search engine (t-plot,
    # profile trace) silently received the bootstrap SD
    # passed off as the classic value, instead of the true Hessian standard
    # error - verified by comparison with the NIST certified values
    # for the Rat43/Richards dataset: the classic value (16.302297817)
    # matched exactly BEFORE Monte Carlo overwrote it.
    # Now they are two distinct variables, neither overwrites the other.
    errori_standard_bootstrap <- apply(mat_boot, 2, sd, na.rm=TRUE)
    names(errori_standard_bootstrap) <- names(parametri_ottimi)
    colnames(mat_boot) <- names(parametri_ottimi)
    matrice_boot_parametri <- mat_boot
    # A row of mat_boot is either completely valid (all p parameters
    # written together in one go) or completely NA (failed
    # iteration: error caught by try() or non-converging fit
    # discarded) - so it is enough to check the first column to count the
    # iterations that actually succeeded.
    n_boot_successo <- sum(!is.na(mat_boot[, 1]))
    n_boot_totale <- n_sim
    if (n_boot_successo < MINIMO_ITERAZIONI_MC) {
      # Too few successful iterations: no Monte Carlo result. The
      # plot, the results table and the other sections automatically return
      # to the classic method (standard errors from the covariance).
      matrice_boot_parametri <- NULL
      matrice_curve_boot <- NULL
      matrice_pred_boot <- NULL
      errori_standard_bootstrap[] <- NA
      showNotification(paste0("Only ", n_boot_successo, " of ", n_boot_totale, " Monte Carlo iterations succeeded (at least ", MINIMO_ITERAZIONI_MC,
                              " are needed): Monte Carlo results are not shown", if (errori_non_disponibili_per_loss) "." else ", classic standard errors and bands are shown instead.",
                              " Try increasing Max Iterations, enabling \"Accept non-converged results\", or improving the initial guesses."),
                       type = "warning", duration = 12)
    }
  }
  
  valore_loss_finale <- calcola_loss_personalizzata(parametri_ottimi, df$x, df$y)
  # With Monte Carlo below the threshold only the warning given above applies (no
  # Monte Carlo result): no contradictory messages.
  mc_insufficiente <- identical(input$metodo_errore, "monte_carlo") && is.null(matrice_boot_parametri)
  if (errori_non_disponibili_per_loss && !mc_insufficiente) {
    if (identical(input$metodo_errore, "monte_carlo")) {
      showNotification(paste0("Classic (Jacobian-based) standard errors and parametric CIs are not shown with the ", toupper(input$tipo_loss),
                              " loss: that formula is valid only for least squares (MSE). The Monte Carlo (empirical) intervals are valid with any loss."),
                       type = "message", duration = 10)
    } else {
      showNotification(paste0("Standard errors are not computed with the ", toupper(input$tipo_loss),
                              " loss: the Jacobian-based formula is valid only for least squares (MSE). Select the Monte Carlo method to obtain standard errors and confidence intervals."),
                       type = "warning", duration = 12)
    }
  }
  if (convergenza_vera && !covarianza_problematica) {
    showNotification("✓ Modeling Successful! Parameter estimation and model convergence completed successfully.", type = "message", duration = 4)
  } else if (convergenza_vera && covarianza_problematica) {
    # NIST validation: from certain starting points (e.g. BoxBOD, MGH17 start 1)
    # the optimizer declares convergence at a degenerate point (flat
    # region, parameters not determinable) far from the true minimum; the
    # signal is precisely the singular covariance matrix.
    showNotification("⚠️ The optimizer reports convergence, but the covariance matrix is singular at this solution, so standard errors cannot be computed. This is often a degenerate point (e.g. a flat region where some parameters are not identifiable) rather than a proper minimum: try different initial guesses and check the fit visually.",
                     type = "warning", duration = 12)
  } else {
    showNotification("⚠️ Fit accepted WITHOUT full convergence (see warning in results). Interpret with caution.", type = "warning", duration = 8)
  }
  
  list(successo=TRUE, parametri=parametri_ottimi, errori=errori_standard, errori_bootstrap=errori_standard_bootstrap,
       formula=formula_stringa, valuta=valuta_modello, residui=residui_vettore, 
       x=df$x, y_pred=y_pred, cov=matrice_cov, sigma=rmse, 
       obs=n, df=df_residual, sse=sse, mse=mse, rmse=rmse, cv_rmse=cv_rmse,
       r_corr=r_corr, r2=r2, r2_adj=r2_adj, aic=aic, aicc=aicc, bic=bic, criteri_non_disponibili=!identical(input$tipo_loss, "mse"), iter=iterazioni,
       curve_boot=matrice_curve_boot, pred_boot=matrice_pred_boot,
       mat_boot_params=matrice_boot_parametri, residui_modificati=if (exists("residui_modificati_attivi")) residui_modificati_attivi else NA, loss_valore=valore_loss_finale, esponente_tolleranza=esponente_tol, 
       x_griglia=x_griglia_estesa, df_originale=df,
       tipo_rumore_mc=if(input$metodo_errore=="monte_carlo") input$tipo_rumore_mc else NA,
       sigma_mc=sigma_mc_usato, fissi=fissi, convergenza_vera=convergenza_vera,
       n_boot_successo=n_boot_successo, n_boot_totale=n_boot_totale,
       tipo_modello_usato=input$tipo_modello, ponderazione=pond, pesi=pesi,
       ripartenze_nm = if (identical(algoritmo, "Nelder-Mead")) list(attive = ripartenze_nm, usate = ripartenze_usate) else NULL)
})