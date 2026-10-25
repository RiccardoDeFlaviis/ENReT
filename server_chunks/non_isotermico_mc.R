# ==============================================================================
# NON-ISOTHERMAL MODULE (Model Comparison)
# ==============================================================================
# Exactly the same scientific logic as the twin module in Batch
# (server_chunks/non_isotermico.R), here applied to the single fits
# accumulated with "Add Current Fit to Comparison" instead of to the samples of
# a batch loaded via CSV - same principle already followed for
# "Model Comp. Shelf-Life" (adaptation of "Shelf-Life & Kinetic Model
# Analysis" of Batch).
#
# KEY STRUCTURAL DIFFERENCE with respect to Batch: in Batch all the
# samples share A SINGLE model (chosen once for the whole
# run), whereas here EACH fit accumulated in confronto_modelli() can
# use a model DIFFERENT from the others. The validation must therefore not
# only recognize which fits use an admitted kinetic model, but also
# group those that share EXACTLY the same order (it is not
# enough that they are all "kinetics": mixing different orders would make no
# scientific sense for a single k-T relationship).
#
# DIRECTLY REUSES (no redefinition) the PURE computation functions
# already defined in non_isotermico.R, which do not depend on any reactive
# state specific to Batch: NI_FORMULE_CONOSCIUTE, ni_riconosci_ordine,
# ni_modello_secondario, NI_PARAMETRI_SECONDARIO, ni_guess_secondario,
# ni_costruisci_profilo_T, NI_PARAMETRI_PROFILO, ni_integra_k_cumulativo,
# ni_quantita_da_K, ni_K_soglia, ni_costruisci_fonte_k, ni_pesca_k,
# ni_gradiente_numerico_secondario. The file is loaded AFTER
# non_isotermico.R (see R/server.R), so these functions are already
# available when this file is sourced.
#
# COMPLETELY SELF-CONTAINED FILE for the rest: it does not modify the internals of
# model_comparison.R, and it reads the $temperatura field of confronto_modelli()
# ONLY as an initial suggestion (one-off) - the temperatures entered
# by the user in THIS module live in their own independent local memory
# (mcni_temperature_locali), never written back into the shared
# field. This avoids an endless reactive ping-pong with the
# analogous observer of mc_sl_, which REALLY writes into that shared
# field through its own separate inputs (mc_sl_temp_*) - two
# observers writing into the same shared field through different
# inputs would "correct" each other endlessly as soon as the two values
# differ even slightly. It can be removed entirely (this file
# + the two hook lines in R/server.R and R/ui.R) without any effect
# on the other modules.
# ==============================================================================


# ---- Safe identifier for the Shiny IDs -------------------------------------
# Minimal local copy (one line), as already done in the twin module of
# Batch, instead of reusing the one in shelf_life.R - for the full
# independence of this file from other modules.
mcni_id_sicuro <- function(nome, indice) paste0(gsub("[^A-Za-z0-9_]", "_", nome), "_", indice)


# ---- Validation of the comparison list --------------------------------------
# Unlike Batch, here the fits must be grouped by detected kinetic ORDER
# (not the whole list shares a single model) and the
# group with at least 3 members chosen - if several groups qualify, the
# largest one is used (in case of a tie, the first found), and this is clearly communicated
# to the user in the status shown, so they understand which fits have been
# actually selected.
mcni_batch_valido <- reactive({
  lista <- confronto_modelli()
  if (length(lista) < 3) {
    return(list(valido = FALSE, messaggio = "Add at least 3 fits to the comparison first, using one of the order-based Reaction Kinetics models (Zero-Order, First-Order, Second-Order, or n-Order)."))
  }
  ordini_per_fit <- sapply(lista, function(s) {
    ord <- ni_riconosci_ordine(s$formula)
    if (is.null(ord)) NA_character_ else ord
  })
  ordini_validi <- ordini_per_fit[!is.na(ordini_per_fit)]
  if (length(ordini_validi) == 0) {
    return(list(valido = FALSE, messaggio = "None of the fits in the comparison use an order-based Reaction Kinetics model (Zero-Order, First-Order, Second-Order, or n-Order) \u2014 this module only works with those."))
  }
  tabella_conteggio <- table(ordini_validi)
  if (max(tabella_conteggio) < 3) {
    return(list(valido = FALSE, messaggio = "At least 3 fits sharing the SAME order-based kinetic model are needed (mixing different kinetic orders isn't valid for a single temperature relationship) \u2014 currently no single order has 3 or more qualifying fits."))
  }
  ordine_scelto <- names(tabella_conteggio)[which.max(tabella_conteggio)]
  indici_qualificati <- which(ordini_per_fit == ordine_scelto)
  list(valido = TRUE, ordine = ordine_scelto, indici = indici_qualificati, lista = lista,
       n_altri_ordini = length(tabella_conteggio) - 1)
})

output$mcni_stato <- renderUI({
  stato <- mcni_batch_valido()
  if (!stato$valido) {
    return(p(style = "font-size: 12px; color: #7f8c8d; font-style: italic;", stato$messaggio))
  }
  nota_altri <- if (stato$n_altri_ordini > 0) paste0(" (", stato$n_altri_ordini, " fit using a different kinetic order were ignored.)") else ""
  p(style = "font-size: 12px; color: #196f3d;",
    icon("circle-check"), " Using ", tags$strong(NI_NOME_ORDINE[[stato$ordine]]),
    " kinetics: ", length(stato$indici), " qualifying fits.", nota_altri)
})


# ---- Table associating temperature -> accumulated fit ----------------------
# INDEPENDENT local memory for the temperatures (key = label of the
# fit), instead of writing into the shared field $temperatura of
# confronto_modelli() as mc_sl_ already does - two modules writing into the
# SAME shared field through TWO SEPARATE inputs (mcni_temp_* and
# mc_sl_temp_*) create an endless reactive ping-pong: as soon as one
# writes a value different from the one the other shows in its own
# field, the other "corrects" it back, triggering the
# first again, endlessly. The root solution is not to share
# the writing at all: this module keeps its own copy, read ONLY as an
# initial suggestion (one-off) from the value already present in
# confronto_modelli() if its own local memory has nothing yet.
mcni_temperature_locali <- reactiveValues()

output$mcni_tabella_temperature <- renderUI({
  stato <- mcni_batch_valido()
  if (!stato$valido) return(NULL)
  righe <- lapply(stato$indici, function(i) {
    s <- stato$lista[[i]]
    id_campo <- paste0("mcni_temp_", mcni_id_sicuro(s$label, i))
    valore_prefill <- if (!is.null(mcni_temperature_locali[[s$label]])) {
      mcni_temperature_locali[[s$label]]
    } else if (!is.null(s$temperatura) && !is.na(s$temperatura)) {
      s$temperatura
    } else NA
    fluidRow(
      column(5, tags$div(style = "padding: 7px 10px; margin-top: 2px; font-size: 13px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; background-color: #ffffff; border: 1px solid #d5dbdb; border-radius: 5px;", s$label)),
      column(4, numericInput(id_campo, NULL, value = valore_prefill, width = "100px"))
    )
  })
  tagList(
    tags$strong(style = "font-size: 13px;", "Temperature per fit:"),
    righe
  )
})

observe({
  stato <- mcni_batch_valido()
  if (!stato$valido) return()
  for (i in stato$indici) {
    s <- stato$lista[[i]]
    id_campo <- paste0("mcni_temp_", mcni_id_sicuro(s$label, i))
    valore <- input[[id_campo]]
    if (!is.null(valore) && !is.na(valore) && !identical(valore, mcni_temperature_locali[[s$label]])) {
      mcni_temperature_locali[[s$label]] <- valore
    }
  }
})

# Table (name, estimated k, persisted T) for the qualified fits - basis both
# for the fit of the secondary model and for its display.
mcni_dati_kT <- reactive({
  stato <- mcni_batch_valido()
  if (!stato$valido) return(NULL)
  in_celsius <- is.null(input$mcni_unita_temp) || input$mcni_unita_temp == "celsius"
  righe <- lapply(stato$indici, function(i) {
    s <- stato$lista[[i]]
    T_val <- mcni_temperature_locali[[s$label]]
    if (!is.null(T_val) && !is.na(T_val) && in_celsius) T_val <- T_val + 273.15
    parametri_fit <- as.list(s$parametri)
    k_val <- parametri_fit[["k"]]
    n_val <- parametri_fit[["n"]]
    if (is.null(T_val) || is.na(T_val) || is.null(k_val) || !is.finite(k_val)) return(NULL)
    data.frame(nome = s$label, T = T_val, k = k_val, n_par = if (is.null(n_val)) NA_real_ else n_val)
  })
  righe <- Filter(Negate(is.null), righe)
  if (length(righe) < 3) return(NULL)
  do.call(rbind, righe)
})

output$mcni_tabella_kT <- renderTable({
  dati <- mcni_dati_kT()
  if (is.null(dati)) return(data.frame(Message = "Enter at least 3 valid temperatures above."))
  data.frame(Fit = dati$nome, `Temperature (K)` = as.character(round(dati$T, 2)), `Fitted k` = as.character(signif(dati$k, 7)), check.names = FALSE)
}, striped = TRUE, hover = TRUE)

# Same "source" logic for the nested bootstrap already used in
# Batch, here applied to the cov/mat_boot fields ALREADY present in every
# snapshot of confronto_modelli() (same field names, no
# adaptation needed for ni_costruisci_fonte_k/ni_pesca_k).
mcni_dati_kT_con_fonti <- reactive({
  stato <- mcni_batch_valido()
  if (!stato$valido) return(NULL)
  in_celsius <- is.null(input$mcni_unita_temp) || input$mcni_unita_temp == "celsius"
  risultati <- lapply(stato$indici, function(i) {
    s <- stato$lista[[i]]
    T_val <- mcni_temperature_locali[[s$label]]
    if (!is.null(T_val) && !is.na(T_val) && in_celsius) T_val <- T_val + 273.15
    parametri_fit <- as.list(s$parametri)
    k_val <- parametri_fit[["k"]]
    if (is.null(T_val) || is.na(T_val) || is.null(k_val) || !is.finite(k_val)) return(NULL)
    list(T = T_val, fonte = ni_costruisci_fonte_k(s))
  })
  risultati <- Filter(Negate(is.null), risultati)
  if (length(risultati) < 3) return(NULL)
  risultati
})


# ---- Plot aesthetics (customizable) -----------------------------------------
# Integrated into the shared "Advanced Plot Aesthetics" modal, the same two
# new tabs added for Batch but with their own prefix mcni_ -
# same "direct input with fallback" scheme already used everywhere in the app.
observeEvent(input$mcni_btn_estetica_secondario, { apri_estetica("Model Comparison Non-Isothermal: Secondary Model") })
observeEvent(input$mcni_btn_estetica_previsione, { apri_estetica("Model Comparison Non-Isothermal: Predictions") })
observeEvent(input$mcni_btn_estetica_k, { apri_estetica("Model Comparison Non-Isothermal: Predictions") })

mcni_converti_temp_display <- function(valori_K) {
  if (isTRUE(input$mcni_mostra_celsius)) valori_K - 273.15 else valori_K
}
mcni_etichetta_temp <- function() {
  if (isTRUE(input$mcni_mostra_celsius)) "Temperature (\u00b0C)" else "Temperature (K)"
}

mcni_alpha_confidenza <- function() {
  livello <- input$mcni_livello_confidenza
  if (is.null(livello) || is.na(livello) || livello <= 0 || livello >= 100) livello <- 95
  1 - livello / 100
}

mcni_leggi_estetica <- function() {
  col_linea_sec <- if (is.null(input$mcni_ln_col_secondario)) "#18bc9c" else input$mcni_ln_col_secondario
  col_banda_sec <- if (is.null(input$mcni_col_banda_secondario) || input$mcni_col_banda_secondario == "curva") col_linea_sec else input$mcni_col_banda_secondario
  list(
    col_punti_sec = if (is.null(input$mcni_pt_col_secondario)) "#2c3e50" else input$mcni_pt_col_secondario,
    cex_punti_sec = if (is.null(input$mcni_cex_punti_secondario)) 1.1 else input$mcni_cex_punti_secondario,
    pch_punti_sec = as.numeric(if (is.null(input$mcni_pch_punti_secondario)) "16" else input$mcni_pch_punti_secondario),
    col_linea_sec = col_linea_sec,
    lwd_curva_sec = if (is.null(input$mcni_lwd_curva_secondario)) 2 else input$mcni_lwd_curva_secondario,
    col_banda_sec = col_banda_sec,
    alpha_banda_sec = if (is.null(input$mcni_alpha_banda_secondario)) 0.2 else input$mcni_alpha_banda_secondario,
    mostra_legenda_sec = if (is.null(input$mcni_mostra_legenda_sec)) TRUE else input$mcni_mostra_legenda_sec,
    cex_legenda_sec = if (is.null(input$mcni_cex_legenda_sec)) 0.85 else input$mcni_cex_legenda_sec,
    pos_legenda_sec = if (is.null(input$mcni_pos_legenda_sec)) "topright" else input$mcni_pos_legenda_sec,
    col_T = if (is.null(input$mcni_col_T)) "#e67e22" else input$mcni_col_T,
    col_C = if (is.null(input$mcni_col_C)) "#18bc9c" else input$mcni_col_C,
    col_k = if (is.null(input$mcni_col_k)) "#18bc9c" else input$mcni_col_k,
    lty_k = as.numeric(if (is.null(input$mcni_lty_k)) "1" else input$mcni_lty_k),
    lwd_k = if (is.null(input$mcni_lwd_k)) 2 else input$mcni_lwd_k,
    mostra_legenda_k = if (is.null(input$mcni_mostra_legenda_k)) TRUE else input$mcni_mostra_legenda_k,
    cex_legenda_k = if (is.null(input$mcni_cex_legenda_k)) 0.85 else input$mcni_cex_legenda_k,
    pos_legenda_k = if (is.null(input$mcni_pos_legenda_k)) "topright" else input$mcni_pos_legenda_k,
    col_soglia = if (is.null(input$mcni_col_soglia)) "#c0392b" else input$mcni_col_soglia,
    col_iso = if (is.null(input$mcni_col_iso)) "#e67e22" else input$mcni_col_iso,
    lty_iso = as.numeric(if (is.null(input$mcni_lty_iso)) "2" else input$mcni_lty_iso),
    lwd_linee = if (is.null(input$mcni_lwd_linee)) 2 else input$mcni_lwd_linee,
    lty_T = as.numeric(if (is.null(input$mcni_lty_T)) "1" else input$mcni_lty_T),
    alpha_T = if (is.null(input$mcni_alpha_T)) 1 else input$mcni_alpha_T,
    mostra_legenda_prev = if (is.null(input$mcni_mostra_legenda_prev)) TRUE else input$mcni_mostra_legenda_prev,
    cex_legenda_prev = if (is.null(input$mcni_cex_legenda_prev)) 0.85 else input$mcni_cex_legenda_prev,
    pos_legenda_prev = if (is.null(input$mcni_pos_legenda_prev)) "topright" else input$mcni_pos_legenda_prev,
    grid_on = if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  )
}


# ---- Choice and fitting of the secondary model k(T) ------------------------
output$mcni_guess_secondario_ui <- renderUI({
  dati <- mcni_dati_kT()
  tipo <- input$mcni_modello_secondario
  if (is.null(dati) || is.null(tipo) || tipo == "custom") return(NULL)
  guess_auto <- ni_guess_secondario(tipo, dati$T, dati$k)
  nomi_par <- NI_PARAMETRI_SECONDARIO[[tipo]]
  larghezza_colonna <- floor(12 / length(nomi_par))
  colonne <- lapply(nomi_par, function(p) {
    column(larghezza_colonna, numericInput(paste0("mcni_guess_sec_", p), paste0("Guess for ", p, ":"), value = signif(guess_auto[[p]], 4)))
  })
  fluidRow(colonne)
})

mcni_secondario_risultato <- reactiveVal(NULL)

# Clears the results of this module when the comparison list is
# emptied (same "reset on new fit" principle already applied to Batch)
# - a fit of the secondary model computed on the PREVIOUS fits
# should not remain visible on screen after clearing everything.
observeEvent(input$svuota_confronto, {
  mcni_secondario_risultato(NULL)
  mcni_previsione_risultato(NULL)
})

observeEvent(input$mcni_fit_secondario, {
  if (!isTRUE(mcni_batch_valido()$valido)) {
    showNotification("Requires at least 3 saved fits in the comparison, all using one of the order-based Reaction Kinetics models (Zero/First/Second/n-Order).", type = "error", duration = 8)
    return()
  }
  dati <- mcni_dati_kT()
  tipo_selezionato <- input$mcni_modello_secondario
  if (is.null(dati) || is.null(tipo_selezionato)) {
    showNotification("Enter at least 3 valid temperatures above first.", type = "error")
    return()
  }
  # Same technique already validated in Batch (see non_isotermico.R): the
  # custom formula is safely validated HERE, then its own
  # text becomes the value of "tipo" used for the rest of the function.
  if (tipo_selezionato == "custom") {
    formula_testo <- input$mcni_secondario_formula_custom
    guess_testo <- input$mcni_secondario_guess_custom
    if (is.null(formula_testo) || nchar(trimws(formula_testo)) == 0 || is.null(guess_testo) || nchar(trimws(guess_testo)) == 0) {
      showNotification("Enter both a custom formula and its initial guesses first.", type = "error")
      return()
    }
    guesses_custom <- tryCatch(parse_guesses(guess_testo), error = function(e) NULL)
    if (is.null(guesses_custom) || length(guesses_custom$guess) == 0) {
      showNotification("Could not parse the initial guesses \u2014 check the syntax (e.g. \"A=1e10[0,], Ea=50000[0,]\").", type = "error")
      return()
    }
    nomi_par <- names(guesses_custom$guess)
    if (!ni_valida_formula_generica(formula_testo, nomi_par, "T")) {
      showNotification("Invalid or unsafe custom formula \u2014 only basic math operations and the parameters/T you declared are allowed.", type = "error", duration = 8)
      return()
    }
    tipo <- formula_testo
    guess_vettore <- guesses_custom$guess
  } else {
    tipo <- tipo_selezionato
    nomi_par <- NI_PARAMETRI_SECONDARIO[[tipo]]
    guess_letti <- sapply(nomi_par, function(p) input[[paste0("mcni_guess_sec_", p)]])
    if (any(sapply(guess_letti, function(v) is.null(v) || is.na(v)))) {
      showNotification("Enter an initial guess for every parameter first.", type = "error")
      return()
    }
    guess_vettore <- setNames(as.numeric(guess_letti), nomi_par)
  }
  if (length(dati$k) <= length(nomi_par)) {
    showNotification(paste0("This secondary model has ", length(nomi_par), " parameters \u2014 at least ", length(nomi_par) + 1, " fits with a known temperature are needed (found ", length(dati$k), ")."), type = "error", duration = 8)
    return()
  }
  
  obiettivo <- function(par) {
    par_nominati <- setNames(as.list(par), nomi_par)
    k_pred <- ni_modello_secondario(tipo, dati$T, par_nominati)
    calcola_loss_generica(k_pred, dati$k, "mse")
  }
  fit <- fit_modello_secondario(function(p) ni_modello_secondario(tipo, dati$T, setNames(as.list(p), nomi_par)), dati$k, setNames(as.numeric(guess_vettore), nomi_par))
  if (is.null(fit) || !isTRUE(fit$convergence == 0)) {
    showNotification("The secondary model fit did not converge \u2014 try adjusting the initial guesses.", type = "error", duration = 8)
    return()
  }
  par_finali <- setNames(as.list(fit$par), nomi_par)
  k_pred_finale <- ni_modello_secondario(tipo, dati$T, par_finali)
  sse <- sum((dati$k - k_pred_finale)^2)
  sst <- sum((dati$k - mean(dati$k))^2)
  r2 <- if (sst > 0) 1 - sse / sst else NA
  
  df_residuo <- length(dati$k) - length(nomi_par)
  mse_vero <- sse / df_residuo
  # Covariance from the Jacobian of the model, s^2 (J^T J)^-1, not from the numerical
  # Hessian of optim. Previously a negative variance (invalid covariance)
  # was turned into a standard error equal to zero (pmax(..., 0)),
  # misleading: now in that case the standard error is "not available".
  matrice_cov <- covarianza_minimi_quadrati(function(p) ni_modello_secondario(tipo, dati$T, setNames(as.list(p), nomi_par)),
                                            setNames(fit$par, nomi_par), sse, df_residuo)
  se_par <- if (!is.null(matrice_cov)) sqrt(diag(matrice_cov)) else rep(NA_real_, length(nomi_par))
  t_crit <- qt(1 - mcni_alpha_confidenza() / 2, df_residuo)
  
  mat_boot_secondario <- NULL
  if (isTRUE(input$mcni_usa_monte_carlo)) {
    dati_con_fonti <- mcni_dati_kT_con_fonti()
    if (!is.null(dati_con_fonti)) {
      n_boot_sec <- if (is.null(input$mcni_n_boot_secondario) || is.na(input$mcni_n_boot_secondario)) 200 else min(max(round(input$mcni_n_boot_secondario), 20), 2000)
      T_fissi <- sapply(dati_con_fonti, function(x) x$T)
      mat_boot_secondario <- matrix(NA_real_, n_boot_sec, length(nomi_par), dimnames = list(NULL, nomi_par))
      withProgress(message = "Running nested bootstrap...", value = 0, {
        for (b in seq_len(n_boot_sec)) {
          if (b %% 10 == 0 || b == n_boot_sec) incProgress(10 / n_boot_sec, detail = paste0("Iteration ", b, " of ", n_boot_sec))
          k_ricampionati <- sapply(dati_con_fonti, function(x) ni_pesca_k(x$fonte))
          obiettivo_b <- function(par) {
            par_nominati <- setNames(as.list(par), nomi_par)
            k_pred <- ni_modello_secondario(tipo, T_fissi, par_nominati)
            calcola_loss_generica(k_pred, k_ricampionati, "mse")
          }
          fit_b <- fit_modello_secondario(function(p) ni_modello_secondario(tipo, T_fissi, setNames(as.list(p), nomi_par)), k_ricampionati, fit$par)
          if (!is.null(fit_b) && isTRUE(fit_b$convergence == 0)) mat_boot_secondario[b, ] <- fit_b$par
        }
      })
      n_boot_ok <- sum(!is.na(mat_boot_secondario[, 1]))
      if (n_boot_ok < 10) {
        mat_boot_secondario <- NULL
        showNotification("Nested bootstrap did not converge often enough \u2014 falling back to the Jacobian-based confidence intervals.", type = "warning", duration = 8)
      }
    }
  }
  
  rmse <- sqrt(mse_vero)
  cv_rmse <- if (mean(dati$k) != 0) (rmse / mean(dati$k)) * 100 else NA
  r2_adj <- if (!is.na(r2) && df_residuo > 0) 1 - (1 - r2) * (length(dati$k) - 1) / df_residuo else NA
  n_oss <- length(dati$k)
  p_par <- length(nomi_par)
  # Secondary model: always least squares (Levenberg-Marquardt).
  ic_sec <- criteri_informazione(sse, n_oss, p_par, "mse")
  aic_sec <- ic_sec$aic; aicc_sec <- ic_sec$aicc; bic_sec <- ic_sec$bic
  
  mcni_secondario_risultato(list(tipo = tipo, par = par_finali, se = setNames(se_par, nomi_par), r2 = r2, r2_adj = r2_adj,
                                  sse = sse, mse = mse_vero, rmse = rmse, cv_rmse = cv_rmse, n_obs = n_oss,
                                  aic = aic_sec, aicc = aicc_sec, bic = bic_sec,
                                  df_residuo = df_residuo, t_crit = t_crit, matrice_cov = matrice_cov,
                                  dati_kT = dati, n_ordine_medio = mean(dati$n_par, na.rm = TRUE),
                                  mat_boot = mat_boot_secondario))
  showNotification("Secondary model fitted successfully.", type = "message")
})

output$mcni_riepilogo_secondario <- renderTable({
  ris <- mcni_secondario_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the secondary model fit above."))
  etichetta_metodo <- if (!is.null(ris$mat_boot)) paste0("Nested Bootstrap (", sum(!is.na(ris$mat_boot[, 1])), ")") else "Linear (Jacobian)"
  data.frame(
    Status = "Success", `Error Method` = etichetta_metodo, `N Obs` = as.character(ris$n_obs), DF = as.character(ris$df_residuo),
    RMSE = as.character(signif(ris$rmse, 5)), `CV(RMSE)%` = as.character(signif(ris$cv_rmse, 5)),
    R2 = as.character(round(ris$r2, 5)), `R2 Adj` = as.character(round(ris$r2_adj, 5)),
    SSE = as.character(signif(ris$sse, 6)), MSE = as.character(signif(ris$mse, 6)),
    check.names = FALSE
  )
}, striped = TRUE, hover = TRUE)

output$mcni_confronto_modelli_secondario <- renderTable({
  ris <- mcni_secondario_risultato()
  if (is.null(ris)) return(NULL)
  data.frame(
    AIC = as.character(signif(ris$aic, 6)),
    AICc = as.character(signif(ris$aicc, 6)),
    BIC = as.character(signif(ris$bic, 6)),
    check.names = FALSE
  )
}, striped = TRUE, hover = TRUE)

output$mcni_tabella_secondario <- renderTable({
  ris <- mcni_secondario_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the secondary model fit above."))
  valori <- unlist(ris$par)
  alpha <- mcni_alpha_confidenza()
  livello_testo <- paste0(round(100 * (1 - alpha), 1), "%")
  if (!is.null(ris$mat_boot)) {
    se <- apply(ris$mat_boot, 2, sd, na.rm = TRUE)[names(valori)]
    ci_bassa <- apply(ris$mat_boot, 2, quantile, probs = alpha / 2, na.rm = TRUE)[names(valori)]
    ci_alta <- apply(ris$mat_boot, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)[names(valori)]
  } else {
    se <- ris$se
    t_crit_live <- qt(1 - alpha / 2, ris$df_residuo)
    ci_bassa <- valori - t_crit_live * se
    ci_alta <- valori + t_crit_live * se
  }
  df_out <- data.frame(Parameter = names(valori), Value = as.character(signif(valori, 5)),
             `Std. Error` = as.character(signif(se, 4)),
             `CI Lower` = as.character(signif(ci_bassa, 5)),
             `CI Upper` = as.character(signif(ci_alta, 5)),
             check.names = FALSE)
  colnames(df_out)[4] <- paste0(livello_testo, " CI Lower")
  colnames(df_out)[5] <- paste0(livello_testo, " CI Upper")
  df_out
}, striped = TRUE, hover = TRUE, na = "")

disegna_mcni_secondario <- function() {
  ris <- mcni_secondario_risultato()
  if (is.null(ris)) {
    plot.new()
    text(0.5, 0.5, "Fit the secondary model above to see the plot here.", col = "#7f8c8d")
    return(invisible(NULL))
  }
  dati <- ris$dati_kT
  T_grid <- seq(min(dati$T) * 0.98, max(dati$T) * 1.02, length.out = 200)
  k_grid <- ni_modello_secondario(ris$tipo, T_grid, ris$par)
  
  stato <- mcni_batch_valido()
  se_k_punti <- sapply(dati$nome, function(n) {
    idx <- which(sapply(stato$lista, function(s) s$label) == n)
    if (length(idx) == 0) return(NA_real_)
    s <- stato$lista[[idx[1]]]
    if (!is.null(s$cov) && "k" %in% rownames(s$cov)) {
      valore <- suppressWarnings(sqrt(s$cov["k", "k"]))
      if (is.finite(valore)) return(valore)
    }
    NA_real_
  })
  
  # Nested bootstrap with fewer than 10 valid iterations: linear band
  # (previously the plot showed no band).
  if (ni_bootstrap_secondario_valido(ris)) {
    alpha <- mcni_alpha_confidenza()
    righe_valide <- which(apply(ris$mat_boot, 1, function(r) all(is.finite(r))))
    if (length(righe_valide) >= 10) {
      mat_curve_boot <- matrix(NA_real_, length(righe_valide), length(T_grid))
      for (i in seq_along(righe_valide)) {
        par_b <- setNames(as.list(ris$mat_boot[righe_valide[i], ]), colnames(ris$mat_boot))
        mat_curve_boot[i, ] <- ni_modello_secondario(ris$tipo, T_grid, par_b)
      }
      if (isTRUE(input$mcni_mostra_banda_confidenza)) {
        banda_inf <- pmax(apply(mat_curve_boot, 2, quantile, probs = alpha / 2, na.rm = TRUE), 0)
        banda_sup <- apply(mat_curve_boot, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      } else {
        banda_sup <- banda_inf <- NULL
      }
      if (isTRUE(input$mcni_mostra_banda_predizione) && is.finite(ris$mse) && ris$mse > 0) {
        rumore_pred <- matrix(rnorm(length(mat_curve_boot), mean = 0, sd = sqrt(ris$mse)), nrow = nrow(mat_curve_boot))
        mat_pred_boot <- mat_curve_boot + rumore_pred
        pi_inf <- pmax(apply(mat_pred_boot, 2, quantile, probs = alpha / 2, na.rm = TRUE), 0)
        pi_sup <- apply(mat_pred_boot, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      } else {
        pi_sup <- pi_inf <- NULL
      }
    } else {
      banda_sup <- banda_inf <- pi_sup <- pi_inf <- NULL
    }
  } else if (!is.null(ris$matrice_cov)) {
    alpha <- mcni_alpha_confidenza()
    t_crit_live <- qt(1 - alpha / 2, ris$df_residuo)
    var_par_grid <- sapply(T_grid, function(T_val) {
      g <- ni_gradiente_numerico_secondario(ris$tipo, T_val, ris$par)
      max(as.numeric(t(g) %*% ris$matrice_cov %*% g), 0)
    })
    se_k_grid <- sqrt(var_par_grid)
    if (isTRUE(input$mcni_mostra_banda_confidenza)) {
      # Pointwise t multiplier, or simultaneous one of Bates & Watts (1988) if requested.
      m_ci <- if (isTRUE(input$mcni_banda_simultanea)) moltiplicatore_banda(ris$df_residuo, length(ris$par), 100 * (1 - alpha), TRUE) else t_crit_live
      if (!is.finite(m_ci)) m_ci <- t_crit_live
      banda_sup <- k_grid + m_ci * se_k_grid
      banda_inf <- pmax(k_grid - m_ci * se_k_grid, 0)
    } else {
      banda_sup <- banda_inf <- NULL
    }
    if (isTRUE(input$mcni_mostra_banda_predizione)) {
      se_pred_grid <- sqrt(var_par_grid + ris$mse)
      pi_sup <- k_grid + t_crit_live * se_pred_grid
      pi_inf <- pmax(k_grid - t_crit_live * se_pred_grid, 0)
    } else {
      pi_sup <- pi_inf <- NULL
    }
  } else {
    banda_sup <- banda_inf <- pi_sup <- pi_inf <- NULL
  }
  
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 1))
  est <- mcni_leggi_estetica()
  dati_T_vis <- mcni_converti_temp_display(dati$T)
  T_grid_vis <- mcni_converti_temp_display(T_grid)
  plot(dati_T_vis, dati$k, type = "n",
       xlim = range(T_grid_vis), ylim = range(c(dati$k - se_k_punti, dati$k + se_k_punti, banda_inf, banda_sup, pi_inf, pi_sup, k_grid), na.rm = TRUE),
       xlab = mcni_etichetta_temp(), ylab = "Rate constant k", main = if (input$mostra_titolo) "Secondary Model: k(T)" else "")
  if (est$grid_on) grid()
  if (!is.null(pi_sup)) {
    polygon(c(T_grid_vis, rev(T_grid_vis)), c(pi_sup, rev(pi_inf)), col = adjustcolor(est$col_banda_sec, alpha.f = est$alpha_banda_sec * 0.6), border = NA)
  }
  if (!is.null(banda_sup)) {
    polygon(c(T_grid_vis, rev(T_grid_vis)), c(banda_sup, rev(banda_inf)), col = adjustcolor(est$col_banda_sec, alpha.f = est$alpha_banda_sec), border = NA)
  }
  lines(T_grid_vis, k_grid, col = est$col_linea_sec, lwd = est$lwd_curva_sec)
  idx_con_errore <- is.finite(se_k_punti)
  if (any(idx_con_errore)) {
    arrows(dati_T_vis[idx_con_errore], pmax(0, dati$k[idx_con_errore] - se_k_punti[idx_con_errore]),
           dati_T_vis[idx_con_errore], dati$k[idx_con_errore] + se_k_punti[idx_con_errore],
           angle = 90, code = 3, length = 0.05, col = est$col_punti_sec, lwd = 1.3)
  }
  points(dati_T_vis, dati$k, pch = est$pch_punti_sec, col = est$col_punti_sec, cex = est$cex_punti_sec)
  if (est$mostra_legenda_sec) {
    livello_testo <- paste0(round(100 * (1 - mcni_alpha_confidenza()), 1), "%")
    voci <- c("Observed k (\u00b1 SE)", "Fitted secondary model")
    colori_legenda <- c(est$col_punti_sec, est$col_linea_sec)
    pch_legenda <- c(est$pch_punti_sec, NA); lty_legenda <- c(NA, 1); lwd_legenda <- c(NA, est$lwd_curva_sec)
    if (!is.null(banda_sup)) {
      voci <- c(voci, paste0(livello_testo, " Confidence Band"))
      colori_legenda <- c(colori_legenda, adjustcolor(est$col_banda_sec, alpha.f = min(est$alpha_banda_sec * 1.5, 1)))
      pch_legenda <- c(pch_legenda, 15); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA)
    }
    if (!is.null(pi_sup)) {
      voci <- c(voci, paste0(livello_testo, " Prediction Band"))
      colori_legenda <- c(colori_legenda, adjustcolor(est$col_banda_sec, alpha.f = est$alpha_banda_sec * 0.75))
      pch_legenda <- c(pch_legenda, 15); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA)
    }
    legend(est$pos_legenda_sec, legend = voci, col = colori_legenda, pch = pch_legenda, lty = lty_legenda, lwd = lwd_legenda,
           bty = "o", bg = "white", cex = est$cex_legenda_sec)
  }
}

output$mcni_grafico_secondario <- renderPlot({
  disegna_mcni_secondario()
})
output$scarica_mcni_secondario <- downloadHandler(
  filename = function() paste0("secondary_model_mc.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_mcni_secondario()
    dev.off()
  }
)


# ---- Temperature profile T(t) ----------------------------------------------
output$mcni_parametri_profilo_ui <- renderUI({
  tipo <- input$mcni_tipo_profilo
  if (is.null(tipo) || tipo == "dati_grezzi" || tipo == "custom") return(NULL)
  if (tipo == "abuso_termico") {
    # Same pattern already used in mcni_A0_C_crit_ui for "Max Time to
    # Simulate" (x_dati is the name of the field here, x in Batch).
    stato <- mcni_batch_valido()
    valore_default_Dtot <- 240
    if (isTRUE(stato$valido)) {
      tutti_x_max <- sapply(stato$lista[stato$indici], function(s) if (!is.null(s$x_dati)) max(s$x_dati, na.rm = TRUE) else NA_real_)
      tutti_x_max <- tutti_x_max[is.finite(tutti_x_max)]
      if (length(tutti_x_max) > 0) valore_default_Dtot <- round(max(tutti_x_max), 4)
    }
    valore_Dtot <- isolate(input$mcni_profilo_Dtot)
    if (is.null(valore_Dtot) || is.na(valore_Dtot)) valore_Dtot <- valore_default_Dtot
    return(tagList(
      fluidRow(
        column(3, numericInput("mcni_profilo_T0", "Baseline Temperature (T0):", value = 277)),
        column(3, numericInput("mcni_profilo_Dtot", "Total Duration:", value = valore_Dtot, min = 0.001)),
        column(3, numericInput("mcni_profilo_n", "Number of Abuses:", value = 3, min = 1, max = 50, step = 1)),
        column(3, numericInput("mcni_profilo_dur", "Duration of Each Abuse:", value = 2, min = 0.001))
      ),
      fluidRow(
        column(4, numericInput("mcni_profilo_amp", "Amplitude of Each Abuse:", value = 5, min = 0)),
        column(4, selectInput("mcni_abuso_direzione", "Direction:", choices = c("Positive (warmer)" = "positivo", "Negative (colder)" = "negativo"))),
        column(4, selectInput("mcni_abuso_forma", "Shape:", choices = c("Sharp Jump" = "salto", "Fast Ramp" = "rampa_veloce", "Soft Ramp" = "rampa_morbida")))
      ),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
        "Baseline Temperature is the steady value between abuses. The abuses are spread evenly across the Total Duration (never exactly at the very start or end), each rising from and returning to the baseline. Sharp Jump is an instant rectangular step; Fast Ramp rises/falls quickly then holds; Soft Ramp rises and falls smoothly over the whole abuse, with no flat plateau. This profile is fully deterministic (no randomness) \u2014 running it again with the same settings always gives the exact same curve.")
    ))
  }
  nomi_par <- NI_PARAMETRI_PROFILO[[tipo]]
  valori_default <- as.list(switch(tipo,
    "lineare" = c(T0 = 277, a = 0.1),
    "quadratico" = c(T0 = 277, a = 0.05, b = 0.001),
    "sinusoidale" = c(Tm = 283, a = 6, L = 24),
    "esponenziale" = c(Th = 293, T0 = 277, J = 0.05)
  ))
  larghezza_colonna <- floor(12 / length(nomi_par))
  colonne <- lapply(nomi_par, function(p) {
    column(larghezza_colonna, numericInput(paste0("mcni_profilo_", p), paste0(p, ":"), value = valori_default[[p]]))
  })
  fluidRow(colonne)
})


# ---- Final prediction --------------------------------------------------------
output$mcni_A0_C_crit_ui <- renderUI({
  stato <- mcni_batch_valido()
  valore_default_A0 <- 10
  valore_default_t_max <- 500
  if (isTRUE(stato$valido)) {
    tutti_A0 <- sapply(stato$lista[stato$indici], function(s) as.list(s$parametri)[["A0"]])
    tutti_A0 <- unlist(tutti_A0[!sapply(tutti_A0, is.null)])
    tutti_A0 <- tutti_A0[is.finite(tutti_A0)]
    if (length(tutti_A0) > 0) valore_default_A0 <- round(mean(tutti_A0), 4)
    
    # Same principle already applied to Batch: maximum raw X among the qualifying
    # fits as a reasonable starting point for Max Time -
    # x_dati is the name of the field here (instead of x in Batch).
    tutti_x_max <- sapply(stato$lista[stato$indici], function(s) if (!is.null(s$x_dati)) max(s$x_dati, na.rm = TRUE) else NA_real_)
    tutti_x_max <- tutti_x_max[is.finite(tutti_x_max)]
    if (length(tutti_x_max) > 0) valore_default_t_max <- round(max(tutti_x_max), 4)
  }
  valore_t_max <- isolate(input$mcni_t_max)
  if (is.null(valore_t_max) || is.na(valore_t_max)) valore_t_max <- valore_default_t_max
  valore_risoluzione <- isolate(input$mcni_risoluzione_griglia)
  if (is.null(valore_risoluzione) || is.na(valore_risoluzione)) valore_risoluzione <- 1000
  fluidRow(
    column(3, numericInput("mcni_A0", "Starting Quantity (A0):", value = valore_default_A0)),
    column(3, numericInput("mcni_C_crit", "Critical Threshold:", value = round(valore_default_A0 * 0.75, 4))),
    column(3, numericInput("mcni_t_max", "Max Time to Simulate:", value = valore_t_max, min = 1)),
    column(3, numericInput("mcni_risoluzione_griglia", "Integration Grid Resolution:", value = valore_risoluzione, min = 20, max = 5000, step = 10))
  )
})

output$mcni_campo_n_ui <- renderUI({
  stato <- mcni_batch_valido()
  if (!isTRUE(stato$valido) || !identical(stato$ordine, "n")) return(NULL)
  ris_sec <- mcni_secondario_risultato()
  valore_default_n <- if (!is.null(ris_sec) && !is.na(ris_sec$n_ordine_medio)) round(ris_sec$n_ordine_medio, 4) else NA_real_
  valore_n <- isolate(input$mcni_n_ordine)
  if (is.null(valore_n) || is.na(valore_n)) valore_n <- if (!is.na(valore_default_n)) valore_default_n else 1.5
  fluidRow(style = "margin-top: 4px;",
    column(4, numericInput("mcni_n_ordine", "Reaction Order (n):", value = valore_n, step = 0.01)),
    column(8, p(style = "font-size: 11px; color: #7f8c8d; margin-top: 30px;",
                if (!is.na(valore_default_n))
                  paste0("Pre-filled with the average n already fitted across the accumulated fits (", valore_default_n, "). Editable, but changing it here means this prediction will no longer exactly match the primary fits.")
                else
                  "Fit the secondary model above first to pre-fill this with the average n across accumulated fits \u2014 for now, enter it manually."))
  )
})

output$mcni_campo_isotermo_ui <- renderUI({
  stato <- mcni_batch_valido()
  if (!isTRUE(stato$valido)) return(NULL)
  valore_T_iso <- isolate(input$mcni_T_iso)
  if (is.null(valore_T_iso) || is.na(valore_T_iso)) valore_T_iso <- 277
  tagList(
    checkboxInput("mcni_confronta_isotermo", "Compare with Isothermal Storage", value = if (is.null(isolate(input$mcni_confronta_isotermo))) FALSE else isolate(input$mcni_confronta_isotermo)),
    conditionalPanel(
      condition = "input.mcni_confronta_isotermo == true",
      fluidRow(
        column(4, numericInput("mcni_T_iso", "Isothermal Comparison Temperature (K):", value = valore_T_iso, step = 0.5))
      ),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Compares the critical time predicted under the chosen temperature profile above against what would happen if the product had instead been stored at this single fixed temperature the whole time \u2014 same secondary model, same starting quantity and threshold, just a constant temperature instead of the profile.")
    )
  )
})

mcni_previsione_risultato <- reactiveVal(NULL)

observeEvent(input$mcni_lancia_previsione, {
  ris_sec <- mcni_secondario_risultato()
  stato <- mcni_batch_valido()
  if (is.null(ris_sec) || !stato$valido) {
    showNotification("Fit the secondary model first.", type = "error")
    return()
  }
  n_ordine_usato <- NULL
  if (identical(stato$ordine, "n")) {
    n_ordine_usato <- input$mcni_n_ordine
    if (is.null(n_ordine_usato) || is.na(n_ordine_usato)) n_ordine_usato <- ris_sec$n_ordine_medio
    if (is.null(n_ordine_usato) || is.na(n_ordine_usato)) {
      showNotification("Could not determine the reaction order 'n' \u2014 enter it manually above.", type = "error")
      return()
    }
  }
  
  tipo_profilo <- input$mcni_tipo_profilo
  funzione_T <- if (tipo_profilo == "dati_grezzi") {
    file_dati <- input$mcni_file_datalogger
    if (is.null(file_dati)) { showNotification("Upload a datalogger file first.", type = "error"); return() }
    df_log <- tryCatch(leggi_file_generico(file_dati$datapath, file_dati$name), error = function(e) NULL)
    if (is.null(df_log) || ncol(df_log) < 2) { showNotification("Could not read the datalogger file (expects 2 columns: time, temperature).", type = "error"); return() }
    colnames(df_log)[1:2] <- c("tempo", "temperatura")
    ni_costruisci_profilo_T("dati_grezzi", NULL, df_log)
  } else if (tipo_profilo == "custom") {
    # Same technique already validated in Batch (see non_isotermico.R).
    formula_testo <- input$mcni_profilo_formula_custom
    guess_testo <- input$mcni_profilo_guess_custom
    if (is.null(formula_testo) || nchar(trimws(formula_testo)) == 0 || is.null(guess_testo) || nchar(trimws(guess_testo)) == 0) {
      showNotification("Enter both a custom formula and its parameter values first.", type = "error")
      return()
    }
    guesses_custom <- tryCatch(parse_guesses(guess_testo), error = function(e) NULL)
    if (is.null(guesses_custom) || length(guesses_custom$guess) == 0) {
      showNotification("Could not parse the parameter values \u2014 check the syntax (e.g. \"T0=277, a=0.1\").", type = "error")
      return()
    }
    nomi_par_custom <- names(guesses_custom$guess)
    if (!ni_valida_formula_generica(formula_testo, nomi_par_custom, "t")) {
      showNotification("Invalid or unsafe custom formula \u2014 only basic math operations and the parameters/t you declared are allowed.", type = "error", duration = 8)
      return()
    }
    ni_costruisci_profilo_T(formula_testo, as.list(guesses_custom$guess))
  } else {
    nomi_par <- NI_PARAMETRI_PROFILO[[tipo_profilo]]
    valori_par <- sapply(nomi_par, function(p) input[[paste0("mcni_profilo_", p)]])
    if (any(sapply(valori_par, function(v) is.null(v) || is.na(v)))) { showNotification("Enter every temperature profile parameter first.", type = "error"); return() }
    par_nominati <- setNames(as.list(as.numeric(valori_par)), nomi_par)
    if (tipo_profilo == "abuso_termico") {
      if (is.null(input$mcni_abuso_direzione) || is.null(input$mcni_abuso_forma)) { showNotification("Choose a Direction and Shape for the abuses first.", type = "error"); return() }
      par_nominati$segno <- if (input$mcni_abuso_direzione == "positivo") 1 else -1
      par_nominati$frazione_bordo <- switch(input$mcni_abuso_forma, "salto" = 0, "rampa_veloce" = 0.15, "rampa_morbida" = 0.5)
      if (par_nominati$dur > par_nominati$Dtot / (par_nominati$n + 1)) {
        showNotification("The abuses would overlap (or run past the start/end) with these settings \u2014 reduce the abuse duration or count, or increase the Total Duration.", type = "error", duration = 8)
        return()
      }
    }
    ni_costruisci_profilo_T(tipo_profilo, par_nominati)
  }
  if (is.null(funzione_T)) { showNotification("Could not build the temperature profile.", type = "error"); return() }
  
  A0 <- input$mcni_A0; C_crit <- input$mcni_C_crit; t_max <- input$mcni_t_max
  if (any(sapply(list(A0, C_crit, t_max), function(v) is.null(v) || is.na(v)))) {
    showNotification("Enter Starting Quantity, Critical Threshold, and Max Time to Simulate.", type = "error")
    return()
  }
  if (C_crit >= A0) { showNotification("Critical Threshold must be lower than Starting Quantity (this module assumes a decaying quantity).", type = "error"); return() }
  n_punti_griglia <- if (is.null(input$mcni_risoluzione_griglia) || is.na(input$mcni_risoluzione_griglia)) 1000 else max(round(input$mcni_risoluzione_griglia), 20)
  
  integrazione <- ni_integra_k_cumulativo(funzione_T, ris_sec$tipo, ris_sec$par, t_max, n_punti = n_punti_griglia)
  curva_C <- ni_quantita_da_K(stato$ordine, A0, integrazione$K, n_ordine_usato)
  
  K_soglia_val <- ni_K_soglia(stato$ordine, A0, C_crit, n_ordine_usato)
  if (K_soglia_val > max(integrazione$K)) {
    t_soglia <- NA
  } else {
    t_soglia <- approx(integrazione$K, integrazione$t, xout = K_soglia_val)$y
  }
  
  confronta_isotermo <- isTRUE(input$mcni_confronta_isotermo)
  T_iso <- input$mcni_T_iso
  if (confronta_isotermo && (is.null(T_iso) || is.na(T_iso))) {
    showNotification("Enter the Isothermal Comparison Temperature first.", type = "error")
    return()
  }
  t_iso <- NA_real_
  curva_C_iso <- NULL
  if (confronta_isotermo) {
    k_iso_puntuale <- ni_modello_secondario(ris_sec$tipo, T_iso, ris_sec$par)
    if (is.finite(k_iso_puntuale) && k_iso_puntuale > 0) {
      t_iso <- K_soglia_val / k_iso_puntuale
      curva_C_iso <- ni_quantita_da_K(stato$ordine, A0, k_iso_puntuale * integrazione$t, n_ordine_usato)
    }
  }
  
  t_soglia_boot <- NULL
  t_iso_boot <- NULL
  banda_C_inf <- banda_C_sup <- NULL
  banda_k_inf <- banda_k_sup <- NULL
  if (!is.null(ris_sec$mat_boot) && !is.na(t_soglia)) {
    n_righe_boot <- nrow(ris_sec$mat_boot)
    t_griglia_boot <- seq(0, t_max, length.out = 200)
    mat_curve_C_boot <- matrix(NA_real_, n_righe_boot, length(t_griglia_boot))
    mat_curve_k_boot <- matrix(NA_real_, n_righe_boot, length(t_griglia_boot))
    withProgress(message = "Propagating uncertainty into the critical-time prediction...", value = 0, {
      for (b in seq_len(n_righe_boot)) {
        if (b %% 10 == 0 || b == n_righe_boot) incProgress(10 / n_righe_boot, detail = paste0("Iteration ", b, " of ", n_righe_boot))
        par_b <- ris_sec$mat_boot[b, ]
        if (any(!is.finite(par_b))) next
        par_b_lista <- setNames(as.list(par_b), colnames(ris_sec$mat_boot))
        integrazione_b <- ni_integra_k_cumulativo(funzione_T, ris_sec$tipo, par_b_lista, t_max, n_punti = n_punti_griglia)
        if (K_soglia_val <= max(integrazione_b$K)) {
          t_soglia_boot[b] <- approx(integrazione_b$K, integrazione_b$t, xout = K_soglia_val)$y
        }
        K_su_griglia_comune <- approx(integrazione_b$t, integrazione_b$K, xout = t_griglia_boot, rule = 2)$y
        mat_curve_C_boot[b, ] <- ni_quantita_da_K(stato$ordine, A0, K_su_griglia_comune, n_ordine_usato)
        mat_curve_k_boot[b, ] <- approx(integrazione_b$t, integrazione_b$k, xout = t_griglia_boot, rule = 2)$y
        if (confronta_isotermo) {
          k_iso_b <- ni_modello_secondario(ris_sec$tipo, T_iso, par_b_lista)
          if (is.finite(k_iso_b) && k_iso_b > 0) t_iso_boot[b] <- K_soglia_val / k_iso_b
        }
      }
    })
    if (is.null(t_soglia_boot)) t_soglia_boot <- rep(NA_real_, n_righe_boot)
    t_soglia_boot <- t_soglia_boot[is.finite(t_soglia_boot)]
    if (length(t_soglia_boot) < 10) t_soglia_boot <- NULL
    if (confronta_isotermo) {
      if (is.null(t_iso_boot)) t_iso_boot <- rep(NA_real_, n_righe_boot)
      t_iso_boot <- t_iso_boot[is.finite(t_iso_boot)]
      if (length(t_iso_boot) < 10) t_iso_boot <- NULL
    }
    
    righe_valide <- rowSums(!is.finite(mat_curve_C_boot)) == 0
    if (sum(righe_valide) >= 10) {
      alpha <- mcni_alpha_confidenza()
      banda_C_inf <- apply(mat_curve_C_boot[righe_valide, , drop = FALSE], 2, quantile, probs = alpha / 2, na.rm = TRUE)
      banda_C_sup <- apply(mat_curve_C_boot[righe_valide, , drop = FALSE], 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      banda_k_inf <- apply(mat_curve_k_boot[righe_valide, , drop = FALSE], 2, quantile, probs = alpha / 2, na.rm = TRUE)
      banda_k_sup <- apply(mat_curve_k_boot[righe_valide, , drop = FALSE], 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
    }
  }
  
  confronto_isotermo <- NULL
  if (confronta_isotermo && !is.na(t_iso)) {
    if (!is.null(t_soglia_boot) && !is.null(t_iso_boot)) {
      alpha <- mcni_alpha_confidenza()
      diff_boot <- as.vector(outer(t_soglia_boot, t_iso_boot, "-"))
      ci_diff <- quantile(diff_boot, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
      prop_sotto <- mean(diff_boot <= 0)
      prop_sopra <- mean(diff_boot >= 0)
      p_value <- min(1, 2 * min(prop_sotto, prop_sopra))
      sig <- as.character(symnum(p_value, corr = FALSE, na = FALSE,
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                  symbols = c("***", "**", "*", ".", "ns")))
      confronto_isotermo <- list(
        diff = t_soglia - t_iso, ci_diff_inf = unname(ci_diff[1]), ci_diff_sup = unname(ci_diff[2]),
        p_value = p_value, significativita = sig,
        metodo = sprintf("Empirical Bootstrap (%d pairwise differences)", length(diff_boot))
      )
    } else {
      confronto_isotermo <- list(diff = t_soglia - t_iso, metodo = "Point estimate only (not enough Monte Carlo iterations for a statistical test)")
    }
  }
  
  mcni_previsione_risultato(list(t = integrazione$t, T = integrazione$T, k = integrazione$k, C = curva_C, A0 = A0, C_crit = C_crit, t_soglia = t_soglia, t_soglia_boot = t_soglia_boot,
                                  T_iso = T_iso, t_iso = t_iso, t_iso_boot = t_iso_boot, C_iso = curva_C_iso, confronto_isotermo = confronto_isotermo,
                                  t_griglia_banda = if (!is.null(banda_C_inf)) seq(0, t_max, length.out = 200) else NULL,
                                  banda_k_inf = banda_k_inf, banda_k_sup = banda_k_sup,
                                  banda_C_inf = banda_C_inf, banda_C_sup = banda_C_sup))
})

output$mcni_risultato_testo <- renderUI({
  ris <- mcni_previsione_risultato()
  if (is.null(ris)) return(NULL)
  if (is.na(ris$t_soglia)) {
    return(p(style = "font-size: 14px; color: #c0392b; font-weight: 500;",
              "The critical threshold is not reached within the simulated time range \u2014 try increasing Max Time to Simulate."))
  }
  ci_testo <- if (!is.null(ris$t_soglia_boot)) {
    alpha <- mcni_alpha_confidenza()
    ci_bassa <- signif(quantile(ris$t_soglia_boot, alpha / 2), 4)
    ci_alta <- signif(quantile(ris$t_soglia_boot, 1 - alpha / 2), 4)
    paste0(" (", round(100 * (1 - alpha), 1), "% CI: ", ci_bassa, " \u2013 ", ci_alta, ")")
  } else ""
  div(style = "background-color: #f0faf8; border-left: 4px solid #18bc9c; border-radius: 4px; padding: 10px 14px; margin-bottom: 10px;",
      tags$strong(style = "font-size: 16px; color: black;", paste0("Predicted critical time: ", signif(ris$t_soglia, 4), " time units", ci_testo)))
})

output$mcni_confronto_isotermo_testo <- renderUI({
  ris <- mcni_previsione_risultato()
  if (is.null(ris) || is.null(ris$confronto_isotermo)) return(NULL)
  conf <- ris$confronto_isotermo
  ci_iso_testo <- if (!is.null(ris$t_iso_boot)) {
    alpha <- mcni_alpha_confidenza()
    ci_bassa <- signif(quantile(ris$t_iso_boot, alpha / 2), 4)
    ci_alta <- signif(quantile(ris$t_iso_boot, 1 - alpha / 2), 4)
    paste0(" (", round(100 * (1 - alpha), 1), "% CI: ", ci_bassa, " \u2013 ", ci_alta, ")")
  } else ""
  div(style = "background-color: #fef5e7; border-left: 4px solid #e67e22; border-radius: 4px; padding: 10px 14px; margin-bottom: 10px;",
      tags$strong(style = "font-size: 14px; color: black;", paste0("Isothermal comparison at ", signif(ris$T_iso, 5), " K: ")),
      if (is.na(ris$t_iso)) {
        span(style = "color: #c0392b;", "critical threshold not reached within the simulated time range at this temperature.")
      } else {
        tagList(
          span(paste0(signif(ris$t_iso, 4), " time units", ci_iso_testo)),
          tags$br(),
          p(style = "font-size: 12px; color: #566573; margin-top: 6px; margin-bottom: 2px;",
            tags$strong("Difference (non-isothermal \u2212 isothermal): "), signif(conf$diff, 4), " time units",
            if (!is.null(conf$p_value)) paste0("  |  ", round(100 * (1 - mcni_alpha_confidenza()), 1), "% CI of the difference: ", signif(conf$ci_diff_inf, 4), " \u2013 ", signif(conf$ci_diff_sup, 4), "  |  p = ", format.pval(conf$p_value, digits = 3), " (", conf$significativita, ")") else ""),
          p(style = "font-size: 11px; color: #7f8c8d;", conf$metodo)
        )
      })
})

output$mcni_area_grafici_previsione <- renderUI({
  if (isTRUE(input$mcni_combina_grafici)) {
    tagList(
      plotOutput("mcni_grafico_combinato", height = "450px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_mcni_combinato", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("mcni_btn_estetica_previsione", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
      hr(),
      plotOutput("mcni_grafico_k", height = "350px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_mcni_grafico_k", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("mcni_btn_estetica_k", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  } else {
    tagList(
      fluidRow(
        column(6, plotOutput("mcni_grafico_profilo", height = "400px")),
        column(6, plotOutput("mcni_grafico_degradazione", height = "400px"))
      ),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_mcni_profilo", "Download Temperature Profile", class = "btn-outline-secondary btn-sm"),
          downloadButton("scarica_mcni_degradazione", "Download Predicted Quantity", class = "btn-outline-secondary btn-sm"),
          actionButton("mcni_btn_estetica_previsione", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize these plots")),
      hr(),
      plotOutput("mcni_grafico_k", height = "350px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_mcni_grafico_k", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("mcni_btn_estetica_k", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  }
})

output$mcni_grafico_profilo <- renderPlot({
  disegna_mcni_profilo()
})
output$scarica_mcni_profilo <- downloadHandler(
  filename = function() paste0("temperature_profile_mc.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_mcni_profilo()
    dev.off()
  }
)
disegna_mcni_profilo <- function() {
  ris <- mcni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  est <- mcni_leggi_estetica()
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 1))
  plot(ris$t, mcni_converti_temp_display(ris$T), type = "l", col = adjustcolor(est$col_T, alpha.f = est$alpha_T), lwd = est$lwd_linee, lty = est$lty_T,
       xlab = "Time", ylab = mcni_etichetta_temp(), main = if (input$mostra_titolo) "Temperature Profile T(t)" else "")
  if (est$grid_on) grid()
}

output$mcni_grafico_k <- renderPlot({
  disegna_mcni_grafico_k()
})
output$scarica_mcni_grafico_k <- downloadHandler(
  filename = function() paste0("rate_constant_k.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_mcni_grafico_k()
    dev.off()
  }
)
disegna_mcni_grafico_k <- function() {
  ris <- mcni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$mcni_mostra_banda_previsione)) ris$banda_k_inf <- ris$banda_k_sup <- NULL
  est <- mcni_leggi_estetica()
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 1))
  ha_banda <- !is.null(ris$banda_k_inf)
  plot(ris$t, ris$k, type = "n",
       ylim = range(c(ris$k, ris$banda_k_inf, ris$banda_k_sup), na.rm = TRUE),
       xlab = "Time", ylab = "Rate constant k", main = if (input$mostra_titolo) "Rate Constant k(t)" else "")
  if (est$grid_on) grid()
  if (ha_banda) {
    polygon(c(ris$t_griglia_banda, rev(ris$t_griglia_banda)), c(ris$banda_k_sup, rev(ris$banda_k_inf)), col = adjustcolor(est$col_k, alpha.f = 0.2), border = NA)
  }
  lines(ris$t, ris$k, col = est$col_k, lwd = est$lwd_k, lty = est$lty_k)
  if (ha_banda && est$mostra_legenda_k) {
    livello_testo <- paste0(round(100 * (1 - mcni_alpha_confidenza()), 1), "%")
    legend(est$pos_legenda_k, legend = paste0(livello_testo, " Confidence Band"), col = adjustcolor(est$col_k, alpha.f = 0.4), pch = 15, bty = "o", bg = "white", cex = est$cex_legenda_k)
  }
}

output$mcni_grafico_degradazione <- renderPlot({
  disegna_mcni_degradazione()
})
output$scarica_mcni_degradazione <- downloadHandler(
  filename = function() paste0("degradation_curve_mc.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_mcni_degradazione()
    dev.off()
  }
)
disegna_mcni_degradazione <- function() {
  ris <- mcni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$mcni_mostra_banda_previsione)) ris$banda_C_inf <- ris$banda_C_sup <- NULL
  est <- mcni_leggi_estetica()
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 1))
  ha_banda <- !is.null(ris$banda_C_inf)
  ha_iso <- !is.null(ris$t_iso) && !is.na(ris$t_iso) && !is.null(ris$C_iso)
  plot(ris$t, ris$C, type = "n",
       ylim = range(c(ris$C, ris$banda_C_inf, ris$banda_C_sup, ris$C_crit, if (ha_iso) ris$C_iso), na.rm = TRUE),
       xlab = "Time", ylab = "Predicted Quantity", main = if (input$mostra_titolo) "Predicted Degradation Curve" else "")
  if (est$grid_on) grid()
  if (ha_banda) {
    polygon(c(ris$t_griglia_banda, rev(ris$t_griglia_banda)), c(ris$banda_C_sup, rev(ris$banda_C_inf)), col = adjustcolor(est$col_C, alpha.f = 0.2), border = NA)
  }
  lines(ris$t, ris$C, col = est$col_C, lwd = est$lwd_linee)
  if (ha_iso) lines(ris$t, ris$C_iso, col = est$col_iso, lwd = est$lwd_linee, lty = est$lty_iso)
  abline(h = ris$C_crit, col = est$col_soglia, lty = 2, lwd = 1.3)
  if (ha_iso) abline(v = ris$t_iso, col = est$col_iso, lty = 2, lwd = 1.3)
  if (!is.na(ris$t_soglia)) points(ris$t_soglia, ris$C_crit, pch = 19, col = "#2980b9", cex = 1.5)
  if (ha_iso) points(ris$t_iso, ris$C_crit, pch = 19, col = est$col_iso, cex = 1.5)
  if (!is.na(ris$t_soglia)) {
    abline(v = ris$t_soglia, col = "#2980b9", lty = 2, lwd = 1.3)
    if (est$mostra_legenda_prev) {
      voci <- c("Critical threshold", "Predicted critical time")
      colori_legenda <- c(est$col_soglia, "#2980b9"); lty_legenda <- c(2, 2); lwd_legenda <- c(1.3, 1.3); pch_legenda <- c(NA, NA)
      if (ha_iso) {
        voci <- c(voci, "Isothermal Curve")
        colori_legenda <- c(colori_legenda, est$col_iso); lty_legenda <- c(lty_legenda, est$lty_iso); lwd_legenda <- c(lwd_legenda, est$lwd_linee); pch_legenda <- c(pch_legenda, NA)
      }
      if (ha_banda) {
        voci <- c(voci, paste0(round(100 * (1 - mcni_alpha_confidenza()), 1), "% Confidence Band"))
        colori_legenda <- c(colori_legenda, adjustcolor(est$col_C, alpha.f = 0.4)); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA); pch_legenda <- c(pch_legenda, 15)
      }
      legend(est$pos_legenda_prev, legend = voci, col = colori_legenda, lty = lty_legenda, lwd = lwd_legenda, pch = pch_legenda, bty = "o", bg = "white", cex = est$cex_legenda_prev)
    }
  }
}

output$mcni_grafico_combinato <- renderPlot({
  disegna_mcni_combinato()
})
output$scarica_mcni_combinato <- downloadHandler(
  filename = function() paste0("combined_plot_mc.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_mcni_combinato()
    dev.off()
  }
)
disegna_mcni_combinato <- function() {
  ris <- mcni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$mcni_mostra_banda_previsione)) ris$banda_C_inf <- ris$banda_C_sup <- NULL
  est <- mcni_leggi_estetica()
  ha_banda <- !is.null(ris$banda_C_inf)
  ha_iso <- !is.null(ris$t_iso) && !is.na(ris$t_iso) && !is.null(ris$C_iso)
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 4.5))
  plot(ris$t, ris$C, type = "n",
       ylim = range(c(ris$C, ris$banda_C_inf, ris$banda_C_sup, ris$C_crit, if (ha_iso) ris$C_iso), na.rm = TRUE),
       xlab = "Time", ylab = "Predicted Quantity", main = if (input$mostra_titolo) "Temperature Profile & Predicted Degradation" else "")
  if (est$grid_on) grid()
  if (ha_banda) {
    polygon(c(ris$t_griglia_banda, rev(ris$t_griglia_banda)), c(ris$banda_C_sup, rev(ris$banda_C_inf)), col = adjustcolor(est$col_C, alpha.f = 0.2), border = NA)
  }
  lines(ris$t, ris$C, col = est$col_C, lwd = est$lwd_linee)
  if (ha_iso) lines(ris$t, ris$C_iso, col = est$col_iso, lwd = est$lwd_linee, lty = est$lty_iso)
  abline(h = ris$C_crit, col = est$col_soglia, lty = 2, lwd = 1.3)
  if (ha_iso) abline(v = ris$t_iso, col = est$col_iso, lty = 2, lwd = 1.3)
  if (!is.na(ris$t_soglia)) points(ris$t_soglia, ris$C_crit, pch = 19, col = "#2980b9", cex = 1.5)
  if (ha_iso) points(ris$t_iso, ris$C_crit, pch = 19, col = est$col_iso, cex = 1.5)
  if (!is.na(ris$t_soglia)) abline(v = ris$t_soglia, col = "#2980b9", lty = 2, lwd = 1.3)
  par(new = TRUE)
  plot(ris$t, mcni_converti_temp_display(ris$T), type = "l", col = adjustcolor(est$col_T, alpha.f = est$alpha_T), lwd = est$lwd_linee, lty = est$lty_T, axes = FALSE, xlab = "", ylab = "")
  axis(4)
  mtext(mcni_etichetta_temp(), side = 4, line = 3)
  if (est$mostra_legenda_prev) {
    voci <- c("Predicted Quantity", "Temperature", "Critical threshold", "Predicted critical time")
    colori_legenda <- c(est$col_C, est$col_T, est$col_soglia, "#2980b9"); lty_legenda <- c(1, est$lty_T, 2, 2); pch_legenda <- c(NA, NA, NA, NA)
    if (ha_iso) {
      voci <- c(voci, "Isothermal Curve")
      colori_legenda <- c(colori_legenda, est$col_iso); lty_legenda <- c(lty_legenda, est$lty_iso); pch_legenda <- c(pch_legenda, NA)
    }
    if (ha_banda) {
      voci <- c(voci, paste0(round(100 * (1 - mcni_alpha_confidenza()), 1), "% Confidence Band (Quantity)"))
      colori_legenda <- c(colori_legenda, adjustcolor(est$col_C, alpha.f = 0.4)); lty_legenda <- c(lty_legenda, NA); pch_legenda <- c(pch_legenda, 15)
    }
    legend(est$pos_legenda_prev, legend = voci, col = colori_legenda, lty = lty_legenda, pch = pch_legenda, lwd = est$lwd_linee, bty = "o", bg = "white", cex = est$cex_legenda_prev)
  }
}


# Part 1+2 (confirmation of temperatures + fit of the secondary model): ALWAYS
# visible, regardless of the validity of the current comparison - unlike
# the rest below. Button disabled via JS (see ui.R)
# when the comparison is not suitable - same mechanism as the
# Batch version (non_isotermico.R).
output$mcni_setup_iniziale_ui <- renderUI({
  tagList(
    hr(),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
    tags$h5("1. Confirm fit temperatures"),
    selectInput("mcni_unita_temp", "Temperature Unit:", choices = c("Celsius" = "celsius", "Kelvin" = "kelvin")),
    fluidRow(
      column(7, uiOutput("mcni_tabella_temperature")),
      column(5, tags$div(style = "padding-top: 28px;",
        tags$button(id = "btn_copia_mcni_kT", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
          style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 6px; display: block;",
          onclick = "copiaTabellaClipboard('mcni_tabella_kT', 'btn_copia_mcni_kT')"),
        tableOutput("mcni_tabella_kT")))
    ),
    
    hr(),
    tags$h5("2. Fit the secondary model k(T)"),
    numericInput("mcni_livello_confidenza", "Confidence/Prediction Level (%):", value = 95, min = 50, max = 99.9, step = 1),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
      "Independent from the global \"Confidence/Prediction Level\" in the sidebar. Governs every confidence/prediction interval computed further down in this module: the parameter table below, the Secondary Model plot's bands, and the final Predicted Critical Time / Predicted Quantity bands. Applies from the next fit or prediction run (re-run them to see it reflected)."),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -4px;",
      "The fit itself (both the point estimate and every bootstrap iteration) uses ", tags$strong("Levenberg-Marquardt"), " (least squares, with analytical derivatives when the formula allows) \u2014 independent from the Loss Function and Algorithm chosen in the sidebar, since the confidence interval math here assumes least squares."),
    selectInput("mcni_modello_secondario", "Secondary model:",
                choices = c("Classic Arrhenius" = "arrhenius",
                            "Generic Modified Arrhenius" = "arrhenius_mod",
                            "Log-Logistic" = "log_logistic",
                            "Simplified Log-Logistic" = "log_logistic_semplice",
                            "Custom Formula" = "custom")),
    conditionalPanel(
      condition = "input.mcni_modello_secondario != 'custom'",
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "The guesses below are pre-filled automatically, not left blank: for the two ", tags$strong("Arrhenius"), " models, via the classic linearization (a simple linear regression of ln(k) against 1/T \u2014 the same \"Arrhenius plot\" trick, already very close to the final fit); for the two ", tags$strong("Log-Logistic"), " models, via simpler heuristics based on the range of the observed temperatures and rate constants (less precise, but a reasonable starting point for the optimizer). Feel free to edit them by hand before fitting."),
      uiOutput("mcni_guess_secondario_ui")
    ),
    conditionalPanel(
      condition = "input.mcni_modello_secondario == 'custom'",
      textInput("mcni_secondario_formula_custom", "Custom Formula (use 'T', in Kelvin):", placeholder = "A * exp(-Ea / (8.314 * T))"),
      textInput("mcni_secondario_guess_custom", "Initial Guesses:", placeholder = "A=1e10[0,], Ea=50000[0,]"),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Same syntax as the sidebar's own Initial Guesses field: \"name=value[lower,upper]\", comma-separated, boundaries optional. Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides T and the parameters you name here. Unlike the 4 built-in models above, there's no automatic starting guess for a custom formula \u2014 you'll need to provide reasonable values yourself. Any boundaries you set are accepted but not enforced during the fit itself (it always uses unconstrained Simplex, same as the 4 built-in models), only clamped in bootstrap resamples if enabled below."),
      p(style = "font-size: 11px; color: #c0392b; margin-top: -6px;",
        icon("triangle-exclamation"), " If your formula has an Arrhenius-like structure (a pre-exponential factor on the order of 1e10\u20131e14 combined with an activation-energy-like term on the order of 1e4\u20131e5), Simplex can fail to find the right answer even from a guess in the right ballpark \u2014 the very different scales of the two kinds of parameter make this a genuinely hard case for it, which is why the 4 built-in models above use a pre-computed starting point from a classic linearization trick instead of leaving it to chance. There's no single fix that reliably works for every custom formula, so treat convergence with some skepticism: check that the fitted curve actually tracks your data well (the R\u00b2 and RMSE below), and that the recovered parameter values are physically sensible, rather than trusting a reported successful fit at face value.")
    ),
    checkboxInput("mcni_mostra_celsius", "Display temperature in \u00b0Celsius on plots (below)", value = TRUE),
    checkboxInput("mcni_usa_monte_carlo", "Propagate primary-fit uncertainty (nested bootstrap)", value = FALSE),
    conditionalPanel(
      condition = "input.mcni_usa_monte_carlo == true",
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "For each fit, resamples k from its own Monte Carlo bootstrap distribution (if that fit used Monte Carlo) or a Gaussian approximation from its Jacobian-based error (if not), refits the secondary model on each resample, and later propagates the same resamples into the predicted critical time below."),
      numericInput("mcni_n_boot_secondario", "Nested Bootstrap Iterations:", value = 200, min = 20, max = 2000, step = 20)
    ),
    actionButton("mcni_fit_secondario", "Fit Secondary Model", class = "btn-primary w-100", icon = icon("calculator")),
    )
  )
})

# Part 3+4 (T(t) profile and final prediction): they remain hidden until the
# current comparison is suitable.
output$mcni_corpo_principale <- renderUI({
  stato <- mcni_batch_valido()
  if (!stato$valido) return(NULL)
  
  tagList(
    br(), br(),
    tags$button(id = "btn_copia_mcni_riepilogo", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('mcni_riepilogo_secondario', 'btn_copia_mcni_riepilogo')"),
    tableOutput("mcni_riepilogo_secondario"),
    tags$button(id = "btn_copia_mcni_confronto_modelli", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('mcni_confronto_modelli_secondario', 'btn_copia_mcni_confronto_modelli')"),
    tableOutput("mcni_confronto_modelli_secondario"),
    tags$button(id = "btn_copia_mcni_secondario", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('mcni_tabella_secondario', 'btn_copia_mcni_secondario')"),
    tableOutput("mcni_tabella_secondario"),
    fluidRow(
      column(6, checkboxInput("mcni_mostra_banda_confidenza", "Show Confidence Band", value = TRUE)),
      column(6, checkboxInput("mcni_mostra_banda_predizione", "Show Prediction Band", value = FALSE))
    ),
    uiOutput("mcni_ui_banda_simultanea"),
    plotOutput("mcni_grafico_secondario", height = "380px"),
    div(style = "margin-top: 4px; margin-bottom: 8px;",
        downloadButton("scarica_mcni_secondario", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("mcni_btn_estetica_secondario", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
    
    hr(),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
    tags$h5("3. Choose the temperature profile T(t)"),
    selectInput("mcni_tipo_profilo", "Temperature profile:",
                choices = c("Raw Data (datalogger upload)" = "dati_grezzi",
                            "Synthetic Thermal Abuse Profile" = "abuso_termico",
                            "Linear: T0 + a\u00b7t" = "lineare",
                            "Quadratic: T0 + a\u00b7t + b\u00b7t\u00b2" = "quadratico",
                            "Sinusoidal: Tm + (a/2)\u00b7sin(2\u03c0t/L + 3\u03c0/2)" = "sinusoidale",
                            "Exponential: Th + (T0-Th)\u00b7exp(-J\u00b7t)" = "esponenziale",
                            "Custom Formula" = "custom")),
    conditionalPanel(
      condition = "input.mcni_tipo_profilo == 'dati_grezzi'",
      fileInput("mcni_file_datalogger", "Datalogger file (2 columns: time, temperature in Kelvin):", accept = c(".csv", ".xlsx", ".xls"))
    ),
    conditionalPanel(
      condition = "input.mcni_tipo_profilo == 'custom'",
      textInput("mcni_profilo_formula_custom", "Custom Formula (use 't', same time units as your data):", placeholder = "T0 + a * t + b * sin(t)"),
      textInput("mcni_profilo_guess_custom", "Parameter Values:", placeholder = "T0=277, a=0.1, b=2"),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Same \"name=value\" syntax as everywhere else in the app (boundaries in brackets are accepted too, but have no effect here \u2014 this profile is a fixed input you're defining, not something being fitted, so there's nothing to constrain). Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides t and the parameters you name here. This formula is evaluated as-is at every time point of the simulation \u2014 it is never fitted to data, so pick values you already consider realistic.")
    ),
    uiOutput("mcni_parametri_profilo_ui"),
    
    hr(),
    tags$h5("4. Set the scenario and run"),
    uiOutput("mcni_A0_C_crit_ui"),
    uiOutput("mcni_campo_n_ui"),
    uiOutput("mcni_campo_isotermo_ui"),
    p(style = "font-size: 11px; color: #c0392b; font-weight: 500; margin-top: -6px;",
      icon("triangle-exclamation"), " Integration Grid Resolution is used both for the point-estimate curve above and, if Monte Carlo is enabled, for ", tags$strong("every single one"), " of its bootstrap iterations \u2014 raising it can noticeably slow down the prediction, and with Monte Carlo on, the slowdown multiplies directly by the number of Bootstrap Iterations set for the secondary fit. It does ", tags$strong("not"), " adjust itself automatically when you enable Monte Carlo \u2014 consider lowering it (e.g. to around 300) yourself in that case, and raise it only if you specifically need finer time resolution (e.g. very short/sharp temperature abuses relative to the total simulated duration)."),
    actionButton("mcni_lancia_previsione", "Predict Critical Time", class = "btn-primary btn-sm w-100", icon = icon("play")),
    ),
    br(), br(),
    uiOutput("mcni_risultato_testo"),
    uiOutput("mcni_confronto_isotermo_testo"),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
      "When \"Propagate primary-fit uncertainty\" is enabled above, the Predicted Quantity curve shows a 95% confidence band. The Temperature Profile never does \u2014 it's a fixed input you provided (raw data or a chosen formula), not an estimated quantity, so there's nothing statistical to attach an interval to."),
    fluidRow(
      if (!is.null(mcni_secondario_risultato()$mat_boot)) column(6, checkboxInput("mcni_mostra_banda_previsione", "Show Confidence Band", value = TRUE)),
      column(6, checkboxInput("mcni_combina_grafici", "Combine into a single dual-axis plot", value = FALSE))
    ),
    uiOutput("mcni_area_grafici_previsione")
  )
})


# Checkbox of the simultaneous band of the secondary plot: only when the band
# comes from the linear approximation; with the nested bootstrap the bands are
# pointwise percentiles and a note appears in its place.
output$mcni_ui_banda_simultanea <- renderUI({
  ris <- mcni_secondario_risultato()
  if (is.null(ris)) return(NULL)
  if (ni_bootstrap_secondario_valido(ris)) {
    return(p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the nested bootstrap (percentiles of the bootstrap curves)."))
  }
  tagList(
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the linear approximation of the secondary model (nested bootstrap not available)."),
    conditionalPanel(condition = "input.mcni_mostra_banda_confidenza",
      div(style = "margin-left: 20px;",
        checkboxInput("mcni_banda_simultanea", "Simultaneous confidence band (Bates & Watts, 1988)", value = FALSE)))
  )
})
