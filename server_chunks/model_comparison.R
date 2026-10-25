# ============================
#  MODEL COMPARISON 
# ============================

confronto_modelli <- reactiveVal(list())

# ---- Conditional visibility: the sections below remain hidden until -----
# at least one fit has been added to the comparison (same "NULL or
# tagList" pattern already documented and used elsewhere in the project - see the comment
# in non_isotermico.R on why a conditionalPanel with a custom boolean
# output is avoided, never verified against a working example in this
# project). Each block includes its own separator (hr()/br()) linking
# it to the next element, so when it is hidden no "orphan"
# separator remains on screen.

output$sezione_matrice_evidenza_ui <- renderUI({
  if (length(confronto_modelli()) == 0) return(NULL)
  tagList(
    hr(),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Pairwise Evidence Ratio Matrix:"),
      tags$button(id = "btn_copia_matrice_evidenza", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('matrice_evidenza_akaike', 'btn_copia_matrice_evidenza')")
    ),
    p(style = "font-size: 12px; color: #c0392b; font-weight: 500;",
      icon("triangle-exclamation"), " This analysis assumes every model here was fitted to the ", tags$strong("same dataset"), " \u2014 it compares different models against each other, not the same model across different data. AIC/AICc/BIC aren't directly comparable across datasets, so if you've added fits from different samples here, this matrix won't have a valid statistical meaning."),
    p(style = "font-size: 12px; color: #566573;",
      "Cell [row, col] = how many times more likely the row model is than the column model, given the data (computed from AICc). Unlike a correlation matrix, this is ", tags$strong("not symmetric"), ": [B, A] is the reciprocal of [A, B]. The diagonal is always 1 (a model compared to itself). Not a classic frequentist p-value, but the standard \"model probability\" interpretation used in the model-selection literature (Burnham & Anderson)."),
    tableOutput("matrice_evidenza_akaike"),
    br()
  )
})

output$sezione_parametri_confronto_ui <- renderUI({
  if (length(confronto_modelli()) == 0) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Parameter Estimates Comparison:"),
      tags$button(id = "btn_copia_confronto_parametri", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('tabella_confronto_parametri', 'btn_copia_confronto_parametri')")
    ),
    p("Estimated value and standard error for each parameter, across all models added to the comparison.", style = "color: #7f8c8d; font-size: 13px;"),
    tableOutput("tabella_confronto_parametri"),
    hr()
  )
})

output$sezione_test_parametri_ui <- renderUI({
  if (length(confronto_modelli()) == 0) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Parameter Significance Test:"),
      tags$button(id = "btn_copia_test_parametri", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('tabella_test_parametri', 'btn_copia_test_parametri')")
    ),
    p(style = "color: #7f8c8d; font-size: 13px;",
      "Choose any two saved models to test whether their parameters differ significantly, using Welch's t-test on the estimates and standard errors (Satterthwaite degrees of freedom, valid regardless of sample size). Only possible when both models share the exact same parameter names, in the same order."),
    fluidRow(
      column(3, uiOutput("selezione_test_a_ui")),
      column(3, uiOutput("selezione_test_b_ui")),
      column(6, div(style = "margin-top: 32px;", actionButton("esegui_test_parametri", "Run Test", class = "btn-primary btn-sm w-100", icon = icon("calculator"))))
    ),
    tags$strong(textOutput("metodo_test_parametri", inline = TRUE)),
    tableOutput("tabella_test_parametri"),
    p(style = "font-size: 11px; color: #7f8c8d;",
      "Significance codes: *** p < 0.001,  ** p < 0.01,  * p < 0.05,  . p < 0.1,  ns = not significant."),
    br()
  )
})

output$sezione_posthoc_intestazione_ui <- renderUI({
  if (length(confronto_modelli()) == 0) return(NULL)
  tagList(
    tags$h4("Post-Hoc Comparison (3+ Models):"),
    p(style = "font-size: 12px; color: #566573;",
      "The equivalent of an ANOVA followed by a post-hoc test for this setting: each model gives one parameter estimate plus its own uncertainty, not raw replicate values, so a classic ANOVA doesn't directly apply. ", tags$strong("Cochran's Q"), " (the standard meta-analysis test for comparing several independent estimates with known standard errors) checks, per parameter, whether the models differ overall; if so, all pairwise comparisons below tell you ", tags$strong("which"), " models differ \u2014 with a multiple-comparison correction, since testing many pairs would otherwise inflate false positives. Requires all selected models to share the same parameter names, same as the test above. When all the compared fits have Monte Carlo distributions, the pairwise comparisons use the bootstrap distributions directly (percentile interval of the differences) and Cochran's Q uses the bootstrap standard errors."),
    uiOutput("selezione_modelli_posthoc"),
    fluidRow(
      column(6, selectInput("metodo_correzione_confronto", "Multiple-Comparison Correction:",
                            choices = SCELTE_CORREZIONE_MULTIPLA, selected = "holm")),
      column(6, br(), actionButton("esegui_posthoc_confronto", "Run Post-Hoc Comparison", class = "btn-primary btn-sm w-100", icon = icon("calculator")))
    ),
    uiOutput("sezione_posthoc_confronto_ui"),
    hr()
  )
})

# Adding logic extracted into a reusable function: both the manual
# "Add Current Fit to Comparison" button and auto-add (if active)
# call it identically, so the behaviour (disambiguation of
# labels, notifications, saved fields) always remains consistent between the two
# entry points.
SOGLIA_MODELLI_CONFRONTO <- 15
esegui_add_a_confronto <- function(silenzioso = FALSE) {
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || is.null(res$successo) || !res$successo) {
    if (!silenzioso) showNotification("Run a successful optimization first before adding it to the comparison.", type = "error")
    return(invisible(FALSE))
  }
  
  lista_attuale <- confronto_modelli()
  etichetta <- trimws(input$etichetta_confronto)
  if (nchar(etichetta) == 0) etichetta <- paste0(input$tipo_modello, " #", length(lista_attuale) + 1)
  
  # If the label already exists (typed by the user or generated
  # automatically), I disambiguate it immediately - two models with the same
  # label would not overwrite each other in the list (which is
  # indexed by position, not by name), but they would make ambiguous the
  # selection in the menus of Parameter Significance Test and Post-Hoc Comparison
  # (based on the label): selecting "the second one" would always end up
  # returning the first one found.
  etichette_esistenti <- sapply(lista_attuale, function(s) s$label)
  if (etichetta %in% etichette_esistenti) {
    etichetta_originale <- etichetta
    suffisso <- 2
    while (paste0(etichetta_originale, " (", suffisso, ")") %in% etichette_esistenti) suffisso <- suffisso + 1
    etichetta <- paste0(etichetta_originale, " (", suffisso, ")")
    if (!silenzioso) showNotification(paste0("\"", etichetta_originale, "\" was already in the comparison - added as \"", etichetta, "\" instead."), type = "warning", duration = 8)
  }
  
  nuovo_snapshot <- list(
    label = etichetta,
    tipo_modello = input$tipo_modello,
    n_param = length(res$parametri),
    obs = res$obs,
    rmse = res$rmse,
    r2 = res$r2,
    r2_adj = res$r2_adj,
    aic = res$aic,
    aicc = res$aicc,
    bic = res$bic,
    algoritmo = input$algoritmo,
    loss = if (ponderazione_attiva(res)) sprintf("%s, w = 1/\u0177^%g", input$tipo_loss, signif(res$ponderazione$zeta, 3)) else input$tipo_loss,
    x_griglia = res$x_griglia,
    y_curva = res$valuta(res$parametri, res$x_griglia),
    parametri = res$parametri,
    errori = res$errori,
    x_dati = res$df_originale$x,
    y_dati = res$df_originale$y,
    residui = res$residui,
    mat_boot = res$mat_boot_params,
    convergenza_vera = isTRUE(res$convergenza_vera),
    simboli_modello = as.list(simboli_parametri()),
    formula = res$formula,
    cov = res$cov,
    # Weighted regression: with weights rmse is the sigma of the variance model
    # (relative); they are needed by Inverse Prediction and Shelf-Life to bring the
    # prediction variance back to the scale of the data.
    ponderazione = res$ponderazione,
    pesi = res$pesi,
    df = res$df,
    temperatura = NA_real_
  )
  lista_attuale[[length(lista_attuale) + 1]] <- nuovo_snapshot
  confronto_modelli(lista_attuale)
  # Warning when the threshold of models is exceeded (at the sixteenth, once;
  # it reappears if, after removing models, the threshold is exceeded
  # again). It does not block anything: memory remains modest, but the overlay plot,
  # table and post-hoc become hard to read and slower.
  if (length(lista_attuale) == SOGLIA_MODELLI_CONFRONTO + 1) {
    showModal(modalDialog(
      title = "Many models in Model Comparison",
      p(sprintf("You have added more than %d models to Model Comparison (%d now).", SOGLIA_MODELLI_CONFRONTO, length(lista_attuale))),
      p("The app keeps working normally, but with many models the overlay plot and its legend become hard to read, the comparison table gets long, and post-hoc comparisons between many selected models take longer: the number of pairs grows quickly (45 pairs with 10 models, 435 with 30)."),
      p("Consider removing the models you no longer need with \u201cRemove Selected Model\u201d or \u201cRemove All the Models\u201d."),
      easyClose = TRUE, footer = modalButton("OK")))
  }
  if (!silenzioso) {
    showNotification(paste("Added to comparison:", etichetta), type = "message")
  } else {
    showNotification(paste("Auto-added to comparison:", etichetta), type = "message", duration = 4)
  }
  if (!isTRUE(res$convergenza_vera)) {
    showNotification(paste0("Note: \"", etichetta, "\" did not fully converge (accepted via the sidebar setting) \u2014 marked in the comparison table."), type = "warning", duration = 8)
  }
  updateTextInput(session, "etichetta_confronto", value = "")
  invisible(TRUE)
}

observeEvent(input$aggiungi_confronto, {
  esegui_add_a_confronto(silenzioso = FALSE)
})

# Auto-add: if the checkbox in the sidebar is active, every successful fit is
# added automatically to the list, without having to press the manual
# button each time - useful for those who explore many models/settings in a
# row and want to accumulate all of them in Model Comparison without an extra click
# for each one. Off by default: automatically adding EVERY fit
# (including those made just as an attempt) might not always be what
# one wants.
#
# [BUG FIX] Do NOT add ignoreInit=TRUE here, even if it would seem the
# "safe" default choice for an observeEvent. elaborazione() is an
# eventReactive(input$esegui, ...): until the "Run Optimization" button
# has ever been pressed, input$esegui equals 0 - and for an actionButton
# 0 is EXPLICITLY treated as a "null" event by Shiny itself
# (isNullEvent() treats it on a par with a real NULL), so
# elaborazione() never produces a real value before the first click
# (it stops silently, like an internal req(FALSE)). With
# ignoreInit=TRUE on THIS observer, the first time elaborazione()
# finally produces a real value (i.e. the user's FIRST click)
# it is mistaken for "the initial trigger to be ignored", and therefore the first
# successful fit was never added automatically - only from the second
# click onwards. Verified by reading the real implementation of
# bindEvent.Observer/bindEvent.reactiveExpr in Shiny (not only the
# documentation, which by itself would suggest the opposite).
observeEvent(elaborazione(), {
  if (!isTRUE(input$auto_add_confronto)) return()
  res <- tryCatch(elaborazione(), error = function(e) NULL)
  if (is.null(res) || is.null(res$successo) || !res$successo) return()
  esegui_add_a_confronto(silenzioso = TRUE)
})

observeEvent(input$svuota_confronto, {
  confronto_modelli(list())
  showNotification("Comparison list cleared.", type = "message")
})

output$selezione_rimozione_ui <- renderUI({
  lista <- confronto_modelli()
  scelte <- if (length(lista) > 0) sapply(lista, function(s) s$label) else character(0)
  selectInput("modello_da_rimuovere", NULL, choices = scelte)
})

observeEvent(input$rimuovi_modello, {
  lista <- confronto_modelli()
  if (length(lista) == 0) {
    showNotification("No models to remove yet.", type = "error")
    return()
  }
  labels_tutte <- sapply(lista, function(s) s$label)
  idx <- which(labels_tutte == input$modello_da_rimuovere)
  if (length(idx) == 0) {
    showNotification("Selected model not found (it may have already been removed). Refresh the selection above.", type = "error")
    return()
  }
  nome_rimosso <- lista[[idx]]$label
  lista[[idx]] <- NULL
  confronto_modelli(lista)
  showNotification(paste("Removed from comparison:", nome_rimosso), type = "message")
})

output$selezione_rinomina_ui <- renderUI({
  lista <- confronto_modelli()
  scelte <- if (length(lista) > 0) sapply(lista, function(s) s$label) else character(0)
  selectInput("modello_da_rinominare", NULL, choices = scelte)
})

observeEvent(input$rinomina_modello, {
  lista <- confronto_modelli()
  if (length(lista) == 0) {
    showNotification("No models to rename yet.", type = "error")
    return()
  }
  nuovo_nome <- trimws(input$nuovo_nome_modello)
  if (nchar(nuovo_nome) == 0) {
    showNotification("Enter a new label before renaming.", type = "error")
    return()
  }
  labels_tutte <- sapply(lista, function(s) s$label)
  idx <- which(labels_tutte == input$modello_da_rinominare)
  if (length(idx) == 0) {
    showNotification("Selected model not found (it may have been removed or already renamed). Refresh the selection above.", type = "error")
    return()
  }
  if (nuovo_nome %in% labels_tutte[-idx[1]]) {
    showNotification("Another model already has this label. Choose a different one.", type = "error")
    return()
  }
  lista[[idx[1]]]$label <- nuovo_nome
  confronto_modelli(lista)
  showNotification(paste("Renamed to:", nuovo_nome), type = "message")
  updateTextInput(session, "nuovo_nome_modello", value = "")
})

tabella_confronto_df <- reactive({
  lista <- confronto_modelli()
  if (length(lista) == 0) return(NULL)
  runs_modelli <- lapply(lista, function(s) {
    if (is.null(s$residui) || is.null(s$x_dati) || length(s$residui) != length(s$x_dati)) return(list(disponibile = FALSE))
    tryCatch(runs_test_residui(residui_ordinati_per_x(s$x_dati, s$residui)), error = function(e) list(disponibile = FALSE))
  })
  data.frame(
    Label = sapply(lista, function(s) s$label),
    Model = sapply(lista, function(s) s$tipo_modello),
    Converged = sapply(lista, function(s) if (is.null(s$convergenza_vera) || isTRUE(s$convergenza_vera)) "Yes" else "NO (caution)"),
    N_Param = sapply(lista, function(s) s$n_param),
    N_Obs = sapply(lista, function(s) s$obs),
    RMSE = round(sapply(lista, function(s) s$rmse), 5),
    R2 = round(sapply(lista, function(s) s$r2), 5),
    R2_Adj = round(sapply(lista, function(s) s$r2_adj), 5),
    AIC = round(sapply(lista, function(s) s$aic), 3),
    AICc = round(sapply(lista, function(s) s$aicc), 3),
    BIC = round(sapply(lista, function(s) if (is.null(s$bic)) NA else s$bic), 3),
    # Runs test on the signs of the residuals (van Boekel, 2008, section 7.8), with the
    # same ordering for all models: a check of systematic
    # patterns, not a selection criterion.
    Z_Too_Few_Runs = sapply(runs_modelli, function(R) if (isTRUE(R$disponibile)) round(R$z_tf, 2) else NA),
    P_Too_Few_Runs = sapply(runs_modelli, function(R) if (isTRUE(R$disponibile)) signif(R$p_poche, 3) else NA),
    Algorithm = sapply(lista, function(s) s$algoritmo),
    Loss = sapply(lista, function(s) s$loss),
    stringsAsFactors = FALSE
  )
})

tabella_confronto_ordinata <- reactive({
  df <- tabella_confronto_df()
  req(df)
  decrescente <- (input$comp_sort_dir == "desc")
  idx <- order(df[[input$comp_sort_by]], decreasing = decrescente, na.last = TRUE)
  df[idx, ]
})

output$tabella_confronto <- renderTable({
  df <- tabella_confronto_ordinata()
  if (is.null(df)) return(data.frame(Message = "No models added yet. Run a fit and click 'Add Current Fit to Comparison'."))
  # renderTable()/xtable apply their own default rounding to the
  # DISPLAY - formats here, only for the on-screen view, without
  # touching the reactive shared with the sorting (which must remain
  # numeric for a correct sort) and with the CSV/Excel export.
  for (col in c("RMSE", "R2", "R2_Adj", "AIC", "AICc", "BIC")) {
    df[[col]] <- ifelse(is.na(df[[col]]), "N/A", sprintf("%.3f", df[[col]]))
  }
  colnames(df) <- gsub("_", " ", colnames(df))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

# ---- Model Probability (Akaike Weights) ----
# The "Akaike weight" is the probability that each model is the best
# in the compared set, given the evidence of the data: it is computed from AICc
# (the criterion recommended by the app), normalizing the relative likelihood
# of each model with respect to the best of the group. It is the standard
# interpretation in the literature (Burnham & Anderson) - it is not a p-value in the
# classic frequentist sense, but a Bayesian-like probability over the set
# of candidate models.
tabella_pesi_akaike <- reactive({
  df <- tabella_confronto_df()
  if (is.null(df) || nrow(df) < 2) return(NULL)
  aicc_vals <- df$AICc
  if (all(is.na(aicc_vals))) return(NULL)
  delta <- aicc_vals - min(aicc_vals, na.rm = TRUE)
  rel_lik <- exp(-delta / 2)
  pesi <- rel_lik / sum(rel_lik, na.rm = TRUE)
  data.frame(Label = df$Label, AICc = aicc_vals, Delta_AICc = delta, Akaike_Weight = pesi, stringsAsFactors = FALSE)
})

# ---- Pairwise Evidence Ratio Matrix ----
# Matrix[i,j] = how many times more probable model i is than
# model j (w_i / w_j), given the evidence of the data. Unlike a
# correlation matrix, it is NOT symmetric (Matrix[j,i] is the reciprocal
# of Matrix[i,j]) - the diagonal is always 1 (a model compared with
# itself).
matrice_evidenza_akaike <- reactive({
  tab <- tabella_pesi_akaike()
  if (is.null(tab)) return(NULL)
  n <- nrow(tab)
  mat <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      mat[i, j] <- tab$Akaike_Weight[i] / tab$Akaike_Weight[j]
    }
  }
  rownames(mat) <- colnames(mat) <- tab$Label
  mat
})

output$matrice_evidenza_akaike <- renderTable({
  mat <- matrice_evidenza_akaike()
  if (is.null(mat)) return(data.frame(Message = "Add at least 2 models with a valid AICc to compute the evidence ratio matrix."))
  df <- as.data.frame(apply(mat, 2, function(col) sprintf("%.2f", col)), stringsAsFactors = FALSE)
  cbind(Model = rownames(mat), df)
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", rownames = FALSE)

tabella_confronto_parametri_df <- reactive({
  lista <- confronto_modelli()
  if (length(lista) == 0) return(NULL)
  
  # Uses the same confidence level set by the user in the sidebar, instead
  # of a fixed 95%. Each model gets its OWN critical value (Student's t with
  # its own residual degrees of freedom, n-p), since different models being
  # compared may have different sample sizes/parameter counts.
  nome_col_lower <- paste0("CI_", input$livello_confidenza, "_Lower")
  nome_col_upper <- paste0("CI_", input$livello_confidenza, "_Upper")
  
  righe <- lapply(lista, function(s) {
    nomi_par <- names(s$parametri)
    stima <- as.numeric(s$parametri)
    errore <- as.numeric(s$errori)
    df_modello <- s$obs - s$n_param
    t_val <- valore_critico(df_modello)
    
    ci_lower <- stima - (t_val * errore)
    ci_upper <- stima + (t_val * errore)
    
    df_riga <- data.frame(
      Label = s$label,
      Model = s$tipo_modello,
      Parameter = nomi_par,
      Estimate = round(stima, 5),
      Std_Error = round(errore, 5),
      CI_Lower = round(ci_lower, 5),
      CI_Upper = round(ci_upper, 5),
      stringsAsFactors = FALSE
    )
    colnames(df_riga)[colnames(df_riga) == "CI_Lower"] <- nome_col_lower
    colnames(df_riga)[colnames(df_riga) == "CI_Upper"] <- nome_col_upper
    
    # If the model was fitted with Monte Carlo (mat_boot available),
    # it also adds the EMPIRICAL interval (direct quantiles of the
    # bootstrap distribution) - same logic already used by the single fit:
    # unlike the one above (Estimate +/- t*SE, which assumes symmetry),
    # this captures any real asymmetries of the distribution. Without
    # this, a model fitted with Monte Carlo here in Model Comparison
    # showed only the symmetric interval, losing the empirical
    # information that the single fit correctly shows for the same type
    # of fit.
    if (!is.null(s$mat_boot) && nrow(s$mat_boot) >= 10) {
      p_inf <- (1 - input$livello_confidenza / 100) / 2
      p_sup <- 1 - p_inf
      colonne_boot_presenti <- nomi_par[nomi_par %in% colnames(s$mat_boot)]
      ci_emp_lower <- rep(NA_real_, length(nomi_par))
      ci_emp_upper <- rep(NA_real_, length(nomi_par))
      idx_presenti <- match(colonne_boot_presenti, nomi_par)
      ci_emp_lower[idx_presenti] <- apply(s$mat_boot[, colonne_boot_presenti, drop = FALSE], 2, quantile, probs = p_inf, na.rm = TRUE)
      ci_emp_upper[idx_presenti] <- apply(s$mat_boot[, colonne_boot_presenti, drop = FALSE], 2, quantile, probs = p_sup, na.rm = TRUE)
      df_riga[[paste0("CI_Empirical_", input$livello_confidenza, "_Lower")]] <- round(ci_emp_lower, 5)
      df_riga[[paste0("CI_Empirical_", input$livello_confidenza, "_Upper")]] <- round(ci_emp_upper, 5)
    }
    df_riga
  })
  
  rownames(righe) <- NULL
  righe <- rbind_riempi_na(righe)
  rownames(righe) <- NULL
  righe
})

output$tabella_confronto_parametri <- renderTable({
  df <- tabella_confronto_parametri_df()
  if (is.null(df)) return(data.frame(Message = "No models added yet."))
  # Applies the Parameter Display Symbols only to the on-screen view, not to the
  # reactive shared with the CSV/Excel export (which must keep the raw
  # names for compatibility with spreadsheets). Uses the map CAPTURED
  # for each model (by label), not the "live" one linked to the
  # current selector of the sidebar - with several different models in
  # comparison, the live map might not correspond to models other
  # than the one currently selected.
  lista_modelli <- confronto_modelli()
  etichette_modelli <- sapply(lista_modelli, function(s) s$label)
  df$Parameter <- mapply(function(n, etichetta_riga) {
    idx <- match(etichetta_riga, etichette_modelli)
    mappa <- if (!is.na(idx)) lista_modelli[[idx]]$simboli_modello else NULL
    if (!is.null(mappa) && !is.null(mappa[[n]])) simbolo_a_html(mappa[[n]]) else simbolo_a_html(simbolo_automatico_pedice(n))
  }, df$Parameter, df$Label)
  # Same explicit formatting with sprintf() used elsewhere in the app, for
  # guaranteed control over the decimal digits shown (renderTable/xtable
  # otherwise apply their own default rounding).
  for (col in c("Estimate", "Std_Error")) {
    df[[col]] <- sprintf("%.3f", df[[col]])
  }
  colonne_ci <- grep("^CI_", colnames(df), value = TRUE)
  for (col in colonne_ci) {
    df[[col]] <- sprintf("%.3f", df[[col]])
  }
  colnames(df) <- gsub("_", " ", colnames(df))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

disegna_grafico_confronto <- function() {
  lista <- confronto_modelli()
  par(mar = c(5, 5, 4, 2) + 0.1)
  if (length(lista) == 0) {
    plot(1, type = "n", main = "No models added yet.", axes = FALSE, xlab = "", ylab = "")
    return(invisible())
  }
  
  mostra_punti <- if (is.null(input$mostra_punti_confronto)) TRUE else input$mostra_punti_confronto
  mostra_legenda <- if (is.null(input$confronto_mostra_legenda)) TRUE else input$confronto_mostra_legenda
  lwd_curve <- if (is.null(input$confronto_lwd_curve)) 2.5 else input$confronto_lwd_curve
  cex_punti <- if (is.null(input$confronto_cex_punti)) 1 else input$confronto_cex_punti
  pch_punti_confronto <- as.numeric(if (is.null(input$confronto_pch_punti)) "16" else input$confronto_pch_punti)
  lty_curve_confronto <- as.numeric(if (is.null(input$confronto_lty_curve)) "1" else input$confronto_lty_curve)
  
  # The range of the axes and the points shown come from the OWN data of each
  # model (saved at the moment it was added to the comparison), not
  # from the dataset currently loaded in the sidebar: models added at different
  # moments can come from different datasets (e.g. different temperatures),
  # so using only the "current" dataset would show wrong or
  # missing points for the models added earlier.
  tutte_x <- unlist(lapply(lista, function(s) c(s$x_griglia, s$x_dati)))
  tutte_y <- unlist(lapply(lista, function(s) c(s$y_curva, s$y_dati)))
  
  # Colour palette: the original fixed one (good for few models, each
  # colour well distinguishable from the others), or a continuous scale - more
  # suitable when there are many models, where many shades are needed
  # that are easy to order visually instead of N arbitrary colours. All
  # generated with base R functions (rainbow(), hcl.colors()), no new
  # dependency.
  tipo_palette <- if (is.null(input$confronto_palette)) "fissa" else input$confronto_palette
  n_modelli <- length(lista)
  colori_usati <- colori_da_palette(tipo_palette, n_modelli)
  colori_punti_confronto <- colori_punti_serie(input$confronto_col_punti, colori_usati)
  
  xlim_confronto <- if (range_personalizzato_attivo(input$confronto_range_x_min, input$confronto_range_x_max)) c(input$confronto_range_x_min, input$confronto_range_x_max) else range(tutte_x, na.rm = TRUE)
  ylim_confronto <- if (range_personalizzato_attivo(input$confronto_range_y_min, input$confronto_range_y_max)) c(input$confronto_range_y_min, input$confronto_range_y_max) else range(tutte_y, na.rm = TRUE)
  plot(1, type = "n", xlim = xlim_confronto, ylim = ylim_confronto,
       main = if (isTRUE(input$mostra_titolo)) "Model Comparison: Overlaid Fitted Curves" else "",
       xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y), las = 1)
  if (is.null(input$mostra_griglia) || isTRUE(input$mostra_griglia)) grid(col = "gray90", lty = "dotted")
  
  if (mostra_punti) {
    for (i in seq_along(lista)) {
      if (!is.null(lista[[i]]$x_dati)) {
        points(lista[[i]]$x_dati, lista[[i]]$y_dati, pch = pch_punti_confronto, col = colori_punti_confronto[i], cex = cex_punti)
      }
    }
  }
  
  for (i in seq_along(lista)) {
    ord <- order(lista[[i]]$x_griglia)
    lines(lista[[i]]$x_griglia[ord], lista[[i]]$y_curva[ord], col = colori_usati[i], lwd = lwd_curve, lty = lty_curve_confronto)
  }
  
  if (mostra_legenda) {
    cex_legenda_confronto <- if (is.null(input$confronto_cex_legenda)) 0.85 else input$confronto_cex_legenda
    pos_legenda_confronto <- if (is.null(input$confronto_pos_legenda)) "topright" else input$confronto_pos_legenda
    legend(pos_legenda_confronto, legend = sapply(lista, function(s) s$label), col = colori_usati, lwd = lwd_curve, lty = lty_curve_confronto, bty = "o", cex = cex_legenda_confronto, bg = "white")
  }
}

output$grafico_confronto <- renderPlot({
  par(family = "sans")
  disegna_grafico_confronto()
}, res = 100)

# ---- Single fits in a single image (one panel per model) ----
# Same models, colours and style as the overlay plot - but each in its
# own panel, with its own data (saved when it was added to the
# comparison) and its own curve only.
colori_confronto_pannelli <- function(n_modelli) {
  colori_da_palette(if (is.null(input$confronto_palette)) "fissa" else input$confronto_palette, n_modelli)
}

disegna_pannello_confronto <- function(i, lista, colori) {
  s <- lista[[i]]
  mostra_punti <- if (is.null(input$mostra_punti_confronto)) TRUE else input$mostra_punti_confronto
  lwd_curve <- if (is.null(input$confronto_lwd_curve)) 2.5 else input$confronto_lwd_curve
  cex_punti <- if (is.null(input$confronto_cex_punti)) 1 else input$confronto_cex_punti
  pch_punti <- as.numeric(if (is.null(input$confronto_pch_punti)) "16" else input$confronto_pch_punti)
  lty_curve <- as.numeric(if (is.null(input$confronto_lty_curve)) "1" else input$confronto_lty_curve)
  
  if (assi_condivisi_composito()) {
    tutte_x <- unlist(lapply(lista, function(m) c(m$x_griglia, m$x_dati)))
    tutte_y <- unlist(lapply(lista, function(m) c(m$y_curva, m$y_dati)))
  } else {
    tutte_x <- c(s$x_griglia, s$x_dati)
    tutte_y <- c(s$y_curva, s$y_dati)
  }
  xlim_p <- if (range_personalizzato_attivo(input$confronto_range_x_min, input$confronto_range_x_max)) c(input$confronto_range_x_min, input$confronto_range_x_max) else range(tutte_x[is.finite(tutte_x)])
  ylim_p <- if (range_personalizzato_attivo(input$confronto_range_y_min, input$confronto_range_y_max)) c(input$confronto_range_y_min, input$confronto_range_y_max) else range(tutte_y[is.finite(tutte_y)])
  
  par(mar = c(5, 5, 4, 2) + 0.1)
  # Title = label of the model, always shown (it identifies the panel).
  plot(1, type = "n", xlim = xlim_p, ylim = ylim_p, main = s$label,
       xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y), las = 1)
  if (is.null(input$mostra_griglia) || isTRUE(input$mostra_griglia)) grid(col = "gray90", lty = "dotted")
  if (mostra_punti && !is.null(s$x_dati)) points(s$x_dati, s$y_dati, pch = pch_punti, col = colori_punti_serie(input$confronto_col_punti, colori)[i], cex = cex_punti)
  ord <- order(s$x_griglia)
  lines(s$x_griglia[ord], s$y_curva[ord], col = colori[i], lwd = lwd_curve, lty = lty_curve)
}

output$scarica_confronto_fit_singoli <- downloadHandler(
  filename = function() nome_file_composito("model_comparison_single_fits"),
  content = function(file) {
    lista <- confronto_modelli()
    if (length(lista) == 0) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No models added yet."))
    } else {
      colori <- colori_confronto_pannelli(length(lista))
      crea_immagine_composita(file, length(lista), function(i) disegna_pannello_confronto(i, lista, colori))
    }
  }
)

# ---- Parameter Distributions (Monte Carlo): one plot per parameter ----
# One box/violin per model, from the whole bootstrap distribution (not only
# the point estimate) - only for the models fitted with Monte Carlo, and only
# if they all share the same set of parameter names (same rule
# already used for the significance test and the post-hoc: a comparison
# "by parameter" among structurally different models would make no sense).
dati_boxplot_confronto <- function() {
  lista_mc <- Filter(function(m) !is.null(m$mat_boot) && nrow(m$mat_boot) >= 10, confronto_modelli())
  if (length(lista_mc) < 2) return(NULL)
  nomi_rif <- names(lista_mc[[1]]$parametri)
  if (!all(sapply(lista_mc, function(m) identical(names(m$parametri), nomi_rif)))) return(NULL)
  list(lista_mc = lista_mc, nomi_par = nomi_rif, mappa = lista_mc[[1]]$simboli_modello)
}

disegna_boxplot_confronto_pannello <- function(i) {
  d <- dati_boxplot_confronto()
  par(mar = c(6, 4, 3, 1))
  if (is.null(d) || i > length(d$nomi_par)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Need at least 2 Monte Carlo models sharing the same parameters.")
    return(invisible(NULL))
  }
  col_box <- if (is.null(input$col_boxplot_confronto)) "#f39c12" else input$col_boxplot_confronto
  mostra_outlier <- if (is.null(input$mostra_outlier_boxplot_confronto)) TRUE else input$mostra_outlier_boxplot_confronto
  col_bordo_box <- if (is.null(input$col_bordo_boxplot_confronto)) "black" else input$col_bordo_boxplot_confronto
  fnt_cex_box <- if (is.null(input$fnt_cex_boxplot_confronto)) 0.8 else input$fnt_cex_boxplot_confronto
  tipo_grafico_box <- if (is.null(input$tipo_grafico_boxplot_confronto)) "boxplot" else input$tipo_grafico_boxplot_confronto
  ordina_crescente_box <- if (is.null(input$ordina_boxplot_confronto)) FALSE else input$ordina_boxplot_confronto
  
  nome_par <- d$nomi_par[i]
  dati_lista <- lapply(d$lista_mc, function(m) m$mat_boot[, nome_par])
  names(dati_lista) <- sapply(d$lista_mc, function(m) m$label)
  if (ordina_crescente_box) {
    mediane <- sapply(dati_lista, median, na.rm = TRUE)
    dati_lista <- dati_lista[order(mediane)]
  }
  simbolo <- interpreta_etichetta(if (!is.null(d$mappa) && !is.null(d$mappa[[nome_par]])) d$mappa[[nome_par]] else simbolo_automatico_pedice(nome_par))
  mostra_x <- titolo_x_attivo(input$mostra_titolo_x_boxplot_confronto)
  righe_nomi <- prepara_margine_nomi_ruotati(names(dati_lista), fnt_cex_box, con_titolo = mostra_x)
  if (tipo_grafico_box == "violin") {
    disegna_violin_gruppi(dati_lista, main = simbolo, ylab = "Value", col = col_box, col_bordo = col_bordo_box, cex_axis = fnt_cex_box)
  } else {
    boxplot(dati_lista, main = simbolo, ylab = "Value", las = 2, col = col_box, border = col_bordo_box,
            outline = mostra_outlier, cex.axis = fnt_cex_box)
  }
  titolo_asse_x_ruotato("Model", righe_nomi, mostra_x)
}

registra_sezione_pannelli("box_confronto",
                          n_pannelli = function() { d <- dati_boxplot_confronto(); if (is.null(d)) 0L else length(d$nomi_par) },
                          disegna = function(k) disegna_boxplot_confronto_pannello(k),
                          prefisso_file = "model_comparison_boxplot")

output$sezione_boxplot_confronto_ui <- renderUI({
  d <- dati_boxplot_confronto()
  if (is.null(d)) return(NULL)
  ui_sezione_pannelli("box_confronto", length(d$nomi_par), "Parameter Distributions (Monte Carlo):",
    "One plot per parameter, one box/violin per model, built from the full Monte Carlo bootstrap distributions (not just the point estimates) \u2014 shows the actual spread and overlap between models at a glance. Boxplot vs Violin Plot is chosen in these plots' Aesthetics (\U0001F3A8).",
    "btn_estetica_boxplot_confronto")
})

# ---- Post-Hoc Comparison (3+ models) ----
# Reuses calcola_cochran_q() and assegna_lettere_cld() already defined in
# batch_elaboration.R (same scope shared among all the server_chunks) -
# no duplicated logic. Unlike the batch, here an additional check
# is needed before proceeding: all the chosen models must share
# exactly the same parameter names (same requirement already imposed
# on the pairwise test above), otherwise a comparison "by parameter" among
# structurally different models would have no meaning.
output$selezione_modelli_posthoc <- renderUI({
  lista <- confronto_modelli()
  if (length(lista) < 3) return(p(style = "font-size: 12px; color: #7f8c8d;", "At least 3 models are needed for a post-hoc comparison."))
  etichette <- sapply(lista, function(s) s$label)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("mc_posthoc_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("mc_posthoc_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("modelli_posthoc_scelti", "Models to include:", choices = etichette, selected = etichette, inline = TRUE)
  )
})
observeEvent(input$mc_posthoc_select_all, {
  lista <- confronto_modelli()
  etichette <- sapply(lista, function(s) s$label)
  updateCheckboxGroupInput(session, "modelli_posthoc_scelti", selected = etichette)
})
observeEvent(input$mc_posthoc_deselect_all, {
  updateCheckboxGroupInput(session, "modelli_posthoc_scelti", selected = character(0))
})

# Converts a raw parameter name into its plain Unicode symbol,
# using the map captured in the record of the FIRST model of the list (the
# check on compatible names already present above guarantees that it is
# valid for all the selected models, since they share the same
# parameter names in order).
etichetta_parametro_confronto <- function(nome_par, modello_riferimento) {
  mappa <- modello_riferimento$simboli_modello
  if (!is.null(mappa) && !is.null(mappa[[nome_par]])) {
    simbolo_a_testo_semplice(mappa[[nome_par]])
  } else {
    simbolo_a_testo_semplice(simbolo_automatico_pedice(nome_par))
  }
}

output$sezione_posthoc_confronto_ui <- renderUI({
  ris <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Omnibus Test (Cochran's Q, per parameter):"),
      tags$button(id = "btn_copia_omnibus_confronto", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('tabella_omnibus_confronto', 'btn_copia_omnibus_confronto')")
    ),
    tableOutput("tabella_omnibus_confronto"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Pairwise Comparisons:"),
      tags$button(id = "btn_copia_posthoc_confronto", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('tabella_posthoc_confronto', 'btn_copia_posthoc_confronto')")
    ),
    tags$em(style = "font-size: 12px; color: #566573;", textOutput("info_posthoc_confronto", inline = TRUE)),
    tableOutput("tabella_posthoc_confronto"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Compact Letter Display:"),
      tags$button(id = "btn_copia_cld_confronto", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('tabella_cld_confronto', 'btn_copia_cld_confronto')")
    ),
    p(style = "font-size: 12px; color: #566573;",
      "Same format as a classic ANOVA post-hoc: models sharing a letter for a given parameter are ", tags$strong("not"), " significantly different from each other; a different letter means they are."),
    tableOutput("tabella_cld_confronto")
  )
})

confronto_posthoc_risultato <- eventReactive(input$esegui_posthoc_confronto, {
  lista_completa <- confronto_modelli()
  etichette_scelte <- input$modelli_posthoc_scelti
  if (is.null(etichette_scelte) || length(etichette_scelte) < 3) {
    return(list(successo = FALSE, messaggio = "Select at least 3 models."))
  }
  etichette_tutte <- sapply(lista_completa, function(s) s$label)
  lista <- lista_completa[match(etichette_scelte, etichette_tutte)]
  names(lista) <- etichette_scelte
  
  n_param_tutti <- sapply(lista, function(m) m$n_param)
  if (length(unique(n_param_tutti)) > 1) {
    return(list(successo = FALSE, messaggio = "The selected models don't all have the same number of parameters. Cannot compare."))
  }
  nomi_riferimento <- names(lista[[1]]$parametri)
  tutti_stessi_nomi <- all(sapply(lista, function(m) identical(names(m$parametri), nomi_riferimento)))
  if (!tutti_stessi_nomi) {
    return(list(successo = FALSE, messaggio = "The selected models don't all share the same parameter names in the same order. Parameters can only be compared one-to-one when their names match exactly."))
  }
  
  nomi_parametri <- nomi_riferimento
  
  # 1. Omnibus test (Cochran's Q), one row per parameter - uses the raw
  # name for the internal lookups, the replaced symbol only for the
  # Parameter column shown (same logic used in the batch).
  # Standard error of the omnibus test consistent with the pairwise comparisons: if
  # all models have the Monte Carlo distributions, the bootstrap standard
  # error is used (standard deviation of the distribution), as already
  # in Batch; otherwise the linear one. Previously the linear one was always
  # used, even when the pairs were compared with the bootstrap
  # distributions, and with losses other than MSE (without linear errors) the test
  # could not be computed.
  omnibus_bootstrap <- all(sapply(lista, function(m) !is.null(m$mat_boot) && nrow(m$mat_boot) >= MINIMO_ITERAZIONI_MC))
  righe_omnibus <- lapply(nomi_parametri, function(nome_par) {
    stime <- sapply(lista, function(m) m$parametri[[nome_par]])
    errori <- if (omnibus_bootstrap) sapply(lista, function(m) sd(m$mat_boot[, nome_par], na.rm = TRUE)) else sapply(lista, function(m) m$errori[[nome_par]])
    q_ris <- calcola_cochran_q(stime, errori)
    sig <- if (is.na(q_ris$p_value)) "N/A" else as.character(symnum(q_ris$p_value, corr = FALSE, na = FALSE, 
                                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
    data.frame(Parameter = etichetta_parametro_confronto(nome_par, lista[[1]]), N_Models = length(lista), 
               Cochran_Q = round(q_ris$Q, 3), DF = q_ris$df, 
               P_Value = if (is.na(q_ris$p_value)) "N/A" else format.pval(q_ris$p_value, digits = 3, eps = 0.001),
               Significant = sig, stringsAsFactors = FALSE)
  })
  df_omnibus <- do.call(rbind, righe_omnibus)
  
  # 2. Post-hoc: all possible pairs, reusing calcola_confronto_coppia
  # (the same function as the pairwise test above), then correction for multiple
  # tests applied separately for each parameter. tab$Parameter
  # here is ALREADY the replaced symbol (calcola_confronto_coppia already applies it
  # internally).
  metodo_correzione <- if (is.null(input$metodo_correzione_confronto)) "holm" else input$metodo_correzione_confronto
  nomi_modelli <- names(lista)
  coppie <- combn(nomi_modelli, 2, simplify = FALSE)
  
  righe_posthoc <- lapply(coppie, function(coppia) {
    confronto <- calcola_confronto_coppia(lista[[coppia[1]]], lista[[coppia[2]]], input$livello_confidenza, lista[[coppia[1]]]$simboli_modello)
    tab <- confronto$tabella
    data.frame(Model_A = coppia[1], Model_B = coppia[2], Parameter = tab$Parameter,
               Value_A = tab$Value_A, Value_B = tab$Value_B, Absolute_Diff = tab$Absolute_Diff,
               P_Value_Raw = tab$P_Value_Raw, stringsAsFactors = FALSE)
  })
  df_posthoc <- do.call(rbind, righe_posthoc)
  
  # The correction compares with the LABELLED name (nomi_parametri_etichettati),
  # consistent with df_posthoc$Parameter (already replaced).
  nomi_parametri_etichettati <- sapply(nomi_parametri, function(p) etichetta_parametro_confronto(p, lista[[1]]))
  df_posthoc$P_Value_Adjusted <- NA_real_
  for (nome_par_etichettato in nomi_parametri_etichettati) {
    idx <- df_posthoc$Parameter == nome_par_etichettato
    df_posthoc$P_Value_Adjusted[idx] <- p.adjust(df_posthoc$P_Value_Raw[idx], method = metodo_correzione)
  }
  df_posthoc$Significant <- ifelse(is.na(df_posthoc$P_Value_Adjusted), "N/A",
                                   as.character(symnum(df_posthoc$P_Value_Adjusted, corr = FALSE, na = FALSE,
                                                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns"))))
  df_posthoc$P_Value_Adjusted <- ifelse(is.na(df_posthoc$P_Value_Adjusted), "N/A", format.pval(df_posthoc$P_Value_Adjusted, digits = 3, eps = 0.001))
  df_posthoc$Value_A <- round(df_posthoc$Value_A, 5)
  df_posthoc$Value_B <- round(df_posthoc$Value_B, 5)
  df_posthoc$Absolute_Diff <- round(df_posthoc$Absolute_Diff, 5)
  
  # 3. Compact Letter Display, per parameter - same distinction between raw
  # name (for the lookup on lista[[m]]$parametri, indexed by raw
  # name) / labelled name (for the filter on df_posthoc, already replaced).
  righe_cld <- lapply(seq_along(nomi_parametri), function(i) {
    nome_par_grezzo <- nomi_parametri[i]
    nome_par_etichettato <- nomi_parametri_etichettati[i]
    sotto <- df_posthoc[df_posthoc$Parameter == nome_par_etichettato, ]
    coppie_sig <- list()
    for (j in seq_len(nrow(sotto))) {
      if (identical(sotto$Significant[j], "ns") || identical(sotto$Significant[j], "N/A")) next
      a <- sotto$Model_A[j]; b <- sotto$Model_B[j]
      coppie_sig[[a]] <- c(coppie_sig[[a]], b)
    }
    lettere <- assegna_lettere_cld(nomi_modelli, coppie_sig)
    valori <- sapply(nomi_modelli, function(m) lista[[m]]$parametri[[nome_par_grezzo]])
    # unname(): avoids the warning "row.names ... discarded" (as in Batch).
    data.frame(Parameter = unname(nome_par_etichettato), Model = nomi_modelli, Value = round(unname(valori), 5), Group = lettere,
               stringsAsFactors = FALSE, row.names = NULL)
  })
  df_cld <- do.call(rbind, righe_cld)
  
  df_posthoc$P_Value_Raw <- NULL
  
  list(successo = TRUE, omnibus = df_omnibus, posthoc = df_posthoc, cld = df_cld,
       metodo_correzione = metodo_correzione, n_confronti = length(coppie) * length(nomi_parametri))
})

output$tabella_omnibus_confronto <- renderTable({
  ris <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Select at least 3 models above and click 'Run Post-Hoc Comparison'."))
  if (!ris$successo) return(data.frame(Message = ris$messaggio))
  tab <- ris$omnibus
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$tabella_posthoc_confronto <- renderTable({
  ris <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(NULL)
  tab <- ris$posthoc
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$tabella_cld_confronto <- renderTable({
  ris <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(NULL)
  ris$cld
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$info_posthoc_confronto <- renderText({
  ris <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return("")
  # The correction is applied parameter by parameter (previously the text said
  # "across all parameters, corrected with...", as if it were a single one).
  n_par <- max(length(unique(ris$posthoc$Parameter)), 1)
  paste0(ris$n_confronti, " pairwise comparisons in total (", ris$n_confronti %/% n_par, " per parameter); p-values corrected within each parameter with the ",
         etichetta_correzione(ris$metodo_correzione), " method.", descrizione_correzione(ris$metodo_correzione))
})

output$scarica_confronto_csv <- downloadHandler(
  filename = function() { paste0("model_comparison_", Sys.Date(), ".csv") },
  content = function(file) {
    df <- tabella_confronto_ordinata()
    if (is.null(df)) return()
    df_param <- tabella_confronto_parametri_df()
    ris_test <- tryCatch(test_parametri_risultato(), error = function(e) NULL)
    df_test <- if (!is.null(ris_test) && isTRUE(ris_test$successo)) ris_test$tabella else data.frame()
    mat_evidenza <- tryCatch(matrice_evidenza_akaike(), error = function(e) NULL)
    
    cat("--- MODEL COMPARISON (GOODNESS-OF-FIT) ---\n", file = file)
    write.table(df, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    if (!is.null(df_param)) {
      cat("\n--- PARAMETER ESTIMATES COMPARISON ---\n", file = file, append = TRUE)
      write.table(df_param, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    if (nrow(df_test) > 0) {
      cat("\n--- PARAMETER SIGNIFICANCE TEST ---\n", file = file, append = TRUE)
      write.table(df_test, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    } else {
      cat("\n--- PARAMETER SIGNIFICANCE TEST ---\n(Not calculated yet: select two models and click \"Run Test\" before exporting to include it here.)\n", file = file, append = TRUE)
    }
    if (!is.null(mat_evidenza)) {
      cat("\n--- PAIRWISE EVIDENCE RATIO MATRIX (AKAIKE WEIGHTS) ---\n", file = file, append = TRUE)
      df_evidenza <- cbind(Model = rownames(mat_evidenza), as.data.frame(mat_evidenza))
      write.table(df_evidenza, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    ris_posthoc <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      cat("\n--- POST-HOC COMPARISON: OMNIBUS TEST (COCHRAN'S Q, 3+ MODELS) ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$omnibus, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON: PAIRWISE (", toupper(etichetta_correzione(ris_posthoc$metodo_correzione)), " CORRECTED WITHIN EACH PARAMETER) ---\n", file = file, append = TRUE, sep = "")
      write.table(ris_posthoc$posthoc, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON: COMPACT LETTER DISPLAY ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$cld, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
  }
)

output$scarica_confronto_excel <- downloadHandler(
  filename = function() { paste0("model_comparison_", Sys.Date(), ".xlsx") },
  content = function(file) {
    df_gof <- tabella_confronto_ordinata()
    df_param <- tabella_confronto_parametri_df()
    if (is.null(df_gof)) return()
    ris_test <- tryCatch(test_parametri_risultato(), error = function(e) NULL)
    df_test <- if (!is.null(ris_test) && isTRUE(ris_test$successo)) ris_test$tabella else data.frame()
    mat_evidenza <- tryCatch(matrice_evidenza_akaike(), error = function(e) NULL)
    
    fogli <- list(Model_Comparison = df_gof)
    if (!is.null(df_param)) fogli$Parameter_Estimates <- df_param
    fogli$Significance_Test <- df_test
    if (!is.null(mat_evidenza)) fogli$Evidence_Ratio_Matrix <- cbind(Model = rownames(mat_evidenza), as.data.frame(mat_evidenza))
    ris_posthoc <- tryCatch(confronto_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      fogli$PostHoc_Omnibus <- ris_posthoc$omnibus
      fogli$PostHoc_Pairwise <- ris_posthoc$posthoc
      fogli$PostHoc_CLD <- ris_posthoc$cld
    }
    writexl::write_xlsx(per_excel(fogli), path = file)
  }
)

output$scarica_grafico_confronto <- downloadHandler(
  filename = function() paste0("model_comparison_plot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") {
      pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    } else {
      jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    }
    disegna_grafico_confronto()
    dev.off()
  }
)

# =========================================================================
# SIGNIFICANCE TEST BETWEEN PARAMETERS OF TWO USER-SELECTED MODELS
# =========================================================================

output$selezione_test_a_ui <- renderUI({
  lista <- confronto_modelli()
  scelte <- if (length(lista) > 0) sapply(lista, function(s) s$label) else character(0)
  selectInput("modello_test_a", "Model A:", choices = scelte)
})

output$selezione_test_b_ui <- renderUI({
  lista <- confronto_modelli()
  scelte <- if (length(lista) > 0) sapply(lista, function(s) s$label) else character(0)
  selezionato_default <- if (length(scelte) >= 2) scelte[length(scelte)] else if (length(scelte) == 1) scelte[1] else NULL
  selectInput("modello_test_b", "Model B:", choices = scelte, selected = selezionato_default)
})

test_parametri_risultato <- eventReactive(input$esegui_test_parametri, {
  lista <- confronto_modelli()
  
  # 1. Check that there are at least 2 models in the comparison
  if (length(lista) < 2) {
    return(list(successo = FALSE, messaggio = "You must add at least 2 models to the comparison to run the test."))
  }
  
  # 2. Retrieve the two freely-chosen models from the user (no longer fixed to the "last two")
  labels_tutte <- sapply(lista, function(s) s$label)
  idx1 <- which(labels_tutte == input$modello_test_a)
  idx2 <- which(labels_tutte == input$modello_test_b)
  if (length(idx1) == 0 || length(idx2) == 0) {
    return(list(successo = FALSE, messaggio = "Select two valid models from the dropdowns above."))
  }
  if (idx1[1] == idx2[1]) {
    return(list(successo = FALSE, messaggio = "Select two DIFFERENT models to compare (Model A and Model B are currently the same)."))
  }
  m1 <- lista[[idx1[1]]]
  m2 <- lista[[idx2[1]]]
  
  # 3. Check whether the number of parameters is equal
  if (m1$n_param != m2$n_param) {
    return(list(successo = FALSE, messaggio = paste0(
      "The models have a different number of parameters (", 
      m1$label, ": ", m1$n_param, " vs ", m2$label, ": ", m2$n_param, "). Cannot compare.")))
  }
  
  # 3b. Check that the parameters are the SAME (by name), not just by count.
  # Comparing only by position would risk pairing up parameters from
  # different models that share the same count but not the same physical
  # meaning (e.g. a Weibull's shape parameter against a Gompertz's growth rate).
  if (!identical(names(m1$parametri), names(m2$parametri))) {
    return(list(successo = FALSE, messaggio = paste0(
      "The two models don't share the same parameter names in the same order (",
      m1$label, ": ", paste(names(m1$parametri), collapse = ", "), " vs ",
      m2$label, ": ", paste(names(m2$parametri), collapse = ", "),
      "). Parameters can only be compared one-to-one when their names match exactly.")))
  }
  
  # 4. The actual statistical comparison: same shared function already
  # used in Batch Elaboration (calcola_confronto_coppia, in
  # batch_elaboration.R) - the "records" of Model Comparison use exactly
  # the same field names (parametri, errori, mat_boot, obs), so
  # no logic duplicated between the two sections.
  risultato_confronto <- calcola_confronto_coppia(m1, m2, input$livello_confidenza, m1$simboli_modello)
  df_test <- risultato_confronto$tabella
  metodo_usato <- risultato_confronto$metodo
  
  # 3c. Warning (non-blocking, but included in the result) if the two models
  # are of a different type: even with identical names, the same symbol can
  # represent different quantities in different models.
  avviso_tipo_diverso <- if (m1$tipo_modello != m2$tipo_modello) {
    paste0("Warning: you are comparing two DIFFERENT model types (", m1$tipo_modello, " vs ", m2$tipo_modello,
           "). Even with matching parameter names, they may not represent the same physical quantity.")
  } else {
    NULL
  }
  
  list(successo = TRUE, tabella = df_test[, setdiff(colnames(df_test), "P_Value_Raw")], m1_label = m1$label, m2_label = m2$label, avviso = avviso_tipo_diverso, metodo = metodo_usato)
})

output$metodo_test_parametri <- renderText({
  ris <- tryCatch(test_parametri_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return("")
  paste("Method used:", ris$metodo)
})

output$tabella_test_parametri <- renderTable({
  ris <- tryCatch(test_parametri_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Select two models above and click 'Run Test'."))
  if (!ris$successo) return(data.frame(Message = ris$messaggio))
  tab <- ris$tabella
  tab$Parameter <- sapply(tab$Parameter, function(n) simbolo_a_html(simbolo_visualizzazione(n)))
  # Explicit formatting with sprintf() for guaranteed control over the
  # decimal digits shown; it applies to whatever numeric column is
  # present (the structure changes between the empirical version and Welch's t-test),
  # excluding Parameter/P_Value (already text)/Significant (already text).
  colonne_numeriche <- setdiff(colnames(tab), c("Parameter", "P_Value", "Significant"))
  for (col in colonne_numeriche) {
    if (is.numeric(tab[[col]])) tab[[col]] <- sprintf("%.3f", tab[[col]])
  }
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

observeEvent(input$esegui_test_parametri, {
  ris <- tryCatch(test_parametri_risultato(), error = function(e) NULL)
  if (!is.null(ris) && isTRUE(ris$successo) && !is.null(ris$avviso)) {
    showNotification(ris$avviso, type = "warning", duration = 8)
  }
}, priority = -1)  # low priority: runs after test_parametri_risultato() has already been evaluated
# =========================================================================
# SHELF-LIFE & KINETIC MODEL ANALYSIS (on fits accumulated in Model Comparison)
# =========================================================================
# Exactly the same logic already verified in shelf_life.R (Batch), here
# applied to the single fits accumulated with "Add Current Fit to Comparison"
# instead of to the samples of a batch loaded via CSV - reuses the same
# shared functions (costruisci_fonte_sl, trova_radice_campione,
# trova_distribuzione_sl_mc, calcola_se_sl_delta_method,
# costruisci_tabella_sl_per_temperatura, costruisci_tabella_arrhenius,
# REGISTRO_MODELLI_SL), not a rewrite.

# ---- Table associating temperature -> accumulated fit ----
output$mc_sl_tabella_temperature <- renderUI({
  lista <- confronto_modelli()
  if (length(lista) < 3) return(p(style = "font-size: 12px; color: #7f8c8d;", "Add at least 3 fits to the comparison first."))
  unita_scelta <- if (is.null(input$mc_sl_unita_temp) || input$mc_sl_unita_temp == "celsius") "\u00b0C" else "K"
  righe <- lapply(seq_along(lista), function(i) {
    fluidRow(
      column(5, tags$div(style = "padding-top: 7px; font-size: 13px;", lista[[i]]$label)),
      column(7, numericInput(paste0("mc_sl_temp_", id_sicuro_campione(lista[[i]]$label, i)), NULL, value = lista[[i]]$temperatura))
    )
  })
  tagList(
    fluidRow(column(5, tags$strong("Fit")), column(7, tags$strong(paste0("Temp. (", unita_scelta, ")")))),
    righe
  )
})

# Persists every temperature entered in the corresponding record in
# confronto_modelli(), not only in the numericInput field on screen - without
# this, adding a new fit (which makes the renderUI above run again,
# since it depends on confronto_modelli()) would rebuild the whole table
# and lose the temperatures already entered for the previous fits.
observe({
  lista <- confronto_modelli()
  if (length(lista) < 3) return()
  for (i in seq_along(lista)) {
    id_campo <- paste0("mc_sl_temp_", id_sicuro_campione(lista[[i]]$label, i))
    valore <- input[[id_campo]]
    if (!is.null(valore) && !is.na(valore) && !identical(valore, lista[[i]]$temperatura)) {
      lista[[i]]$temperatura <- valore
      confronto_modelli(lista)
    }
  }
})

# ---- Critical Y Value: pre-filled with the mean of all the observed Y
# values in the accumulated fits - same logic already used in Batch.
output$mc_sl_y_critico_ui <- renderUI({
  lista <- confronto_modelli()
  valore_default <- 50
  if (length(lista) > 0) {
    tutti_y <- unlist(lapply(lista, function(s) s$y_dati))
    if (length(tutti_y) > 0) valore_default <- round(mean(tutti_y, na.rm = TRUE), 3)
  }
  numericInput("mc_sl_y_critico", "Critical Y Value:", value = valore_default)
})

# Pre-filled with the maximum X observed among all the accumulated fits (the
# same "overlaid plot" cited) - same logic already used in Batch.
output$mc_sl_x_max_ui <- renderUI({
  lista <- confronto_modelli()
  valore_default <- 1000
  if (length(lista) > 0) {
    tutti_x <- unlist(lapply(lista, function(s) s$x_dati))
    if (length(tutti_x) > 0) valore_default <- round(max(tutti_x, na.rm = TRUE), 3)
  }
  numericInput("mc_sl_x_max", "X Search Range Max:", value = valore_default)
})

# ---- Dynamic guess fields, generated based on the chosen kinetic model -
# same logic already used in Batch.
output$mc_sl_guess_dinamici <- renderUI({
  modello_scelto <- REGISTRO_MODELLI_SL[[input$mc_sl_modello_cinetico]]
  if (is.null(modello_scelto) || input$mc_sl_modello_cinetico == "custom") return(NULL)
  nomi_par <- modello_scelto$parametri
  campi <- lapply(nomi_par, function(p) {
    descrizione <- if (p %in% names(DESCRIZIONI_PARAMETRI_SL)) DESCRIZIONI_PARAMETRI_SL[[p]] else p
    numericInput(paste0("mc_sl_guess_", p), paste0("Guess for ", p, " (", descrizione, "):"), value = modello_scelto$guess_default[[p]])
  })
  tagList(campi, p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "If the fit doesn't converge, try adjusting these."))
})

# ---- Formula of the selected kinetic model, shown centred -
# same logic already used in Batch.
output$mc_sl_grafico_formula <- renderPlot({
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  plot.new()
  modello_scelto <- sl_ottieni_modello(input$mc_sl_modello_cinetico, input$mc_sl_formula_custom, input$mc_sl_guess_custom)
  if (is.null(modello_scelto)) {
    disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f8f9fa", border = "#dee2e6", lwd = 1.5)
    text(0.5, 0.5, if (identical(input$mc_sl_modello_cinetico, "custom")) "Enter a valid custom formula and guesses above." else "Choose a kinetic model above.", cex = 1.0, col = "#7f8c8d")
    return(invisible(NULL))
  }
  formula_sostituita <- modello_scelto$formula
  nomi_tutti <- modello_scelto$parametri[order(-nchar(modello_scelto$parametri))]
  mappa_sl <- as.list(modello_scelto$simboli)
  for (nome in nomi_tutti) {
    simbolo <- if (!is.null(mappa_sl[[nome]])) mappa_sl[[nome]] else simbolo_automatico_pedice(nome)
    if (!identical(simbolo, nome)) formula_sostituita <- gsub(paste0("\\b", nome, "\\b"), simbolo, formula_sostituita)
  }
  disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f0faf8", border = "#18bc9c", lwd = 2)
  
  divisione <- dividi_formula_lunga(formula_sostituita)
  lunghezza_riferimento <- max(nchar(divisione$riga1), if (!is.null(divisione$riga2)) nchar(divisione$riga2) else 0)
  # Same calibration correction applied to the main formula
  # (Single Fit) and to the Batch copy of this same Shelf-Life formula.
  riferimento_cex <- 65
  cex_adattivo <- if (lunghezza_riferimento <= riferimento_cex) 1.5 else max(0.55, 1.5 * (riferimento_cex / lunghezza_riferimento)^0.6)
  
  if (is.null(divisione$riga2)) {
    espr <- tryCatch(str2expression(paste0("y == ", divisione$riga1)), error = function(e) NULL)
    if (is.null(espr) || length(espr) != 1) espr <- tryCatch(str2expression(paste0("y == ", modello_scelto$formula)), error = function(e) NULL)
    if (!is.null(espr) && length(espr) == 1) {
      tryCatch({ text(0.5, 0.5, espr[[1]], cex = cex_adattivo, font = 3, col = "black") },
               error = function(e) { text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black") })
    } else {
      text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black")
    }
  } else {
    espr1 <- tryCatch(str2expression(paste0("y == ", divisione$riga1)), error = function(e) NULL)
    espr2 <- tryCatch(str2expression(divisione$riga2), error = function(e) NULL)
    if (!is.null(espr1) && length(espr1) == 1 && !is.null(espr2) && length(espr2) == 1) {
      tryCatch({
        text(0.5, 0.64, espr1[[1]], cex = cex_adattivo, font = 3, col = "black")
        text(0.5, 0.36, espr2[[1]], cex = cex_adattivo, font = 3, col = "black")
      }, error = function(e) { text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black") })
    } else {
      text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black")
    }
  }
})

output$mc_sl_grafico_formula_contenitore <- renderUI({
  modello_scelto <- sl_ottieni_modello(input$mc_sl_modello_cinetico, input$mc_sl_formula_custom, input$mc_sl_guess_custom)
  altezza <- 70
  if (!is.null(modello_scelto)) {
    formula_sostituita <- modello_scelto$formula
    nomi_tutti <- modello_scelto$parametri[order(-nchar(modello_scelto$parametri))]
    mappa_sl <- as.list(modello_scelto$simboli)
    for (nome in nomi_tutti) {
      simbolo <- if (!is.null(mappa_sl[[nome]])) mappa_sl[[nome]] else simbolo_automatico_pedice(nome)
      if (!identical(simbolo, nome)) formula_sostituita <- gsub(paste0("\\b", nome, "\\b"), simbolo, formula_sostituita)
    }
    divisione <- dividi_formula_lunga(formula_sostituita)
    if (!is.null(divisione$riga2)) altezza <- 120
  }
  plotOutput("mc_sl_grafico_formula", height = paste0(altezza, "px"))
})

mc_sl_risultato <- reactiveVal(NULL)

output$sezione_mc_sl_risultati_ui <- renderUI({
  ris <- mc_sl_risultato()
  if (is.null(ris)) return(NULL)
  tagList(
    br(),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Shelf Life per Fit:"),
      tags$button(id = "btn_copia_mc_sl_per_temperatura", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('mc_sl_tabella_per_temperatura', 'btn_copia_mc_sl_per_temperatura')")
    ),
    fluidRow(
      column(6, selectInput("mc_sl_temp_sort_by", "Sort By:", choices = c("Sample" = "Sample", "Temperature (\u00b0C)" = "Temperature_C", "Shelf Life" = "Shelf_Life", "CI Lower" = "CI_Lower", "CI Upper" = "CI_Upper", "Source" = "Source"))),
      column(6, selectInput("mc_sl_temp_sort_dir", "Order:", choices = c("Ascending" = "asc", "Descending" = "desc")))
    ),
    tableOutput("mc_sl_tabella_per_temperatura"),
    
    br(),
    uiOutput("sezione_barre_mc_sl_ui"),
    
    br(),
    uiOutput("sezione_boxplot_mc_sl_ui"),
    
    br(),
    tags$h4("Post-Hoc Comparison (3+ Fits):"),
    p(style = "font-size: 12px; color: #566573;",
      "Compares the shelf life estimated for each fit above (not the kinetic model yet), same logic as the Post-Hoc Comparison above."),
    uiOutput("mc_sl_selezione_campioni_posthoc"),
    fluidRow(
      column(6, selectInput("mc_sl_metodo_correzione_posthoc", "Multiple-Comparison Correction:", 
                            choices = SCELTE_CORREZIONE_MULTIPLA, selected = "holm")),
      column(6, br(), actionButton("mc_sl_esegui_posthoc", "Run Post-Hoc Comparison", class = "btn-primary btn-sm w-100", icon = icon("calculator")))
    ),
    uiOutput("sezione_mc_sl_posthoc_ui"),
    
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Kinetic Model Parameters:"),
      tags$button(id = "btn_copia_mc_sl_arrhenius", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('mc_sl_tabella_arrhenius', 'btn_copia_mc_sl_arrhenius')")
    ),
    p(style = "font-size: 11px; color: #21618c; font-style: italic;", descrivi_tipo_modellazione_sl(ris)),
    tableOutput("mc_sl_tabella_arrhenius"),
    tags$em(style = "font-size: 12px; color: #566573;", textOutput("mc_sl_info_bootstrap", inline = TRUE)),
    
    br(), br(),
    tags$h4("Shelf-Life vs Temperature Plot:"),
    fluidRow(
      column(6, checkboxInput("mc_sl_mostra_ci", "Show Confidence Band (curve position)", value = TRUE)),
      column(6, checkboxInput("mc_sl_mostra_pi", "Show Prediction Band (incl. sample scatter)", value = FALSE))
    ),
    if (isTRUE(ris$n_arr_successo >= MINIMO_ITERAZIONI_MC)) {
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the nested bootstrap (percentiles of the bootstrap curves).")
    } else {
      tagList(
        p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
          "Bands from the linear approximation of the kinetic model (delta method); enable \u201cPropagate uncertainty (nested bootstrap)\u201d above for bootstrap bands."),
        conditionalPanel(condition = "input.mc_sl_mostra_ci",
          div(style = "margin-left: 20px;",
            checkboxInput("mc_sl_banda_simultanea", "Simultaneous confidence band (Bates & Watts, 1988)", value = FALSE)))
      )
    },
    plotOutput("mc_sl_grafico_arrhenius", height = "420px"),
    div(style = "margin-top: 8px; margin-bottom: 20px;", 
        downloadButton("mc_sl_scarica_grafico", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_mc_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
    
    tags$h4("Predict Shelf Life at a New Temperature:"),
    fluidRow(
      column(6, numericInput("mc_sl_temp_predizione", "Temperature:", value = 25)),
      column(6, br())
    ),
    verbatimTextOutput("mc_sl_predizione_risultato"),
    
    div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 6px; margin-top: 15px; margin-bottom: 25px; max-width: 50%;",
        tags$h5(style = "margin-top: 0; color: #2c3e50;", icon("file-export"), " Export Shelf-Life Results"),
        downloadButton("mc_sl_scarica_csv", "Download as CSV", class = "btn-outline-secondary btn-sm"),
        downloadButton("mc_sl_scarica_excel", "Download as Excel (.xlsx)", class = "btn-outline-secondary btn-sm")
    )
  )
})

observeEvent(input$mc_sl_esegui_analisi, {
  # Clears the stale Post-Hoc of any previous analysis: without
  # this, it remained visible (computed on sources/fits by then replaced)
  # until the user pressed "Run Post-Hoc" explicitly again.
  mc_sl_posthoc_risultato(NULL)
  
  lista <- confronto_modelli()
  if (length(lista) < 3) { showNotification("Add at least 3 fits to the comparison first.", type = "error"); return() }
  nomi <- sapply(lista, function(s) s$label)
  
  temperature_raw <- sapply(seq_along(lista), function(i) input[[paste0("mc_sl_temp_", id_sicuro_campione(nomi[i], i))]])
  mancanti <- nomi[sapply(temperature_raw, function(v) is.null(v) || is.na(v))]
  if (length(mancanti) > 0) {
    showNotification(paste("Enter a temperature for every fit first. Missing:", paste(mancanti, collapse = ", ")), type = "error", duration = 8)
    return()
  }
  temperature_num <- as.numeric(temperature_raw)
  names(temperature_num) <- nomi
  temperature_K <- if (input$mc_sl_unita_temp == "celsius") temperature_num + 273.15 else temperature_num
  if (any(temperature_K <= 0)) {
    showNotification("Temperatures in Kelvin must be positive - check the unit selected and the values entered.", type = "error")
    return()
  }
  
  x_min <- input$mc_sl_x_min; x_max <- input$mc_sl_x_max
  if (is.na(x_min) || is.na(x_max) || x_min >= x_max) {
    showNotification("Check the X Search Range (min must be less than max).", type = "error")
    return()
  }
  y_critico <- input$mc_sl_y_critico
  
  # Each accumulated fit has ITS OWN formula (they can be models
  # different from each other, unlike the batch where all samples
  # share the same model) - costruisci_fonte_sl() uses it for
  # each fit individually, so this works correctly even
  # if the accumulated fits are of different models.
  fonti <- list()
  withProgress(message = "Finding shelf life for each fit...", value = 0, {
    for (i in seq_along(lista)) {
      s <- lista[[i]]
      res_adattato <- list(parametri = s$parametri, mat_boot = s$mat_boot, cov = s$cov, rmse = s$rmse, df = s$df, ponderazione = s$ponderazione, pesi = s$pesi)
      fonti[[nomi[i]]] <- costruisci_fonte_sl(res_adattato, s$formula, y_critico, x_min, x_max, input$mc_sl_tipo_intervallo)
      incProgress(1 / length(lista))
    }
  })
  
  falliti <- names(Filter(function(f) !isTRUE(f$successo), fonti))
  if (length(falliti) > 0) {
    showNotification(paste("Could not find a shelf life for:", paste(falliti, collapse = ", "), "- try widening the X Search Range."), type = "warning", duration = 10)
  }
  fonti_ok <- fonti[sapply(fonti, function(f) isTRUE(f$successo))]
  if (length(fonti_ok) < 3) {
    showNotification("Need at least 3 fits with a valid shelf life estimate to fit the kinetic model.", type = "error")
    return()
  }
  
  T_K_vec <- temperature_K[names(fonti_ok)]
  sl_puntuali <- sapply(fonti_ok, function(f) f$puntuale)
  
  modello_scelto <- sl_ottieni_modello(input$mc_sl_modello_cinetico, input$mc_sl_formula_custom, input$mc_sl_guess_custom)
  if (is.null(modello_scelto)) {
    if (identical(input$mc_sl_modello_cinetico, "custom")) {
      showNotification("Enter a valid custom formula and initial guesses first (check the syntax and that only allowed functions/parameters/x are used).", type = "error", duration = 8)
    } else {
      showNotification("Choose a kinetic model first.", type = "error")
    }
    return()
  }
  formula_modello <- modello_scelto$formula
  nomi_parametri_modello <- modello_scelto$parametri
  
  if (identical(input$mc_sl_modello_cinetico, "custom")) {
    guess_modello <- setNames(as.numeric(modello_scelto$guess_default[nomi_parametri_modello]), nomi_parametri_modello)
  } else {
    guess_letti <- sapply(nomi_parametri_modello, function(p) input[[paste0("mc_sl_guess_", p)]])
    if (any(sapply(guess_letti, function(v) is.null(v) || is.na(v)))) {
      showNotification(paste("Enter an initial guess for every parameter of the", modello_scelto$label, "model first."), type = "error")
      return()
    }
    guess_modello <- setNames(as.numeric(guess_letti), nomi_parametri_modello)
  }
  
  obiettivo_arr <- function(par) {
    valori_completi <- setNames(as.list(par), names(guess_modello))
    y_pred <- valuta_formula_generica(formula_modello, valori_completi, T_K_vec)
    calcola_loss_generica(y_pred, sl_puntuali, "mse")
  }
  modello_sl <- function(p) valuta_formula_generica(formula_modello, setNames(as.list(p), names(guess_modello)), T_K_vec)
  jacobiano_sl_base <- crea_jacobiano_modello(formula_modello, names(guess_modello))
  jacobiano_sl <- function(p) jacobiano_sl_base(p, T_K_vec)
  fit_puntuale <- fit_modello_secondario(modello_sl, sl_puntuali, guess_modello, jacobiano_sl)
  if (is.null(fit_puntuale) || !isTRUE(fit_puntuale$convergence == 0)) {
    showNotification(paste0("The ", modello_scelto$label, " fit on the point estimates did not converge - try a different initial guess, or check the shelf life values and temperatures found below."), type = "error", duration = 10)
    return()
  }
  
  if (isTRUE(input$mc_sl_usa_bootstrap)) {
    n_boot_arr <- if (is.null(input$mc_sl_n_boot)) 1000 else min(max(round(input$mc_sl_n_boot), 100), 5000)
    mat_arr_boot <- matrix(NA_real_, n_boot_arr, length(nomi_parametri_modello))
    withProgress(message = paste0("Computing nested ", modello_scelto$label, " bootstrap..."), value = 0, {
      for (b in seq_len(n_boot_arr)) {
        sl_iterazione <- sapply(fonti_ok, function(f) {
          if (f$tipo == "empirico") sample(f$valori, 1)
          else if (f$tipo == "gaussiano") rnorm(1, f$media, f$se)
          else f$puntuale
        })
        obiettivo_b <- function(par) {
          valori_completi <- setNames(as.list(par), names(guess_modello))
          y_pred <- valuta_formula_generica(formula_modello, valori_completi, T_K_vec)
          calcola_loss_generica(y_pred, sl_iterazione, "mse")
        }
        fit_b <- fit_modello_secondario(modello_sl, sl_iterazione, fit_puntuale$par, jacobiano_sl)
        if (!is.null(fit_b) && isTRUE(fit_b$convergence == 0)) mat_arr_boot[b, ] <- fit_b$par
        if (b %% 20 == 0) incProgress(20 / n_boot_arr)
      }
    })
    colnames(mat_arr_boot) <- names(guess_modello)
    n_arr_successo <- sum(!is.na(mat_arr_boot[, 1]))
  } else {
    # Bootstrap disabled: same safe representation already tested in
    # Batch (empty matrix but with the same column names).
    n_boot_arr <- 0
    mat_arr_boot <- matrix(NA_real_, 0, length(nomi_parametri_modello), dimnames = list(NULL, names(guess_modello)))
    n_arr_successo <- 0
  }
  
  # Standard errors from the Hessian of the point fit - same formula
  # verified in Batch: the optimized loss actually returns the
  # raw sum (SSE), not a mean despite the name "mse" - so
  # 2*SSE/(n-p) * inverse(Hessian_of_SSE) applies directly.
  n_oss_arr <- length(fonti_ok)
  df_den_arr <- n_oss_arr - length(nomi_parametri_modello)
  # Covariance from the Jacobian of the model, s^2 (J^T J)^-1, not from the numerical
  # Hessian of optim (inaccurate with correlated parameters such as Ea and q).
  cov_hessiana <- covarianza_minimi_quadrati(modello_sl, setNames(fit_puntuale$par, names(guess_modello)), fit_puntuale$value, df_den_arr, jacobiano_sl)
  errori_hessiana <- if (!is.null(cov_hessiana)) sqrt(diag(cov_hessiana)) else NULL
  if (!is.null(errori_hessiana)) names(errori_hessiana) <- names(guess_modello)
  
  mc_sl_risultato(list(successo = TRUE, fonti = fonti_ok, T_K = T_K_vec, sl_puntuali = sl_puntuali,
                     fit_puntuale = fit_puntuale$par, mat_arr_boot = mat_arr_boot, n_arr_successo = n_arr_successo,
                     errori_hessiana = errori_hessiana, cov_modello = cov_hessiana, df_cinetico = df_den_arr,
                     n_arr_totale = n_boot_arr, y_critico = y_critico, unita_temp = input$mc_sl_unita_temp,
                     formula_modello = formula_modello, modello_label = modello_scelto$label,
                     simboli_modello = as.list(modello_scelto$simboli)))
  if (n_boot_arr > 0) {
    showNotification(paste0(modello_scelto$label, " analysis complete: ", n_arr_successo, "/", n_boot_arr, " bootstrap iterations successful."),
                      type = if (n_arr_successo >= n_boot_arr * 0.5) "message" else "warning", duration = 8)
  } else {
    showNotification(paste0(modello_scelto$label, " analysis complete (point estimate only, bootstrap not computed)."), type = "message", duration = 6)
  }
})

output$mc_sl_tabella_per_temperatura <- renderTable({
  ris <- mc_sl_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the Shelf-Life analysis first."))
  df <- costruisci_tabella_sl_per_temperatura(ris, input$mc_sl_livello_confidenza)
  campo_ord <- if (is.null(input$mc_sl_temp_sort_by) || !(input$mc_sl_temp_sort_by %in% colnames(df))) "Sample" else input$mc_sl_temp_sort_by
  decrescente <- isTRUE(input$mc_sl_temp_sort_dir == "desc")
  tab <- df[order(df[[campo_ord]], decreasing = decrescente, na.last = TRUE), ]
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

# ---- Post-Hoc Comparison on the shelf-life times estimated per fit - same
# logic already used for Batch, reusing the same shared function
# costruisci_oggetto_confronto_sl() (defined in shelf_life.R).
# ---- Bar comparison of the shelf-life times among fits with
# Hessian/Linear uncertainty (Model Comparison) ----
output$grafico_barre_mc_sl <- renderPlot({
  ris <- mc_sl_risultato()
  if (is.null(ris)) { plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  disegna_barre_sl_condiviso(ris$fonti, "mc_sl_barre_")
}, res = 100)

output$sezione_barre_mc_sl_ui <- renderUI({
  ris <- mc_sl_risultato()
  fonti_gaussiane <- if (!is.null(ris)) Filter(function(f) identical(f$tipo, "gaussiano"), ris$fonti) else list()
  if (length(fonti_gaussiane) < 2) return(NULL)
  tagList(
    tags$h4("Shelf Life Comparison (Linear/Jacobian Method):"),
    p(style = "font-size: 12px; color: #566573;",
      "One bar per fit (its point estimate, with an error bar from the Jacobian/Linear-propagated standard error) \u2014 fits whose shelf life uncertainty came from Monte Carlo are not included here (they're in the boxplot/violin below instead)."),
    plotOutput("grafico_barre_mc_sl", height = "450px"),
    div(style = "margin-top: 8px;", 
        downloadButton("scarica_barre_mc_sl", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_barre_mc_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
  )
})

output$scarica_barre_mc_sl <- downloadHandler(
  filename = function() paste0("model_comparison_shelf_life_barchart.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    ris <- mc_sl_risultato()
    if (!is.null(ris)) disegna_barre_sl_condiviso(ris$fonti, "mc_sl_barre_")
    dev.off()
  }
)

# ---- Boxplot/violin comparison of the shelf-life times among fits (Model Comparison) ----
output$grafico_boxplot_mc_sl <- renderPlot({
  ris <- mc_sl_risultato()
  if (is.null(ris)) { plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  disegna_boxplot_sl_condiviso(ris$fonti, "mc_sl_box_")
}, res = 100)

output$sezione_boxplot_mc_sl_ui <- renderUI({
  ris <- mc_sl_risultato()
  fonti_empiriche <- if (!is.null(ris)) Filter(function(f) identical(f$tipo, "empirico") && length(f$valori) >= 10, ris$fonti) else list()
  if (length(fonti_empiriche) < 2) return(NULL)
  tagList(
    tags$h4("Shelf Life Distribution Across Fits:"),
    p(style = "font-size: 12px; color: #566573;",
      "One box/violin per fit, built from the full nested bootstrap distribution of its shelf life \u2014 fits whose shelf life uncertainty came from the Jacobian/Linear method are not included (only a point estimate and standard error exist for those, not a distribution)."),
    plotOutput("grafico_boxplot_mc_sl", height = "450px"),
    div(style = "margin-top: 8px;", 
        downloadButton("scarica_boxplot_mc_sl", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_boxplot_mc_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
  )
})

output$scarica_boxplot_mc_sl <- downloadHandler(
  filename = function() paste0("model_comparison_shelf_life_boxplot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    ris <- mc_sl_risultato()
    if (!is.null(ris)) disegna_boxplot_sl_condiviso(ris$fonti, "mc_sl_box_")
    dev.off()
  }
)

mc_sl_posthoc_risultato <- reactiveVal(NULL)

output$sezione_mc_sl_posthoc_ui <- renderUI({
  ris <- mc_sl_posthoc_risultato()
  if (is.null(ris)) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Omnibus Test (Cochran's Q):"),
      tags$button(id = "btn_copia_mc_sl_omnibus", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('mc_sl_tabella_omnibus_posthoc', 'btn_copia_mc_sl_omnibus')")
    ),
    tableOutput("mc_sl_tabella_omnibus_posthoc"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Pairwise Comparisons:"),
      tags$button(id = "btn_copia_mc_sl_posthoc", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('mc_sl_tabella_posthoc', 'btn_copia_mc_sl_posthoc')")
    ),
    tableOutput("mc_sl_tabella_posthoc"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Compact Letter Display:"),
      tags$button(id = "btn_copia_mc_sl_cld", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('mc_sl_tabella_cld_posthoc', 'btn_copia_mc_sl_cld')")
    ),
    tableOutput("mc_sl_tabella_cld_posthoc")
  )
})
output$mc_sl_selezione_campioni_posthoc <- renderUI({
  ris <- mc_sl_risultato()
  if (is.null(ris)) return(NULL)
  nomi <- names(ris$fonti)
  if (length(nomi) == 0) return(NULL)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("mc_sl_posthoc_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("mc_sl_posthoc_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("mc_sl_campioni_posthoc", "Fits to include:", choices = nomi, selected = nomi, inline = TRUE)
  )
})
observeEvent(input$mc_sl_posthoc_select_all, {
  ris <- mc_sl_risultato()
  if (is.null(ris)) return()
  updateCheckboxGroupInput(session, "mc_sl_campioni_posthoc", selected = names(ris$fonti))
})
observeEvent(input$mc_sl_posthoc_deselect_all, {
  updateCheckboxGroupInput(session, "mc_sl_campioni_posthoc", selected = character(0))
})

observeEvent(input$mc_sl_esegui_posthoc, {
  ris <- mc_sl_risultato()
  if (is.null(ris)) { mc_sl_posthoc_risultato(list(successo = FALSE, messaggio = "Run the Shelf-Life analysis first.")); return() }
  lista <- confronto_modelli()
  
  nomi_fonti <- names(ris$fonti)
  # If the user deselected some fit in the selector above, limits
  # the comparison to the chosen ones - if the selector has not yet been
  # rendered (NULL) uses all of them, unchanged behaviour.
  if (!is.null(input$mc_sl_campioni_posthoc)) nomi_fonti <- intersect(nomi_fonti, input$mc_sl_campioni_posthoc)
  oggetti <- lapply(nomi_fonti, function(nome) {
    idx <- match(nome, sapply(lista, function(s) s$label))
    df_residuo <- if (!is.na(idx) && !is.null(lista[[idx]]$obs) && !is.null(lista[[idx]]$parametri)) {
      max(lista[[idx]]$obs - length(lista[[idx]]$parametri), 1)
    } else 1
    costruisci_oggetto_confronto_sl(ris$fonti[[nome]], df_residuo)
  })
  names(oggetti) <- nomi_fonti
  nomi_validi <- names(oggetti)[sapply(oggetti, function(o) is.finite(o$errori[["Shelf_Life"]]) && o$errori[["Shelf_Life"]] > 0)]
  if (length(nomi_validi) < 3) {
    mc_sl_posthoc_risultato(list(successo = FALSE, messaggio = "Need at least 3 selected fits with a usable uncertainty (Monte Carlo or Linear) on their shelf life estimate.")); return()
  }
  oggetti <- oggetti[nomi_validi]
  
  stime <- sapply(oggetti, function(o) o$parametri[["Shelf_Life"]])
  errori <- sapply(oggetti, function(o) o$errori[["Shelf_Life"]])
  q_ris <- calcola_cochran_q(stime, errori)
  sig_omnibus <- if (is.na(q_ris$p_value)) "N/A" else as.character(symnum(q_ris$p_value, corr = FALSE, na = FALSE,
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
  df_omnibus <- data.frame(N_Fits = length(oggetti), Cochran_Q = round(q_ris$Q, 3), DF = q_ris$df,
                           P_Value = if (is.na(q_ris$p_value)) "N/A" else format.pval(q_ris$p_value, digits = 3, eps = 0.001),
                           Significant = sig_omnibus, stringsAsFactors = FALSE)
  
  metodo_correzione <- if (is.null(input$mc_sl_metodo_correzione_posthoc)) "holm" else input$mc_sl_metodo_correzione_posthoc
  nomi_fit <- names(oggetti)
  coppie <- combn(nomi_fit, 2, simplify = FALSE)
  righe_posthoc <- lapply(coppie, function(coppia) {
    confronto <- calcola_confronto_coppia(oggetti[[coppia[1]]], oggetti[[coppia[2]]], input$mc_sl_livello_confidenza)
    tab <- confronto$tabella
    data.frame(Fit_A = coppia[1], Fit_B = coppia[2], Value_A = tab$Value_A, Value_B = tab$Value_B,
               Absolute_Diff = tab$Absolute_Diff, P_Value_Raw = tab$P_Value_Raw, stringsAsFactors = FALSE)
  })
  df_posthoc <- do.call(rbind, righe_posthoc)
  df_posthoc$P_Value_Adjusted <- p.adjust(df_posthoc$P_Value_Raw, method = metodo_correzione)
  df_posthoc$Significant <- as.character(symnum(df_posthoc$P_Value_Adjusted, corr = FALSE, na = FALSE,
                                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
  df_posthoc$P_Value_Adjusted <- format.pval(df_posthoc$P_Value_Adjusted, digits = 3, eps = 0.001)
  df_posthoc$Value_A <- round(df_posthoc$Value_A, 3); df_posthoc$Value_B <- round(df_posthoc$Value_B, 3)
  df_posthoc$Absolute_Diff <- round(df_posthoc$Absolute_Diff, 3)
  df_posthoc$P_Value_Raw <- NULL
  
  coppie_sig <- list()
  for (i in seq_len(nrow(df_posthoc))) {
    if (identical(df_posthoc$Significant[i], "ns")) next
    a <- df_posthoc$Fit_A[i]; b <- df_posthoc$Fit_B[i]
    coppie_sig[[a]] <- c(coppie_sig[[a]], b)
  }
  lettere <- assegna_lettere_cld(nomi_fit, coppie_sig)
  df_cld <- data.frame(Fit = nomi_fit, Shelf_Life = round(stime, 3), Group = lettere, stringsAsFactors = FALSE)
  
  mc_sl_posthoc_risultato(list(successo = TRUE, omnibus = df_omnibus, posthoc = df_posthoc, cld = df_cld))
})

output$mc_sl_tabella_omnibus_posthoc <- renderTable({
  ris <- tryCatch(mc_sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Run the Post-Hoc Comparison above."))
  if (!isTRUE(ris$successo)) return(data.frame(Message = ris$messaggio))
  tab <- ris$omnibus
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$mc_sl_tabella_posthoc <- renderTable({
  ris <- tryCatch(mc_sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(data.frame(Message = "-"))
  tab <- ris$posthoc
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$mc_sl_tabella_cld_posthoc <- renderTable({
  ris <- tryCatch(mc_sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(data.frame(Message = "-"))
  tab <- ris$cld
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$mc_sl_tabella_arrhenius <- renderTable({
  ris <- mc_sl_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the Shelf-Life analysis first."))
  tab <- costruisci_tabella_arrhenius(ris, input$mc_sl_livello_confidenza)
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$mc_sl_info_bootstrap <- renderText({
  ris <- mc_sl_risultato()
  if (is.null(ris)) return("")
  if (ris$n_arr_totale == 0) {
    paste0("Model: ", ris$modello_label, ". Point estimate only (bootstrap not computed). x = temperature in Kelvin, R = 8.314462618 J/(mol\u00b7K) where applicable.")
  } else {
    paste0("Model: ", ris$modello_label, ". Nested bootstrap: ", ris$n_arr_successo, " / ", ris$n_arr_totale,
           " iterations successful. x = temperature in Kelvin, R = 8.314462618 J/(mol\u00b7K) where applicable.")
  }
})

disegna_grafico_mc_arrhenius <- function() {
  ris <- mc_sl_risultato()
  disegna_grafico_arrhenius_generico(ris, prefisso = "mc_sl_")
}

output$mc_sl_grafico_arrhenius <- renderPlot({ disegna_grafico_mc_arrhenius() })

output$mc_sl_scarica_grafico <- downloadHandler(
  filename = function() paste0("model_comparison_shelf_life_plot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_grafico_mc_arrhenius()
    dev.off()
  }
)

output$mc_sl_predizione_risultato <- renderPrint({
  ris <- mc_sl_risultato()
  if (is.null(ris)) { cat("Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  T_pred <- input$mc_sl_temp_predizione
  if (is.null(T_pred) || is.na(T_pred)) { cat("Enter a temperature."); return(invisible(NULL)) }
  T_pred_K <- if (ris$unita_temp == "celsius") T_pred + 273.15 else T_pred
  valori_boot <- ris$mat_arr_boot
  pred_boot <- apply(valori_boot, 1, function(par) {
    if (any(!is.finite(par))) return(NA_real_)
    valuta_formula_generica(ris$formula_modello, as.list(setNames(par, colnames(valori_boot))), T_pred_K)
  })
  pred_boot <- pred_boot[is.finite(pred_boot)]
  sl_puntuale_pred <- valuta_formula_generica(ris$formula_modello, as.list(ris$fit_puntuale), T_pred_K)
  cat(sprintf("Predicted Shelf Life at %.2f: %.4f\n", T_pred, sl_puntuale_pred))
  if (length(pred_boot) >= 10) {
    p_inf <- (1 - input$mc_sl_livello_confidenza / 100) / 2; p_sup <- 1 - p_inf
    ci <- quantile(pred_boot, probs = c(p_inf, p_sup), na.rm = TRUE)
    cat(sprintf("%s%% Confidence Interval: [%.4f, %.4f]\n", input$mc_sl_livello_confidenza, ci[1], ci[2]))
    cat(sprintf("(from %d/%d successful bootstrap iterations)", length(pred_boot), nrow(valori_boot)))
  } else {
    if (ris$n_arr_totale == 0) {
      cat("Confidence interval not available (bootstrap not computed).")
    } else {
      cat("Not enough successful bootstrap iterations to compute a confidence interval.")
    }
  }
})

output$mc_sl_scarica_csv <- downloadHandler(
  filename = function() "model_comparison_shelf_life_results.csv",
  content = function(file) {
    ris <- mc_sl_risultato()
    if (is.null(ris)) { writeLines("Run the Shelf-Life analysis first.", file); return() }
    df_sl <- costruisci_tabella_sl_per_temperatura(ris, input$mc_sl_livello_confidenza)
    df_arr <- costruisci_tabella_arrhenius(ris, input$mc_sl_livello_confidenza)
    write.csv(df_sl, file, row.names = FALSE)
    cat("\n", file = file, append = TRUE)
    suppressWarnings(write.table(df_arr, file, sep = ",", row.names = FALSE, append = TRUE))
    ris_posthoc <- tryCatch(mc_sl_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: OMNIBUS TEST (COCHRAN'S Q, 3+ FITS) ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$omnibus, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: PAIRWISE ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$posthoc, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: COMPACT LETTER DISPLAY ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$cld, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
  }
)

output$mc_sl_scarica_excel <- downloadHandler(
  filename = function() "model_comparison_shelf_life_results.xlsx",
  content = function(file) {
    ris <- mc_sl_risultato()
    if (is.null(ris)) return()
    df_sl <- costruisci_tabella_sl_per_temperatura(ris, input$mc_sl_livello_confidenza)
    df_arr <- costruisci_tabella_arrhenius(ris, input$mc_sl_livello_confidenza)
    df_info <- data.frame(Property = c("Model", "Critical Y Value", "Confidence Level", "Nested Bootstrap Iterations"),
                          Value = c(paste0(ris$modello_label, " (x in Kelvin, R = 8.314462618 J/(mol*K) where applicable)"), 
                                   as.character(ris$y_critico), paste0(input$mc_sl_livello_confidenza, "%"),
                                   if (ris$n_arr_totale == 0) "Not computed (point estimate only)" else paste0(ris$n_arr_successo, "/", ris$n_arr_totale)),
                          stringsAsFactors = FALSE)
    fogli <- list(Shelf_Life_Per_Temperature = df_sl, Kinetic_Model_Parameters = df_arr, Info = df_info)
    ris_posthoc <- tryCatch(mc_sl_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      fogli$PostHoc_Omnibus <- ris_posthoc$omnibus
      fogli$PostHoc_Pairwise <- ris_posthoc$posthoc
      fogli$PostHoc_CLD <- ris_posthoc$cld
    }
    writexl::write_xlsx(per_excel(fogli), path = file)
  }
)
