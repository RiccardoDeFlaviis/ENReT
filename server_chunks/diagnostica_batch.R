# =============================================================================
# RESIDUAL DIAGNOSTICS IN BATCH
# =============================================================================
# Below the "Goodness-of-Fit Diagnostics" table of Batch: "Compute
# residual diagnostics" button, summary table (one row per sample), selector
# of a sample or of "All samples" (arrangement by sample or by type of
# plot), single downloads, combined images and ZIP in folders.
# Reuses without modifying them the diagnostic functions of Single Fit
# (diagnostica_residui.R): each sample is transformed into the same form
# as the Single Fit result by res_da_campione_batch(). Prefix of the
# identifiers: bdr_ ("batch_diag_" is already used for the sorting of the
# goodness-of-fit table).
# =============================================================================

TIPI_GRAFICI_DIAGNOSTICI <- list(
  residui  = list(etichetta = "Residuals",          file = "residuals",          disegna = function(r, t = NULL) disegna_grafico_residui(r, t, limiti_personalizzati = FALSE)),
  qq       = list(etichetta = "Normal Q-Q plot",    file = "qq_plot",            disegna = function(r, t = NULL) disegna_grafico_qq(r, t, limiti_personalizzati = FALSE)),
  lag      = list(etichetta = "Lag plot",           file = "lag_plot",           disegna = function(r, t = NULL) disegna_grafico_lag(r, t)),
  varianza = list(etichetta = "Variance structure", file = "variance_structure", disegna = function(r, t = NULL) disegna_grafico_sd_media(r, t))
)
ID_TABELLE_BATCH <- list(runs = "bdr_tab_runs", lof = "bdr_tab_lof", omogeneita = "bdr_tab_omogeneita", repliche = "bdr_tab_repliche")
SCELTA_TUTTI_CAMPIONI <- "__tutti__"

nome_file_sicuro <- function(x) gsub("^_+|_+$", "", gsub("[^A-Za-z0-9._-]+", "_", x))

# A Batch sample in the form of the Single Fit result: data used
# in the fit (after any transformations), residuals recomputed from the formula
# exactly as in Batch, free and fixed parameters, weights.
res_da_campione_batch <- function(r, formula, fissi, loss) {
  if (is.null(r) || !isTRUE(r$successo) || is.null(r$x) || is.null(r$y)) return(NULL)
  prev <- tryCatch(valuta_formula_generica(formula, c(as.list(r$parametri), fissi), r$x), error = function(e) NULL)
  if (is.null(prev) || length(prev) != length(r$y) || any(!is.finite(prev))) return(NULL)
  list(successo = TRUE, df_originale = data.frame(x = r$x, y = r$y), x = r$x, residui = r$y - prev,
       parametri = r$parametri, formula = formula, fissi = fissi, criteri_non_disponibili = !identical(loss, "mse"),
       rmse = r$rmse, df = r$df, ponderazione = r$ponderazione, pesi = r$pesi)
}

# Summary table: one row per sample, numbers only.
riepilogo_diagnostica_batch <- function(lista_res) {
  righe <- lapply(names(lista_res), function(nm) {
    r <- lista_res[[nm]]
    if (is.null(r)) return(data.frame(Sample = nm, N = NA_integer_, Note = "fit failed or not available", stringsAsFactors = FALSE))
    dg <- tryCatch(diagnostica_residui(r), error = function(e) NULL)
    if (is.null(dg)) return(data.frame(Sample = nm, N = NA_integer_, Note = "diagnostics not available", stringsAsFactors = FALSE))
    riga <- data.frame(Sample = nm, N = dg$n,
      Lag1_Autocorrelation = signif(dg$lag$r1, 3), Lag1_Bound = signif(dg$lag$banda, 3),
      Runs_Z_Too_Few = if (isTRUE(dg$runs$disponibile)) signif(dg$runs$z_tf, 3) else NA_real_,
      Runs_P_Too_Few = if (isTRUE(dg$runs$disponibile)) signif(dg$runs$p_poche, 3) else NA_real_,
      LOF_F = if (isTRUE(dg$lof$disponibile)) signif(dg$lof$F, 4) else NA_real_,
      LOF_P = if (isTRUE(dg$lof$disponibile)) signif(dg$lof$p, 3) else NA_real_,
      Bartlett_P = if (isTRUE(dg$omogeneita$disponibile)) signif(dg$omogeneita$bartlett[["p"]], 3) else NA_real_,
      Brown_Forsythe_P = if (isTRUE(dg$omogeneita$disponibile) && !is.null(dg$omogeneita$brown_forsythe)) signif(dg$omogeneita$brown_forsythe[["p"]], 3) else NA_real_,
      b = if (isTRUE(dg$sd_media$disponibile)) signif(dg$sd_media$b, 3) else NA_real_,
      Zeta_2b = if (isTRUE(dg$sd_media$disponibile)) signif(2 * dg$sd_media$b, 3) else NA_real_,
      stringsAsFactors = FALSE)
    if (ponderazione_attiva(r)) riga$Weighting_Zeta <- signif(r$ponderazione$zeta, 4)
    riga
  })
  rbind_riempi_na(righe)
}

# Version for the screen: p < 0.05 and autocorrelation outside the band in
# orange, column names without underscores.
riepilogo_per_schermo <- function(df) {
  if (is.null(df)) return(NULL)
  evidenzia <- function(v, cond) ifelse(is.na(v), "", ifelse(cond & !is.na(cond), sprintf("<span style='color:#d35400;font-weight:bold;'>%s</span>", format(v)), format(v)))
  out <- df
  for (cl in intersect(c("Runs_P_Too_Few", "LOF_P", "Bartlett_P", "Brown_Forsythe_P"), names(df))) out[[cl]] <- evidenzia(df[[cl]], df[[cl]] < 0.05)
  if ("Lag1_Autocorrelation" %in% names(df)) out$Lag1_Autocorrelation <- evidenzia(df$Lag1_Autocorrelation, abs(df$Lag1_Autocorrelation) > df$Lag1_Bound)
  for (cl in names(out)) if (!is.character(out[[cl]])) out[[cl]] <- ifelse(is.na(out[[cl]]), "", format(out[[cl]]))
  colnames(out) <- gsub("_", " ", colnames(out))
  out
}

# ---- State ----
bdr_stato <- reactiveVal(NULL)
bdr_osservatori_stile <- character(0)
# A new Run of Batch invalidates the diagnostics previously computed.
observeEvent(batch_risultati(), { bdr_stato(NULL) }, ignoreInit = TRUE, ignoreNULL = FALSE)
bdr_valido <- function() { s <- bdr_stato(); if (!is.null(s) && identical(s$br, batch_risultati())) s else NULL }

dimensioni_bdr <- function() dimensioni_esportazione()
apri_dispositivo_bdr <- function(percorso, fattore_w = 1, fattore_h = 1) {
  d <- dimensioni_bdr()
  if (identical(input$formato_esportazione, "pdf")) pdf(percorso, width = d$width_in * fattore_w, height = d$height_in * fattore_h)
  else jpeg(percorso, width = round(d$width_px * fattore_w), height = round(d$height_px * fattore_h), res = d$dpi, quality = 95)
}
estensione_bdr <- function() if (identical(input$formato_esportazione, "pdf")) "pdf" else "jpg"

# ---- Computation ----
observeEvent(input$bdr_calcola, {
  br <- batch_risultati()
  if (is.null(br) || length(br$risultati) == 0) { showNotification("Run the Batch Elaboration first.", type = "warning"); return() }
  lista_res <- lapply(br$risultati, function(r) res_da_campione_batch(r, br$formula, br$fissi, br$loss_usata))
  names(lista_res) <- names(br$risultati)
  riepilogo <- riepilogo_diagnostica_batch(lista_res)
  nomi_ok <- names(lista_res)[!sapply(lista_res, is.null)]
  if (length(nomi_ok) == 0) { showNotification("No successfully fitted samples: residual diagnostics are not available.", type = "warning"); return() }
  bdr_stato(list(br = br, res = lista_res[nomi_ok], riepilogo = riepilogo, nomi = nomi_ok))
  
  # Style buttons of "All samples": residuals and lag plot open the panel
  # of the residuals, Q-Q plot and variance structure that of the Q-Q plot.
  # Each observer is created only once, even when recomputing.
  for (i in seq_along(nomi_ok)) for (tp in names(TIPI_GRAFICI_DIAGNOSTICI)) local({
    id <- paste0("bdr_allest_", i, "_", tp); pannello <- if (tp %in% c("residui", "lag")) "Residuals Plot" else "Q-Q Plot"
    if (!id %in% bdr_osservatori_stile) { bdr_osservatori_stile <<- c(bdr_osservatori_stile, id); observeEvent(input[[id]], { apri_estetica(pannello) }, ignoreInit = TRUE) }
  })
  # Plots and downloads for "All samples": one output per sample and type,
  # with the same identifier in both arrangements.
  for (i in seq_along(nomi_ok)) for (tp in names(TIPI_GRAFICI_DIAGNOSTICI)) local({
    ii <- i; tt <- tp; nm <- nomi_ok[ii]
    output[[paste0("bdr_all_", ii, "_", tt)]] <- renderPlot({
      s <- bdr_valido(); if (is.null(s)) return(NULL)
      par(family = "sans"); TIPI_GRAFICI_DIAGNOSTICI[[tt]]$disegna(s$res[[nm]], if (identical(input$bdr_disposizione, "tipo")) nm else NULL)
    })
    output[[paste0("bdr_alldl_", ii, "_", tt)]] <- downloadHandler(
      filename = function() paste0(nome_file_sicuro(nm), "_", TIPI_GRAFICI_DIAGNOSTICI[[tt]]$file, ".", estensione_bdr()),
      content = function(file) { s <- bdr_valido(); apri_dispositivo_bdr(file); on.exit(dev.off()); par(family = "sans")
        TIPI_GRAFICI_DIAGNOSTICI[[tt]]$disegna(s$res[[nm]], paste(TIPI_GRAFICI_DIAGNOSTICI[[tt]]$etichetta, "\u2013", nm)) })
  })
})

# ---- Interface ----
output$bdr_sezione <- renderUI({
  br <- batch_risultati()
  if (is.null(br) || length(br$risultati) == 0) return(NULL)
  s <- bdr_valido()
  tagList(
    hr(),
    tags$h4("Residual Diagnostics"),
    p(style = "font-size: 12px; color: #566573;",
      "The diagnostics of the Single Fit (residual plots, normal Q-Q plot, lag plot, variance structure, runs test and, with replicates, lack-of-fit and variance homogeneity tests) for each sample of the batch",
      if (isTRUE(br$pesi_batch$attiva)) ", computed on the weighted residuals" else "", "."),
    fluidRow(column(6, actionButton("bdr_calcola", "Compute Residual Diagnostics", class = "btn-primary btn-sm w-100", icon = icon("calculator")))),
    if (is.null(s)) p(style = "font-size: 12px; color: #7f8c8d; margin-top: 8px;", "Press the button to compute the diagnostics for the current batch results.")
    else tagList(
      titolo_con_copia("Summary for all samples", "bdr_riepilogo"),
      tags$style(HTML("#bdr_riepilogo { overflow-x: auto; } #bdr_riepilogo table td { white-space: nowrap; }")),
      tableOutput("bdr_riepilogo"),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "In orange: p-values below 0.05 and lag-1 autocorrelations outside their approximate 95% bounds. With many samples and several tests per sample, some significant results are expected by chance alone (about one in twenty tests): confirm an isolated signal with the plots of that sample. Empty cells: test not available (no replicates, too few replicates, or a loss other than MSE)."),
      fluidRow(
        column(5, selectInput("bdr_campione", "Sample:", choices = c(setNames(s$nomi, s$nomi), setNames(SCELTA_TUTTI_CAMPIONI, sprintf("All samples (%d plots)", 4 * length(s$nomi)))), selected = s$nomi[1])),
        column(7, conditionalPanel(condition = sprintf("input.bdr_campione == '%s'", SCELTA_TUTTI_CAMPIONI),
          radioButtons("bdr_disposizione", "Arrange by:", choices = c("Sample" = "campione", "Plot type" = "tipo"), selected = "campione", inline = TRUE)))
      ),
      uiOutput("bdr_vista")
    )
  )
})

output$bdr_riepilogo <- renderTable({ s <- bdr_valido(); if (is.null(s)) return(NULL); riepilogo_per_schermo(s$riepilogo) },
                                    sanitize.text.function = function(x) x, na = "")

# Selected sample
bdr_res_scelto <- reactive({ s <- bdr_valido(); nm <- input$bdr_campione
  if (is.null(s) || is.null(nm) || identical(nm, SCELTA_TUTTI_CAMPIONI) || !nm %in% s$nomi) NULL else s$res[[nm]] })
bdr_dg_scelto <- reactive({ r <- bdr_res_scelto(); if (is.null(r)) NULL else tryCatch(diagnostica_residui(r), error = function(e) NULL) })

output$bdr_vista <- renderUI({
  s <- bdr_valido(); if (is.null(s)) return(NULL)
  scelta <- input$bdr_campione; if (is.null(scelta)) return(NULL)
  bottone_stile <- function(id) actionButton(id, "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot (same style as the Single Fit diagnostic plots)")
  if (!identical(scelta, SCELTA_TUTTI_CAMPIONI)) {
    dg <- bdr_dg_scelto(); if (is.null(dg)) return(p("Diagnostics not available for this sample."))
    cella <- function(tp, est) column(6, plotOutput(paste0("bdr_g_", tp), height = "380px"),
      div(style = "margin-top: 8px; margin-bottom: 12px;", downloadButton(paste0("bdr_dl_", tp), "Download Plot", class = "btn-outline-secondary btn-sm"), if (!is.null(est)) bottone_stile(est)))
    return(tagList(
      fluidRow(cella("residui", "bdr_est_res"), cella("qq", "bdr_est_qq")),
      fluidRow(cella("lag", "bdr_est_lag"), cella("varianza", "bdr_est_var")),
      sezione_runs(dg, ID_TABELLE_BATCH), sezione_repliche(dg, ID_TABELLE_BATCH)))
  }
  per_tipo <- identical(input$bdr_disposizione, "tipo"); n <- length(s$nomi)
  intestazione <- tagList(
    p(style = "font-size: 12px; color: #d35400;", icon("triangle-exclamation"), sprintf(" %d plots (4 per sample): drawing them may take a while.", 4 * n)),
    div(style = "margin-bottom: 12px;",
      downloadButton("bdr_dl_composite", if (per_tipo) "Download combined plots (one image per plot type)" else "Download combined plots (one image per sample)", class = "btn-outline-primary btn-sm"),
      downloadButton("bdr_dl_zip", if (per_tipo) "Download all plots (ZIP, one folder per plot type)" else "Download all plots (ZIP, one folder per sample)", class = "btn-outline-secondary btn-sm")),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;", "Plot style: the same settings as the Single Fit diagnostic plots (\U0001F3A8 buttons in the single-sample view). Combined images use the layout settings of the combined image of the single fits."))
  cella <- function(i, tp, larghezza, altezza) column(larghezza, plotOutput(paste0("bdr_all_", i, "_", tp), height = altezza),
    div(style = "margin-top: 4px; margin-bottom: 12px;", downloadButton(paste0("bdr_alldl_", i, "_", tp), "Download", class = "btn-outline-secondary btn-sm"),
        bottone_stile(paste0("bdr_allest_", i, "_", tp))))
  blocchi <- if (!per_tipo) lapply(seq_len(n), function(i) tagList(tags$h5(style = "margin-top: 16px; color: #2c3e50;", s$nomi[i]),
      fluidRow(cella(i, "residui", 6, "340px"), cella(i, "qq", 6, "340px")), fluidRow(cella(i, "lag", 6, "340px"), cella(i, "varianza", 6, "340px"))))
    else lapply(names(TIPI_GRAFICI_DIAGNOSTICI), function(tp) tagList(tags$h5(style = "margin-top: 16px; color: #2c3e50;", TIPI_GRAFICI_DIAGNOSTICI[[tp]]$etichetta),
      do.call(fluidRow, lapply(seq_len(n), function(i) cella(i, tp, 4, "300px")))))
  tagList(intestazione, blocchi)
})

# Plots, downloads and tables of the selected sample
for (tp in names(TIPI_GRAFICI_DIAGNOSTICI)) local({
  tt <- tp
  output[[paste0("bdr_g_", tt)]] <- renderPlot({ r <- bdr_res_scelto(); if (is.null(r)) return(NULL); par(family = "sans"); TIPI_GRAFICI_DIAGNOSTICI[[tt]]$disegna(r) })
  output[[paste0("bdr_dl_", tt)]] <- downloadHandler(
    filename = function() paste0(nome_file_sicuro(input$bdr_campione), "_", TIPI_GRAFICI_DIAGNOSTICI[[tt]]$file, ".", estensione_bdr()),
    content = function(file) { r <- bdr_res_scelto(); apri_dispositivo_bdr(file); on.exit(dev.off()); par(family = "sans")
      TIPI_GRAFICI_DIAGNOSTICI[[tt]]$disegna(r, paste(TIPI_GRAFICI_DIAGNOSTICI[[tt]]$etichetta, "\u2013", input$bdr_campione)) })
})
output$bdr_tab_runs <- renderTable({ senza_underscore(tabelle_diagnostica_residui(bdr_dg_scelto())$Runs_Test) }, na = "")
output$bdr_tab_lof <- renderTable({ senza_underscore(tabelle_diagnostica_residui(bdr_dg_scelto())$Lack_of_Fit) }, na = "")
output$bdr_tab_omogeneita <- renderTable({ senza_underscore(tabelle_diagnostica_residui(bdr_dg_scelto())$Variance_Homogeneity) }, na = "")
output$bdr_tab_repliche <- renderTable({ senza_underscore(tabelle_diagnostica_residui(bdr_dg_scelto())$Replicates) }, na = "", digits = 6)
# Lag plot and variance structure use the style of residuals and Q-Q plot.
observeEvent(input$bdr_est_res, { apri_estetica("Residuals Plot") })
observeEvent(input$bdr_est_lag, { apri_estetica("Residuals Plot") })
observeEvent(input$bdr_est_qq, { apri_estetica("Q-Q Plot") })
observeEvent(input$bdr_est_var, { apri_estetica("Q-Q Plot") })

# ---- Multiple downloads ----
# Combined images, according to the arrangement: one per sample (4 panels)
# or one per type of plot (one panel per sample), in a ZIP.
scrivi_zip_bdr <- function(cartella, file) {
  file_generati <- list.files(cartella, recursive = TRUE)
  if (length(file_generati) == 0) { writeLines("No plots available: compute the residual diagnostics first.", file.path(cartella, "README.txt")); file_generati <- "README.txt" }
  cartella_corrente <- getwd(); on.exit(setwd(cartella_corrente)); setwd(cartella)
  zip::zip(zipfile = file, files = file_generati)
}
output$bdr_dl_composite <- downloadHandler(
  filename = function() paste0("batch_residual_diagnostics_combined_", Sys.Date(), ".zip"),
  content = function(file) {
    s <- bdr_valido(); cartella <- tempfile("bdr_composite_"); dir.create(cartella); ext <- estensione_bdr(); tipi <- TIPI_GRAFICI_DIAGNOSTICI
    if (!is.null(s)) {
      if (identical(input$bdr_disposizione, "tipo")) {
        for (tp in names(tipi)) tryCatch(crea_immagine_composita(file.path(cartella, paste0(tipi[[tp]]$file, "_all_samples.", ext)), length(s$nomi),
          function(k) { par(family = "sans"); tipi[[tp]]$disegna(s$res[[s$nomi[k]]], paste(tipi[[tp]]$etichetta, "\u2013", s$nomi[k])) }), error = function(e) NULL)
      } else {
        for (nm in s$nomi) tryCatch(crea_immagine_composita(file.path(cartella, paste0(nome_file_sicuro(nm), "_diagnostics.", ext)), length(tipi),
          function(k) { par(family = "sans"); tipi[[k]]$disegna(s$res[[nm]], paste(tipi[[k]]$etichetta, "\u2013", nm)) }), error = function(e) NULL)
      }
    }
    scrivi_zip_bdr(cartella, file)
  })
# All single plots, in folders: one per sample or one per type.
output$bdr_dl_zip <- downloadHandler(
  filename = function() paste0("batch_residual_diagnostics_plots_", Sys.Date(), ".zip"),
  content = function(file) {
    s <- bdr_valido(); cartella <- tempfile("bdr_zip_"); dir.create(cartella); ext <- estensione_bdr(); per_tipo <- identical(input$bdr_disposizione, "tipo")
    if (!is.null(s)) for (nm in s$nomi) for (tp in names(TIPI_GRAFICI_DIAGNOSTICI)) {
      sotto <- if (per_tipo) TIPI_GRAFICI_DIAGNOSTICI[[tp]]$file else nome_file_sicuro(nm)
      nome <- if (per_tipo) nome_file_sicuro(nm) else TIPI_GRAFICI_DIAGNOSTICI[[tp]]$file
      dir.create(file.path(cartella, sotto), showWarnings = FALSE)
      percorso <- file.path(cartella, sotto, paste0(nome, ".", ext))
      tryCatch({ apri_dispositivo_bdr(percorso); par(family = "sans"); TIPI_GRAFICI_DIAGNOSTICI[[tp]]$disegna(s$res[[nm]], paste(TIPI_GRAFICI_DIAGNOSTICI[[tp]]$etichetta, "\u2013", nm)); dev.off() },
               error = function(e) { if (!is.null(dev.list())) dev.off() })
    }
    scrivi_zip_bdr(cartella, file)
  })

# Summary table for the Batch exports (NULL if not computed
# for the current results).
riepilogo_diagnostica_per_export <- function() { s <- bdr_valido(); if (is.null(s)) NULL else s$riepilogo }
