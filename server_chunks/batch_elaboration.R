# =========================================================================
# BATCH ELABORATION
# =========================================================================
# Applies the model/settings already configured in the sidebar to multiple
# datasets in one pass. Deliberately uses a LEAN, separate pipeline instead
# of reusing elaborazione() as-is: no bootstrap curve/prediction matrices,
# no 1000-point dense curve - only what the two summary tables and the one
# overlay plot actually need, since this section is explicitly meant to be
# fast across many datasets, with no parallel processing available in this
# browser-based environment (see the sidebar's own notes on Monte Carlo
# cost for the same reason).

# ---- Reading the batch data, depending on the chosen input method ----

output$batch_selezione_colonna_id <- renderUI({
  file_input <- input$batch_file_lungo
  if (is.null(file_input)) return(NULL)
  intestazioni <- tryCatch({
    if (grepl("\\.(xlsx|xls)$", file_input$name, ignore.case = TRUE)) {
      colnames(readxl::read_excel(file_input$datapath, n_max = 0))
    } else {
      colnames(read.csv(file_input$datapath, nrows = 0))
    }
  }, error = function(e) NULL)
  if (is.null(intestazioni)) return(p(style = "color: #c0392b; font-size: 12px;", "Could not read column headers from this file."))
  tagList(
    selectInput("batch_colonna_id", "ID Column (groups the samples):", choices = intestazioni, selected = intestazioni[1]),
    # "None" uses a distinctive sentinel ("__nessuna__") instead of an
    # empty string: some browsers handle inconsistently the
    # options with an empty value in a drop-down menu (once
    # another column was chosen, "None" could no longer be reselected) -
    # a non-empty sentinel, but one practically impossible to
    # collide with a real column name, avoids the problem.
    selectInput("batch_colonna_temperatura", "Temperature Column (optional, for non-isothermal analysis below):",
                choices = c("None" = "__nessuna__", intestazioni), selected = "__nessuna__"),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
      "Can be the same column as the ID column (e.g. if samples are literally named after their storage temperature).")
  )
})

# Shared between this renderUI and leggi_dati_batch() further below (which
# redefines it identically inside itself for compatibility, but this one here
# is the one actually used by the preview, since leggi_dati_batch()
# defines it as a function NESTED in its own scope, not visible
# from outside).
leggi_file_generico <- function(path, nome_file) {
  if (grepl("\\.(xlsx|xls)$", nome_file, ignore.case = TRUE)) leggi_excel_auto_header(path)
  else leggi_csv_auto_header(path)
}

# Preview of the samples detected in the file (grouped by the ID column
# chosen above), as a checkable list - allows discarding some
# datasets BEFORE launching the whole elaboration, instead of having to
# remove them afterwards (useful above all here, since a single Long
# Format file can easily contain many samples together).
output$batch_selezione_campioni_lungo <- renderUI({
  file_input <- input$batch_file_lungo
  if (is.null(file_input) || is.null(input$batch_colonna_id)) return(NULL)
  df <- tryCatch(leggi_file_generico(file_input$datapath, file_input$name), error = function(e) NULL)
  if (is.null(df) || !(input$batch_colonna_id %in% colnames(df))) return(NULL)
  nomi_rilevati <- unique(as.character(df[[input$batch_colonna_id]]))
  if (length(nomi_rilevati) == 0) return(NULL)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("batch_lungo_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("batch_lungo_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("batch_campioni_lungo_scelti", paste0("Samples detected (", length(nomi_rilevati), "), uncheck any to exclude them from the batch:"),
                       choices = nomi_rilevati, selected = nomi_rilevati, inline = TRUE)
  )
})
observeEvent(input$batch_lungo_select_all, {
  file_input <- input$batch_file_lungo
  if (is.null(file_input) || is.null(input$batch_colonna_id)) return()
  df <- tryCatch(leggi_file_generico(file_input$datapath, file_input$name), error = function(e) NULL)
  if (is.null(df) || !(input$batch_colonna_id %in% colnames(df))) return()
  nomi_rilevati <- unique(as.character(df[[input$batch_colonna_id]]))
  updateCheckboxGroupInput(session, "batch_campioni_lungo_scelti", selected = nomi_rilevati)
})
observeEvent(input$batch_lungo_deselect_all, {
  updateCheckboxGroupInput(session, "batch_campioni_lungo_scelti", selected = character(0))
})

# ---- Example Datasets (real data) ----
output$batch_info_dataset_esempio <- renderUI({
  d <- DATASET_BATCH_ESEMPIO[[input$batch_dataset_esempio]]
  if (is.null(d)) return(NULL)
  div(style = "background-color: #f4f6f7; border-left: 4px solid #2980b9; padding: 8px 10px; margin-bottom: 8px; font-size: 12px;",
      p(style = "margin: 0 0 4px 0;", d$descrizione),
      p(style = "margin: 0 0 4px 0;", tags$strong("Units: "), d$unita),
      p(style = "margin: 0 0 4px 0;", tags$strong("Source: "), d$fonte),
      p(style = "margin: 0; color: #7f8c8d;", "Real data with no reference parameters. ",
        if (!is.null(d$temperature)) "The sample temperatures are loaded too, so the Shelf-Life and Non-Isothermal analyses below can be used." else ""))
})
output$batch_selezione_campioni_esempio <- renderUI({
  d <- DATASET_BATCH_ESEMPIO[[input$batch_dataset_esempio]]
  if (is.null(d)) return(NULL)
  nomi <- names(d$campioni)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("batch_esempio_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("batch_esempio_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("batch_campioni_esempio_scelti", paste0("Samples (", length(nomi), "), uncheck any to exclude them from the batch:"),
                       choices = nomi, selected = nomi, inline = TRUE)
  )
})
observeEvent(input$batch_esempio_select_all, {
  d <- DATASET_BATCH_ESEMPIO[[input$batch_dataset_esempio]]
  if (!is.null(d)) updateCheckboxGroupInput(session, "batch_campioni_esempio_scelti", selected = names(d$campioni))
})
observeEvent(input$batch_esempio_deselect_all, {
  updateCheckboxGroupInput(session, "batch_campioni_esempio_scelti", selected = character(0))
})

# Returns a NAMED list of data.frames (one per sample), each
# with columns x,y - or NULL with a message if reading fails.
leggi_dati_batch_grezzi <- function() {
  metodo <- input$batch_metodo_input
  
  if (metodo == "multi_csv") {
    file_input <- input$batch_file_multi
    if (is.null(file_input)) return(list(successo = FALSE, messaggio = "Choose at least one CSV file."))
    campioni <- list()
    nomi_visti <- character(0)
    nomi_duplicati_avvisati <- character(0)
    for (i in seq_len(nrow(file_input))) {
      df <- tryCatch(leggi_csv_auto_header(file_input$datapath[i]), error = function(e) NULL)
      if (is.null(df) || ncol(df) < 2) next
      indici_xy <- trova_colonne_xy_numeriche(df)
      if (is.null(indici_xy)) next
      df <- df[, indici_xy]
      colnames(df) <- c("x", "y")
      df$x <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df$x), fixed = TRUE)))
      df$y <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df$y), fixed = TRUE)))
      nome_campione <- tools::file_path_sans_ext(file_input$name[i])
      # Two files can have the SAME name (e.g. uploaded from different
      # folders, Shiny reports only the file name, not the full
      # path) - without this check, the second would silently overwrite
      # the first in the list, losing a whole sample
      # without any warning.
      if (nome_campione %in% nomi_visti) {
        nomi_duplicati_avvisati <- c(nomi_duplicati_avvisati, nome_campione)
        suffisso <- 2
        while (paste0(nome_campione, " (", suffisso, ")") %in% nomi_visti) suffisso <- suffisso + 1
        nome_campione <- paste0(nome_campione, " (", suffisso, ")")
      }
      nomi_visti <- c(nomi_visti, nome_campione)
      campioni[[nome_campione]] <- df[, c("x", "y")]
    }
    if (length(nomi_duplicati_avvisati) > 0) {
      showNotification(paste0("Some uploaded files had the same name (", paste(unique(nomi_duplicati_avvisati), collapse = ", "), 
                              ") - the duplicates were kept and renamed with a suffix, e.g. \"(2)\", instead of being silently dropped."), 
                       type = "warning", duration = 10)
    }
    if (length(campioni) == 0) return(list(successo = FALSE, messaggio = "None of the uploaded files could be read as valid x,y data."))
    return(list(successo = TRUE, campioni = campioni))
    
  } else if (metodo == "lungo") {
    file_input <- input$batch_file_lungo
    if (is.null(file_input)) return(list(successo = FALSE, messaggio = "Choose a CSV or Excel file."))
    if (is.null(input$batch_colonna_id)) return(list(successo = FALSE, messaggio = "Select the ID column first."))
    df <- tryCatch(leggi_file_generico(file_input$datapath, file_input$name), error = function(e) NULL)
    if (is.null(df)) return(list(successo = FALSE, messaggio = "Could not read the file."))
    df <- as.data.frame(df)
    colonna_id <- input$batch_colonna_id
    colonna_temp <- input$batch_colonna_temperatura
    ha_colonna_temp <- !is.null(colonna_temp) && colonna_temp != "__nessuna__" && colonna_temp %in% colnames(df)
    
    # Temperature extracted BEFORE renaming/discarding the columns for X and Y,
    # so it works correctly even if colonna_temp == colonna_id (a
    # user might use the temperature itself as the sample name) -
    # in that case setdiff() below excludes it only once anyway,
    # no conflict.
    temperature_da_colonna <- NULL
    if (ha_colonna_temp) {
      temperature_da_colonna <- tapply(df[[colonna_temp]], df[[colonna_id]], function(v) suppressWarnings(as.numeric(gsub(",", ".", as.character(v[1]), fixed = TRUE))))
    }
    
    altre_colonne <- setdiff(colnames(df), if (ha_colonna_temp) c(colonna_id, colonna_temp) else colonna_id)
    if (length(altre_colonne) < 2) return(list(successo = FALSE, messaggio = "The file needs at least an ID column plus two more columns (x and y)."))
    # The two columns are renamed together and by position. Previously one
    # at a time and by name: with a file whose columns are called exactly
    # "y" and "x", in this order (data loaded as y, x, the use case of
    # "Swap X and Y"), the first renaming created two "x" columns and the second
    # renamed both of them to "y", making x disappear (error
    # "replacement has 0 rows").
    indici_xy <- match(altre_colonne[1:2], colnames(df))
    colnames(df)[indici_xy] <- c("x", "y")
    df$x <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df$x), fixed = TRUE)))
    df$y <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df$y), fixed = TRUE)))
    if (all(is.na(df$x)) || all(is.na(df$y))) {
      return(list(successo = FALSE, messaggio = paste0("Columns \"", altre_colonne[1], "\" and \"", altre_colonne[2], "\" don't look numeric. If the file uses a comma as the decimal separator, make sure columns are separated by semicolons (;), not commas.")))
    }
    
    gruppi <- split(df[, c("x", "y")], df[[colonna_id]])
    if (length(gruppi) == 0) return(list(successo = FALSE, messaggio = "No groups found using the selected ID column."))
    # Applies the pre-selection made by the user in the checkable list
    # above the Run button, if present - deselected samples are not
    # even processed, instead of running the whole fit and then
    # having to discard them afterwards.
    if (!is.null(input$batch_campioni_lungo_scelti)) {
      gruppi <- gruppi[names(gruppi) %in% input$batch_campioni_lungo_scelti]
      if (length(gruppi) == 0) return(list(successo = FALSE, messaggio = "No samples selected \u2014 check at least one box above."))
      if (!is.null(temperature_da_colonna)) temperature_da_colonna <- temperature_da_colonna[names(temperature_da_colonna) %in% input$batch_campioni_lungo_scelti]
    }
    return(list(successo = TRUE, campioni = gruppi, temperature = temperature_da_colonna))
    
  } else if (metodo == "largo") {
    file_input <- input$batch_file_largo
    if (is.null(file_input)) return(list(successo = FALSE, messaggio = "Choose a CSV or Excel file."))
    df <- tryCatch(leggi_file_generico(file_input$datapath, file_input$name), error = function(e) NULL)
    if (is.null(df)) return(list(successo = FALSE, messaggio = "Could not read the file."))
    df <- as.data.frame(df)
    n_col <- ncol(df)
    if (n_col < 2 || n_col %% 2 != 0) return(list(successo = FALSE, messaggio = "Wide format requires an even number of columns (pairs of x,y)."))
    campioni <- list()
    nomi_visti_largo <- character(0)
    nomi_colonne <- colnames(df)
    for (i in seq(1, n_col, by = 2)) {
      x_vals <- suppressWarnings(as.numeric(df[[i]]))
      y_vals <- suppressWarnings(as.numeric(df[[i + 1]]))
      validi <- is.finite(x_vals) & is.finite(y_vals)
      if (sum(validi) < 3) next
      nome_x <- nomi_colonne[i]
      nome_campione <- if (grepl("_x$", nome_x, ignore.case = TRUE)) sub("_x$", "", nome_x, ignore.case = TRUE) else paste0("Sample_", (i + 1) %/% 2)
      # Same protection already applied to the multi-CSV upload: if two
      # pairs of columns produced the same sample name, the second
      # must not silently overwrite the first.
      if (nome_campione %in% nomi_visti_largo) {
        suffisso <- 2
        while (paste0(nome_campione, " (", suffisso, ")") %in% nomi_visti_largo) suffisso <- suffisso + 1
        nome_campione <- paste0(nome_campione, " (", suffisso, ")")
      }
      nomi_visti_largo <- c(nomi_visti_largo, nome_campione)
      campioni[[nome_campione]] <- data.frame(x = x_vals[validi], y = y_vals[validi])
    }
    if (length(campioni) == 0) return(list(successo = FALSE, messaggio = "No valid x,y column pairs found."))
    return(list(successo = TRUE, campioni = campioni))
    
  } else if (metodo == "largo_x_condivisa") {
    file_input <- input$batch_file_largo_x_condivisa
    if (is.null(file_input)) return(list(successo = FALSE, messaggio = "Choose a CSV or Excel file."))
    df <- tryCatch(leggi_file_generico(file_input$datapath, file_input$name), error = function(e) NULL)
    if (is.null(df)) return(list(successo = FALSE, messaggio = "Could not read the file."))
    df <- as.data.frame(df)
    n_col <- ncol(df)
    if (n_col < 2) return(list(successo = FALSE, messaggio = "This format needs at least 2 columns: a shared x, plus one or more y columns."))
    x_vals <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[1]]), fixed = TRUE)))
    if (all(is.na(x_vals))) return(list(successo = FALSE, messaggio = "The first column doesn't look numeric \u2014 it should be the shared x values."))
    nomi_colonne <- colnames(df)
    campioni <- list()
    nomi_visti_condivisa <- character(0)
    for (i in 2:n_col) {
      y_vals <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[i]]), fixed = TRUE)))
      validi <- is.finite(x_vals) & is.finite(y_vals)
      if (sum(validi) < 3) next
      # Sample name from the column header, if present and not a
      # placeholder name such as "V2" (assigned by R when the file does not have
      # a real header row) - otherwise numbered.
      nome_campione <- if (!is.null(nomi_colonne) && i <= length(nomi_colonne) && nzchar(nomi_colonne[i]) && !grepl("^V[0-9]+$", nomi_colonne[i])) nomi_colonne[i] else paste0("Sample_", i - 1)
      if (nome_campione %in% nomi_visti_condivisa) {
        suffisso <- 2
        while (paste0(nome_campione, " (", suffisso, ")") %in% nomi_visti_condivisa) suffisso <- suffisso + 1
        nome_campione <- paste0(nome_campione, " (", suffisso, ")")
      }
      nomi_visti_condivisa <- c(nomi_visti_condivisa, nome_campione)
      campioni[[nome_campione]] <- data.frame(x = x_vals[validi], y = y_vals[validi])
    }
    if (length(campioni) == 0) return(list(successo = FALSE, messaggio = "No valid y columns found (each needs at least 3 numeric points paired with x)."))
    return(list(successo = TRUE, campioni = campioni))
    
  } else if (metodo == "incolla") {
    testo <- input$batch_dati_incollati
    if (is.null(testo) || trimws(testo) == "") return(list(successo = FALSE, messaggio = "Paste some data first."))
    righe <- tryCatch(read.table(text = testo, header = FALSE, col.names = c("id", "x", "y"), fill = TRUE,
                                 colClasses = "character", sep = ""), error = function(e) NULL)
    if (is.null(righe) || nrow(righe) == 0) return(list(successo = FALSE, messaggio = "Could not parse the pasted data. Expected: sample_id, x, y (one row per point)."))
    # Same tolerance already used for Direct Input: the comma as
    # decimal separator (common when pasting from Excel under an Italian locale)
    # is converted before interpreting the values as numbers.
    x_num <- suppressWarnings(as.numeric(gsub(",", ".", righe$x, fixed = TRUE)))
    y_num <- suppressWarnings(as.numeric(gsub(",", ".", righe$y, fixed = TRUE)))
    righe_valide <- is.finite(x_num) & is.finite(y_num) & nchar(trimws(righe$id)) > 0
    if (sum(righe_valide) == 0) return(list(successo = FALSE, messaggio = "No valid rows found in the pasted data."))
    df_pulito <- data.frame(id = righe$id[righe_valide], x = x_num[righe_valide], y = y_num[righe_valide])
    campioni <- split(df_pulito[, c("x", "y")], df_pulito$id)
    return(list(successo = TRUE, campioni = campioni))
    
  } else if (metodo == "esempio") {
    # Example datasets embedded in the app (DATASET_BATCH_ESEMPIO in
    # model_registry.R): same structure returned by the Long format with a
    # temperature column, so everything downstream (fit,
    # Shelf-Life, Non-Isothermal) works without changes.
    d <- DATASET_BATCH_ESEMPIO[[input$batch_dataset_esempio]]
    if (is.null(d)) return(list(successo = FALSE, messaggio = "Choose an example dataset."))
    campioni <- d$campioni; temperature <- d$temperature
    scelti <- input$batch_campioni_esempio_scelti
    if (!is.null(scelti)) {
      campioni <- campioni[names(campioni) %in% scelti]
      if (!is.null(temperature)) temperature <- temperature[names(temperature) %in% scelti]
    }
    if (length(campioni) == 0) return(list(successo = FALSE, messaggio = "Select at least one sample of the example dataset."))
    return(list(successo = TRUE, campioni = campioni, temperature = temperature))
  }
  
  list(successo = FALSE, messaggio = "Unknown input method.")
}

# Wraps leggi_dati_batch_grezzi() applying the chosen Y pre-treatment
# (if any) to the y column of EVERY sample - same strategy already
# used for dati() in the single fit, so every downstream consumer (fit,
# plots, residuals, export) already sees the transformed data. The domain
# check (e.g. Ln requires positive values) is done ONCE on
# all samples together, BEFORE transforming any of them: if even a
# single sample had a value outside the domain, the whole reading fails
# with a clear message, instead of leaving some samples transformed
# and others not (an inconsistent mixture would be much harder to notice).
leggi_dati_batch <- function() {
  risultato <- leggi_dati_batch_grezzi()
  tipo_trasf <- input$batch_trasformazione_y
  if (!isTRUE(risultato$successo)) return(risultato)
  # "Swap X and Y in every sample": applied to the raw data of all
  # input methods, before any transformation (the temperature, kept
  # separately in risultato$temperature, is not touched).
  if (isTRUE(input$batch_inverti_xy)) risultato$campioni <- lapply(risultato$campioni, inverti_colonne_xy)
  if (!is.null(tipo_trasf) && tipo_trasf != "nessuna") {
    tutti_y <- unlist(lapply(risultato$campioni, function(df) df$y))
    ris_prova <- applica_trasformazione_y(tutti_y, tipo_trasf)
    if (!is.null(ris_prova$messaggio_errore)) {
      return(list(successo = FALSE, messaggio = ris_prova$messaggio_errore))
    }
    risultato$campioni <- lapply(risultato$campioni, function(df) applica_trasformazione_y_su_df(df, tipo_trasf, silenzioso = TRUE))
  }
  tipo_densita <- input$batch_trasformazione_densita
  if (!is.null(tipo_densita) && tipo_densita != "nessuna") {
    risultato$campioni <- lapply(risultato$campioni, function(df) converti_a_densita(
      df, tipo_densita, input$batch_densita_media_bin,
      input$batch_densita_confine_inf_modo, input$batch_densita_confine_inf_valore,
      input$batch_densita_confine_sup_modo, input$batch_densita_confine_sup_valore))
  }
  risultato$campioni <- lapply(risultato$campioni, function(df) aggiungi_punti_ancora(
    df, input$batch_densita_ancora_zero, input$batch_densita_ancora_max, input$batch_densita_ancora_max_valore))
  risultato
}

# ---- Lean single-sample fit (parameters, SE, GoF - no bootstrap, no dense curve) ----
adatta_campione_batch <- function(df, formula_stringa, guesses, fissi, limiti_inferiori, limiti_superiori, tipo_loss, algoritmo, maxiter_scelto,
                                   usa_monte_carlo = FALSE, n_boot = 100, tipo_rumore = "bootstrap", sigma_noto = NA, ponderazione = NULL) {
  n <- nrow(df); p <- length(guesses)
  # Weighted regression (MSE only), as in Single Fit: weights w = 1/yhat^zeta
  # with iteratively reweighted least squares. ponderazione = NULL (or not
  # active): radice_pesi stays NULL and the computation is identical to the usual one.
  pesato <- isTRUE(ponderazione$attiva) && identical(tipo_loss, "mse")
  radice_pesi <- NULL; pesi <- NULL
  if (n <= p) return(list(successo = FALSE, messaggio = paste0("Not enough points (", n, ") for ", p, " free parameters.")))
  # Convergence tolerance chosen in section 5, according to the algorithm
  # actually used (as in Single Fit).
  tolleranza <- 10^esponente_tolleranza(input$esponente_tolleranza, algoritmo_effettivo(algoritmo, tipo_loss))
  
  # Exactly the same range of algorithms as Single Fit (LM/L-BFGS-B/
  # BFGS/Nelder-Mead), extracted into a single helper shared between the
  # point fit and EVERY iteration of the Monte Carlo bootstrap below - so the
  # two paths cannot diverge through an oversight, exactly the
  # risk to keep in mind when touching this function. It always returns
  # the same form (par/hessian/convergenza_vera), regardless of
  # which branch produced it, so the rest of the function (SE, R2, AIC...)
  # does not need to know which algorithm was used to compute it.
  #
  # "LM (MSE only)": if the operator had chosen LM with a Loss other
  # than MSE, the sidebar already corrects it automatically to Nelder-Mead
  # (global observe() in R/server.R, not duplicated here) - this branch
  # therefore in practice fires only when the combination is already valid.
  # LM uses nls.lm() on a vector of RESIDUALS (not a scalar loss like
  # the other three), since Levenberg-Marquardt specifically minimizes
  # the sum of squares of the returned residuals - a large but
  # FINITE value (not NA/Inf, which nls.lm does not tolerate) for invalid predictions,
  # exactly the same choice already made in Single Fit.
  #
  # calcola_hessiana=FALSE for the bootstrap: same saving of computation
  # already present before this change (the Hessian of each single
  # iteration is never needed, the uncertainty comes from the DISPERSION among the
  # iterations, not from the Hessian of each one) - nls.lm computes its
  # own internally anyway as a by-product of its algorithm, so for
  # LM there is neither a real saving to make nor an avoidable cost.
  esegui_fit_algoritmo <- function(guess_iniziale, y_target, calcola_hessiana = TRUE) {
    obiettivo_locale <- function(par) {
      valori_completi <- as.list(c(setNames(as.list(par), names(guesses)), fissi))
      y_pred <- tryCatch(valuta_formula_generica(formula_stringa, valori_completi, df$x), error = function(e) rep(NA_real_, n))
      calcola_loss_generica(y_pred, y_target, tipo_loss, if (is.null(radice_pesi)) NULL else radice_pesi^2)
    }
    if (algoritmo == "LM" && tipo_loss == "mse") {
      jacobiano_modello_b <- crea_jacobiano_modello(formula_stringa, names(guesses))
      jacobiano_residui_locale <- function(par) {
        J <- -jacobiano_modello_b(par, df$x, fissi)
        J[!is.finite(J)] <- 0
        if (!is.null(radice_pesi)) J <- J * radice_pesi
        J
      }
      residui_lm_locale <- function(par) {
        valori_completi <- as.list(c(setNames(as.list(par), names(guesses)), fissi))
        y_pred <- tryCatch(valuta_formula_generica(formula_stringa, valori_completi, df$x), error = function(e) rep(NA_real_, n))
        if (any(!is.finite(y_pred))) return(rep(1e5, n))
        if (is.null(radice_pesi)) y_target - y_pred else radice_pesi * (y_target - y_pred)
      }
      fit <- nls.lm(par = guess_iniziale, fn = residui_lm_locale, jac = jacobiano_residui_locale, lower = limiti_inferiori, upper = limiti_superiori,
                    control = controllo_lm(maxiter_scelto, length(guess_iniziale), tolleranza))
      list(par = fit$par, hessian = fit$hessian, convergenza_vera = fit$info %in% 1:4)
    } else if (algoritmo == "L-BFGS-B") {
      fit <- optim(par = guess_iniziale, fn = obiettivo_locale, method = "L-BFGS-B", lower = limiti_inferiori, upper = limiti_superiori,
                   hessian = calcola_hessiana, control = controllo_optim("L-BFGS-B", maxiter_scelto, guess_iniziale, tolleranza))
      list(par = fit$par, hessian = if (calcola_hessiana) fit$hessian else NULL, convergenza_vera = fit$convergence == 0)
    } else {
      # "else" covers BFGS and Nelder-Mead (neither of them supports
      # boundaries in optim(), exactly the same limitation already present
      # in Single Fit for these two methods) - and, as a purely
      # residual case, any "LM" arrived here with a non-MSE Loss
      # despite the automatic correction in the sidebar: BFGS as a
      # safe fallback, not a silent crash.
      metodo_effettivo <- if (algoritmo %in% c("BFGS", "Nelder-Mead")) algoritmo else algoritmo_effettivo(algoritmo, tipo_loss)
      fit <- optim_con_ripartenze(par = guess_iniziale, fn = obiettivo_locale, method = metodo_effettivo, hessian = calcola_hessiana, control = controllo_optim(metodo_effettivo, maxiter_scelto, guess_iniziale, tolleranza),
                                  ripartenze = !isFALSE(input$nm_ripartenze))
      list(par = fit$par, hessian = if (calcola_hessiana) fit$hessian else NULL, convergenza_vera = fit$convergence == 0)
    }
  }
  
  fit_ris <- tryCatch(esegui_fit_algoritmo(guesses, df$y, calcola_hessiana = TRUE), error = function(e) NULL)
  
  if (is.null(fit_ris)) return(list(successo = FALSE, messaggio = "Optimization error."))
  
  # Iteratively reweighted least squares, as in Single Fit.
  iterazioni_irls <- 0
  if (pesato) {
    msg_positivi <- "Weighting 1/\u0177^\u03b6 requires positive predicted values at all data points."
    prev_di <- function(par) tryCatch(valuta_formula_generica(formula_stringa, as.list(c(setNames(as.list(par), names(guesses)), fissi)), df$x), error = function(e) rep(NA_real_, n))
    variazione <- Inf
    for (ciclo in 1:30) {
      y_corrente <- prev_di(fit_ris$par)
      if (any(!is.finite(y_corrente)) || any(y_corrente <= 0)) return(list(successo = FALSE, messaggio = msg_positivi))
      radice_pesi <- sqrt(1 / y_corrente^ponderazione$zeta)
      f <- tryCatch(esegui_fit_algoritmo(fit_ris$par, df$y, calcola_hessiana = TRUE), error = function(e) NULL)
      if (is.null(f)) return(list(successo = FALSE, messaggio = "Weighted fit failed."))
      variazione <- max(abs(f$par - fit_ris$par) / pmax(abs(fit_ris$par), 1e-12))
      fit_ris <- f; iterazioni_irls <- ciclo
      if (is.finite(variazione) && variazione < 1e-9) break
    }
    y_corrente <- prev_di(fit_ris$par)
    if (any(!is.finite(y_corrente)) || any(y_corrente <= 0)) return(list(successo = FALSE, messaggio = msg_positivi))
    pesi <- 1 / y_corrente^ponderazione$zeta; radice_pesi <- sqrt(pesi)
  }
  
  parametri_ottimi <- fit_ris$par
  names(parametri_ottimi) <- names(guesses)
  valori_completi <- as.list(c(setNames(as.list(parametri_ottimi), names(guesses)), fissi))
  y_pred <- valuta_formula_generica(formula_stringa, valori_completi, df$x)
  residui <- df$y - y_pred
  sse <- sum(residui^2)
  # Value of the loss ACTUALLY minimized (not necessarily SSE:
  # for Huber/MAE/Log10Cosh/RMSLE it is a different number, computed with the
  # same function used during the search) - R2/AIC/BIC instead always remain
  # based on SSE by definition of those metrics, regardless of
  # which loss was chosen to drive the optimization.
  loss_finale <- calcola_loss_generica(y_pred, df$y, tipo_loss, pesi)
  df_residual <- n - p
  mse <- sse / df_residual
  rmse <- sqrt(mse)
  ss_tot <- sum((df$y - mean(df$y))^2)
  r2 <- if (ss_tot > 0) 1 - sse / ss_tot else NA
  r2_adj <- if (!is.na(r2) && df_residual > 0) 1 - (1 - r2) * (n - 1) / df_residual else NA
  ic <- criteri_informazione(sse, n, p, tipo_loss)
  aic <- ic$aic; aicc <- ic$aicc; bic <- ic$bic
  # With weights: weighted SSE, sigma, R^2 and criteria, as in Single Fit.
  if (pesato) {
    sse <- sum(pesi * residui^2); mse <- sse / df_residual; rmse <- sqrt(mse)
    media_pesata <- sum(pesi * df$y) / sum(pesi); ss_tot <- sum(pesi * (df$y - media_pesata)^2)
    r2 <- if (ss_tot > 0) 1 - sse / ss_tot else NA
    r2_adj <- if (!is.na(r2) && df_residual > 0) 1 - (1 - r2) * (n - 1) / df_residual else NA
    ic <- criteri_informazione(sse, n, p, tipo_loss, sum(log(pesi)))
    aic <- ic$aic; aicc <- ic$aicc; bic <- ic$bic
  }
  
  # Covariance matrix of the parameters: ALWAYS computed (the Hessian is
  # already available via hessian=TRUE regardless of the chosen method),
  # not only when Monte Carlo is missing as before - it is also needed for the
  # delta method of Shelf-Life/Arrhenius further below, which needs it
  # also for the samples fitted with Monte Carlo (the uncertainty on the
  # parameters and on the SE remains available anyway via the Hessian).
  # Multiplier 1 for LM, 2 for the other three: the Hessian of nls.lm()
  # (LM) and that of optim() (the other three) do NOT share the same
  # scale convention - exactly the same distinction already present in
  # Single Fit (see "moltiplicatore" in motore_calcolo.R), verified here
  # by comparing the resulting standard errors among the 4 algorithms on the
  # same dataset: without this distinction, the SE of LM turned out
  # systematically larger by a factor of the square root of 2 than the
  # other three (which instead match each other exactly), despite practically
  # identical point estimates of the parameters.
  moltiplicatore_cov <- if (algoritmo == "LM" && tipo_loss == "mse") 1 else 2
  matrice_cov <- if (!is.null(fit_ris$hessian)) tryCatch({
    if ((algoritmo != "LM" || pesato) && tipo_loss == "mse") {
      # Least squares with BFGS / L-BFGS-B / Nelder-Mead: covariance from the
      # Jacobian of the model, s^2 (J^T J)^-1, as in Single Fit (see
      # jacobiano_numerico in rendering_output.R), not from the Hessian of optim.
      # With weights, for every algorithm, s^2 (J^T W J)^-1 with the weighted Jacobian.
      J_cov <- crea_jacobiano_modello(formula_stringa, names(guesses))(parametri_ottimi, df$x, fissi)
      if (pesato) J_cov <- J_cov * radice_pesi
      mse * inverti_hessiana(crossprod(J_cov))
    } else {
      moltiplicatore_cov * mse * inverti_hessiana(fit_ris$hessian)
    }
  }, error = function(e) NULL) else NULL
  # As in Single Fit: classic standard errors only for least squares
  # (with other losses the formula is not valid: Monte Carlo is used), and never
  # an invalid covariance (negative or non-finite variances).
  if (!identical(tipo_loss, "mse")) matrice_cov <- NULL
  if (!is.null(matrice_cov) && !all(is.finite(diag(matrice_cov)) & diag(matrice_cov) > 0)) matrice_cov <- NULL
  if (!is.null(matrice_cov)) { colnames(matrice_cov) <- names(guesses); rownames(matrice_cov) <- names(guesses) }
  
  # Standard error: Hessian (default, fast) or Monte Carlo bootstrap
  # on the residuals if requested - same resampling technique already used
  # for the single fit, here however it reports only the empirical SE in the same
  # already existing "Estimate/SE" table, without building the whole
  # empirical distribution/CI (which would make the batch output heavier, in
  # contrast with the idea of keeping it lean).
  errori_standard <- rep(NA_real_, p)
  n_boot_successo <- NA_integer_
  mat_boot_salvata <- NULL
  if (usa_monte_carlo) {
    mat_boot <- matrix(NA_real_, n_boot, p)
    # Same noise generation method chosen in the sidebar and used by
    # Single Fit (previously Batch always resampled the residuals, even with
    # "Parametric (Gaussian, known sigma)" selected).
    gaussiano <- identical(tipo_rumore, "gaussiano")
    sigma_usato <- if (gaussiano && is.finite(sigma_noto) && sigma_noto > 0) sigma_noto else rmse
    # Residuals to be resampled as in Single Fit (Davison & Hinkley, 1997).
    J_boot <- if (!gaussiano && identical(tipo_loss, "mse")) tryCatch(crea_jacobiano_modello(formula_stringa, names(guesses))(parametri_ottimi, df$x, fissi), error = function(e) NULL) else NULL
    # With weights: weighted residuals and leverages from the weighted Jacobian; the noise is
    # brought back to the scale of each point by dividing by sqrt(w).
    if (pesato && !is.null(J_boot)) J_boot <- J_boot * radice_pesi
    residui_boot <- residui_per_bootstrap(if (pesato) residui * radice_pesi else residui, J_boot, is.null(input$residui_modificati) || isTRUE(input$residui_modificati))
    for (b in seq_len(n_boot)) {
      rumore <- if (gaussiano) rnorm(n, mean = 0, sd = sigma_usato) else sample(residui_boot, n, replace = TRUE)
      if (pesato) rumore <- rumore / radice_pesi
      y_boot <- y_pred + rumore
      fit_boot <- tryCatch(esegui_fit_algoritmo(parametri_ottimi, y_boot, calcola_hessiana = FALSE), error = function(e) NULL)
      if (!is.null(fit_boot) && isTRUE(fit_boot$convergenza_vera)) mat_boot[b, ] <- fit_boot$par
    }
    n_boot_successo <- sum(!is.na(mat_boot[, 1]))
    if (n_boot_successo >= MINIMO_ITERAZIONI_MC) {
      errori_standard <- apply(mat_boot, 2, sd, na.rm = TRUE)
      # Kept for the Parameter Significance Test between 2 samples (same
      # technique already used in Model Comparison: if BOTH chosen samples
      # have a bootstrap matrix, the comparison uses the empirical test instead
      # of the parametric Welch's t-test) - no additional computation, it is
      # simply kept instead of being discarded after use.
      colnames(mat_boot) <- names(guesses)
      mat_boot_salvata <- mat_boot
    }
  }
  if (!usa_monte_carlo || n_boot_successo < MINIMO_ITERAZIONI_MC) {
    if (!is.null(matrice_cov)) errori_standard <- sqrt(diag(matrice_cov))
  }
  names(errori_standard) <- names(guesses)
  
  convergenza_vera <- isTRUE(fit_ris$convergenza_vera)
  
  list(successo = TRUE, parametri = parametri_ottimi, errori = errori_standard, mat_boot = mat_boot_salvata, cov = matrice_cov,
       errore_metodo = if (usa_monte_carlo && !is.na(n_boot_successo) && n_boot_successo >= MINIMO_ITERAZIONI_MC) paste0("Monte Carlo, ", if (identical(tipo_rumore, "gaussiano")) "Gaussian" else if (is.null(input$residui_modificati) || isTRUE(input$residui_modificati)) "residual bootstrap, leverage-corrected" else "residual bootstrap, raw residuals", " (", n_boot_successo, "/", n_boot, ")") else "Linear (Jacobian)",
       obs = n, df = df_residual, sse = sse, mse = mse, rmse = rmse, r2 = r2, r2_adj = r2_adj, loss_finale = loss_finale,
       aic = aic, aicc = aicc, bic = bic, convergenza_vera = convergenza_vera,
       x = df$x, y = df$y, x_griglia = seq(min(df$x), max(df$x), length.out = 150),
       messaggio = if (convergenza_vera) "OK" else "Did not fully converge",
       ponderazione = if (pesato) modifyList(ponderazione, list(iterazioni_irls = iterazioni_irls)) else list(attiva = FALSE), pesi = pesi)
}

# ---- Main batch run ----
batch_risultati <- reactiveVal(NULL)

# ---- Conditional visibility: the two sections below remain hidden until -
# no batch has been launched (batch_risultati() is NULL until it is
# run successfully, see the assignment further below). Same "NULL
# or tagList" pattern already used in this file and in the Non-Isothermal module.

# List of the parameters of the currently selected model, with a
# checkbox each to choose whether to include them in the randomization
# of Auto-retry - important above all when some parameters are
# already easily estimable from the data themselves (e.g. N0 and the asymptote A in a
# growth model, often readable almost directly from the curve)
# while others are much harder (e.g. mu_max, lambda): perturbing
# ALL of them together at every attempt wastes budget on parameters that were
# already good, and - more importantly - drastically reduces the probability
# that the WHOLE perturbed combination lands well in the same attempt
# (that probability decreases as a power of the number of parameters
# perturbed together, not linearly). All ticked by default, to
# reproduce exactly the behaviour before this option.
output$batch_retry_parametri_ui <- renderUI({
  if (!isTRUE(input$batch_retry_convergenza)) return(NULL)
  guess_testo <- input$guess_iniziali
  if (is.null(guess_testo) || nchar(trimws(guess_testo)) == 0) return(NULL)
  parsed <- tryCatch(parse_guesses(guess_testo), error = function(e) NULL)
  if (is.null(parsed) || length(parsed$guess) == 0) return(NULL)
  # names(parsed$guess), not parsed$tutti_nomi: the latter also includes
  # any FIXED parameters (syntax $value$), which perturba_guess_log
  # never touches anyway - a checkbox for a fixed parameter would be
  # misleading, since leaving it ticked or not would have no effect.
  nomi <- names(parsed$guess)
  tagList(
    tags$strong(style = "font-size: 13px;", "Parameters to randomize during retry:"),
    fluidRow(style = "margin-top: 4px;",
      lapply(nomi, function(p) {
        id_campo <- paste0("batch_perturba_", p)
        valore_attuale <- isolate(input[[id_campo]])
        column(3, checkboxInput(id_campo, p, value = if (is.null(valore_attuale)) TRUE else valore_attuale))
      })
    ),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
      "Uncheck a parameter whose initial guess you already trust (e.g. N0 or the asymptote in a growth model, often readable almost directly off the data) so retries focus their randomization on the harder-to-guess ones instead \u2014 fewer parameters randomized together means a much higher chance each retry lands in a sensible region for all of them at once. All checked by default, same behavior as before this option existed.")
  )
})

output$sezione_batch_test_parametri_ui <- renderUI({
  if (is.null(batch_risultati())) return(NULL)
  tagList(
    br(),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Parameter Significance Test:"),
      tags$button(id = "btn_copia_batch_test", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('batch_tabella_test', 'btn_copia_batch_test')")
    ),
    p(style = "font-size: 12px; color: #566573;",
      "Same statistical logic as Model Comparison's Parameter Significance Test, applied to two chosen samples here instead of two models \u2014 an empirical bootstrap test if both samples used Monte Carlo, otherwise a Welch's t-test."),
    uiOutput("batch_selezione_campioni_test"),
    fluidRow(
      column(6, actionButton("batch_esegui_test", "Run Test", class = "btn-primary btn-sm w-100", icon = icon("calculator")))
    ),
    tags$em(style = "font-size: 12px; color: #566573;", textOutput("batch_info_metodo_test", inline = TRUE)),
    tableOutput("batch_tabella_test"),
    br()
  )
})

output$sezione_batch_posthoc_intestazione_ui <- renderUI({
  if (is.null(batch_risultati())) return(NULL)
  tagList(
    tags$h4("Post-Hoc Comparison (3+ Samples):"),
    p(style = "font-size: 12px; color: #566573;",
      "The equivalent of an ANOVA followed by a post-hoc test for this exact setting: each sample gives one parameter estimate plus its own uncertainty, not raw replicate values, so a classic ANOVA doesn't directly apply. ", tags$strong("Cochran's Q"), " (the standard meta-analysis test for comparing several independent estimates with known standard errors) checks, per parameter, whether the samples differ overall; if so, all pairwise comparisons below (same test as above, run for every pair) tell you ", tags$strong("which"), " samples differ \u2014 with a multiple-comparison correction, since testing many pairs would otherwise inflate false positives. When all the compared fits have Monte Carlo distributions, the pairwise comparisons use the bootstrap distributions directly (percentile interval of the differences) and Cochran's Q uses the bootstrap standard errors."),
    uiOutput("batch_selezione_campioni_posthoc"),
    fluidRow(
      column(6, selectInput("batch_metodo_correzione", "Multiple-Comparison Correction:",
                            choices = SCELTE_CORREZIONE_MULTIPLA, selected = "holm")),
      column(6, br(), actionButton("batch_esegui_posthoc", "Run Post-Hoc Comparison", class = "btn-primary btn-sm w-100", icon = icon("calculator")))
    ),
    uiOutput("sezione_batch_posthoc_ui")
  )
})

# ---- Manage Samples: rename/remove a single sample from the list
# already computed, without having to relaunch the whole batch ----
output$batch_selezione_rinomina_ui <- renderUI({
  br <- batch_risultati()
  if (is.null(br) || length(br$risultati) == 0) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a batch elaboration first."))
  selectInput("batch_campione_da_rinominare", NULL, choices = names(br$risultati))
})

observeEvent(input$batch_rinomina_campione, {
  br <- batch_risultati()
  if (is.null(br)) return()
  nome_vecchio <- input$batch_campione_da_rinominare
  nome_nuovo <- trimws(input$batch_nuovo_nome_campione)
  if (is.null(nome_vecchio) || !(nome_vecchio %in% names(br$risultati))) return()
  if (nchar(nome_nuovo) == 0) {
    showNotification("Enter a new label first.", type = "error"); return()
  }
  if (nome_nuovo != nome_vecchio && nome_nuovo %in% names(br$risultati)) {
    showNotification(paste0("\"", nome_nuovo, "\" is already used by another sample \u2014 choose a different label."), type = "error"); return()
  }
  names(br$risultati)[names(br$risultati) == nome_vecchio] <- nome_nuovo
  br$risultati[[nome_nuovo]]$nome <- nome_nuovo
  batch_risultati(br)
  updateTextInput(session, "batch_nuovo_nome_campione", value = "")
  showNotification(paste0("Renamed \"", nome_vecchio, "\" to \"", nome_nuovo, "\"."), type = "message")
})

output$batch_selezione_rimozione_ui <- renderUI({
  br <- batch_risultati()
  if (is.null(br) || length(br$risultati) == 0) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a batch elaboration first."))
  selectInput("batch_campione_da_rimuovere", NULL, choices = names(br$risultati))
})

observeEvent(input$batch_rimuovi_campione, {
  br <- batch_risultati()
  if (is.null(br)) return()
  nome <- input$batch_campione_da_rimuovere
  if (is.null(nome) || !(nome %in% names(br$risultati))) return()
  br$risultati[[nome]] <- NULL
  # Recomputes the counters on the REMAINING samples, so they stay consistent with
  # the list after the removal instead of still referring to the original batch.
  br$n_totale <- length(br$risultati)
  br$n_successo <- sum(sapply(br$risultati, function(r) isTRUE(r$successo)))
  batch_risultati(br)
  # Also clears the results of test/post-hoc: they might refer
  # to the sample just removed.
  batch_test_parametri_risultato(NULL)
  batch_posthoc_risultato(NULL)
  showNotification(paste0("Removed \"", nome, "\"."), type = "message")
})

# Base rbind() does not support the "fill the missing columns with NA" that
# would be needed here (a failed sample has fewer columns than a successful one,
# e.g. all the _Estimate/_SE columns are missing) - dplyr::bind_rows() or
# data.table::rbindlist(fill=TRUE) would do it, but to avoid adding a
# new dependency I replicate the same logic by hand.
rbind_riempi_na <- function(lista_df) {
  tutte_colonne <- unique(unlist(lapply(lista_df, colnames)))
  lista_completata <- lapply(lista_df, function(df) {
    mancanti <- setdiff(tutte_colonne, colnames(df))
    for (col in mancanti) df[[col]] <- NA
    df[tutte_colonne]
  })
  do.call(rbind, lista_completata)
}

observeEvent(input$esegui_batch, {
  # NOTE for whoever touches this code in the future: from this version, Batch
  # Elaboration REALLY uses the Algorithm chosen in the sidebar (LM/Nelder-Mead/
  # BFGS/L-BFGS-B), no longer always BFGS/L-BFGS-B regardless - see
  # adatta_campione_batch(). The old warning "Batch does not use your
  # Algorithm" that was here has been removed because it would no longer be true.
  #
  # "Accept non-converged optimization results" (sidebar) however remains
  # a deliberate exception: it does NOT apply here. Batch always shows the real
  # convergence state in the table (column "Converged": Yes / "NO
  # (caution)"), regardless of that checkbox - a non-converging
  # sample is never "promoted" to converged just because
  # that checkbox is active, unlike Single Fit. With
  # Auto-retry, a non-converging fit is retried anyway with a
  # new starting point, which is already the sensible equivalent of
  # "accepting it" in an automated context over many samples.
  
  # Clears the stale results of the Parameter Significance Test and of the
  # Post-Hoc Comparison of any previous Batch: without this,
  # they remained visible (computed on samples by then replaced) until
  # the user pressed their explicit "Run" buttons again.
  batch_test_parametri_risultato(NULL)
  batch_posthoc_risultato(NULL)
  
  dati_batch <- leggi_dati_batch()
  if (!isTRUE(dati_batch$successo)) {
    showNotification(paste("Batch failed:", dati_batch$messaggio), type = "error")
    return()
  }
  campioni <- dati_batch$campioni
  # The RMSLE is defined only for non-negative values: the observed
  # (and predicted) values below zero are treated as 0 in the computation of the loss.
  # Previously this happened silently: now the user is warned.
  if (identical(input$tipo_loss, "rmsle")) {
    n_negativi <- sum(sapply(campioni, function(cc) sum(cc$y < 0, na.rm = TRUE)))
    if (n_negativi > 0) showNotification(sprintf("RMSLE is defined only for non-negative values: %d observed value(s) below zero, across the samples, are treated as 0 in the loss.", n_negativi), type = "warning", duration = 10)
  }
  
  guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) NULL)
  if (is.null(guesses_raw)) {
    showNotification("Cannot start batch: check the Initial Guesses field in the sidebar first.", type = "error")
    return()
  }
  # Which parameters to include in the randomization of Auto-retry (see
  # "Parameters to randomize during retry" above) - a missing checkbox
  # (renderUI not yet executed, or retry disabled) is equivalent to
  # "include", to reproduce the behaviour from before this option
  # existed. names(guesses_raw$guess), not guesses_raw$tutti_nomi:
  # the latter also includes any FIXED parameters, which do not
  # belong to the guess/lower/upper vector used by perturba_guess_log -
  # using it would break the positional alignment between guess_correnti and
  # this vector.
  parametri_perturbabili <- sapply(names(guesses_raw$guess), function(p) {
    valore <- input[[paste0("batch_perturba_", p)]]
    if (is.null(valore)) TRUE else isTRUE(valore)
  })
  formula_stringa <- costruisci_formula(input$tipo_modello, input$formula_custom)
  if (!valida_formula(formula_stringa, guesses_raw$tutti_nomi)) {
    showNotification("Cannot start batch: invalid or unsafe formula.", type = "error")
    return()
  }
  
  # Pre-validation of ALL samples before launching the batch, so
  # a problematic dataset is discovered immediately and not after having already spent
  # time on the previous ones.
  p_libero <- length(guesses_raw$guess)
  campioni_insufficienti <- names(campioni)[sapply(campioni, nrow) <= p_libero]
  if (length(campioni_insufficienti) > 0) {
    showNotification(paste0("Batch failed: these samples don't have enough points for ", p_libero, 
                            " free parameters: ", paste(campioni_insufficienti, collapse = ", ")), type = "error", duration = 10)
    return()
  }
  
  maxiter_scelto <- if (is.null(input$maxiter_ottimizzazione) || is.na(input$maxiter_ottimizzazione) || input$maxiter_ottimizzazione <= 0) 200 else input$maxiter_ottimizzazione
  maxiter_scelto <- min(max(maxiter_scelto, 50), 2000)
  guess_correnti <- guesses_raw$guess
  warm_start <- isTRUE(input$batch_warm_start)
  retry_convergenza <- isTRUE(input$batch_retry_convergenza)
  n_tentativi_max <- if (is.null(input$batch_retry_max_tentativi) || is.na(input$batch_retry_max_tentativi)) 25 else min(max(round(input$batch_retry_max_tentativi), 1), 100)
  
  # Generates a new guess by perturbing each parameter by a random power of
  # 10 between -2 and +2 (two orders of magnitude above or below),
  # keeping the original sign (multiplying by a power of 10,
  # always positive, never changes it) - if the original guess is zero
  # (for which the multiplication would always give zero), it uses a small
  # random value as fallback. Respects any limits (lower/upper)
  # if set, so as not to generate a value outside a constraint chosen
  # by the user.
  #
  # With adattivo=TRUE, the interval of the exponent is no longer fixed at
  # [-2,+2] but shrinks on the side where the boundary (lower/upper) is close
  # to the guess - otherwise a fraction of the attempts (up to almost 50%
  # when the guess is right next to a boundary) would always give the SAME
  # value "capped" at the boundary, wasting attempts on a point already tried
  # instead of exploring new territory. The space "saved" on the narrow
  # side is redistributed on the opposite side (if it has room to
  # accommodate it), keeping the total search width at 4 when
  # possible - it shrinks only in the rare case in which the guess is
  # squeezed between TWO close boundaries at the same time, where there is no way to
  # redistribute anywhere. Applied only for positive guesses (the
  # practical case of all the parameters actually constrained in the
  # registry); for negative or zero guesses it falls back to the original fixed range.
  perturba_guess_log <- function(guess, lower, upper, adattivo = FALSE, perturbabile = NULL) {
    # Base limit of the exponent (in orders of magnitude): reduced from 2 to
    # 1.5 with respect to the original version - with more than one parameter
    # perturbed together, a narrower range significantly increases
    # the probability that ALL parameters fall into a sensible zone
    # in the same attempt (e.g. with 4 parameters, +-1.5 is about 3 times
    # more likely than +-2 to return a useful attempt), against
    # a maximum reach per single attempt that drops from a
    # factor of 100 to a factor of ~32 - a reach that in any case does not reliably
    # fix an initial guess very far from the truth (see
    # the discussion on the adaptive range), so giving it up weighs
    # little compared with the gain in the common case.
    LIMITE_ESPONENTE <- 1.5
    nuovo <- sapply(seq_along(guess), function(i) {
      v <- guess[i]
      # Parameter excluded from the randomization (checkbox unticked in
      # "Parameters to randomize during retry"): it stays exactly at its
      # original guess at every attempt, not just "almost unchanged" -
      # zero perturbation, not a narrow interval around v.
      if (!is.null(perturbabile) && !isTRUE(perturbabile[i])) return(v)
      if (v == 0) return(runif(1, -0.1, 0.1))
      esp_inf <- -LIMITE_ESPONENTE; esp_sup <- LIMITE_ESPONENTE
      if (isTRUE(adattivo) && v > 0) {
        up <- upper[i]; lo <- lower[i]
        spazio_sopra <- if (is.finite(up)) max(0, log10(up / v)) else Inf
        spazio_sotto <- if (is.finite(lo) && lo > 0) max(0, log10(v / lo)) else Inf
        deficit_sopra <- max(0, LIMITE_ESPONENTE - spazio_sopra)
        deficit_sotto <- max(0, LIMITE_ESPONENTE - spazio_sotto)
        margine_sotto_extra <- if (is.finite(spazio_sotto)) max(0, spazio_sotto - LIMITE_ESPONENTE) else Inf
        margine_sopra_extra <- if (is.finite(spazio_sopra)) max(0, spazio_sopra - LIMITE_ESPONENTE) else Inf
        esp_sup <- min(LIMITE_ESPONENTE, spazio_sopra) + min(deficit_sotto, margine_sopra_extra)
        esp_inf <- -min(LIMITE_ESPONENTE, spazio_sotto) - min(deficit_sopra, margine_sotto_extra)
      }
      v * 10^runif(1, esp_inf, esp_sup)
    })
    nuovo <- pmax(nuovo, lower)
    nuovo <- pmin(nuovo, upper)
    nuovo
  }
  
  # A fit can "converge" according to the internal criteria of optim() but
  # still land on a point with an ill-conditioned Hessian (e.g.
  # poorly identifiable or mutually correlated parameters, or a flat zone
  # of the loss surface) - in that case the standard errors turn out
  # NA even with convergenza_vera = TRUE. A fit is considered really
  # acceptable only if BOTH conditions are met.
  # The additional check on standard error vs parameter is instead
  # OPTIONAL (at the operator's choice, dedicated checkbox): it discards a fit
  # if even a single parameter has a standard error that exceeds the absolute
  # value of its own estimate - a rough but common signal of a
  # parameter that is not reliably identifiable (the estimate would not
  # even be one standard error away from zero). Evaluated LAST
  # and in AND with "&&" (short-circuit): at this point errors and parameters
  # are already guaranteed finite/non-NA by the previous conditions. The
  # percentage threshold is chosen by the operator (100% = original
  # behaviour, standard error not greater than the parameter itself).
  scarta_errore_alto <- isTRUE(input$batch_retry_scarta_errore_alto)
  soglia_errore_pct <- if (is.null(input$batch_retry_soglia_errore_pct) || is.na(input$batch_retry_soglia_errore_pct)) 100 else max(input$batch_retry_soglia_errore_pct, 0.001)
  # Finite standard errors required only with the MSE loss: with the other losses
  # the classic standard errors are not computed (formula valid only
  # for least squares), so previously no fit ever turned out
  # acceptable - Auto-retry repeated every sample up to the maximum number of
  # attempts and the Batch Monte Carlo (run only on acceptable fits)
  # never started.
  errori_attesi <- identical(input$tipo_loss, "mse")
  fit_accettabile <- function(esito) {
    errori_ok <- if (errori_attesi) !anyNA(esito$errori) && all(is.finite(esito$errori)) else TRUE
    base_ok <- isTRUE(esito$convergenza_vera) && errori_ok &&
      !is.na(esito$r2) && esito$r2 >= 0 && !is.na(esito$r2_adj) && esito$r2_adj >= 0
    if (!base_ok) return(FALSE)
    if (scarta_errore_alto && errori_attesi && any(esito$errori > (soglia_errore_pct / 100) * abs(esito$parametri))) return(FALSE)
    TRUE
  }
  
  # The "Use Linear error method" checkbox is the OPT-OUT: Monte Carlo for the
  # batch starts only if that checkbox is unticked AND the error method
  # chosen in the sidebar is actually Monte Carlo - otherwise it always remains
  # the fast point fit (Hessian), as already explained in the UI.
  usa_mc_batch <- !isTRUE(input$batch_forza_lineare) && input$metodo_errore == "monte_carlo"
  n_boot_batch <- if (is.null(input$n_boot) || is.na(input$n_boot)) 100 else min(max(round(input$n_boot), 10), 5000)
  
  # ---- Weighted regression (same choice as the sidebar, MSE only) ----
  # fixed zeta (1, 2 or from the user) equal for all samples, or from the
  # replicates: common to all samples (groups of replicates of all
  # samples in a single SD vs mean regression, which presupposes the
  # same variance structure, for example the same analytical method)
  # or estimated sample by sample from its own replicates only.
  schema_pesi_b <- if (is.null(input$ponderazione)) "nessuna" else input$ponderazione
  pesi_batch <- list(attiva = FALSE)
  if (!identical(schema_pesi_b, "nessuna")) {
    if (!identical(input$tipo_loss, "mse")) {
      showNotification(paste0("Weighting applies only to least squares (MSE loss): it is ignored with the ", toupper(input$tipo_loss), " loss."), type = "warning", duration = 8)
    } else if (identical(schema_pesi_b, "repliche")) {
      modo_zeta <- if (identical(input$batch_zeta_modo, "campione")) "campione" else "comune"
      if (modo_zeta == "comune") {
        repl_tutti <- do.call(rbind, lapply(campioni, function(cc) { g <- raggruppa_per_x(cc$x, cc$y, rep(0, nrow(cc)), cc$y); g$tab[g$tab$n >= 2, , drop = FALSE] }))
        st <- if (is.null(repl_tutti) || nrow(repl_tutti) == 0) list(disponibile = FALSE, motivo = "no replicates in the samples") else calcola_sd_media(repl_tutti)
        if (!isTRUE(st$disponibile)) {
          showNotification(paste0("Batch not run: zeta cannot be estimated from the replicates (", st$motivo, "). Choose a fixed zeta, zeta per sample, or no weighting."), type = "error", duration = 10)
          return()
        }
        pesi_batch <- list(attiva = TRUE, schema = "repliche", modo = "comune", zeta = 2 * st$b, b = st$b, b_ci = st$ci, gruppi = st$gruppi)
      } else pesi_batch <- list(attiva = TRUE, schema = "repliche", modo = "campione")
    } else {
      zeta_b <- switch(schema_pesi_b, zeta1 = 1, zeta2 = 2, personalizzato = suppressWarnings(as.numeric(input$ponderazione_zeta)), NA_real_)
      if (length(zeta_b) != 1 || !is.finite(zeta_b)) { showNotification("Batch not run: enter a valid value of zeta for the weighting.", type = "error", duration = 10); return() }
      pesi_batch <- list(attiva = TRUE, schema = schema_pesi_b, zeta = zeta_b)
    }
  }
  
  risultati <- list()
  n_campioni <- length(campioni)
  withProgress(message = "Running batch elaboration...", value = 0, {
    for (i in seq_along(campioni)) {
      nome <- names(campioni)[i]
      incProgress(1 / n_campioni, detail = paste0("Sample ", i, " of ", n_campioni, ": ", nome))
      # Weights of this sample (NULL without weights).
      pond_i <- NULL
      if (isTRUE(pesi_batch$attiva)) {
        if (identical(pesi_batch$modo, "campione")) {
          st_i <- stima_potenza_varianza(campioni[[i]]$x, campioni[[i]]$y)
          if (!isTRUE(st_i$disponibile)) {
            risultati[[nome]] <- list(successo = FALSE, messaggio = paste0("zeta cannot be estimated from this sample's replicates (", st_i$motivo, ")"), nome = nome)
            next
          }
          pond_i <- list(attiva = TRUE, schema = "repliche", modo = "campione", zeta = 2 * st_i$b, b = st_i$b, b_ci = st_i$ci)
        } else pond_i <- pesi_batch
      }
      if (retry_convergenza) {
        # Search phase: always with the Linear method (fast), NEVER with
        # Monte Carlo regardless of what was chosen for the
        # batch - it looks ONLY for a starting point that converges, repeating
        # with perturbed guesses if necessary. The cost of the bootstrap (if
        # requested) is paid ONLY ONCE at the end, on the good
        # result - not on every intermediate attempt that might be
        # discarded (the internal order of adatta_campione_batch runs
        # the whole Monte Carlo loop BEFORE knowing whether the fit is
        # convergent, so doing it at every attempt would be a concrete waste
        # when several attempts are needed).
        esito <- adatta_campione_batch(campioni[[i]], formula_stringa, guess_correnti, guesses_raw$fissi,
                                       guesses_raw$lower, guesses_raw$upper, input$tipo_loss, input$algoritmo, maxiter_scelto,
                                       usa_monte_carlo = FALSE, n_boot = n_boot_batch, ponderazione = pond_i)
        tentativo <- 0
        # Best attempt (lowest loss, in the loss chosen by the user)
        # among the successful ones: if in the end no attempt meets the
        # criteria, this one is kept instead of the last, which could also be
        # the worst one (previously the last one was always kept).
        loss_di <- function(es) if (isTRUE(es$successo) && length(es$loss_finale) == 1 && is.finite(es$loss_finale)) es$loss_finale else Inf
        esito_migliore <- esito
        while (tentativo < n_tentativi_max && !fit_accettabile(esito)) {
          tentativo <- tentativo + 1
          incProgress(0, detail = paste0("Sample ", i, " of ", n_campioni, ": ", nome, " (retry ", tentativo, "/", n_tentativi_max, ")"))
          guess_perturbato <- perturba_guess_log(guess_correnti, guesses_raw$lower, guesses_raw$upper, adattivo = isTRUE(input$batch_retry_range_adattivo), perturbabile = parametri_perturbabili)
          esito <- adatta_campione_batch(campioni[[i]], formula_stringa, guess_perturbato, guesses_raw$fissi,
                                         guesses_raw$lower, guesses_raw$upper, input$tipo_loss, input$algoritmo, maxiter_scelto,
                                         usa_monte_carlo = FALSE, n_boot = n_boot_batch, ponderazione = pond_i)
          if (loss_di(esito) < loss_di(esito_migliore)) esito_migliore <- esito
        }
        if (!fit_accettabile(esito) && loss_di(esito_migliore) < loss_di(esito)) esito <- esito_migliore
        # Tracks how many guesses were tried in total (the
        # initial one + any retries) and whether, at the end of the loop, the
        # chosen acceptance criteria are really met -
        # captured HERE, before any Monte Carlo re-fit below, so
        # they reflect the state of the search for the starting point itself,
        # not the outcome of the subsequent bootstrap. Visible in the table only
        # when Auto-retry is active (columns added further below).
        numero_guess_tentati <- tentativo + 1
        criteri_soddisfatti <- fit_accettabile(esito)
        if (usa_mc_batch && fit_accettabile(esito)) {
          incProgress(0, detail = paste0("Sample ", i, " of ", n_campioni, ": ", nome, " (Monte Carlo on converged fit)"))
          esito <- adatta_campione_batch(campioni[[i]], formula_stringa, esito$parametri, guesses_raw$fissi,
                                         guesses_raw$lower, guesses_raw$upper, input$tipo_loss, input$algoritmo, maxiter_scelto,
                                         usa_monte_carlo = TRUE, n_boot = n_boot_batch, tipo_rumore = input$tipo_rumore_mc, sigma_noto = if (is.null(input$sigma_gaussiano)) NA else input$sigma_gaussiano, ponderazione = pond_i)
        }
        esito$numero_guess_tentati <- numero_guess_tentati
        esito$criteri_retry_soddisfatti <- criteri_soddisfatti
      } else {
        esito <- adatta_campione_batch(campioni[[i]], formula_stringa, guess_correnti, guesses_raw$fissi,
                                       guesses_raw$lower, guesses_raw$upper, input$tipo_loss, input$algoritmo, maxiter_scelto,
                                       usa_monte_carlo = usa_mc_batch, n_boot = n_boot_batch, tipo_rumore = input$tipo_rumore_mc, sigma_noto = if (is.null(input$sigma_gaussiano)) NA else input$sigma_gaussiano, ponderazione = pond_i)
      }
      esito$nome <- nome
      risultati[[nome]] <- esito
      if (warm_start && isTRUE(esito$successo)) guess_correnti <- esito$parametri
    }
  })
  
  n_successo <- sum(sapply(risultati, function(r) isTRUE(r$successo)))
  batch_risultati(list(risultati = risultati, formula = formula_stringa, n_totale = n_campioni, n_successo = n_successo,
                        temperature_da_file = dati_batch$temperature, simboli_modello = as.list(simboli_parametri()),
                        retry_usato = retry_convergenza, retry_max_usato = n_tentativi_max,
                        algoritmo_usato = algoritmo_effettivo(input$algoritmo, input$tipo_loss), loss_usata = input$tipo_loss, mc_richiesto = usa_mc_batch, n_boot_richiesto = n_boot_batch,
                        pesi_batch = pesi_batch, fissi = guesses_raw$fissi, nm_ripartenze = !isFALSE(input$nm_ripartenze)))
  # Clears the Shelf-Life & Arrhenius results (if any): they refer
  # to the samples of the PREVIOUS batch - without this, after a new Run of the
  # batch, results by then inconsistent with the current
  # samples/temperatures would remain visible, without any warning. Cleared here (after
  # the batch has ACTUALLY succeeded), not at the start of the function, so
  # a Run attempt that failed validation does not erase results
  # of Shelf-Life still valid from a previous successful batch.
  sl_risultato(NULL)
  showNotification(paste0("Batch complete: ", n_successo, "/", n_campioni, " samples fitted successfully."), 
                    type = if (n_successo == n_campioni) "message" else "warning", duration = 8)
})

output$stato_batch <- renderText({
  br <- batch_risultati()
  if (is.null(br)) return("")
  paste0("Last run: ", br$n_successo, " / ", br$n_totale, " samples fitted successfully.")
})

# Summary of the settings ACTUALLY used for the last completed
# run - not the current state of the sidebar, which the operator
# might have changed after launching the batch (same principle
# already followed for retry_max_usato) - same informative style already used
# in Single Fit ("Minimized Loss Function..."/"Error Estimation Method").
output$batch_info_impostazioni <- renderUI({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  algoritmo_label <- switch(br$algoritmo_usato,
                             "LM" = "Levenberg-Marquardt", "Nelder-Mead" = "Simplex (Nelder-Mead)",
                             "BFGS" = "Quasi-Newton (BFGS)", "L-BFGS-B" = "Bounded Quasi-Newton (L-BFGS-B)",
                             br$algoritmo_usato)
  metodo_errore_label <- if (isTRUE(br$mc_richiesto)) {
    paste0("Monte Carlo requested (", br$n_boot_richiesto, " iterations per sample \u2014 falls back to Linear Approximation for any sample where fewer than 10 iterations converge)")
  } else {
    "Linear Approximation (Jacobian Z-scores)"
  }
  div(style = "background-color: #f4f6f7; border-left: 4px solid #566573; border-radius: 4px; padding: 10px 14px; margin-bottom: 12px; font-size: 13px;",
      tags$strong("Settings used for this run:"), tags$br(),
      "Algorithm: ", algoritmo_label, tags$br(),
      "Loss Function: ", br$loss_usata, tags$br(),
      if (isTRUE(br$pesi_batch$attiva)) tagList("Weighting: ", descrizione_pesi_batch(br$pesi_batch), tags$br()),
      if (identical(br$algoritmo_usato, "Nelder-Mead")) tagList("Nelder-Mead restarts: ", if (isTRUE(br$nm_ripartenze)) "on" else "off", tags$br()),
      "Error Estimation Method: ", metodo_errore_label)
})

descrizione_pesi_batch <- function(pb) {
  if (identical(pb$modo, "campione")) return("w = 1/\u0177^\u03b6, \u03b6 = 2b estimated for each sample from its replicates (see the Weighting Zeta column)")
  paste0("w = 1/\u0177^", signif(pb$zeta, 4),
         if (identical(pb$modo, "comune")) sprintf(" (\u03b6 = 2b common to all samples, b = %.3f, 95%% CI %.3f to %.3f, from %d groups of replicates)", pb$b, pb$b_ci[1], pb$b_ci[2], pb$gruppi)
         else if (identical(pb$schema, "zeta1")) " (variance proportional to the mean)" else if (identical(pb$schema, "zeta2")) " (constant coefficient of variation)" else "")
}

# ---- Overlay plot ----
output$batch_selezione_campioni_overlay <- renderUI({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  if (length(nomi_ok) == 0) return(NULL)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("batch_overlay_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("batch_overlay_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("batch_campioni_overlay", "Samples to include:", choices = nomi_ok, selected = nomi_ok, inline = TRUE)
  )
})
observeEvent(input$batch_overlay_select_all, {
  br <- batch_risultati()
  if (is.null(br)) return()
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  updateCheckboxGroupInput(session, "batch_campioni_overlay", selected = nomi_ok)
})
observeEvent(input$batch_overlay_deselect_all, {
  updateCheckboxGroupInput(session, "batch_campioni_overlay", selected = character(0))
})

disegna_grafico_batch <- function() {
  br <- batch_risultati()
  par(mar = c(5, 5, 4, 2) + 0.1)
  if (is.null(br)) {
    plot(1, type = "n", main = "Run a batch elaboration first.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  risultati_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
  # If the user chose a subset of samples to show (checkboxes
  # above the plot), filters to that - if the selector has not yet
  # been rendered (NULL, e.g. right after a new Run) shows all of them, so the
  # plot does not appear empty before the user interacts with the selector.
  if (!is.null(input$batch_campioni_overlay)) {
    risultati_ok <- risultati_ok[names(risultati_ok) %in% input$batch_campioni_overlay]
  }
  if (length(risultati_ok) == 0) {
    plot(1, type = "n", main = "No sample selected \u2014 check at least one box above.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  mostra_punti <- if (is.null(input$batch_mostra_punti)) TRUE else input$batch_mostra_punti
  cex_punti <- if (is.null(input$batch_cex_punti)) 1 else input$batch_cex_punti
  lwd_linee <- if (is.null(input$batch_lwd_linee)) 2.5 else input$batch_lwd_linee
  pos_legenda <- if (is.null(input$batch_pos_legenda)) "topright" else input$batch_pos_legenda
  pch_punti_batch <- as.numeric(if (is.null(input$batch_pch_punti)) "16" else input$batch_pch_punti)
  lty_linee_batch <- as.numeric(if (is.null(input$batch_lty_linee)) "1" else input$batch_lty_linee)
  mostra_legenda_batch <- if (is.null(input$batch_mostra_legenda)) TRUE else input$batch_mostra_legenda
  tutte_x <- unlist(lapply(risultati_ok, function(r) c(r$x_griglia, r$x)))
  tutte_y <- unlist(lapply(risultati_ok, function(r) {
    valori_completi_griglia <- as.list(c(setNames(as.list(r$parametri), names(r$parametri))))
    y_curva <- tryCatch(valuta_formula_generica(br$formula, valori_completi_griglia, r$x_griglia), error = function(e) NA)
    c(y_curva, r$y)
  }))
  
  # Palette: the original fixed one, or a continuous scale - same
  # choice already offered in Overlaid Fitted Curves (Model Comparison) and in the
  # Sensitivity Plot, particularly useful with many samples in the batch.
  tipo_palette_batch <- if (is.null(input$batch_palette)) "fissa" else input$batch_palette
  n_campioni_batch <- length(risultati_ok)
  colori_usati <- colori_da_palette(tipo_palette_batch, n_campioni_batch)
  colori_punti_batch <- colori_punti_serie(input$batch_col_punti, colori_usati)
  
  titolo <- if (isTRUE(input$mostra_titolo)) "Batch Elaboration: Overlaid Fitted Curves" else ""
  xlim_batch <- if (range_personalizzato_attivo(input$batch_range_x_min, input$batch_range_x_max)) c(input$batch_range_x_min, input$batch_range_x_max) else range(tutte_x, na.rm = TRUE)
  ylim_batch <- if (range_personalizzato_attivo(input$batch_range_y_min, input$batch_range_y_max)) c(input$batch_range_y_min, input$batch_range_y_max) else range(tutte_y, na.rm = TRUE)
  plot(1, type = "n", xlim = xlim_batch, ylim = ylim_batch,
       main = titolo, xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y), las = 1)
  if (is.null(input$mostra_griglia) || isTRUE(input$mostra_griglia)) grid(col = "gray90", lty = "dotted")
  
  for (i in seq_along(risultati_ok)) {
    r <- risultati_ok[[i]]
    if (mostra_punti) points(r$x, r$y, pch = pch_punti_batch, col = colori_punti_batch[i], cex = cex_punti)
    valori_completi <- as.list(setNames(as.list(r$parametri), names(r$parametri)))
    y_curva <- valuta_formula_generica(br$formula, valori_completi, r$x_griglia)
    lines(r$x_griglia, y_curva, col = colori_usati[i], lwd = lwd_linee, lty = lty_linee_batch)
  }
  if (mostra_legenda_batch) {
    cex_legenda_batch <- if (is.null(input$batch_cex_legenda)) 0.8 else input$batch_cex_legenda
    legend(pos_legenda, legend = names(risultati_ok), col = colori_usati, lwd = lwd_linee, lty = lty_linee_batch, bty = "o", cex = cex_legenda_batch, bg = "white")
  }
}

output$grafico_batch_overlay <- renderPlot({ disegna_grafico_batch() })

# ---- Single fits in a single image (one panel per sample) ----
# Same samples shown in the overlay plot (respects the selection
# made with the checkboxes above it), same colours per sample, same
# symbols/line widths - but each in its own panel, with only its own points and
# its own curve.
campioni_batch_per_pannelli <- function() {
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  risultati_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
  if (!is.null(input$batch_campioni_overlay)) risultati_ok <- risultati_ok[names(risultati_ok) %in% input$batch_campioni_overlay]
  if (length(risultati_ok) == 0) return(NULL)
  curve <- lapply(risultati_ok, function(r) {
    y <- tryCatch(valuta_formula_generica(br$formula, as.list(setNames(as.list(r$parametri), names(r$parametri))), r$x_griglia), error = function(e) rep(NA_real_, length(r$x_griglia)))
    list(x = r$x_griglia, y = y)
  })
  tipo_palette_batch <- if (is.null(input$batch_palette)) "fissa" else input$batch_palette
  n <- length(risultati_ok)
  colori <- colori_da_palette(tipo_palette_batch, n)
  list(risultati = risultati_ok, curve = curve, colori = colori, colori_punti = colori_punti_serie(input$batch_col_punti, colori))
}

disegna_pannello_batch <- function(i, dati_pannelli) {
  r <- dati_pannelli$risultati[[i]]
  curva <- dati_pannelli$curve[[i]]
  nome <- names(dati_pannelli$risultati)[i]
  mostra_punti <- if (is.null(input$batch_mostra_punti)) TRUE else input$batch_mostra_punti
  cex_punti <- if (is.null(input$batch_cex_punti)) 1 else input$batch_cex_punti
  lwd_linee <- if (is.null(input$batch_lwd_linee)) 2.5 else input$batch_lwd_linee
  pch_punti <- as.numeric(if (is.null(input$batch_pch_punti)) "16" else input$batch_pch_punti)
  lty_linee <- as.numeric(if (is.null(input$batch_lty_linee)) "1" else input$batch_lty_linee)
  
  if (assi_condivisi_composito()) {
    tutte_x <- unlist(lapply(seq_along(dati_pannelli$risultati), function(k) c(dati_pannelli$curve[[k]]$x, dati_pannelli$risultati[[k]]$x)))
    tutte_y <- unlist(lapply(seq_along(dati_pannelli$risultati), function(k) c(dati_pannelli$curve[[k]]$y, dati_pannelli$risultati[[k]]$y)))
  } else {
    tutte_x <- c(curva$x, r$x)
    tutte_y <- c(curva$y, r$y)
  }
  xlim_p <- if (range_personalizzato_attivo(input$batch_range_x_min, input$batch_range_x_max)) c(input$batch_range_x_min, input$batch_range_x_max) else range(tutte_x[is.finite(tutte_x)])
  ylim_p <- if (range_personalizzato_attivo(input$batch_range_y_min, input$batch_range_y_max)) c(input$batch_range_y_min, input$batch_range_y_max) else range(tutte_y[is.finite(tutte_y)])
  
  par(mar = c(5, 5, 4, 2) + 0.1)
  # The panel title is the name of the sample: it is the only way to
  # know which sample is which, so it is always shown, even
  # with the titles disabled in the sidebar.
  plot(1, type = "n", xlim = xlim_p, ylim = ylim_p, main = nome,
       xlab = interpreta_etichetta(input$label_x), ylab = interpreta_etichetta(input$label_y), las = 1)
  if (is.null(input$mostra_griglia) || isTRUE(input$mostra_griglia)) grid(col = "gray90", lty = "dotted")
  if (mostra_punti) points(r$x, r$y, pch = pch_punti, col = dati_pannelli$colori_punti[i], cex = cex_punti)
  lines(curva$x, curva$y, col = dati_pannelli$colori[i], lwd = lwd_linee, lty = lty_linee)
}

output$scarica_batch_fit_singoli <- downloadHandler(
  filename = function() nome_file_composito("batch_single_fits"),
  content = function(file) {
    dati_pannelli <- campioni_batch_per_pannelli()
    if (is.null(dati_pannelli)) {
      crea_immagine_composita(file, 1, function(i) plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run a batch elaboration first."))
    } else {
      crea_immagine_composita(file, length(dati_pannelli$risultati), function(i) disegna_pannello_batch(i, dati_pannelli))
    }
  }
)

output$scarica_batch_grafico <- downloadHandler(
  filename = function() paste0("batch_overlay.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_grafico_batch()
    dev.off()
  }
)

# ---- Summary tables (wide format: one row per sample) ----
# Converts a raw parameter name (e.g. "MU") into its plain Unicode
# symbol (e.g. "μmax") using the symbol map captured at the moment of the run
# (not a "live" version that could become misaligned if the user changes
# model in the sidebar AFTER launching the batch) - if the parameter does not
# have a defined symbol, it returns the name as it is.
etichetta_parametro_batch <- function(nome_par, br) {
  mappa <- br$simboli_modello
  if (!is.null(mappa) && !is.null(mappa[[nome_par]])) {
    simbolo_a_testo_semplice(mappa[[nome_par]])
  } else {
    simbolo_a_testo_semplice(simbolo_automatico_pedice(nome_par))
  }
}

batch_tabella_parametri_df <- reactive({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  righe <- lapply(br$risultati, function(r) {
    if (!isTRUE(r$successo)) return(data.frame(Sample = r$nome, Converged = "NO", stringsAsFactors = FALSE))
    riga <- data.frame(Sample = r$nome, Converged = if (isTRUE(r$convergenza_vera)) "Yes" else "NO (caution)", stringsAsFactors = FALSE)
    for (nome_par in names(r$parametri)) {
      etichetta <- etichetta_parametro_batch(nome_par, br)
      riga[[paste0(etichetta, "_Estimate")]] <- round(r$parametri[[nome_par]], 5)
      riga[[paste0(etichetta, "_SE")]] <- round(r$errori[[nome_par]], 5)
    }
    riga
  })
  rbind_riempi_na(righe)
})

# Dynamically generated choices of the "Sort By" selector: unlike
# the diagnostics table (always the same columns: AICc, RMSE, etc.),
# here the parameter columns (e.g. "\u03bcmax_Estimate") change depending on the
# model used in the batch, so they cannot be listed as fixed
# choices in the UI.
output$batch_par_sort_choices_ui <- renderUI({
  df <- batch_tabella_parametri_df()
  if (is.null(df)) return(NULL)
  selectInput("batch_par_sort_by", "Sort By:", choices = colnames(df))
})

batch_tabella_parametri_ordinata <- reactive({
  df <- batch_tabella_parametri_df()
  if (is.null(df)) return(NULL)
  campo_ord <- if (is.null(input$batch_par_sort_by) || !(input$batch_par_sort_by %in% colnames(df))) "Sample" else input$batch_par_sort_by
  decrescente <- isTRUE(input$batch_par_sort_dir == "desc")
  idx <- order(df[[campo_ord]], decreasing = decrescente, na.last = TRUE)
  df[idx, ]
})

output$tabella_batch_parametri <- renderTable({
  df <- batch_tabella_parametri_ordinata()
  if (is.null(df)) return(data.frame(Message = "Run a batch elaboration first."))
  for (col in setdiff(colnames(df), c("Sample", "Converged"))) {
    if (is.numeric(df[[col]])) df[[col]] <- sprintf("%.5f", df[[col]])
  }
  # Same underscore->space replacement already done for the
  # Diagnostics table: here the columns are "<parameter>_Estimate"/"<parameter>_SE"
  # generated dynamically, so they become "<parameter> Estimate"/"<parameter> SE".
  colnames(df) <- gsub("_", " ", colnames(df))
  # "SE" -> "St.Err." ONLY here, in the display: the source column
  # stays "_SE" (not touched) because it is shared with the CSV/Excel export,
  # which must remain stable - same principle already followed for the
  # underscores. The "$" anchor limits the replacement to the end of the name,
  # so it does not accidentally touch an "SE" appearing elsewhere.
  colnames(df) <- gsub(" SE$", " St.Err.", colnames(df))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

# ---- Comparison of parameters among samples: histograms (Hessian/Linear) and
# boxplot/violin (Monte Carlo) - separated because they require a different type
# of data (only the point estimate in the first case, the whole
# bootstrap distribution in the second) and are therefore two distinct plots,
# not two variants of the same one.
batch_campioni_hessian <- reactive({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  Filter(function(r) isTRUE(r$successo) && is.null(r$mat_boot), br$risultati)
})

batch_campioni_montecarlo <- reactive({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  Filter(function(r) isTRUE(r$successo) && !is.null(r$mat_boot) && nrow(r$mat_boot) >= 10, br$risultati)
})

# ---- Parameter Distributions (Linear/Hessian): one plot per parameter ----
# One bar per sample (point estimate with an error bar from the Hessian standard
# error). At least 3 samples fitted with the linear method are needed.
dati_istogrammi_batch <- function() {
  campioni <- batch_campioni_hessian()
  if (is.null(campioni) || length(campioni) < 3) return(NULL)
  list(campioni = campioni, nomi_par = names(campioni[[1]]$parametri),
       mappa = tryCatch(batch_risultati()$simboli_modello, error = function(e) NULL))
}

disegna_istogramma_batch_pannello <- function(i) {
  d <- dati_istogrammi_batch()
  par(mar = c(6, 4, 3, 1))
  if (is.null(d) || i > length(d$nomi_par)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Need at least 3 samples fitted with the Linear (Jacobian) method.")
    return(invisible(NULL))
  }
  col_ist <- if (is.null(input$batch_col_ist_par)) "#f39c12" else input$batch_col_ist_par
  col_bordo_ist <- if (is.null(input$batch_col_bordo_ist_par)) "black" else input$batch_col_bordo_ist_par
  fnt_cex_ist <- if (is.null(input$batch_fnt_cex_ist_par)) 0.8 else input$batch_fnt_cex_ist_par
  ordina_crescente <- if (is.null(input$batch_ordina_ist_par)) FALSE else input$batch_ordina_ist_par
  
  nome_par <- d$nomi_par[i]
  valori <- sapply(d$campioni, function(r) r$parametri[[nome_par]])
  errori <- sapply(d$campioni, function(r) r$errori[[nome_par]])
  nomi_campioni <- sapply(d$campioni, function(r) r$nome)
  validi <- is.finite(valori)
  valori <- valori[validi]; errori <- errori[validi]; nomi_campioni <- nomi_campioni[validi]
  simbolo <- interpreta_etichetta(if (!is.null(d$mappa) && !is.null(d$mappa[[nome_par]])) d$mappa[[nome_par]] else simbolo_automatico_pedice(nome_par))
  if (ordina_crescente) {
    idx_ord <- order(valori)
    valori <- valori[idx_ord]; errori <- errori[idx_ord]; nomi_campioni <- nomi_campioni[idx_ord]
  }
  mostra_x <- titolo_x_attivo(input$batch_mostra_titolo_x_ist_par)
  righe_nomi <- prepara_margine_nomi_ruotati(nomi_campioni, fnt_cex_ist, con_titolo = mostra_x)
  posizioni <- barplot(valori, names.arg = nomi_campioni, main = simbolo, ylab = "Value",
                       col = col_ist, border = col_bordo_ist, las = 2, cex.names = fnt_cex_ist,
                       ylim = limiti_asse_barre(c(valori, valori - errori, valori + errori)))
  titolo_asse_x_ruotato("Sample", righe_nomi, mostra_x)
  # Error bars only where the standard error is finite (it may not be
  # for a single sample even if the fit itself succeeded).
  idx_con_errore <- which(is.finite(errori) & errori > 0)
  if (length(idx_con_errore) > 0) {
    arrows(posizioni[idx_con_errore], valori[idx_con_errore] - errori[idx_con_errore],
           posizioni[idx_con_errore], valori[idx_con_errore] + errori[idx_con_errore],
           angle = 90, code = 3, length = 0.05, col = col_bordo_ist, lwd = 1.2)
  }
}

registra_sezione_pannelli("ist_batch",
                          n_pannelli = function() { d <- dati_istogrammi_batch(); if (is.null(d)) 0L else length(d$nomi_par) },
                          disegna = function(k) disegna_istogramma_batch_pannello(k),
                          prefisso_file = "batch_parameter_histogram")

output$sezione_istogrammi_batch_ui <- renderUI({
  d <- dati_istogrammi_batch()
  if (is.null(d)) return(NULL)
  ui_sezione_pannelli("ist_batch", length(d$nomi_par), "Parameter Distributions (Linear/Jacobian Method):",
    "One bar chart per parameter, one bar per sample (its point estimate, with an error bar from the Jacobian-based standard error) \u2014 shows how much each parameter varies sample to sample.",
    "btn_estetica_istogrammi_batch")
})

# ---- Parameter Distributions (Monte Carlo): one plot per parameter ----
# One box/violin per sample, from the whole bootstrap distribution. At least
# 2 samples fitted with Monte Carlo are needed.
dati_boxplot_batch <- function() {
  campioni <- batch_campioni_montecarlo()
  if (is.null(campioni) || length(campioni) < 2) return(NULL)
  list(campioni = campioni, nomi_par = names(campioni[[1]]$parametri),
       mappa = tryCatch(batch_risultati()$simboli_modello, error = function(e) NULL))
}

disegna_boxplot_batch_pannello <- function(i) {
  d <- dati_boxplot_batch()
  par(mar = c(6, 4, 3, 1))
  if (is.null(d) || i > length(d$nomi_par)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Need at least 2 samples fitted with Monte Carlo.")
    return(invisible(NULL))
  }
  col_box <- if (is.null(input$batch_col_box_par)) "#f39c12" else input$batch_col_box_par
  col_bordo_box <- if (is.null(input$batch_col_bordo_box_par)) "black" else input$batch_col_bordo_box_par
  mostra_outlier <- if (is.null(input$batch_mostra_outlier_box_par)) TRUE else input$batch_mostra_outlier_box_par
  fnt_cex_box <- if (is.null(input$batch_fnt_cex_box_par)) 0.8 else input$batch_fnt_cex_box_par
  tipo_grafico <- if (is.null(input$batch_tipo_grafico_box_par)) "boxplot" else input$batch_tipo_grafico_box_par
  ordina_crescente <- if (is.null(input$batch_ordina_box_par)) FALSE else input$batch_ordina_box_par
  
  nome_par <- d$nomi_par[i]
  dati_lista <- lapply(d$campioni, function(r) r$mat_boot[, nome_par])
  names(dati_lista) <- sapply(d$campioni, function(r) r$nome)
  if (ordina_crescente) {
    mediane <- sapply(dati_lista, median, na.rm = TRUE)
    dati_lista <- dati_lista[order(mediane)]
  }
  simbolo <- interpreta_etichetta(if (!is.null(d$mappa) && !is.null(d$mappa[[nome_par]])) d$mappa[[nome_par]] else simbolo_automatico_pedice(nome_par))
  mostra_x <- titolo_x_attivo(input$batch_mostra_titolo_x_box_par)
  righe_nomi <- prepara_margine_nomi_ruotati(names(dati_lista), fnt_cex_box, con_titolo = mostra_x)
  if (tipo_grafico == "violin") {
    disegna_violin_gruppi(dati_lista, main = simbolo, ylab = "Value", col = col_box, col_bordo = col_bordo_box, cex_axis = fnt_cex_box)
  } else {
    boxplot(dati_lista, main = simbolo, ylab = "Value", las = 2, col = col_box, border = col_bordo_box,
            outline = mostra_outlier, cex.axis = fnt_cex_box)
  }
  titolo_asse_x_ruotato("Sample", righe_nomi, mostra_x)
}

registra_sezione_pannelli("box_batch",
                          n_pannelli = function() { d <- dati_boxplot_batch(); if (is.null(d)) 0L else length(d$nomi_par) },
                          disegna = function(k) disegna_boxplot_batch_pannello(k),
                          prefisso_file = "batch_parameter_boxplot")

output$sezione_boxplot_batch_ui <- renderUI({
  d <- dati_boxplot_batch()
  if (is.null(d)) return(NULL)
  ui_sezione_pannelli("box_batch", length(d$nomi_par), "Parameter Distributions (Monte Carlo):",
    "One plot per parameter, one box/violin per sample, built from the full Monte Carlo bootstrap distributions (not just the point estimates). Boxplot vs Violin Plot is chosen in these plots' Aesthetics (\U0001F3A8).",
    "btn_estetica_boxplot_batch")
})

batch_tabella_diagnostica_df <- reactive({
  br <- batch_risultati()
  if (is.null(br)) return(NULL)
  righe <- lapply(br$risultati, function(r) {
    if (!isTRUE(r$successo)) return(data.frame(Sample = r$nome, Status = r$messaggio, stringsAsFactors = FALSE))
    riga <- data.frame(Sample = r$nome, Status = r$messaggio, Error_Method = r$errore_metodo, N_Obs = r$obs, DF = r$df,
               Loss_Value = signif(r$loss_finale, 5), RMSE = round(r$rmse, 5), R2 = round(r$r2, 5), R2_Adj = round(r$r2_adj, 5),
               AIC = round(r$aic, 3), AICc = round(r$aicc, 3), BIC = round(r$bic, 3), stringsAsFactors = FALSE)
    # Columns added ONLY if the current batch used Auto-retry:
    # how many guesses were tried in total, and whether in the end the chosen
    # acceptance criteria (convergence, valid errors, non-negative R2,
    # and any threshold on the standard error) were really
    # met - a sample with "No" here still has a result (it is not
    # a total failure like r$successo=FALSE), but it reached the
    # maximum number of attempts without ever meeting the chosen criteria,
    # so it deserves an extra look before trusting it.
    if (isTRUE(br$retry_usato)) {
      riga[["Initial_Guesses_Tried"]] <- if (is.null(r$numero_guess_tentati)) NA_integer_ else r$numero_guess_tentati
      riga[["Retry_Criteria_Met"]] <- if (is.null(r$criteri_retry_soddisfatti)) NA_character_ else if (isTRUE(r$criteri_retry_soddisfatti)) "Yes" else "No"
    }
    # With weighted regression: zeta used for the sample (with zeta per
    # sample it differs from sample to sample); column absent without weights.
    if (isTRUE(br$pesi_batch$attiva)) riga[["Weighting_Zeta"]] <- if (ponderazione_attiva(r)) signif(r$ponderazione$zeta, 4) else NA_real_
    riga
  })
  rbind_riempi_na(righe)
})

# Exactly the same sorting mechanism already used in the Comparison
# Table of Model Comparison.
output$batch_nota_criteri <- renderUI({
  br <- batch_risultati()
  if (is.null(br) || is.null(br$loss_usata) || identical(br$loss_usata, "mse")) return(NULL)
  p(style = "font-size: 11px; color: #e67e22; margin-top: -6px;", tags$strong("AIC, AICc and BIC: "), NOTA_CRITERI_LOSS)
})

batch_tabella_diagnostica_ordinata <- reactive({
  df <- batch_tabella_diagnostica_df()
  if (is.null(df)) return(NULL)
  campo_ord <- if (is.null(input$batch_diag_sort_by) || !(input$batch_diag_sort_by %in% colnames(df))) "Sample" else input$batch_diag_sort_by
  decrescente <- isTRUE(input$batch_diag_sort_dir == "desc")
  idx <- order(df[[campo_ord]], decreasing = decrescente, na.last = TRUE)
  df[idx, ]
})

output$tabella_batch_diagnostica <- renderTable({
  df <- batch_tabella_diagnostica_ordinata()
  if (is.null(df)) return(data.frame(Message = "Run a batch elaboration first."))
  # With a loss other than MSE the AIC/AICc/BIC columns are not shown
  # (note under the table, output$batch_nota_criteri).
  br_loss <- batch_risultati()$loss_usata
  if (!is.null(br_loss) && !identical(br_loss, "mse")) df <- df[, setdiff(colnames(df), c("AIC", "AICc", "BIC")), drop = FALSE]
  if (is.null(df)) return(data.frame(Message = "Run a batch elaboration first."))
  for (col in c("RMSE", "R2", "R2_Adj", "AIC", "AICc", "BIC")) {
    if (col %in% colnames(df) && is.numeric(df[[col]])) df[[col]] <- ifelse(is.na(df[[col]]), "N/A", sprintf("%.5f", df[[col]]))
  }
  # Adaptive format (not fixed at 5 decimals like RMSE/R2/AIC): the value
  # of the loss can vary by many orders of magnitude depending on the
  # chosen type and the scale of the data - "%.5g" automatically switches to
  # scientific notation for very small or very large values, instead
  # of truncating them to "0.00000" with a fixed format.
  if ("Loss_Value" %in% colnames(df) && is.numeric(df[["Loss_Value"]])) {
    df[["Loss_Value"]] <- ifelse(is.na(df[["Loss_Value"]]), "N/A", sprintf("%.5g", df[["Loss_Value"]]))
  }
  # Format "attempts/maximum" (e.g. "10/100"): the maximum is the one
  # ACTUALLY used in that run (batch_risultati()$retry_max_usato), not
  # the current value of the slider, which might have been changed
  # after launching the batch.
  if ("Initial_Guesses_Tried" %in% colnames(df)) {
    br <- batch_risultati()
    massimo_usato <- if (is.null(br) || is.null(br$retry_max_usato)) NA_integer_ else br$retry_max_usato
    df[["Initial_Guesses_Tried"]] <- ifelse(is.na(df[["Initial_Guesses_Tried"]]), "N/A",
                                             paste0(as.character(as.integer(round(df[["Initial_Guesses_Tried"]]))), "/", massimo_usato))
  }
  # "No" highlighted in red: it is the signal that that sample has
  # exhausted all Auto-retry attempts without ever meeting the
  # chosen acceptance criteria - it is worth noticing it at a
  # glance, not only by reading the text.
  if ("Retry_Criteria_Met" %in% colnames(df)) {
    df[["Retry_Criteria_Met"]] <- ifelse(is.na(df[["Retry_Criteria_Met"]]), "N/A",
                                          ifelse(df[["Retry_Criteria_Met"]] == "No",
                                                 "<span style='color:#c0392b; font-weight:600;'>No</span>",
                                                 df[["Retry_Criteria_Met"]]))
  }
  # Space instead of underscore ONLY in the headers shown on
  # screen (the names with underscores remain intact internally, they are needed
  # for the "Sort By" selector and to refer to the columns in the code) -
  # applied here, last, to all columns for consistency (not only
  # to those just added, which shared the same problem already
  # present in Error_Method/N_Obs/R2_Adj). R2/R2_Adj become R with "2"
  # as superscript: sanitize.text.function is already disabled for this
  # table (needed to colour "No" above), so the HTML \u003csup\u003e passes
  # without needing anything else.
  colnames(df) <- gsub("_", " ", colnames(df))
  colnames(df) <- gsub("^R2$", "R<sup>2</sup>", colnames(df))
  colnames(df) <- gsub("^R2 Adj$", "R<sup>2</sup> Adj", colnames(df))
  # Loss type in the header (e.g. "Loss Value (mse)"), same style
  # as the "[Type: mse]" already used in Single Fit - the raw code, not
  # a prettified label, for consistency with that precedent.
  colnames(df) <- gsub("^Loss Value$", paste0("Loss Value (", input$tipo_loss, ")"), colnames(df))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", sanitize.text.function = function(x) x)

# ---- Export ----
# ---- Parameter Significance Test (between 2 chosen samples) ----
# Adapted from exactly the same logic as Model Comparison
# (test_parametri_risultato in model_comparison.R), simplified here: in the
# batch the model is always the same by construction, so neither
# the check on the number of parameters nor the warning on different model
# types that were needed there are required.
output$batch_selezione_campioni_test <- renderUI({
  br <- batch_risultati()
  if (is.null(br)) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a batch elaboration first."))
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  if (length(nomi_ok) < 2) return(p(style = "font-size: 12px; color: #7f8c8d;", "At least 2 successfully fitted samples are needed."))
  fluidRow(
    column(6, selectInput("batch_campione_test_a", "Sample A:", choices = nomi_ok, selected = nomi_ok[1])),
    column(6, selectInput("batch_campione_test_b", "Sample B:", choices = nomi_ok, selected = nomi_ok[2]))
  )
})

# Pure statistical comparison between 2 samples (extracted here because reused both
# by the single pairwise test below and by the post-hoc on 3+ samples further down -
# no logic duplicated between the two).
calcola_confronto_coppia <- function(m1, m2, livello_confidenza, mappa_simboli = NULL) {
  p1 <- as.numeric(m1$parametri); se1 <- as.numeric(m1$errori)
  p2 <- as.numeric(m2$parametri); se2 <- as.numeric(m2$errori)
  # Converts a raw parameter name into its plain Unicode symbol,
  # if a map was provided by the caller (batch or Model Comparison,
  # each with its own source for this map) - otherwise the name
  # stays as it is.
  etichetta_parametro <- function(nome_par) {
    if (!is.null(mappa_simboli) && !is.null(mappa_simboli[[nome_par]])) {
      simbolo_a_testo_semplice(mappa_simboli[[nome_par]])
    } else {
      simbolo_a_testo_semplice(simbolo_automatico_pedice(nome_par))
    }
  }
  nomi_etichettati <- sapply(names(m1$parametri), etichetta_parametro)
  
  entrambi_bootstrap <- !is.null(m1$mat_boot) && !is.null(m2$mat_boot) && 
                         nrow(m1$mat_boot) >= MINIMO_ITERAZIONI_MC && nrow(m2$mat_boot) >= MINIMO_ITERAZIONI_MC
  
  if (entrambi_bootstrap) {
    p_inf <- (1 - livello_confidenza / 100) / 2
    p_sup <- 1 - p_inf
    n_par <- length(p1)
    
    diff_val <- ci_diff_inf <- ci_diff_sup <- p_value <- rep(NA_real_, n_par)
    n_coppie_usate <- 0; n_coppie_possibili <- 0
    for (j in seq_len(n_par)) {
      v1 <- m1$mat_boot[, j]; v1 <- v1[is.finite(v1)]
      v2 <- m2$mat_boot[, j]; v2 <- v2[is.finite(v2)]
      if (length(v1) < MINIMO_ITERAZIONI_MC || length(v2) < MINIMO_ITERAZIONI_MC) next
      # All n1 x n2 differences up to one million; beyond that (e.g. 5000 x 5000 =
      # 25 million, ~200 MB and a slow sort, which in the browser could
      # freeze the app) a SYSTEMATIC sample of one million: one difference
      # every k in the complete sequence. Deterministic (same data, same
      # result) and statistically equivalent, because the bootstrap
      # iterations are independent and in random order.
      n_tot <- length(v1) * length(v2); n_coppie_possibili <- n_tot
      if (n_tot <= 1e6) {
        diff_boot <- as.vector(outer(v1, v2, "-"))
      } else {
        k <- round(seq(1, n_tot, length.out = 1e6)) - 1
        diff_boot <- v1[k %% length(v1) + 1] - v2[k %/% length(v1) + 1]
      }
      n_coppie_usate <- length(diff_boot)
      ci_diff <- quantile(diff_boot, probs = c(p_inf, p_sup), na.rm = TRUE)
      diff_val[j] <- abs(p1[j] - p2[j])
      ci_diff_inf[j] <- ci_diff[1]
      ci_diff_sup[j] <- ci_diff[2]
      prop_sotto <- mean(diff_boot <= 0)
      prop_sopra <- mean(diff_boot >= 0)
      p_value[j] <- min(1, 2 * min(prop_sotto, prop_sopra))
    }
    
    significativita <- symnum(p_value, corr = FALSE, na = FALSE, 
                              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                              symbols = c("***", "**", "*", ".", "ns"))
    
    df_test <- data.frame(
      Parameter = nomi_etichettati,
      Value_A = round(p1, 5),
      Value_B = round(p2, 5),
      Absolute_Diff = round(diff_val, 5),
      CI_Diff_Lower = round(ci_diff_inf, 5),
      CI_Diff_Upper = round(ci_diff_sup, 5),
      P_Value_Raw = p_value,
      P_Value = ifelse(is.na(p_value), "N/A", format.pval(p_value, digits = 3, eps = 1/n_coppie_usate)),
      Significant = ifelse(is.na(p_value), "N/A", as.character(significativita))
    )
    metodo_usato <- if (n_coppie_possibili > n_coppie_usate) {
      sprintf("Empirical Bootstrap (both samples fitted with Monte Carlo; a systematic sample of %s of the %s possible pairwise differences used per parameter)",
              format(n_coppie_usate, big.mark = ","), format(n_coppie_possibili, big.mark = ","))
    } else sprintf("Empirical Bootstrap (both samples fitted with Monte Carlo; all %s pairwise differences used per parameter)", format(n_coppie_usate, big.mark = ","))
    
  } else {
    df1 <- max(m1$obs - length(m1$parametri), 1)
    df2 <- max(m2$obs - length(m2$parametri), 1)
    diff_val <- abs(p1 - p2)
    se_diff <- sqrt(se1^2 + se2^2)
    df_welch <- (se1^2 + se2^2)^2 / ((se1^4 / df1) + (se2^4 / df2))
    df_welch <- pmax(df_welch, 1)
    t_score <- diff_val / se_diff
    p_value <- 2 * (1 - pt(t_score, df = df_welch))
    
    significativita <- symnum(p_value, corr = FALSE, na = FALSE, 
                              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                              symbols = c("***", "**", "*", ".", "ns"))
    
    df_test <- data.frame(
      Parameter = nomi_etichettati,
      Value_A = round(p1, 5),
      Value_B = round(p2, 5),
      Absolute_Diff = round(diff_val, 5),
      Std_Error_Diff = round(se_diff, 5),
      T_Score = round(t_score, 3),
      DF_Welch = round(df_welch, 1),
      P_Value_Raw = p_value,
      P_Value = format.pval(p_value, digits = 3, eps = 0.001),
      Significant = as.character(significativita)
    )
    metodo_usato <- "Welch's t-test (parametric) \u2014 at least one of the two samples used the Linear error method, so no bootstrap distribution is available for the empirical test."
  }
  
  list(successo = TRUE, tabella = df_test, metodo = metodo_usato)
}

batch_test_parametri_risultato <- reactiveVal(NULL)
observeEvent(input$batch_esegui_test, {
  br <- batch_risultati()
  if (is.null(br)) { batch_test_parametri_risultato(list(successo = FALSE, messaggio = "Run a batch elaboration first.")); return() }
  if (is.null(input$batch_campione_test_a) || is.null(input$batch_campione_test_b)) {
    batch_test_parametri_risultato(list(successo = FALSE, messaggio = "Select two samples from the dropdowns above.")); return()
  }
  if (input$batch_campione_test_a == input$batch_campione_test_b) {
    batch_test_parametri_risultato(list(successo = FALSE, messaggio = "Select two DIFFERENT samples to compare.")); return()
  }
  m1 <- br$risultati[[input$batch_campione_test_a]]
  m2 <- br$risultati[[input$batch_campione_test_b]]
  if (!isTRUE(m1$successo) || !isTRUE(m2$successo)) {
    batch_test_parametri_risultato(list(successo = FALSE, messaggio = "Both selected samples must have been fitted successfully.")); return()
  }
  batch_test_parametri_risultato(calcola_confronto_coppia(m1, m2, input$livello_confidenza, br$simboli_modello))
})

output$batch_tabella_test <- renderTable({
  ris <- tryCatch(batch_test_parametri_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Select two samples above and click 'Run Test'."))
  if (!ris$successo) return(data.frame(Message = ris$messaggio))
  tab <- ris$tabella[, setdiff(colnames(ris$tabella), "P_Value_Raw")]
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$batch_info_metodo_test <- renderText({
  ris <- tryCatch(batch_test_parametri_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return("")
  ris$metodo
})

# ---- Post-Hoc Comparison (3+ samples) ----
# Cochran's Q (weighted homogeneity test, standard in meta-analysis for
# comparing several independent estimates each with its own uncertainty -
# here we do not have raw replicates per sample as a classic ANOVA would
# require, only an estimate + its standard error, so this is the
# statistically correct counterpart, not an approximation) as the omnibus
# test, followed by pairwise comparisons with correction for multiple tests
# as post-hoc - same role as a Tukey HSD after an ANOVA.
calcola_cochran_q <- function(stime, errori_standard) {
  validi <- is.finite(stime) & is.finite(errori_standard) & errori_standard > 0
  if (sum(validi) < 3) return(list(Q = NA, df = NA, p_value = NA))
  stime <- stime[validi]; errori_standard <- errori_standard[validi]
  pesi <- 1 / errori_standard^2
  media_pesata <- sum(pesi * stime) / sum(pesi)
  Q <- sum(pesi * (stime - media_pesata)^2)
  df <- sum(validi) - 1
  p_value <- 1 - pchisq(Q, df)
  list(Q = Q, df = df, p_value = p_value)
}

output$batch_selezione_campioni_posthoc <- renderUI({
  br <- batch_risultati()
  if (is.null(br)) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a batch elaboration first."))
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  if (length(nomi_ok) < 3) return(p(style = "font-size: 12px; color: #7f8c8d;", "At least 3 successfully fitted samples are needed."))
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("batch_posthoc_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("batch_posthoc_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("batch_campioni_posthoc", "Samples to include:", choices = nomi_ok, selected = nomi_ok, inline = TRUE)
  )
})
observeEvent(input$batch_posthoc_select_all, {
  br <- batch_risultati()
  if (is.null(br)) return()
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  updateCheckboxGroupInput(session, "batch_campioni_posthoc", selected = nomi_ok)
})
observeEvent(input$batch_posthoc_deselect_all, {
  updateCheckboxGroupInput(session, "batch_campioni_posthoc", selected = character(0))
})

batch_posthoc_risultato <- reactiveVal(NULL)

output$sezione_batch_posthoc_ui <- renderUI({
  ris <- batch_posthoc_risultato()
  if (is.null(ris)) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Omnibus Test (Cochran's Q, per parameter):"),
      tags$button(id = "btn_copia_batch_omnibus", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('batch_tabella_omnibus', 'btn_copia_batch_omnibus')")
    ),
    tableOutput("batch_tabella_omnibus"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Pairwise Comparisons:"),
      tags$button(id = "btn_copia_batch_posthoc", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('batch_tabella_posthoc', 'btn_copia_batch_posthoc')")
    ),
    tags$em(style = "font-size: 12px; color: #566573;", textOutput("batch_info_posthoc", inline = TRUE)),
    tableOutput("batch_tabella_posthoc"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Compact Letter Display:"),
      tags$button(id = "btn_copia_batch_cld", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('batch_tabella_cld', 'btn_copia_batch_cld')")
    ),
    p(style = "font-size: 12px; color: #566573;",
      "Same format as a classic ANOVA post-hoc: samples sharing a letter for a given parameter are ", tags$strong("not"), " significantly different from each other; a different letter means they are."),
    tableOutput("batch_tabella_cld")
  )
})
observeEvent(input$batch_esegui_posthoc, {
  br <- batch_risultati()
  if (is.null(br)) { batch_posthoc_risultato(list(successo = FALSE, messaggio = "Run a batch elaboration first.")); return() }
  campioni_scelti <- input$batch_campioni_posthoc
  if (is.null(campioni_scelti) || length(campioni_scelti) < 3) {
    batch_posthoc_risultato(list(successo = FALSE, messaggio = "Select at least 3 samples.")); return()
  }
  lista <- br$risultati[campioni_scelti]
  if (!all(sapply(lista, function(r) isTRUE(r$successo)))) {
    batch_posthoc_risultato(list(successo = FALSE, messaggio = "All selected samples must have been fitted successfully.")); return()
  }
  
  nomi_parametri <- names(lista[[1]]$parametri)
  metodo_correzione <- if (is.null(input$batch_metodo_correzione)) "holm" else input$batch_metodo_correzione
  nomi_campioni <- names(lista)
  coppie <- combn(nomi_campioni, 2, simplify = FALSE)
  n_coppie <- length(coppie)
  
  withProgress(message = "Running Post-Hoc Comparison...", value = 0, {
  
  # 1. Omnibus test (Cochran's Q), one row per parameter - uses the raw
  # name for the internal lookups, the replaced symbol only for the
  # Parameter column shown (avoids breaking the subsequent lookups that search
  # by raw name in the parameter vectors).
  righe_omnibus <- lapply(nomi_parametri, function(nome_par) {
    stime <- sapply(lista, function(r) r$parametri[[nome_par]])
    errori <- sapply(lista, function(r) r$errori[[nome_par]])
    q_ris <- calcola_cochran_q(stime, errori)
    sig <- if (is.na(q_ris$p_value)) "N/A" else as.character(symnum(q_ris$p_value, corr = FALSE, na = FALSE, 
                                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
    data.frame(Parameter = etichetta_parametro_batch(nome_par, br), N_Samples = length(lista), 
               Cochran_Q = round(q_ris$Q, 3), DF = q_ris$df, 
               P_Value = if (is.na(q_ris$p_value)) "N/A" else format.pval(q_ris$p_value, digits = 3, eps = 0.001),
               Significant = sig, stringsAsFactors = FALSE)
  })
  df_omnibus <- do.call(rbind, righe_omnibus)
  
  # 2. Post-hoc: all possible pairs, reusing the same logic as the
  # single pairwise test, then correction for multiple tests on the raw
  # p-value (internal column already present in every comparison). tab$Parameter
  # here is ALREADY the replaced symbol (calcola_confronto_coppia already applies it
  # internally), so the correction loop below must compare
  # with the LABELLED name, not with the raw one of nomi_parametri.
  # Progress step per pair: it is the real bottleneck, since
  # the number of pairs grows quadratically with the number of samples
  # (n samples -> n*(n-1)/2 comparisons), unlike the omnibus above
  # which scales only with the number of parameters (usually few).
  righe_posthoc <- lapply(seq_along(coppie), function(i) {
    incProgress(1 / n_coppie, detail = paste0("Pair ", i, " of ", n_coppie))
    coppia <- coppie[[i]]
    confronto <- calcola_confronto_coppia(lista[[coppia[1]]], lista[[coppia[2]]], input$livello_confidenza, br$simboli_modello)
    tab <- confronto$tabella
    data.frame(Sample_A = coppia[1], Sample_B = coppia[2], Parameter = tab$Parameter,
               Value_A = tab$Value_A, Value_B = tab$Value_B, Absolute_Diff = tab$Absolute_Diff,
               P_Value_Raw = tab$P_Value_Raw, stringsAsFactors = FALSE)
  })
  df_posthoc <- do.call(rbind, righe_posthoc)
  
  # The correction must be applied PER PARAMETER (each parameter has its own
  # set of pairwise comparisons, corrected separately from the other
  # parameters - not all together in a single block). Compares with the
  # LABELLED name (nomi_parametri_etichettati), consistent with df_posthoc$Parameter.
  nomi_parametri_etichettati <- sapply(nomi_parametri, function(p) etichetta_parametro_batch(p, br))
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
  df_posthoc$P_Value_Raw <- NULL
  
  batch_posthoc_risultato(list(successo = TRUE, omnibus = df_omnibus, posthoc = df_posthoc, 
       metodo_correzione = metodo_correzione, n_confronti = length(coppie) * length(nomi_parametri),
       nomi_parametri_grezzi = nomi_parametri, nomi_parametri_etichettati = nomi_parametri_etichettati))
  })
})

output$batch_tabella_omnibus <- renderTable({
  ris <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Select at least 3 samples above and click 'Run Post-Hoc Comparison'."))
  if (!ris$successo) return(data.frame(Message = ris$messaggio))
  tab <- ris$omnibus
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$batch_tabella_posthoc <- renderTable({
  ris <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(NULL)
  if (!ris$successo) return(NULL)
  tab <- ris$posthoc
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", digits = 4)

output$batch_info_posthoc <- renderText({
  ris <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return("")
  # The correction is applied parameter by parameter (previously the text said
  # "across all parameters, corrected with...", as if it were a single one).
  n_par <- max(length(unique(ris$posthoc$Parameter)), 1)
  paste0(ris$n_confronti, " pairwise comparisons in total (", ris$n_confronti %/% n_par, " per parameter); p-values corrected within each parameter with the ",
         etichetta_correzione(ris$metodo_correzione), " method.", descrizione_correzione(ris$metodo_correzione))
})

# ---- Compact Letter Display (CLD) ----
# Classic format of ANOVA post-hoc tests (e.g. Tukey HSD): two samples
# share a letter if and only if they were NOT found significantly
# different. Standard greedy algorithm: goes through the samples in order, forms a
# group with all those mutually compatible (no significant pair
# within it), assigns a letter, repeats on the remaining ones -
# computed separately for each parameter, since the differences
# can vary from one parameter to another.
assegna_lettere_cld <- function(nomi_campioni, coppie_significative) {
  n <- length(nomi_campioni)
  alfabeto <- letters
  
  e_significativa <- function(a, b) {
    (a %in% names(coppie_significative) && b %in% coppie_significative[[a]]) ||
    (b %in% names(coppie_significative) && a %in% coppie_significative[[b]])
  }
  # Compatible = NOT significantly different. IMPORTANT: vicini() must
  # exclude the node itself - including it (however true "compatible with
  # itself" may be statistically) would prevent Bron-Kerbosch from ever reducing
  # the set P, causing infinite recursion.
  e_compatibile <- function(a, b) !e_significativa(a, b)
  
  vicini <- function(v, insieme) Filter(function(u) u != v && e_compatibile(v, u), insieme)
  
  # Bron-Kerbosch (without pivoting): enumerates ALL the MAXIMAL cliques of the
  # compatibility graph - unlike a simple greedy grouping
  # "assign a letter and move on", this correctly handles
  # non-transitive cases (e.g. sample C compatible with both
  # A and B, but A and B incompatible with each other: C must appear in
  # BOTH cliques/letters, not only in the first one found).
  clique_trovate <- list()
  bron_kerbosch <- function(R, P, X) {
    if (length(P) == 0 && length(X) == 0) {
      clique_trovate[[length(clique_trovate) + 1]] <<- R
      return(invisible(NULL))
    }
    P_corrente <- P
    for (v in P) {
      bron_kerbosch(c(R, v), intersect(P_corrente, vicini(v, nomi_campioni)), intersect(X, vicini(v, nomi_campioni)))
      P_corrente <- setdiff(P_corrente, v)
      X <- c(X, v)
    }
  }
  bron_kerbosch(character(0), nomi_campioni, character(0))
  
  # Sorts the cliques for reproducibility (same order of letters at every
  # run): by first element in common with the original order of the
  # samples, then by size.
  ordine_indice <- function(clique) min(match(clique, nomi_campioni))
  clique_trovate <- clique_trovate[order(sapply(clique_trovate, ordine_indice))]
  
  lettere_assegnate <- setNames(vector("list", n), nomi_campioni)
  for (nc in nomi_campioni) lettere_assegnate[[nc]] <- character(0)
  for (i in seq_along(clique_trovate)) {
    lettera <- alfabeto[i]
    for (c in clique_trovate[[i]]) lettere_assegnate[[c]] <- c(lettere_assegnate[[c]], lettera)
  }
  sapply(nomi_campioni, function(c) paste(sort(lettere_assegnate[[c]]), collapse = ""))
}

batch_tabella_cld_df <- reactive({
  ris <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
  br <- batch_risultati()
  if (is.null(ris) || !isTRUE(ris$successo) || is.null(br)) return(NULL)
  
  nomi_campioni <- input$batch_campioni_posthoc
  
  righe <- lapply(seq_along(ris$nomi_parametri_grezzi), function(i) {
    nome_par_grezzo <- ris$nomi_parametri_grezzi[i]
    nome_par_etichettato <- ris$nomi_parametri_etichettati[i]
    sotto <- ris$posthoc[ris$posthoc$Parameter == nome_par_etichettato, ]
    coppie_sig <- list()
    for (j in seq_len(nrow(sotto))) {
      if (identical(sotto$Significant[j], "ns") || identical(sotto$Significant[j], "N/A")) next
      a <- sotto$Sample_A[j]; b <- sotto$Sample_B[j]
      coppie_sig[[a]] <- c(coppie_sig[[a]], b)
    }
    lettere <- assegna_lettere_cld(nomi_campioni, coppie_sig)
    valori <- sapply(nomi_campioni, function(c) br$risultati[[c]]$parametri[[nome_par_grezzo]])
    # unname(): the parameter name arrived with a label, which data.frame()
    # tried to use as row names (warning "row.names ... discarded").
    data.frame(Parameter = unname(nome_par_etichettato), Sample = nomi_campioni, Value = round(unname(valori), 5), Group = lettere,
               stringsAsFactors = FALSE, row.names = NULL)
  })
  do.call(rbind, righe)
})

output$batch_tabella_cld <- renderTable({
  df <- batch_tabella_cld_df()
  if (is.null(df)) return(NULL)
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", digits = 4)

output$scarica_batch_csv <- downloadHandler(
  filename = function() paste0("batch_elaboration_", Sys.Date(), ".csv"),
  content = function(file) {
    br <- batch_risultati()
    df_par <- batch_tabella_parametri_df()
    df_diag <- batch_tabella_diagnostica_df()
    if (is.null(df_par)) return()
    ris_test <- tryCatch(batch_test_parametri_risultato(), error = function(e) NULL)
    df_test <- if (!is.null(ris_test) && isTRUE(ris_test$successo)) ris_test$tabella[, setdiff(colnames(ris_test$tabella), "P_Value_Raw")] else data.frame()
    ris_posthoc <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
    trasf_y_usata <- if (is.null(input$batch_trasformazione_y) || input$batch_trasformazione_y == "nessuna") "None (raw values)" else {
      scelte_trasf <- costruisci_scelte_trasformazione_y()
      nomi_scelte <- names(scelte_trasf)[scelte_trasf == input$batch_trasformazione_y]
      if (length(nomi_scelte) > 0) nomi_scelte[1] else input$batch_trasformazione_y
    }
    
    cat("--- INFO ---\n", file = file)
    cat(paste0("Y Pre-transformation (applied to all samples): ", trasf_y_usata, "\n"), file = file, append = TRUE)
    cat(paste0("Model: ", input$tipo_modello, "\n"), file = file, append = TRUE)
    cat(paste0("Loss Function: ", input$tipo_loss, "\n"), file = file, append = TRUE)
    cat(paste0("Weighting: ", if (isTRUE(br$pesi_batch$attiva)) descrizione_pesi_batch(br$pesi_batch) else "None", "\n"), file = file, append = TRUE)
    cat("\n--- PARAMETER ESTIMATION RESULTS (ALL SAMPLES) ---\n", file = file, append = TRUE)
    write.table(df_par, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat("\n--- FITTING PARAMETERS & GOODNESS-OF-FIT DIAGNOSTICS (ALL SAMPLES) ---\n", file = file, append = TRUE)
    write.table(df_diag, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    df_bdr <- riepilogo_diagnostica_per_export()
    if (!is.null(df_bdr)) {
      cat("\n--- RESIDUAL DIAGNOSTICS (ALL SAMPLES) ---\n", file = file, append = TRUE)
      write.table(df_bdr, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE, na = "")
    }
    if (nrow(df_test) > 0) {
      cat("\n--- PARAMETER SIGNIFICANCE TEST (2 SAMPLES) ---\n", file = file, append = TRUE)
      write.table(df_test, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      cat("\n--- POST-HOC COMPARISON: OMNIBUS TEST (COCHRAN'S Q, 3+ SAMPLES) ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$omnibus, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON: PAIRWISE (", toupper(etichetta_correzione(ris_posthoc$metodo_correzione)), " CORRECTED WITHIN EACH PARAMETER) ---\n", file = file, append = TRUE, sep = "")
      write.table(ris_posthoc$posthoc, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      df_cld <- batch_tabella_cld_df()
      if (!is.null(df_cld)) {
        cat("\n--- POST-HOC COMPARISON: COMPACT LETTER DISPLAY ---\n", file = file, append = TRUE)
        write.table(df_cld, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      }
    }
  }
)

output$scarica_batch_excel <- downloadHandler(
  filename = function() paste0("batch_elaboration_", Sys.Date(), ".xlsx"),
  content = function(file) {
    br <- batch_risultati()
    df_par <- batch_tabella_parametri_df()
    df_diag <- batch_tabella_diagnostica_df()
    if (is.null(df_par)) return()
    trasf_y_usata <- if (is.null(input$batch_trasformazione_y) || input$batch_trasformazione_y == "nessuna") "None (raw values)" else {
      scelte_trasf <- costruisci_scelte_trasformazione_y()
      nomi_scelte <- names(scelte_trasf)[scelte_trasf == input$batch_trasformazione_y]
      if (length(nomi_scelte) > 0) nomi_scelte[1] else input$batch_trasformazione_y
    }
    df_info <- data.frame(Property = c("Y Pre-transformation (applied to all samples)", "Model", "Loss Function", "Weighting"),
                          Value = c(trasf_y_usata, input$tipo_modello, input$tipo_loss,
                                    if (isTRUE(br$pesi_batch$attiva)) descrizione_pesi_batch(br$pesi_batch) else "None"), stringsAsFactors = FALSE)
    ris_test <- tryCatch(batch_test_parametri_risultato(), error = function(e) NULL)
    df_test <- if (!is.null(ris_test) && isTRUE(ris_test$successo)) ris_test$tabella[, setdiff(colnames(ris_test$tabella), "P_Value_Raw")] else data.frame()
    ris_posthoc <- tryCatch(batch_posthoc_risultato(), error = function(e) NULL)
    fogli <- list(Info = df_info, Parameter_Estimates = df_par, Diagnostics = df_diag)
    df_bdr <- riepilogo_diagnostica_per_export()
    if (!is.null(df_bdr)) fogli$Residual_Diagnostics <- df_bdr
    if (nrow(df_test) > 0) fogli$Significance_Test <- df_test
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      fogli$PostHoc_Omnibus <- ris_posthoc$omnibus
      fogli$PostHoc_Pairwise <- ris_posthoc$posthoc
      df_cld <- batch_tabella_cld_df()
      if (!is.null(df_cld)) fogli$PostHoc_CLD <- df_cld
    }
    writexl::write_xlsx(per_excel(fogli), path = file)
  }
)
