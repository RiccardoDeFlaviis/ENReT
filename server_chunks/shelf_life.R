# =========================================================================
# SHELF-LIFE & ARRHENIUS ANALYSIS (Batch Elaboration)
# =========================================================================
# For each sample of the batch: finds the time (X) at which Y crosses a
# critical threshold (root-finding), with its uncertainty (empirical
# distribution if the sample used Monte Carlo, otherwise a Normal
# approximation via the delta method on the covariance matrix - same technique already
# used in Inverse Prediction for the single fit). Then, ONLY the rigorous
# version: a nested bootstrap that at each iteration draws a shelf-life
# value for each temperature (from the appropriate source) and refits
# the nonlinear Arrhenius model (reusing the same formula already present
# in the registry, not linearized) - the result is a true bootstrap
# distribution of Ea and q that correctly propagates the uncertainty through the
# two estimation stages. Verified with a complete simulation (coverage
# of the 95% CI, no systematic bias) before writing this file.


# Registry of the kinetic models available for secondary modelling of
# Shelf-Life vs Temperature - each with its own formula (always as a
# function of x = temperature in Kelvin), parameter names, and default
# guesses. Adding a new model only means adding a new
# entry here: everything else (nested bootstrap, tables, plot, prediction
# tool) reads dynamically from this structure, it is not bound
# to a fixed number of parameters.
REGISTRO_MODELLI_SL <- list(
  arrhenius = list(
    label = "Arrhenius",
    formula = "exp((Ea / (x * 8.314462618)) - q)",
    parametri = c("Ea", "q"),
    guess_default = c(Ea = 10000, q = 1),
    simboli = c(Ea = "E[a]")
  ),
  q10 = list(
    label = "Q10 (Temperature Quotient)",
    # Fixed reference at 293.15 K (20\u00b0C) inside the formula, not a
    # free parameter: without a numerical anchor, the exponent (x/10, with x
    # in Kelvin ~270-320) would be huge, making A astronomically
    # small/large and the optimization unstable. With the anchor, A becomes
    # directly interpretable as "shelf life at 20\u00b0C" - a normal
    # number, numerically verified before writing this code.
    formula = "A * (Q10^((293.15 - x)/10))",
    parametri = c("A", "Q10"),
    guess_default = c(A = 30, Q10 = 2)
  ),
  eyring = list(
    label = "Eyring",
    # Same form as Arrhenius, with in addition the factor (1/x) in front of
    # the exponential - the real mathematical signature that distinguishes Eyring from
    # Arrhenius (it derives from the k_B*T/h term of transition state
    # theory).
    formula = "(1/x) * exp((H / (x * 8.314462618)) - q)",
    parametri = c("H", "q"),
    guess_default = c(H = 50000, q = 1)
  )
)

# Shared helper for Shelf-Life: returns a "model" object with the
# SAME FORM as an element of REGISTRO_MODELLI_SL (label/formula/
# parametri/guess_default/simboli), whether it is a
# registered model or a custom formula - so all the rest of the code
# (fit, preview plot, bootstrap, tables) that already reads
# modello_scelto$... works identically in both cases, without needing
# to be touched. For "custom", parameters and guess_default are
# derived from the text written by the operator (same "name=value" syntax
# used everywhere in the app), not from a fixed list; no substitution of
# symbols (shows the parameter names as written). "x" is the
# same independent variable already used by all 3 registered
# models (temperature in Kelvin), not "T" as in the
# Non-Isothermal module - here the registry already uses "x" by convention.
sl_ottieni_modello <- function(tipo_scelto, formula_testo = NULL, guess_testo = NULL) {
  if (is.null(tipo_scelto)) return(NULL)
  if (tipo_scelto != "custom") return(REGISTRO_MODELLI_SL[[tipo_scelto]])
  if (is.null(formula_testo) || nchar(trimws(formula_testo)) == 0 || is.null(guess_testo) || nchar(trimws(guess_testo)) == 0) return(NULL)
  guesses_custom <- tryCatch(parse_guesses(guess_testo), error = function(e) NULL)
  if (is.null(guesses_custom) || length(guesses_custom$guess) == 0) return(NULL)
  nomi_par <- names(guesses_custom$guess)
  if (!ni_valida_formula_generica(formula_testo, nomi_par, "x")) return(NULL)
  list(label = "Custom", formula = formula_testo, parametri = nomi_par,
       guess_default = as.list(guesses_custom$guess), simboli = NULL)
}

# Converts a sample name + its POSITION into a safe ID for a Shiny
# input (only letters, digits, underscore). The position is included on purpose:
# two different names can sanitize in the same way (e.g. "Sample-A" and
# "Sample A" would both become "Sample_A"), and without a disambiguator
# they would silently share the same field in the UI - with the position
# included, the ID is always unique regardless of the characters in the name.
id_sicuro_campione <- function(nome, indice) paste0(gsub("[^A-Za-z0-9_]", "_", nome), "_", indice)

# Descriptive labels for the known parameters (shown in the guess field),
# to help understand what is being estimated - fallback to the name alone for
# any parameter not listed here.
DESCRIZIONI_PARAMETRI_SL <- c(
  Ea = "Activation Energy, J/mol",
  H = "Activation Enthalpy, J/mol",
  q = "log of the pre-exponential term",
  A = "Shelf life at 20\u00b0C (293.15 K)",
  Q10 = "Temperature quotient (SL ratio per 10\u00b0 change)"
)

# ---- Critical Y Value: pre-filled with the mean of all the Y values
# observed in all samples of the current batch - a reasonable starting
# point instead of an arbitrary fixed number, still editable
# by hand. It is updated only when the batch is ACTUALLY run again
# (batch_risultati() changes value only then), not at every render - so
# a value changed by hand by the user stays intact until
# the batch is launched again with potentially different data.
output$sl_y_critico_ui <- renderUI({
  br <- batch_risultati()
  valore_default <- 50
  if (!is.null(br)) {
    risultati_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
    tutti_y <- unlist(lapply(risultati_ok, function(r) r$y))
    if (length(tutti_y) > 0) valore_default <- round(mean(tutti_y, na.rm = TRUE), 3)
  }
  numericInput("sl_y_critico", "Critical Y Value:", value = valore_default)
})

# Pre-filled with the maximum X observed among all the successful samples
# of the batch (the same "overlaid plot" cited) - a reasonable starting
# point for the root search, still editable by hand.
output$sl_x_max_ui <- renderUI({
  br <- batch_risultati()
  valore_default <- 1000
  if (!is.null(br)) {
    risultati_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
    tutti_x <- unlist(lapply(risultati_ok, function(r) r$x))
    if (length(tutti_x) > 0) valore_default <- round(max(tutti_x, na.rm = TRUE), 3)
  }
  numericInput("sl_x_max", "X Search Range Max:", value = valore_default)
})

# ---- Dynamic guess fields, generated based on the chosen kinetic model ----
output$sl_guess_dinamici <- renderUI({
  modello_scelto <- REGISTRO_MODELLI_SL[[input$sl_modello_cinetico]]
  if (is.null(modello_scelto) || input$sl_modello_cinetico == "custom") return(NULL)
  nomi_par <- modello_scelto$parametri
  # Stacked vertically (one field per row), no longer side by side: in a
  # narrow column (1/3 of the page) two side-by-side fields would be too
  # compressed to be comfortably readable.
  campi <- lapply(nomi_par, function(p) {
    descrizione <- if (p %in% names(DESCRIZIONI_PARAMETRI_SL)) DESCRIZIONI_PARAMETRI_SL[[p]] else p
    numericInput(paste0("sl_guess_", p), paste0("Guess for ", p, " (", descrizione, "):"), value = modello_scelto$guess_default[[p]])
  })
  tagList(
    campi,
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
      "If the fit doesn't converge, try adjusting these.")
  )
})

# ---- Table associating temperature -> sample ----
# Always shown after a successful batch (regardless of the input method
# used) - pre-filled from the fourth column if the long format
# provided it, otherwise empty to be filled in by hand.
output$sl_tabella_temperature <- renderUI({
  br <- batch_risultati()
  if (is.null(br)) return(p(style = "font-size: 12px; color: #7f8c8d;", "Run a batch elaboration first."))
  nomi_ok <- names(Filter(function(r) isTRUE(r$successo), br$risultati))
  if (length(nomi_ok) < 3) return(p(style = "font-size: 12px; color: #7f8c8d;", "At least 3 successfully fitted samples are needed."))
  
  temp_da_file <- br$temperature_da_file
  unita_scelta <- if (is.null(input$sl_unita_temp) || input$sl_unita_temp == "celsius") "\u00b0C" else "K"
  righe <- lapply(seq_along(nomi_ok), function(i) {
    n <- nomi_ok[i]
    id_campo <- paste0("sl_temp_", id_sicuro_campione(n, i))
    # Preserves the value already entered by hand (if present) instead of
    # always overwriting it with the pre-filling from the file - otherwise
    # changing only the unit above (which makes the whole table rebuild,
    # since it now also depends on it) would have silently reset
    # every temperature already typed by hand.
    valore_corrente <- isolate(input[[id_campo]])
    valore_prefill <- if (!is.null(valore_corrente) && !is.na(valore_corrente)) {
      valore_corrente
    } else if (!is.null(temp_da_file) && n %in% names(temp_da_file)) {
      temp_da_file[[n]]
    } else NA
    fluidRow(
      column(5, tags$div(style = "padding-top: 7px; font-size: 13px;", n)),
      column(7, numericInput(id_campo, NULL, value = valore_prefill))
    )
  })
  tagList(
    if (!is.null(temp_da_file) && length(temp_da_file) > 0) {
      p(style = "font-size: 11px; color: #21618c; margin-bottom: 4px;",
        "Pre-filled from the uploaded file's 4th column \u2014 double check these match the unit selected above (", unita_scelta, ").")
    },
    fluidRow(column(5, tags$strong("Sample")), column(7, tags$strong(paste0("Temp. (", unita_scelta, ")")))),
    righe
  )
})

# ---- Robust root-finding: instead of calling uniroot() directly
# on the interval [x_min, x_max] chosen by the user (which uniroot() ALWAYS
# evaluates also at the ends, for its check on the sign change),
# it first scans many intermediate points, discards the non-finite ones (overflow
# or underflow of the exponential terms of the model - a real problem
# discovered and numerically verified: with a too wide range, exp() can
# overflow to Inf both in the numerator and in the denominator in some
# models, giving Inf/Inf = NaN exactly at the point that uniroot() checks
# first), and calls uniroot() only on the FIRST finite sub-interval with a
# sign change found - much more stable than the full range.
trova_intervallo_valido <- function(obiettivo, x_min, x_max, n_scan = 200) {
  x_scan <- seq(x_min, x_max, length.out = n_scan)
  y_scan <- sapply(x_scan, function(x) tryCatch(obiettivo(x), error = function(e) NA_real_))
  validi <- is.finite(y_scan)
  if (sum(validi) < 2) return(NULL)
  x_validi <- x_scan[validi]
  y_validi <- y_scan[validi]
  segni <- sign(y_validi)
  cambi <- which(diff(segni) != 0)
  if (length(cambi) == 0) return(NULL)
  i <- cambi[1]
  c(x_validi[i], x_validi[i + 1])
}

# ---- Root-finding: point shelf life of a sample ----
trova_radice_campione <- function(parametri, formula_stringa, y_critico, x_min, x_max) {
  obiettivo <- function(x) valuta_formula_generica(formula_stringa, as.list(parametri), x) - y_critico
  intervallo <- trova_intervallo_valido(obiettivo, x_min, x_max)
  if (is.null(intervallo)) return(NA_real_)
  tryCatch(uniroot(obiettivo, interval = intervallo)$root, error = function(e) NA_real_)
}

# ---- Empirical bootstrap distribution of the shelf life (Monte Carlo samples) ----
trova_distribuzione_sl_mc <- function(mat_boot, formula_stringa, y_critico, x_min, x_max) {
  n_boot <- nrow(mat_boot)
  sl_boot <- rep(NA_real_, n_boot)
  for (b in seq_len(n_boot)) {
    par_b <- mat_boot[b, ]
    if (any(!is.finite(par_b))) next
    obiettivo <- function(x) valuta_formula_generica(formula_stringa, as.list(par_b), x) - y_critico
    intervallo <- trova_intervallo_valido(obiettivo, x_min, x_max)
    if (is.null(intervallo)) next
    sl_boot[b] <- tryCatch(uniroot(obiettivo, interval = intervallo)$root, error = function(e) NA_real_)
  }
  sl_boot[is.finite(sl_boot)]
}

# ---- Delta method: SE of the shelf life for a Linear (Hessian) sample ----
# Exactly the same logic already used in predizione_inversa.R for the single
# fit, applied here to a sample of the batch.
calcola_se_sl_delta_method <- function(parametri, cov, rmse, formula_stringa, x0, tipo_intervallo) {
  if (is.null(cov) || anyNA(cov)) return(NA_real_)
  h_x <- passo_derivata(x0)
  valori <- as.list(parametri)
  df_dx <- (valuta_formula_generica(formula_stringa, valori, x0 + h_x) - valuta_formula_generica(formula_stringa, valori, x0 - h_x)) / (2 * h_x)
  if (is.na(df_dx) || abs(df_dx) < 1e-12) return(NA_real_)
  
  gradiente_par <- numeric(length(parametri))
  for (j in seq_along(parametri)) {
    h_j <- passo_derivata(parametri[j])
    p_piu <- parametri; p_piu[j] <- p_piu[j] + h_j
    p_meno <- parametri; p_meno[j] <- p_meno[j] - h_j
    gradiente_par[j] <- (valuta_formula_generica(formula_stringa, as.list(p_piu), x0) - valuta_formula_generica(formula_stringa, as.list(p_meno), x0)) / (2 * h_j)
  }
  
  var_param <- as.numeric(t(gradiente_par) %*% cov %*% gradiente_par)
  var_y <- if (tipo_intervallo == "predizione") rmse^2 else 0
  sqrt((var_y + var_param) / (df_dx^2))
}

# ---- Builds the "source" from which to draw an SL value for a sample:
# empirical (Monte Carlo) if available, otherwise Gaussian simulated via the
# delta method (Linear), otherwise only the estimated point (no
# propagable uncertainty). ----
costruisci_fonte_sl <- function(r, formula_stringa, y_critico, x_min, x_max, tipo_intervallo) {
  x0 <- trova_radice_campione(r$parametri, formula_stringa, y_critico, x_min, x_max)
  if (is.na(x0)) return(list(successo = FALSE, messaggio = "No root found in the given X search range."))
  
  if (!is.null(r$mat_boot)) {
    sl_boot <- trova_distribuzione_sl_mc(r$mat_boot, formula_stringa, y_critico, x_min, x_max)
    if (length(sl_boot) >= 10) {
      return(list(successo = TRUE, tipo = "empirico", valori = sl_boot, puntuale = x0, se = sd(sl_boot)))
    }
  }
  if (!is.null(r$cov)) {
    # With a weighted fit the prediction variance at the shelf-life time, where
    # the curve equals y_critico, is sigma^2 y_critico^zeta; without weights sigma^2.
    se_delta <- calcola_se_sl_delta_method(r$parametri, r$cov, r$rmse * sqrt(varianza_relativa(r, y_critico)), formula_stringa, x0, tipo_intervallo)
    if (!is.na(se_delta)) {
      return(list(successo = TRUE, tipo = "gaussiano", media = x0, se = se_delta, puntuale = x0, df = r$df))
    }
  }
  list(successo = TRUE, tipo = "solo_punto", puntuale = x0, se = NA_real_)
}

# ---- Main run: nested Arrhenius bootstrap ----
sl_risultato <- reactiveVal(NULL)

output$sezione_sl_risultati_ui <- renderUI({
  ris <- sl_risultato()
  if (is.null(ris)) return(NULL)
  tagList(
    br(), br(),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Shelf Life per Temperature:"),
      tags$button(id = "btn_copia_sl_per_temperatura", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('sl_tabella_per_temperatura', 'btn_copia_sl_per_temperatura')")
    ),
    fluidRow(
      column(6, selectInput("sl_temp_sort_by", "Sort By:", choices = c("Sample" = "Sample", "Temperature (\u00b0C)" = "Temperature_C", "Shelf Life" = "Shelf_Life", "CI Lower" = "CI_Lower", "CI Upper" = "CI_Upper", "Source" = "Source"))),
      column(6, selectInput("sl_temp_sort_dir", "Order:", choices = c("Ascending" = "asc", "Descending" = "desc")))
    ),
    tableOutput("sl_tabella_per_temperatura"),
    
    br(),
    uiOutput("sezione_barre_sl_ui"),
    
    br(),
    uiOutput("sezione_boxplot_sl_ui"),
    
    br(),
    tags$h4("Post-Hoc Comparison (3+ Samples):"),
    p(style = "font-size: 12px; color: #566573;",
      "Compares the shelf life estimated for each sample above (not the kinetic model yet). ", tags$strong("Cochran's Q"), " checks whether the samples' shelf life differs overall; if so, all pairwise comparisons below tell you ", tags$strong("which"), " samples differ, with a multiple-comparison correction. Samples with only a point estimate (no Monte Carlo or Linear uncertainty available) are excluded, since this test needs an uncertainty for each sample."),
    uiOutput("sl_selezione_campioni_posthoc"),
    fluidRow(
      column(6, selectInput("sl_metodo_correzione_posthoc", "Multiple-Comparison Correction:", 
                            choices = SCELTE_CORREZIONE_MULTIPLA, selected = "holm")),
      column(6, br(), actionButton("sl_esegui_posthoc", "Run Post-Hoc Comparison", class = "btn-primary btn-sm w-100", icon = icon("calculator")))
    ),
    uiOutput("sezione_sl_posthoc_ui"),
    
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h4(style = "margin: 0;", "Kinetic Model Parameters:"),
      tags$button(id = "btn_copia_sl_arrhenius", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px;",
        onclick = "copiaTabellaClipboard('sl_tabella_arrhenius', 'btn_copia_sl_arrhenius')")
    ),
    p(style = "font-size: 11px; color: #21618c; font-style: italic;", descrivi_tipo_modellazione_sl(ris)),
    tableOutput("sl_tabella_arrhenius"),
    tags$em(style = "font-size: 12px; color: #566573;", textOutput("sl_info_bootstrap", inline = TRUE)),
    
    br(), br(),
    tags$h4("Shelf-Life vs Temperature Plot:"),
    fluidRow(
      column(6, checkboxInput("sl_mostra_ci", "Show Confidence Band (curve position)", value = TRUE)),
      column(6, checkboxInput("sl_mostra_pi", "Show Prediction Band (incl. sample scatter)", value = FALSE))
    ),
    if (isTRUE(ris$n_arr_successo >= MINIMO_ITERAZIONI_MC)) {
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the nested bootstrap (percentiles of the bootstrap curves).")
    } else {
      tagList(
        p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
          "Bands from the linear approximation of the kinetic model (delta method); enable \u201cPropagate uncertainty (nested bootstrap)\u201d above for bootstrap bands."),
        conditionalPanel(condition = "input.sl_mostra_ci",
          div(style = "margin-left: 20px;",
            checkboxInput("sl_banda_simultanea", "Simultaneous confidence band (Bates & Watts, 1988)", value = FALSE)))
      )
    },
    plotOutput("sl_grafico_arrhenius", height = "420px"),
    div(style = "margin-top: 8px; margin-bottom: 20px;", 
        downloadButton("sl_scarica_grafico", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
    
    tags$h4("Predict Shelf Life at a New Temperature:"),
    p(style = "font-size: 12px; color: #566573;",
      "Uses the full nested bootstrap distribution of Ea and q, so the confidence interval correctly reflects the uncertainty propagated from every sample."),
    fluidRow(
      column(6, numericInput("sl_temp_predizione", "Temperature:", value = 25)),
      column(6, br())
    ),
    verbatimTextOutput("sl_predizione_risultato"),
    
    div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 6px; margin-top: 15px; margin-bottom: 25px; max-width: 50%;",
        tags$h5(style = "margin-top: 0; color: #2c3e50;", icon("file-export"), " Export Shelf-Life & Arrhenius Results"),
        p(style = "font-size: 13px; color: #7f8c8d;", "Shelf life per temperature and kinetic model parameters, in a single file."),
        downloadButton("sl_scarica_csv", "Download as CSV", class = "btn-outline-secondary btn-sm"),
        downloadButton("sl_scarica_excel", "Download as Excel (.xlsx)", class = "btn-outline-secondary btn-sm")
    )
  )
})

observeEvent(input$sl_esegui_analisi, {
  # Clears the stale Post-Hoc of any previous analysis: without
  # this, it remained visible (computed on sources/samples by then replaced)
  # until the user pressed "Run Post-Hoc" explicitly again.
  sl_posthoc_risultato(NULL)
  
  br <- batch_risultati()
  if (is.null(br)) { showNotification("Run a batch elaboration first.", type = "error"); return() }
  risultati_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
  nomi <- names(risultati_ok)
  if (length(nomi) < 3) { showNotification("Need at least 3 successfully fitted samples.", type = "error"); return() }
  
  temperature_raw <- sapply(seq_along(nomi), function(i) input[[paste0("sl_temp_", id_sicuro_campione(nomi[i], i))]])
  mancanti <- nomi[sapply(temperature_raw, function(v) is.null(v) || is.na(v))]
  if (length(mancanti) > 0) {
    showNotification(paste("Enter a temperature for every sample first. Missing:", paste(mancanti, collapse = ", ")), type = "error", duration = 8)
    return()
  }
  temperature_num <- as.numeric(temperature_raw)
  names(temperature_num) <- nomi
  temperature_K <- if (input$sl_unita_temp == "celsius") temperature_num + 273.15 else temperature_num
  if (any(temperature_K <= 0)) {
    showNotification("Temperatures in Kelvin must be positive - check the unit selected and the values entered.", type = "error")
    return()
  }
  
  x_min <- input$sl_x_min; x_max <- input$sl_x_max
  if (is.na(x_min) || is.na(x_max) || x_min >= x_max) {
    showNotification("Check the X Search Range (min must be less than max).", type = "error")
    return()
  }
  y_critico <- input$sl_y_critico
  
  fonti <- list()
  withProgress(message = "Finding shelf life for each sample...", value = 0, {
    for (n in nomi) {
      fonti[[n]] <- costruisci_fonte_sl(risultati_ok[[n]], br$formula, y_critico, x_min, x_max, input$sl_tipo_intervallo)
      incProgress(1 / length(nomi))
    }
  })
  
  falliti <- names(Filter(function(f) !isTRUE(f$successo), fonti))
  if (length(falliti) > 0) {
    showNotification(paste("Could not find a shelf life for:", paste(falliti, collapse = ", "), "- try widening the X Search Range."), type = "warning", duration = 10)
  }
  fonti_ok <- fonti[sapply(fonti, function(f) isTRUE(f$successo))]
  if (length(fonti_ok) < 3) {
    showNotification("Need at least 3 samples with a valid shelf life estimate to fit the kinetic model.", type = "error")
    return()
  }
  
  T_K_vec <- temperature_K[names(fonti_ok)]
  sl_puntuali <- sapply(fonti_ok, function(f) f$puntuale)
  
  modello_scelto <- sl_ottieni_modello(input$sl_modello_cinetico, input$sl_formula_custom, input$sl_guess_custom)
  if (is.null(modello_scelto)) {
    if (identical(input$sl_modello_cinetico, "custom")) {
      showNotification("Enter a valid custom formula and initial guesses first (check the syntax and that only allowed functions/parameters/x are used).", type = "error", duration = 8)
    } else {
      showNotification("Choose a kinetic model first.", type = "error")
    }
    return()
  }
  formula_modello <- modello_scelto$formula
  nomi_parametri_modello <- modello_scelto$parametri
  
  if (identical(input$sl_modello_cinetico, "custom")) {
    # The guess has already been entirely parsed inside sl_ottieni_modello()
    # (a single text field, not a numericInput per parameter).
    guess_modello <- setNames(as.numeric(modello_scelto$guess_default[nomi_parametri_modello]), nomi_parametri_modello)
  } else {
    guess_letti <- sapply(nomi_parametri_modello, function(p) input[[paste0("sl_guess_", p)]])
    if (any(sapply(guess_letti, function(v) is.null(v) || is.na(v)))) {
      showNotification(paste("Enter an initial guess for every parameter of the", modello_scelto$label, "model first."), type = "error")
      return()
    }
    guess_modello <- setNames(as.numeric(guess_letti), nomi_parametri_modello)
  }
  
  # Point fit on the central values, used as a "warm" starting point
  # for each iteration of the bootstrap - without it, convergence can be
  # much less reliable (verified during testing).
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
  
  if (isTRUE(input$sl_usa_bootstrap)) {
    n_boot_arr <- if (is.null(input$sl_n_boot_arrhenius) || is.na(input$sl_n_boot_arrhenius)) 1000 else min(max(round(input$sl_n_boot_arrhenius), 100), 5000)
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
    # Bootstrap disabled: EMPTY matrix (0 rows) but with the same column
    # names it would have with the bootstrap active - all the downstream
    # code that already handles the "few valid iterations" case (< 10)
    # also correctly handles this limit case (zero rows), without
    # needing a separate if/else branch in each of those places.
    n_boot_arr <- 0
    mat_arr_boot <- matrix(NA_real_, 0, length(nomi_parametri_modello), dimnames = list(NULL, names(guess_modello)))
    n_arr_successo <- 0
  }
  
  # Standard errors from the Hessian of the point fit (Nelder-Mead, not LM ->
  # multiplier 2). The optimized loss (calcola_loss_generica with
  # tipo_loss="mse") actually returns sum(errori^2) - the raw
  # sum (SSE), NOT a mean, despite the name of the parameter - so
  # fit_puntuale$value is already on the SSE scale, the same standard formula
  # 2*SSE/(n-p) * inverse(Hessian_of_SSE) applies directly, without
  # needing any conversion. Numerically verified against nls()
  # as an independent check on two different models (final ratio
  # exactly 1:1 when the objective uses sum() as here) before writing it
  # in the code. ALWAYS available, at a cost
  # close to zero (the Hessian is already computed together with the fit itself),
  # used by the table as an alternative when the bootstrap is off.
  n_oss_arr <- length(fonti_ok)
  df_den_arr <- n_oss_arr - length(nomi_parametri_modello)
  # Covariance from the Jacobian of the model, s^2 (J^T J)^-1, not from the numerical
  # Hessian of optim (inaccurate with correlated parameters such as Ea and q).
  cov_hessiana <- covarianza_minimi_quadrati(modello_sl, setNames(fit_puntuale$par, names(guess_modello)), fit_puntuale$value, df_den_arr, jacobiano_sl)
  errori_hessiana <- if (!is.null(cov_hessiana)) sqrt(diag(cov_hessiana)) else NULL
  if (!is.null(errori_hessiana)) names(errori_hessiana) <- names(guess_modello)
  
  sl_risultato(list(successo = TRUE, fonti = fonti_ok, T_K = T_K_vec, sl_puntuali = sl_puntuali,
                     fit_puntuale = fit_puntuale$par, mat_arr_boot = mat_arr_boot, n_arr_successo = n_arr_successo,
                     errori_hessiana = errori_hessiana, cov_modello = cov_hessiana, df_cinetico = df_den_arr,
                     n_arr_totale = n_boot_arr, y_critico = y_critico, unita_temp = input$sl_unita_temp,
                     formula_modello = formula_modello, modello_label = modello_scelto$label, 
                     # as.list() is necessary: modello_scelto$simboli is a
                     # named VECTOR (c(...)), not a list - looking up
                     # with [[ an absent name in a vector gives the error
                     # "subscript out of bounds" (unlike a
                     # list, which would return NULL). Same conversion
                     # already done by the original mechanism of the main
                     # fit (as.list(simboli_preset)).
                     simboli_modello = as.list(modello_scelto$simboli)))
  if (n_boot_arr > 0) {
    showNotification(paste0(modello_scelto$label, " analysis complete: ", n_arr_successo, "/", n_boot_arr, " bootstrap iterations successful."),
                      type = if (n_arr_successo >= n_boot_arr * 0.5) "message" else "warning", duration = 8)
  } else {
    showNotification(paste0(modello_scelto$label, " analysis complete (point estimate only, bootstrap not computed)."), type = "message", duration = 6)
  }
})

# ---- Output: shelf life by temperature (with uncertainty) ----
# Builds the "shelf life by temperature" table (with CI) - pure
# function, reused both by the on-screen renderTable and by both exports,
# so there is no risk that they end up showing different data.
costruisci_tabella_sl_per_temperatura <- function(ris, livello) {
  righe <- lapply(names(ris$fonti), function(n) {
    f <- ris$fonti[[n]]
    T_C_mostrata <- ris$T_K[[n]] - 273.15
    if (f$tipo == "empirico") {
      p_inf <- (1 - livello / 100) / 2; p_sup <- 1 - p_inf
      ci <- quantile(f$valori, probs = c(p_inf, p_sup), na.rm = TRUE)
      data.frame(Sample = n, Temperature_C = round(T_C_mostrata, 1), Shelf_Life = round(f$puntuale, 4),
                 CI_Lower = round(ci[1], 4), CI_Upper = round(ci[2], 4), Source = "Monte Carlo (empirical)", stringsAsFactors = FALSE)
    } else if (f$tipo == "gaussiano") {
      # Student's t (not Z), consistent with the convention already used everywhere
      # in the app - the degrees of freedom of the original sample were
      # saved directly in the source at the moment it was built.
      crit <- valore_critico(f$df)
      data.frame(Sample = n, Temperature_C = round(T_C_mostrata, 1), Shelf_Life = round(f$puntuale, 4),
                 CI_Lower = round(f$media - crit * f$se, 4), CI_Upper = round(f$media + crit * f$se, 4), 
                 Source = "Linear (Delta Method)", stringsAsFactors = FALSE)
    } else {
      data.frame(Sample = n, Temperature_C = round(T_C_mostrata, 1), Shelf_Life = round(f$puntuale, 4),
                 CI_Lower = NA, CI_Upper = NA, Source = "Point estimate only", stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, righe)
}

output$sl_tabella_per_temperatura <- renderTable({
  ris <- sl_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the Shelf-Life analysis first."))
  df <- costruisci_tabella_sl_per_temperatura(ris, input$sl_livello_confidenza)
  campo_ord <- if (is.null(input$sl_temp_sort_by) || !(input$sl_temp_sort_by %in% colnames(df))) "Sample" else input$sl_temp_sort_by
  decrescente <- isTRUE(input$sl_temp_sort_dir == "desc")
  tab <- df[order(df[[campo_ord]], decreasing = decrescente, na.last = TRUE), ]
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

# ---- Post-Hoc Comparison for the shelf-life times estimated per sample ----
# Reuses the same three shared functions already used by Batch Elaboration and
# Model Comparison (calcola_cochran_q, calcola_confronto_coppia,
# assegna_lettere_cld), building for each sample a "comparable"
# object with a single quantity (the estimated shelf life) instead of the
# usual parameters of a model - same structure {parametri, errori,
# mat_boot}, so calcola_confronto_coppia() works without changes.
# Draws a boxplot/violin comparison of the SHELF-LIFE TIMES among
# samples/fits, using only those with empirical uncertainty (fonte$tipo ==
# "empirico", i.e. derived from a true nested bootstrap with many
# replicated values) - those with Gaussian uncertainty (fonte$tipo ==
# "gaussiano", propagated via the delta method when the PRIMARY fit uses the
# Hessian) have only a point + standard error, not a true distribution
# that can be shown in a boxplot/violin. Shared between Batch and Model
# Comparison, which both pass a "fonti" list with exactly the same
# structure.
# Draws a bar chart of the SHELF-LIFE TIMES for the samples/fits
# with Gaussian uncertainty (fonte$tipo == "gaussiano", i.e. propagated via the
# delta method when the PRIMARY fit uses the Hessian) - one bar per
# sample (the point estimate), with an error bar from the standard error.
# Complementary to disegna_boxplot_sl_condiviso() below: together, the two
# plots cover EVERY sample with a computed shelf-life time, never
# none or both (same logic already used for the comparison of the
# parameters: bar chart for Hessian, boxplot/violin for Monte Carlo).
disegna_barre_sl_condiviso <- function(fonti, prefisso_input) {
  campo <- function(suffisso) input[[paste0(prefisso_input, suffisso)]]
  fonti_gaussiane <- Filter(function(f) identical(f$tipo, "gaussiano"), fonti)
  par(mar = c(6, 4, 3, 1))
  if (length(fonti_gaussiane) < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Need at least 2 samples/fits with a Jacobian/Linear-derived shelf life to compare (currently have fewer).")
    return(invisible(NULL))
  }
  col_barre <- if (is.null(campo("col_barre"))) "#f39c12" else campo("col_barre")
  col_bordo_barre <- if (is.null(campo("col_bordo_barre"))) "black" else campo("col_bordo_barre")
  fnt_cex_barre <- if (is.null(campo("fnt_cex_barre"))) 0.8 else campo("fnt_cex_barre")
  ordina_crescente <- if (is.null(campo("ordina_barre"))) FALSE else campo("ordina_barre")
  
  valori <- sapply(fonti_gaussiane, function(f) f$media)
  errori <- sapply(fonti_gaussiane, function(f) f$se)
  nomi_campioni <- names(fonti_gaussiane)
  if (ordina_crescente) {
    idx_ord <- order(valori)
    valori <- valori[idx_ord]; errori <- errori[idx_ord]; nomi_campioni <- nomi_campioni[idx_ord]
  }
  righe_nomi <- prepara_margine_nomi_ruotati(nomi_campioni, fnt_cex_barre, con_titolo = titolo_x_attivo(campo("mostra_titolo_x")))
  posizioni <- barplot(valori, names.arg = nomi_campioni, main = "Shelf Life", ylab = "Shelf Life",
                       col = col_barre, border = col_bordo_barre, las = 2, cex.names = fnt_cex_barre,
                       ylim = limiti_asse_barre(c(valori, valori - errori, valori + errori)))
  # Batch: one sample per temperature; Model Comparison: one fit per entry.
  titolo_asse_x_ruotato(if (startsWith(prefisso_input, "mc_")) "Fit" else "Sample", righe_nomi, titolo_x_attivo(campo("mostra_titolo_x")))
  idx_con_errore <- which(is.finite(errori) & errori > 0)
  if (length(idx_con_errore) > 0) {
    arrows(posizioni[idx_con_errore], valori[idx_con_errore] - errori[idx_con_errore],
           posizioni[idx_con_errore], valori[idx_con_errore] + errori[idx_con_errore],
           angle = 90, code = 3, length = 0.05, col = col_bordo_barre, lwd = 1.2)
  }
}

disegna_boxplot_sl_condiviso <- function(fonti, prefisso_input) {
  campo <- function(suffisso) input[[paste0(prefisso_input, suffisso)]]
  fonti_empiriche <- Filter(function(f) identical(f$tipo, "empirico") && length(f$valori) >= 10, fonti)
  par(mar = c(6, 4, 3, 1))
  if (length(fonti_empiriche) < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Need at least 2 samples/fits with a Monte Carlo-derived shelf life (not Jacobian/Linear) to compare (currently have fewer).")
    return(invisible(NULL))
  }
  col_box <- if (is.null(campo("col_box"))) "#f39c12" else campo("col_box")
  col_bordo_box <- if (is.null(campo("col_bordo_box"))) "black" else campo("col_bordo_box")
  mostra_outlier <- if (is.null(campo("mostra_outlier_box"))) TRUE else campo("mostra_outlier_box")
  fnt_cex_box <- if (is.null(campo("fnt_cex_box"))) 0.8 else campo("fnt_cex_box")
  tipo_grafico <- if (is.null(campo("tipo_grafico_box"))) "boxplot" else campo("tipo_grafico_box")
  ordina_crescente <- if (is.null(campo("ordina_box"))) FALSE else campo("ordina_box")
  
  dati_lista <- lapply(fonti_empiriche, function(f) f$valori)
  if (ordina_crescente) {
    mediane <- sapply(dati_lista, median, na.rm = TRUE)
    dati_lista <- dati_lista[order(mediane)]
  }
  if (tipo_grafico == "violin") {
    righe_nomi <- prepara_margine_nomi_ruotati(names(dati_lista), fnt_cex_box, con_titolo = titolo_x_attivo(campo("mostra_titolo_x")))
    disegna_violin_gruppi(dati_lista, main = "Shelf Life", ylab = "Shelf Life", col = col_box, col_bordo = col_bordo_box, cex_axis = fnt_cex_box)
  } else {
    righe_nomi <- prepara_margine_nomi_ruotati(names(dati_lista), fnt_cex_box, con_titolo = titolo_x_attivo(campo("mostra_titolo_x")))
    boxplot(dati_lista, main = "Shelf Life", ylab = "Shelf Life", las = 2, col = col_box, border = col_bordo_box,
            outline = mostra_outlier, cex.axis = fnt_cex_box)
  }
  titolo_asse_x_ruotato(if (startsWith(prefisso_input, "mc_")) "Fit" else "Sample", righe_nomi, titolo_x_attivo(campo("mostra_titolo_x")))
}

costruisci_oggetto_confronto_sl <- function(fonte, df_residuo_originale) {
  # calcola_confronto_coppia() (Welch's t-test branch, used when not
  # both samples have a bootstrap) computes the degrees of freedom as
  # m1$obs - length(m1$parametri) - here parametri always has length 1
  # ("Shelf_Life"), so setting obs = df_residuo_originale + 1 gives back
  # EXACTLY the residual degrees of freedom of the original fit of the
  # sample, instead of the wrong value (1) that would result from leaving
  # $obs absent.
  obs_iniettato <- df_residuo_originale + 1
  # Monte Carlo distribution with fewer than MINIMO_ITERAZIONI_MC valid values:
  # no usable uncertainty, as in the rest of the app (Single Fit and
  # Batch do not show Monte Carlo results below this threshold). Previously its
  # standard deviation still entered the omnibus test, while the
  # pairwise comparison discarded it (threshold already present there).
  if (fonte$tipo == "empirico" && sum(is.finite(fonte$valori)) < MINIMO_ITERAZIONI_MC) {
    return(list(parametri = c(Shelf_Life = fonte$puntuale), errori = c(Shelf_Life = NA_real_), mat_boot = NULL, obs = obs_iniettato))
  }
  if (fonte$tipo == "empirico") {
    valori <- fonte$valori
    mat_boot <- matrix(valori, ncol = 1, dimnames = list(NULL, "Shelf_Life"))
    list(parametri = c(Shelf_Life = fonte$puntuale), errori = c(Shelf_Life = sd(valori, na.rm = TRUE)), mat_boot = mat_boot, obs = obs_iniettato)
  } else if (fonte$tipo == "gaussiano") {
    list(parametri = c(Shelf_Life = fonte$puntuale), errori = c(Shelf_Life = fonte$se), mat_boot = NULL, obs = obs_iniettato)
  } else {
    # Point estimate only, no propagable uncertainty - error NA, it will be
    # automatically excluded both from the omnibus (which requires errors > 0 and finite)
    # and from the pairwise comparisons.
    list(parametri = c(Shelf_Life = fonte$puntuale), errori = c(Shelf_Life = NA_real_), mat_boot = NULL, obs = obs_iniettato)
  }
}

# ---- Bar comparison of the shelf-life times among samples with
# Hessian/Linear uncertainty (Batch) ----
output$grafico_barre_sl <- renderPlot({
  ris <- sl_risultato()
  if (is.null(ris)) { plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  disegna_barre_sl_condiviso(ris$fonti, "sl_barre_")
}, res = 100)

output$sezione_barre_sl_ui <- renderUI({
  ris <- sl_risultato()
  fonti_gaussiane <- if (!is.null(ris)) Filter(function(f) identical(f$tipo, "gaussiano"), ris$fonti) else list()
  if (length(fonti_gaussiane) < 2) return(NULL)
  tagList(
    tags$h4("Shelf Life Comparison (Linear/Jacobian Method):"),
    p(style = "font-size: 12px; color: #566573;",
      "One bar per sample (its point estimate, with an error bar from the Jacobian/Linear-propagated standard error) \u2014 samples whose shelf life uncertainty came from Monte Carlo are not included here (they're in the boxplot/violin below instead)."),
    plotOutput("grafico_barre_sl", height = "450px"),
    div(style = "margin-top: 8px;", 
        downloadButton("scarica_barre_sl", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_barre_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
  )
})

output$scarica_barre_sl <- downloadHandler(
  filename = function() paste0("shelf_life_barchart.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    ris <- sl_risultato()
    if (!is.null(ris)) disegna_barre_sl_condiviso(ris$fonti, "sl_barre_")
    dev.off()
  }
)

# ---- Boxplot/violin comparison of the shelf-life times among samples (Batch) ----
output$grafico_boxplot_sl <- renderPlot({
  ris <- sl_risultato()
  if (is.null(ris)) { plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  disegna_boxplot_sl_condiviso(ris$fonti, "sl_box_")
}, res = 100)

output$sezione_boxplot_sl_ui <- renderUI({
  ris <- sl_risultato()
  fonti_empiriche <- if (!is.null(ris)) Filter(function(f) identical(f$tipo, "empirico") && length(f$valori) >= 10, ris$fonti) else list()
  if (length(fonti_empiriche) < 2) return(NULL)
  tagList(
    tags$h4("Shelf Life Distribution Across Samples:"),
    p(style = "font-size: 12px; color: #566573;",
      "One box/violin per sample, built from the full nested bootstrap distribution of its shelf life \u2014 samples whose shelf life uncertainty came from the Jacobian/Linear method are not included (only a point estimate and standard error exist for those, not a distribution)."),
    plotOutput("grafico_boxplot_sl", height = "450px"),
    div(style = "margin-top: 8px;", 
        downloadButton("scarica_boxplot_sl", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("btn_estetica_boxplot_sl", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
  )
})

output$scarica_boxplot_sl <- downloadHandler(
  filename = function() paste0("shelf_life_boxplot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    ris <- sl_risultato()
    if (!is.null(ris)) disegna_boxplot_sl_condiviso(ris$fonti, "sl_box_")
    dev.off()
  }
)

sl_posthoc_risultato <- reactiveVal(NULL)

output$sezione_sl_posthoc_ui <- renderUI({
  ris <- sl_posthoc_risultato()
  if (is.null(ris)) return(NULL)
  tagList(
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Omnibus Test (Cochran's Q):"),
      tags$button(id = "btn_copia_sl_omnibus", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('sl_tabella_omnibus_posthoc', 'btn_copia_sl_omnibus')")
    ),
    tableOutput("sl_tabella_omnibus_posthoc"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Pairwise Comparisons:"),
      tags$button(id = "btn_copia_sl_posthoc", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('sl_tabella_posthoc', 'btn_copia_sl_posthoc')")
    ),
    tableOutput("sl_tabella_posthoc"),
    tags$div(style = "display: flex; align-items: center; gap: 10px;",
      tags$h5(style = "margin: 15px 0 0 0;", "Compact Letter Display:"),
      tags$button(id = "btn_copia_sl_cld", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
        style = "padding-top: 2px; padding-bottom: 2px; margin-top: 15px;",
        onclick = "copiaTabellaClipboard('sl_tabella_cld_posthoc', 'btn_copia_sl_cld')")
    ),
    p(style = "font-size: 12px; color: #566573;",
      "Samples sharing a letter are ", tags$strong("not"), " significantly different from each other; a different letter means they are."),
    tableOutput("sl_tabella_cld_posthoc")
  )
})
output$sl_selezione_campioni_posthoc <- renderUI({
  ris <- sl_risultato()
  if (is.null(ris)) return(NULL)
  nomi <- names(ris$fonti)
  if (length(nomi) == 0) return(NULL)
  tagList(
    div(style = "margin-bottom: 4px;",
        actionButton("sl_posthoc_select_all", "Select All", class = "btn-outline-secondary btn-sm"),
        actionButton("sl_posthoc_deselect_all", "Deselect All", class = "btn-outline-secondary btn-sm")),
    checkboxGroupInput("sl_campioni_posthoc", "Samples to include:", choices = nomi, selected = nomi, inline = TRUE)
  )
})
observeEvent(input$sl_posthoc_select_all, {
  ris <- sl_risultato()
  if (is.null(ris)) return()
  updateCheckboxGroupInput(session, "sl_campioni_posthoc", selected = names(ris$fonti))
})
observeEvent(input$sl_posthoc_deselect_all, {
  updateCheckboxGroupInput(session, "sl_campioni_posthoc", selected = character(0))
})

observeEvent(input$sl_esegui_posthoc, {
  ris <- sl_risultato()
  if (is.null(ris)) { sl_posthoc_risultato(list(successo = FALSE, messaggio = "Run the Shelf-Life analysis first.")); return() }
  
  br <- batch_risultati()
  if (is.null(br)) { sl_posthoc_risultato(list(successo = FALSE, messaggio = "Batch results are no longer available \u2014 run the batch and the Shelf-Life analysis again.")); return() }
  nomi_fonti <- names(ris$fonti)
  # If the user deselected some sample in the selector above,
  # limits the comparison to the chosen ones - if the selector has not yet
  # been rendered (NULL) uses all of them, unchanged behaviour.
  if (!is.null(input$sl_campioni_posthoc)) nomi_fonti <- intersect(nomi_fonti, input$sl_campioni_posthoc)
  oggetti <- lapply(nomi_fonti, function(nome) {
    fit_originale <- br$risultati[[nome]]
    df_residuo <- if (!is.null(fit_originale$obs) && !is.null(fit_originale$parametri)) {
      max(fit_originale$obs - length(fit_originale$parametri), 1)
    } else 1
    costruisci_oggetto_confronto_sl(ris$fonti[[nome]], df_residuo)
  })
  names(oggetti) <- nomi_fonti
  nomi_validi <- names(oggetti)[sapply(oggetti, function(o) is.finite(o$errori[["Shelf_Life"]]) && o$errori[["Shelf_Life"]] > 0)]
  if (length(nomi_validi) < 3) {
    sl_posthoc_risultato(list(successo = FALSE, messaggio = "Need at least 3 selected samples with a usable uncertainty (Monte Carlo or Linear) on their shelf life estimate.")); return()
  }
  oggetti <- oggetti[nomi_validi]
  
  # 1. Omnibus (Cochran's Q) - here a single quantity is compared (the
  # shelf life), not a loop per parameter as in Batch/Model Comparison.
  stime <- sapply(oggetti, function(o) o$parametri[["Shelf_Life"]])
  errori <- sapply(oggetti, function(o) o$errori[["Shelf_Life"]])
  q_ris <- calcola_cochran_q(stime, errori)
  sig_omnibus <- if (is.na(q_ris$p_value)) "N/A" else as.character(symnum(q_ris$p_value, corr = FALSE, na = FALSE,
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
  df_omnibus <- data.frame(N_Samples = length(oggetti), Cochran_Q = round(q_ris$Q, 3), DF = q_ris$df,
                           P_Value = if (is.na(q_ris$p_value)) "N/A" else format.pval(q_ris$p_value, digits = 3, eps = 0.001),
                           Significant = sig_omnibus, stringsAsFactors = FALSE)
  
  # 2. Pairwise, correction for multiple tests - the real bottleneck,
  # since the number of pairs grows quadratically with the
  # number of samples.
  metodo_correzione <- if (is.null(input$sl_metodo_correzione_posthoc)) "holm" else input$sl_metodo_correzione_posthoc
  nomi_campioni <- names(oggetti)
  coppie <- combn(nomi_campioni, 2, simplify = FALSE)
  n_coppie <- length(coppie)
  withProgress(message = "Running Shelf-Life Post-Hoc Comparison...", value = 0, {
  righe_posthoc <- lapply(seq_along(coppie), function(i) {
    incProgress(1 / n_coppie, detail = paste0("Pair ", i, " of ", n_coppie))
    coppia <- coppie[[i]]
    confronto <- calcola_confronto_coppia(oggetti[[coppia[1]]], oggetti[[coppia[2]]], input$sl_livello_confidenza)
    tab <- confronto$tabella
    data.frame(Sample_A = coppia[1], Sample_B = coppia[2], Value_A = tab$Value_A, Value_B = tab$Value_B,
               Absolute_Diff = tab$Absolute_Diff, P_Value_Raw = tab$P_Value_Raw, stringsAsFactors = FALSE)
  })
  })
  df_posthoc <- do.call(rbind, righe_posthoc)
  df_posthoc$P_Value_Adjusted <- p.adjust(df_posthoc$P_Value_Raw, method = metodo_correzione)
  df_posthoc$Significant <- as.character(symnum(df_posthoc$P_Value_Adjusted, corr = FALSE, na = FALSE,
                                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")))
  df_posthoc$P_Value_Adjusted <- format.pval(df_posthoc$P_Value_Adjusted, digits = 3, eps = 0.001)
  df_posthoc$Value_A <- round(df_posthoc$Value_A, 3); df_posthoc$Value_B <- round(df_posthoc$Value_B, 3)
  df_posthoc$Absolute_Diff <- round(df_posthoc$Absolute_Diff, 3)
  df_posthoc$P_Value_Raw <- NULL
  
  # 3. Compact Letter Display
  coppie_sig <- list()
  for (i in seq_len(nrow(df_posthoc))) {
    if (identical(df_posthoc$Significant[i], "ns")) next
    a <- df_posthoc$Sample_A[i]; b <- df_posthoc$Sample_B[i]
    coppie_sig[[a]] <- c(coppie_sig[[a]], b)
  }
  lettere <- assegna_lettere_cld(nomi_campioni, coppie_sig)
  df_cld <- data.frame(Sample = nomi_campioni, Shelf_Life = round(stime, 3), Group = lettere, stringsAsFactors = FALSE)
  
  sl_posthoc_risultato(list(successo = TRUE, omnibus = df_omnibus, posthoc = df_posthoc, cld = df_cld))
})

output$sl_tabella_omnibus_posthoc <- renderTable({
  ris <- tryCatch(sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris)) return(data.frame(Message = "Run the Post-Hoc Comparison above."))
  if (!isTRUE(ris$successo)) return(data.frame(Message = ris$messaggio))
  tab <- ris$omnibus
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$sl_tabella_posthoc <- renderTable({
  ris <- tryCatch(sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(data.frame(Message = "-"))
  tab <- ris$posthoc
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", digits = 4)

output$sl_tabella_cld_posthoc <- renderTable({
  ris <- tryCatch(sl_posthoc_risultato(), error = function(e) NULL)
  if (is.null(ris) || !isTRUE(ris$successo)) return(data.frame(Message = "-"))
  tab <- ris$cld
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A", digits = 4)

# ---- Output: Arrhenius parameters (Ea, q) with CI from the nested bootstrap ----
costruisci_tabella_arrhenius <- function(ris, livello) {
  p_inf <- (1 - livello / 100) / 2; p_sup <- 1 - p_inf
  # Converts the raw name of the parameter (e.g. "Ea") into its plain Unicode
  # symbol (e.g. "Ea" -> "E" with subscript "a") if the model has one
  # defined, otherwise leaves the name as it is.
  etichetta_parametro <- function(nome_par) {
    if (!is.null(ris$simboli_modello) && !is.null(ris$simboli_modello[[nome_par]])) {
      simbolo_a_testo_semplice(ris$simboli_modello[[nome_par]])
    } else {
      simbolo_a_testo_semplice(simbolo_automatico_pedice(nome_par))
    }
  }
  n_parametri <- length(colnames(ris$mat_arr_boot))
  df_den <- length(ris$fonti) - n_parametri
  t_crit <- if (df_den > 0) qt(p_sup, df_den) else NA_real_
  righe <- lapply(colnames(ris$mat_arr_boot), function(nome_par) {
    valori <- ris$mat_arr_boot[, nome_par]; valori <- valori[is.finite(valori)]
    stima <- ris$fit_puntuale[[nome_par]]
    if (length(valori) >= 10) {
      # Bootstrap available: CI from the empirical percentiles, SE as the
      # standard deviation of the same bootstrap distribution.
      ci <- quantile(valori, probs = c(p_inf, p_sup), na.rm = TRUE)
      return(data.frame(Parameter = etichetta_parametro(nome_par), Estimate = round(stima, 3),
                        Std_Error = round(sd(valori), 3), CI_Lower = round(ci[1], 3), CI_Upper = round(ci[2], 3), stringsAsFactors = FALSE))
    }
    # Bootstrap not available (off, or too few valid iterations):
    # fallback to the standard error from the Hessian of the point fit itself
    # (always computed, at a cost close to zero, regardless of the
    # bootstrap) - a "classic" CI estimate +- t*SE instead of the percentiles.
    se_h <- if (!is.null(ris$errori_hessiana)) ris$errori_hessiana[[nome_par]] else NA_real_
    if (!is.null(se_h) && is.finite(se_h) && is.finite(t_crit)) {
      return(data.frame(Parameter = etichetta_parametro(nome_par), Estimate = round(stima, 3),
                        Std_Error = round(se_h, 3), CI_Lower = round(stima - t_crit * se_h, 3), CI_Upper = round(stima + t_crit * se_h, 3), stringsAsFactors = FALSE))
    }
    data.frame(Parameter = etichetta_parametro(nome_par), Estimate = round(stima, 3), Std_Error = NA, CI_Lower = NA, CI_Upper = NA, stringsAsFactors = FALSE)
  })
  do.call(rbind, righe)
}

# Describes in one sentence, for the operator, which of the different possible
# combinations produced the uncertainty shown in the table above -
# useful because the same table can derive from conceptually
# very different paths:
#  1) Hessian of the secondary fit alone (bootstrap off) - no
#     uncertainty of the primary fits is propagated, only that of the
#     Arrhenius/Q10/Eyring/custom fit itself;
#  2) nested bootstrap that resamples from the standard error (Hessian)
#     of each primary fit - all samples used the Linear method;
#  3) nested bootstrap that resamples from the empirical distribution of
#     each primary fit - all samples used Monte Carlo;
#  or a mix of 2 and 3, if the samples did not all use the same method.
descrivi_tipo_modellazione_sl <- function(ris) {
  if (is.null(ris$n_arr_totale) || ris$n_arr_totale == 0) {
    return("Secondary model uncertainty: from the Jacobian of the secondary (kinetic) model's own fit \u2014 no bootstrap, no uncertainty propagated from the primary fits.")
  }
  tipi <- sapply(ris$fonti, function(f) f$tipo)
  n_gauss <- sum(tipi == "gaussiano"); n_emp <- sum(tipi == "empirico"); n_punto <- sum(tipi == "solo_punto")
  parti <- character(0)
  if (n_gauss > 0) parti <- c(parti, sprintf("%d sample%s via the primary fit's Linear (Jacobian) error", n_gauss, if (n_gauss > 1) "s" else ""))
  if (n_emp > 0) parti <- c(parti, sprintf("%d sample%s via the primary fit's Monte Carlo distribution", n_emp, if (n_emp > 1) "s" else ""))
  if (n_punto > 0) parti <- c(parti, sprintf("%d sample%s with no primary uncertainty available (point estimate only)", n_punto, if (n_punto > 1) "s" else ""))
  paste0("Secondary model uncertainty: nested bootstrap propagating each sample's own primary-fit uncertainty (", paste(parti, collapse = "; "), ").")
}

output$sl_tabella_arrhenius <- renderTable({
  ris <- sl_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the Shelf-Life analysis first."))
  tab <- costruisci_tabella_arrhenius(ris, input$sl_livello_confidenza)
  colnames(tab) <- gsub("_", " ", colnames(tab))
  tab
}, striped = TRUE, hover = TRUE, bordered = TRUE, na = "N/A")

output$sl_info_bootstrap <- renderText({
  ris <- sl_risultato()
  if (is.null(ris)) return("")
  if (ris$n_arr_totale == 0) {
    paste0("Model: ", ris$modello_label, ". Point estimate only (bootstrap not computed). x = temperature in Kelvin, R = 8.314462618 J/(mol\u00b7K) where applicable.")
  } else {
    paste0("Model: ", ris$modello_label, ". Nested bootstrap: ", ris$n_arr_successo, " / ", ris$n_arr_totale, 
           " iterations successful. x = temperature in Kelvin, R = 8.314462618 J/(mol\u00b7K) where applicable.")
  }
})

# ---- Formula of the selected kinetic model, shown centred ----
# Same technique already used for the "Fitted Model Equation" of the main
# fit (plotmath via str2expression), simplified here: the parameters
# of these models (Ea, q, A, Q10, H) do not have a map of custom
# symbols as in the main fit, so no substitution -
# only the direct plotmath parsing of the formula. Reactive only to the
# CHOICE of the model (there is no need to have already run the analysis): it is only the
# symbolic form, it does not require estimated parameters to make sense.
disegna_formula_modello_sl <- function() {
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  plot.new()
  modello_scelto <- sl_ottieni_modello(input$sl_modello_cinetico, input$sl_formula_custom, input$sl_guess_custom)
  if (is.null(modello_scelto)) {
    disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f8f9fa", border = "#dee2e6", lwd = 1.5)
    text(0.5, 0.5, if (identical(input$sl_modello_cinetico, "custom")) "Enter a valid custom formula and guesses above." else "Choose a kinetic model above.", cex = 1.0, col = "#7f8c8d")
    return(invisible(NULL))
  }
  # Symbol substitution (e.g. Ea -> E[a]) before plotmath parsing - same
  # technique as costruisci_testo_formula_sostituita() in the main fit: sorts
  # by decreasing length (so that a shorter name does not "eat" part
  # of a longer one) and replaces only whole words (\\b...\\b).
  formula_sostituita <- modello_scelto$formula
  nomi_tutti <- modello_scelto$parametri[order(-nchar(modello_scelto$parametri))]
  mappa_sl <- as.list(modello_scelto$simboli)
  for (nome in nomi_tutti) {
    simbolo <- if (!is.null(mappa_sl[[nome]])) mappa_sl[[nome]] else simbolo_automatico_pedice(nome)
    if (!identical(simbolo, nome)) {
      formula_sostituita <- gsub(paste0("\\b", nome, "\\b"), simbolo, formula_sostituita)
    }
  }
  disegna_rettangolo_arrotondato(0, 0, 1, 1, raggio = 0.06, col = "#f0faf8", border = "#18bc9c", lwd = 2)
  
  divisione <- dividi_formula_lunga(formula_sostituita)
  lunghezza_riferimento <- max(nchar(divisione$riga1), if (!is.null(divisione$riga2)) nchar(divisione$riga2) else 0)
  # Same calibration correction as the main formula (Single Fit)
  # - here the container lives in the sidebar (narrower than the main
  # column), so the reference is proportionally lower,
  # but still much more generous than the old constant (35/48), which
  # already made Eyring (41 characters) and any medium-length custom
  # formula shrink.
  riferimento_cex <- 65
  cex_adattivo <- if (lunghezza_riferimento <= riferimento_cex) 1.5 else max(0.55, 1.5 * (riferimento_cex / lunghezza_riferimento)^0.6)
  
  if (is.null(divisione$riga2)) {
    espr <- tryCatch(str2expression(paste0("y == ", divisione$riga1)), error = function(e) NULL)
    if (is.null(espr) || length(espr) != 1) espr <- tryCatch(str2expression(paste0("y == ", modello_scelto$formula)), error = function(e) NULL)
    if (!is.null(espr) && length(espr) == 1) {
      tryCatch({
        text(0.5, 0.5, espr[[1]], cex = cex_adattivo, font = 3, col = "black")
      }, error = function(e) {
        text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black")
      })
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
      }, error = function(e) {
        text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black")
      })
    } else {
      text(0.5, 0.5, paste("y =", modello_scelto$formula), cex = min(cex_adattivo, 1.1), family = "mono", col = "black")
    }
  }
}

output$sl_grafico_formula_contenitore <- renderUI({
  modello_scelto <- sl_ottieni_modello(input$sl_modello_cinetico, input$sl_formula_custom, input$sl_guess_custom)
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
  plotOutput("sl_grafico_formula", height = paste0(altezza, "px"))
})

output$sl_grafico_formula <- renderPlot({ disegna_formula_modello_sl() })

# ---- Plot: Shelf Life vs Temperature, with the fitted Arrhenius curve ----
disegna_grafico_arrhenius_generico <- function(ris, prefisso = "sl_") {
  par(mar = c(5, 5, 4, 2) + 0.1)
  if (is.null(ris)) {
    plot(1, type = "n", main = "Run the Shelf-Life analysis first.", axes = FALSE, xlab = "", ylab = "")
    return(invisible(NULL))
  }
  campo <- function(suffisso) input[[paste0(prefisso, suffisso)]]
  
  T_C_vec <- ris$T_K - 273.15
  titolo <- if (isTRUE(input$mostra_titolo)) paste0("Shelf-Life vs Temperature (", ris$modello_label, " Fit)") else ""
  col_punti <- if (is.null(campo("col_punti"))) "#2980b9" else campo("col_punti")
  col_curva <- if (is.null(campo("col_curva"))) "#e74c3c" else campo("col_curva")
  col_banda_scelto <- campo("col_banda")
  col_banda <- if (is.null(col_banda_scelto) || col_banda_scelto == "curva") col_curva else col_banda_scelto
  alpha_banda <- if (is.null(campo("alpha_banda"))) 0.15 else campo("alpha_banda")
  cex_punti <- if (is.null(campo("cex_punti"))) 1.3 else campo("cex_punti")
  lwd_curva <- if (is.null(campo("lwd_curva"))) 2.5 else campo("lwd_curva")
  mostra_legenda <- if (is.null(campo("mostra_legenda"))) TRUE else campo("mostra_legenda")
  cex_legenda_sl <- if (is.null(campo("cex_legenda"))) 0.8 else campo("cex_legenda")
  pch_punti <- as.numeric(if (is.null(campo("pch_punti"))) "16" else campo("pch_punti"))
  livello <- input$sl_livello_confidenza
  
  # Same CIs already computed for the table (not recomputed differently
  # here), so the numbers shown in the plot and in the table always
  # coincide exactly.
  tab_sl <- costruisci_tabella_sl_per_temperatura(ris, livello)
  ci_lower_vec <- tab_sl$CI_Lower[match(names(ris$fonti), tab_sl$Sample)]
  ci_upper_vec <- tab_sl$CI_Upper[match(names(ris$fonti), tab_sl$Sample)]
  ci_lower_vec[is.na(ci_lower_vec)] <- ris$sl_puntuali[is.na(ci_lower_vec)]
  ci_upper_vec[is.na(ci_upper_vec)] <- ris$sl_puntuali[is.na(ci_upper_vec)]
  
  T_C_griglia <- seq(min(T_C_vec) - 2, max(T_C_vec) + 2, length.out = 150)
  T_K_griglia <- T_C_griglia + 273.15
  y_curva <- valuta_formula_generica(ris$formula_modello, as.list(ris$fit_puntuale), T_K_griglia)
  
  # Residual deviation of the point kinetic fit (how much the observed
  # shelf-life values depart from the fitted curve, net of the uncertainty
  # on the parameters) - needed to build the prediction band further below,
  # since ris does not save it ready for use.
  y_predetto_ai_dati <- valuta_formula_generica(ris$formula_modello, as.list(ris$fit_puntuale), ris$T_K)
  residui_cinetici <- ris$sl_puntuali - y_predetto_ai_dati
  df_residuo_cinetico <- max(length(ris$sl_puntuali) - length(ris$fit_puntuale), 1)
  sd_residuo_cinetico <- sqrt(sum(residui_cinetici^2, na.rm = TRUE) / df_residuo_cinetico)
  
  # Band around the curve: evaluates the Arrhenius model at EVERY
  # temperature of the grid using EVERY (Ea,q) pair of the nested
  # bootstrap, then takes the percentiles column by column - it reflects
  # the uncertainty propagated through both estimation stages. For the
  # CONFIDENCE band this is used directly (uncertainty on the
  # position of the true curve); for the PREDICTION band a Gaussian noise
  # is added to each bootstrap curve with standard deviation equal
  # to the residual deviation just computed (same technique already used for
  # the Inverse Prediction adapter on saved models) - it also reflects
  # the natural dispersion of the single samples around the curve,
  # not only the uncertainty on the parameters, and is therefore always wider.
  mostra_ci_banda <- if (is.null(campo("mostra_ci"))) TRUE else campo("mostra_ci")
  mostra_pi_banda <- if (is.null(campo("mostra_pi"))) FALSE else campo("mostra_pi")
  ci_banda_inf <- ci_banda_sup <- pi_banda_inf <- pi_banda_sup <- NULL
  righe_valide <- which(apply(ris$mat_arr_boot, 1, function(r) all(is.finite(r))))
  if ((mostra_ci_banda || mostra_pi_banda) && length(righe_valide) >= 10) {
    mat_curve_boot <- matrix(NA_real_, length(righe_valide), length(T_K_griglia))
    for (i in seq_along(righe_valide)) {
      par_b <- ris$mat_arr_boot[righe_valide[i], ]
      mat_curve_boot[i, ] <- valuta_formula_generica(ris$formula_modello, as.list(par_b), T_K_griglia)
    }
    p_inf <- (1 - livello / 100) / 2; p_sup <- 1 - p_inf
    if (mostra_ci_banda) {
      ci_banda_inf <- apply(mat_curve_boot, 2, quantile, probs = p_inf, na.rm = TRUE)
      ci_banda_sup <- apply(mat_curve_boot, 2, quantile, probs = p_sup, na.rm = TRUE)
    }
    if (mostra_pi_banda && is.finite(sd_residuo_cinetico) && sd_residuo_cinetico > 0) {
      rumore_pred <- matrix(rnorm(length(mat_curve_boot), mean = 0, sd = sd_residuo_cinetico), nrow = nrow(mat_curve_boot))
      mat_pred_boot <- mat_curve_boot + rumore_pred
      pi_banda_inf <- apply(mat_pred_boot, 2, quantile, probs = p_inf, na.rm = TRUE)
      pi_banda_sup <- apply(mat_pred_boot, 2, quantile, probs = p_sup, na.rm = TRUE)
    }
  }
  # Without nested bootstrap (or with fewer than 10 valid iterations): bands from the
  # linear approximation of the kinetic model, as already in
  # Non-Isothermal. Confidence: delta method, SE(T)^2 = g^T Cov g with the
  # analytical Jacobian; pointwise multiplier t(df) or simultaneous one of
  # Bates & Watts (1988), sqrt(p F(p, df; alpha)). Prediction: in addition the
  # residual variance of the kinetic model, always pointwise. Previously, without
  # bootstrap, the plot showed no band at all.
  banda_simultanea_sl <- FALSE
  if ((mostra_ci_banda || mostra_pi_banda) && is.null(ci_banda_inf) && is.null(pi_banda_inf) && !is.null(ris$cov_modello)) {
    G <- tryCatch(crea_jacobiano_modello(ris$formula_modello, names(ris$fit_puntuale))(ris$fit_puntuale, T_K_griglia), error = function(e) NULL)
    if (!is.null(G) && all(is.finite(G))) {
      se_curva_sl <- sqrt(pmax(rowSums((G %*% ris$cov_modello) * G), 0))
      p_cin <- length(ris$fit_puntuale)
      if (mostra_ci_banda) {
        banda_simultanea_sl <- isTRUE(campo("banda_simultanea"))
        m_ci <- moltiplicatore_banda(ris$df_cinetico, p_cin, livello, banda_simultanea_sl)
        if (is.finite(m_ci)) { ci_banda_inf <- y_curva - m_ci * se_curva_sl; ci_banda_sup <- y_curva + m_ci * se_curva_sl }
      }
      if (mostra_pi_banda) {
        m_pi <- moltiplicatore_banda(ris$df_cinetico, p_cin, livello, FALSE)
        se_pred_sl <- sqrt(se_curva_sl^2 + sd_residuo_cinetico^2)
        if (is.finite(m_pi)) { pi_banda_inf <- y_curva - m_pi * se_pred_sl; pi_banda_sup <- y_curva + m_pi * se_pred_sl }
      }
    }
  }
  
  y_min <- allarga_estremo(min(c(ci_lower_vec, ci_banda_inf, pi_banda_inf), na.rm = TRUE), "giu")
  y_max <- allarga_estremo(max(c(ci_upper_vec, ci_banda_sup, pi_banda_sup), na.rm = TRUE), "su")
  
  plot(T_C_vec, ris$sl_puntuali, type = "n",
       xlim = range(T_C_vec) + c(-2, 2), ylim = c(max(0, y_min), y_max),
       main = titolo, xlab = "Temperature (\u00b0C)", ylab = "Shelf Life", las = 1)
  if (is.null(input$mostra_griglia) || isTRUE(input$mostra_griglia)) grid(col = "gray90", lty = "dotted")
  
  if (!is.null(pi_banda_inf)) {
    polygon(c(T_C_griglia, rev(T_C_griglia)), c(pi_banda_inf, rev(pi_banda_sup)), col = adjustcolor(col_banda, alpha.f = alpha_banda * 0.6), border = NA)
  }
  if (!is.null(ci_banda_inf)) {
    polygon(c(T_C_griglia, rev(T_C_griglia)), c(ci_banda_inf, rev(ci_banda_sup)), col = adjustcolor(col_banda, alpha.f = alpha_banda), border = NA)
  }
  lines(T_C_griglia, y_curva, col = col_curva, lwd = lwd_curva)
  arrows(T_C_vec, pmax(0, ci_lower_vec), T_C_vec, ci_upper_vec, angle = 90, code = 3, length = 0.05, col = col_punti, lwd = 1.5)
  points(T_C_vec, ris$sl_puntuali, pch = pch_punti, col = col_punti, cex = cex_punti)
  
  if (mostra_legenda) {
    etichette <- c(paste0("Sample shelf life (", livello, "% CI)"), paste0(ris$modello_label, " fit"))
    colori_legenda <- c(col_punti, col_curva)
    pch_legenda <- c(pch_punti, NA)
    lty_legenda <- c(NA, 1)
    lwd_legenda <- c(NA, lwd_curva)
    if (!is.null(ci_banda_inf)) {
      etichette <- c(etichette, paste0(livello, "% Confidence Band", if (banda_simultanea_sl) " (simultaneous)" else ""))
      colori_legenda <- c(colori_legenda, adjustcolor(col_banda, alpha.f = 0.5))
      pch_legenda <- c(pch_legenda, 15); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA)
    }
    if (!is.null(pi_banda_inf)) {
      etichette <- c(etichette, paste0(livello, "% Prediction Band"))
      colori_legenda <- c(colori_legenda, adjustcolor(col_banda, alpha.f = 0.3))
      pch_legenda <- c(pch_legenda, 15); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA)
    }
    pos_legenda_sl <- if (is.null(campo("pos_legenda"))) "topright" else campo("pos_legenda")
    legend(pos_legenda_sl, legend = etichette, col = colori_legenda, pch = pch_legenda, lty = lty_legenda, lwd = lwd_legenda,
           bty = "o", cex = cex_legenda_sl, bg = "white")
  }
}

disegna_grafico_arrhenius <- function() {
  ris <- sl_risultato()
  disegna_grafico_arrhenius_generico(ris, prefisso = "sl_")
}

output$sl_grafico_arrhenius <- renderPlot({ disegna_grafico_arrhenius() })

output$sl_scarica_grafico <- downloadHandler(
  filename = function() paste0("arrhenius_shelf_life.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_grafico_arrhenius()
    dev.off()
  }
)

# ---- Prediction: shelf life at a NEW (untested) temperature ----
output$sl_predizione_risultato <- renderPrint({
  ris <- sl_risultato()
  if (is.null(ris)) { cat("Run the Shelf-Life analysis first."); return(invisible(NULL)) }
  if (is.null(input$sl_temp_predizione) || is.na(input$sl_temp_predizione)) { cat("Enter a temperature."); return(invisible(NULL)) }
  
  T_pred_K <- if (input$sl_unita_temp == "celsius") input$sl_temp_predizione + 273.15 else input$sl_temp_predizione
  if (T_pred_K <= 0) { cat("Temperature in Kelvin must be positive."); return(invisible(NULL)) }
  
  valori_boot <- ris$mat_arr_boot[, 1:2, drop = FALSE]
  sl_boot_pred <- apply(valori_boot, 1, function(par) {
    if (any(!is.finite(par))) return(NA_real_)
    valuta_formula_generica(ris$formula_modello, as.list(setNames(par, colnames(valori_boot))), T_pred_K)
  })
  sl_boot_pred <- sl_boot_pred[is.finite(sl_boot_pred)]
  
  sl_puntuale_pred <- valuta_formula_generica(ris$formula_modello, as.list(ris$fit_puntuale), T_pred_K)
  
  if (length(sl_boot_pred) < 10) {
    motivo <- if (ris$n_arr_totale == 0) "bootstrap not computed" else "too few successful bootstrap iterations"
    cat(sprintf("Predicted shelf life at %.1f\u00b0: %.3f (confidence interval not available - %s)", input$sl_temp_predizione, sl_puntuale_pred, motivo))
  } else {
    livello <- input$sl_livello_confidenza
    p_inf <- (1 - livello / 100) / 2; p_sup <- 1 - p_inf
    ci <- quantile(sl_boot_pred, probs = c(p_inf, p_sup), na.rm = TRUE)
    cat(sprintf("Predicted shelf life at %.1f\u00b0: %.3f\n", input$sl_temp_predizione, sl_puntuale_pred))
    cat(sprintf("%s%% Confidence Interval: [%.3f, %.3f]\n", livello, ci[1], ci[2]))
    cat(sprintf("(from %d/%d successful nested bootstrap iterations)", length(sl_boot_pred), nrow(valori_boot)))
  }
})

# ---- Export ----
output$sl_scarica_csv <- downloadHandler(
  filename = function() paste0("shelf_life_arrhenius_", Sys.Date(), ".csv"),
  content = function(file) {
    ris <- sl_risultato()
    if (is.null(ris)) return()
    df_sl <- costruisci_tabella_sl_per_temperatura(ris, input$sl_livello_confidenza)
    df_arr <- costruisci_tabella_arrhenius(ris, input$sl_livello_confidenza)
    
    cat("--- SHELF LIFE PER TEMPERATURE ---\n", file = file)
    write.table(df_sl, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat("\n--- ARRHENIUS PARAMETERS ---\n", file = file, append = TRUE)
    write.table(df_arr, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    cat(paste0("\nModel: ", ris$modello_label, ". x = temperature in Kelvin, R = 8.314462618 J/(mol*K) where applicable.\n"), file = file, append = TRUE)
    cat(sprintf("Critical Y value used: %s\n", ris$y_critico), file = file, append = TRUE)
    cat(sprintf("Confidence level: %s%%\n", input$sl_livello_confidenza), file = file, append = TRUE)
    if (ris$n_arr_totale == 0) {
      cat("Nested bootstrap: not computed (point estimate only)\n", file = file, append = TRUE)
    } else {
      cat(sprintf("Nested bootstrap: %d/%d iterations successful\n", ris$n_arr_successo, ris$n_arr_totale), file = file, append = TRUE)
    }
    ris_posthoc <- tryCatch(sl_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: OMNIBUS TEST (COCHRAN'S Q, 3+ SAMPLES) ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$omnibus, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: PAIRWISE ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$posthoc, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
      cat("\n--- POST-HOC COMPARISON ON SHELF LIFE: COMPACT LETTER DISPLAY ---\n", file = file, append = TRUE)
      write.table(ris_posthoc$cld, file, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE)
    }
  }
)

output$sl_scarica_excel <- downloadHandler(
  filename = function() paste0("shelf_life_arrhenius_", Sys.Date(), ".xlsx"),
  content = function(file) {
    ris <- sl_risultato()
    if (is.null(ris)) return()
    df_sl <- costruisci_tabella_sl_per_temperatura(ris, input$sl_livello_confidenza)
    df_arr <- costruisci_tabella_arrhenius(ris, input$sl_livello_confidenza)
    df_info <- data.frame(Property = c("Model", "Critical Y Value", "Confidence Level", "Nested Bootstrap Iterations"),
                          Value = c(paste0(ris$modello_label, " (x in Kelvin, R = 8.314462618 J/(mol*K) where applicable)"), 
                                   as.character(ris$y_critico), paste0(input$sl_livello_confidenza, "%"),
                                   if (ris$n_arr_totale == 0) "Not computed (point estimate only)" else paste0(ris$n_arr_successo, "/", ris$n_arr_totale)),
                          stringsAsFactors = FALSE)
    fogli <- list(Shelf_Life_Per_Temperature = df_sl, Kinetic_Model_Parameters = df_arr, Info = df_info)
    ris_posthoc <- tryCatch(sl_posthoc_risultato(), error = function(e) NULL)
    if (!is.null(ris_posthoc) && isTRUE(ris_posthoc$successo)) {
      fogli$PostHoc_Omnibus <- ris_posthoc$omnibus
      fogli$PostHoc_Pairwise <- ris_posthoc$posthoc
      fogli$PostHoc_CLD <- ris_posthoc$cld
    }
    writexl::write_xlsx(per_excel(fogli), path = file)
  }
)
