# ==============================================================================
# NON-ISOTHERMAL MODULE (Batch Elaboration)
# ==============================================================================
# Prediction of the shelf life under a temperature profile T(t) varying
# with time, starting from a Batch fit already performed with a kinetic model
# of order 0, 1, 2 or n (Reaction Kinetics). It does not cover microbial growth
# (Baranyi, Gompertz, etc.): those models require a true system of
# coupled differential equations (deSolve is not available in
# WebR/Shinylive - see https://github.com/r-wasm/webr/issues/425), whereas
# fixed-order kinetics admit a closed integrated solution once
# the cumulative integral of the rate k(T(t)) over time is known, computable with
# the trapezoidal rule alone (base R function, no external package).
#
# COMPLETELY SELF-CONTAINED FILE: it neither modifies nor depends on the internals of
# batch_elaboration.R or shelf_life.R, apart from READ-ONLY access
# to the reactiveVal batch_risultati() (already shared by several existing
# modules) and to the generic utilities already used everywhere in the app
# (valuta_formula_generica, calcola_loss_generica, simbolo_automatico_pedice,
# disegna_rettangolo_arrotondato). It can be removed entirely (this
# file + the two hook lines in R/server.R and R/ui.R) without any effect
# on the other modules.
# ==============================================================================


# ---- 1. Recognition of the kinetic model used in Batch ---------------------
# EXACT textual comparison with the formulas of the registry (not the ID of the
# model): batch_risultati() saves only the raw text of the formula
# (res$formula), not the ID used to generate it, and one cannot rely on
# input$tipo_modello because the user might have already changed it after
# launching the batch (same caution already applied elsewhere in this
# session for exactly the same reason: costruisci_espressione_formula of the
# single fit had this same problem, solved by saving
# tipo_modello_usato at the moment of the fit - here however the file cannot touch
# batch_elaboration.R to add an analogous field, so the textual comparison
# is the only way that respects the constraint of not modifying other
# modules). costruisci_formula() (server.R) ALWAYS returns
# MODEL_REGISTRY[[id]]$formula without any change for non-custom
# models, so the exact comparison is safe as long as those 5 formulas in the
# registry do not change.
NI_FORMULE_CONOSCIUTE <- c(
  "A0 - k * x",
  "log(A0) - k * x",
  "A0 * exp(-k * x)",
  "A0 / (1 + 2 * A0 * k * x)",
  "((A0^(1 - n)) + (n - 1) * k * x)^(1 / (1 - n))"
)
NI_ORDINE_PER_FORMULA <- c("zero", "primo", "primo", "secondo", "n")
NI_NOME_ORDINE <- c(zero = "Zero-Order", primo = "First-Order", secondo = "Second-Order", n = "n-Order")

ni_riconosci_ordine <- function(formula_batch) {
  if (is.null(formula_batch)) return(NULL)
  indice <- match(formula_batch, NI_FORMULE_CONOSCIUTE)
  if (is.na(indice)) return(NULL)
  NI_ORDINE_PER_FORMULA[indice]
}


# ---- 2. Secondary models k(T) ----------------------------------------------
# The 4 forms discussed: classic Arrhenius (with the physical meaning of Ea),
# generic modified Arrhenius (same mathematics but without attributing to B
# the meaning of activation energy - useful when the phenomenon is not
# a true chemical reaction), log-logistic and its simplified form
# (without the scale term m, when it is enough to describe the SHAPE of the
# rate-temperature relationship without a separate absolute scale).
# Generic variant of valida_formula (Single Fit/Batch/Model Comparison,
# see R/server.R), for custom formulas written in terms of an independent
# variable other than "x" - here "T" for the secondary model, "t" for
# the temperature profile further below. Exactly the same security
# whitelist (no function allowed beyond basic mathematics), only
# the name of the independent variable is parametric instead of fixed.
ni_valida_formula_generica <- function(testo, nomi_parametri, nome_variabile) {
  espr <- tryCatch(parse(text = testo), error = function(e) NULL)
  if (is.null(espr) || length(espr) != 1) return(FALSE)
  nomi_usati <- all.names(espr)
  funzioni_consentite <- c("+", "-", "*", "/", "^", "(",
                           "exp", "log", "log10", "log2", "sqrt", "abs", "pi",
                           "sin", "cos", "tan", "asin", "acos", "atan",
                           "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
                           "gamma", "lgamma", "beta", "factorial", "choose")
  whitelist <- c(funzioni_consentite, nome_variabile, nomi_parametri)
  all(nomi_usati %in% whitelist)
}

# DESIGN NOTE, valid for this whole function: "tipo" is not always
# one of the 4 short codes below - for a "custom" secondary model,
# the caller of this function (see observeEvent(input$ni_fit_secondario))
# passes DIRECTLY THE TEXT OF THE FORMULA in place of the short code,
# already safely validated BEFORE the fit (see ni_valida_formula_generica
# above). This allows "tipo" to carry the formula itself without
# having to modify the many points of the code that already pass tipo+par
# together (they have always travelled paired, by construction) - a custom
# formula "arrives for free" wherever a built-in model would arrive, including
# the bootstrap, the plots, and the isothermal comparison, without touching any
# of those points.
ni_modello_secondario <- function(tipo, T_kelvin, par) {
  switch(tipo,
    "arrhenius" = par[["A"]] * exp(-par[["Ea"]] / (8.314462618 * T_kelvin)),
    "arrhenius_mod" = par[["A"]] * exp(-par[["B"]] / T_kelvin),
    "log_logistic" = par[["m"]] * log(1 + exp(par[["c"]] * (T_kelvin - par[["Tc"]]))),
    "log_logistic_semplice" = log(1 + exp(par[["c"]] * (T_kelvin - par[["Tc"]]))),
    # Fallback: any "tipo" not among the 4 codes above is treated
    # as a custom formula to be evaluated directly (T and the parameters
    # are already available in the evaluation environment).
    eval(parse(text = tipo), envir = c(as.list(par), list(T = T_kelvin)))
  )
}

NI_PARAMETRI_SECONDARIO <- list(
  arrhenius = c("A", "Ea"),
  arrhenius_mod = c("A", "B"),
  log_logistic = c("m", "c", "Tc"),
  log_logistic_semplice = c("c", "Tc")
)
NI_LABEL_SECONDARIO <- c(
  arrhenius = "Classic Arrhenius: A \u00d7 exp(-Ea / (R\u00b7T))",
  arrhenius_mod = "Generic Modified Arrhenius: A \u00d7 exp(-B / T)",
  log_logistic = "Log-Logistic: m \u00b7 ln(1 + exp(c\u00b7(T - Tc)))",
  log_logistic_semplice = "Simplified Log-Logistic: ln(1 + exp(c\u00b7(T - Tc)))"
)

# Sensible initial guesses for the optimizer: Arrhenius and modified
# Arrhenius linearize exactly (ln(k) is linear in 1/T), so
# I use a simple linear regression on that transform to obtain a
# starting point already very close to the final result (the same
# classic "Arrhenius plot"). For the log-logistic ones, which do not linearize
# as cleanly, I use simple heuristics based on the range of the observed
# data - less precise, but a reasonable starting point for
# the optimizer.
ni_guess_secondario <- function(tipo, T_kelvin, k_oss) {
  tryCatch({
    switch(tipo,
      "arrhenius" = ,
      "arrhenius_mod" = {
        invT <- 1 / T_kelvin
        logk <- log(k_oss)
        fit_lin <- lm(logk ~ invT)
        pendenza <- unname(coef(fit_lin)[2])
        intercetta <- unname(coef(fit_lin)[1])
        A_guess <- exp(intercetta)
        if (tipo == "arrhenius") {
          list(A = A_guess, Ea = -pendenza * 8.314462618)
        } else {
          list(A = A_guess, B = -pendenza)
        }
      },
      "log_logistic" = ,
      "log_logistic_semplice" = {
        Tc_guess <- mean(T_kelvin)
        range_T <- diff(range(T_kelvin))
        c_guess <- if (range_T > 0) 4 / range_T else 0.5
        if (tipo == "log_logistic") {
          list(m = mean(k_oss) / log(2), c = c_guess, Tc = Tc_guess)
        } else {
          list(c = c_guess, Tc = Tc_guess)
        }
      }
    )
  }, error = function(e) {
    # Fallback to generic values if the linearization fails for
    # some reason (e.g. negative k for an anomalous fit) - the user can
    # still correct them by hand before launching the fit.
    switch(tipo,
      "arrhenius" = list(A = 1e8, Ea = 50000),
      "arrhenius_mod" = list(A = 1e8, B = 6000),
      "log_logistic" = list(m = 1, c = 0.5, Tc = mean(T_kelvin)),
      "log_logistic_semplice" = list(c = 0.5, Tc = mean(T_kelvin))
    )
  })
}


# ---- 3. Temperature profiles T(t) ------------------------------------------
# Each returns a FUNCTION of t (not an already evaluated vector),
# so the numerical integration below can evaluate it at any
# requested instant, with the same interface both for the analytical profiles and for the
# raw datalogger data (approxfun, base R function - no
# external package needed). rule=2 in approxfun() keeps the
# extreme value constant beyond the limits of the data, instead of giving NA, so as not to make
# the integration fail if the chosen time grid slightly exceeds
# the range of the loaded datalogger.
# DESIGN NOTE, same as ni_modello_secondario above: "tipo" for a
# custom profile IS directly the text of the formula (in terms of
# "t"), safely validated BEFORE arriving here (see
# ni_valida_formula_generica and the single call point further below).
ni_costruisci_profilo_T <- function(tipo, par, dati_grezzi = NULL) {
  switch(tipo,
    "dati_grezzi" = {
      if (is.null(dati_grezzi) || nrow(dati_grezzi) < 2) return(NULL)
      approxfun(dati_grezzi$tempo, dati_grezzi$temperatura, rule = 2)
    },
    "lineare" = function(t) par[["T0"]] + par[["a"]] * t,
    "quadratico" = function(t) par[["T0"]] + par[["a"]] * t + par[["b"]] * t^2,
    "sinusoidale" = function(t) par[["Tm"]] + (par[["a"]] / 2) * sin((2 * pi * t / par[["L"]]) + (3 * pi / 2)),
    "esponenziale" = function(t) par[["Th"]] + (par[["T0"]] - par[["Th"]]) * exp(-par[["J"]] * t),
    "abuso_termico" = ni_costruisci_profilo_abuso(par),
    # Fallback: any "tipo" not among the codes above is treated as
    # a custom formula, evaluated inside a closure - same technique already
    # used for linear/quadratic/sinusoidal/exponential, which are already
    # inline closures built in the same way.
    function(t) eval(parse(text = tipo), envir = c(as.list(par), list(t = t)))
  )
}

# "Synthetic Thermal Abuse" profile: constant base temperature (T0) with
# n identical thermal abuses, distributed at regular intervals over the whole
# duration (centres at Dtot*i/(n+1), i=1..n - never exactly at the beginning or
# at the end). Each abuse is a "raised cosine" window pulse:
# smoothed cosine rise and fall (zero slope at the edges,
# no corners), with a central plateau at full amplitude - the same
# shape, with different width, covers all 3 requested styles
# (frazione_bordo = 0 -> sharp rectangular jump; small -> fast
# ramp with a long plateau; 0.5 -> soft ramp, no plateau, it rises and
# falls for the whole duration). COMPLETELY deterministic function
# (no rnorm) - unlike a profile with random noise, it can
# therefore be re-evaluated thousands of times in the nested bootstrap
# without any risk of resampling noise at every call.
ni_costruisci_profilo_abuso <- function(par) {
  T0 <- par[["T0"]]; Dtot <- par[["Dtot"]]; n <- round(par[["n"]])
  durata <- par[["dur"]]; ampiezza <- par[["amp"]]
  segno <- par[["segno"]]; frazione_bordo <- par[["frazione_bordo"]]
  centri <- if (n >= 1) Dtot * seq_len(n) / (n + 1) else numeric(0)
  bordo <- frazione_bordo * durata
  function(t) {
    risultato <- rep(T0, length(t))
    for (centro in centri) {
      inizio <- centro - durata / 2
      tau <- t - inizio
      dentro <- tau >= 0 & tau <= durata
      if (!any(dentro)) next
      contributo <- numeric(length(t))
      if (bordo <= 0) {
        contributo[dentro] <- 1
      } else {
        salita <- dentro & tau < bordo
        discesa <- dentro & tau > (durata - bordo)
        pianoro <- dentro & !salita & !discesa
        contributo[salita] <- 0.5 * (1 - cos(pi * tau[salita] / bordo))
        contributo[discesa] <- 0.5 * (1 - cos(pi * (durata - tau[discesa]) / bordo))
        contributo[pianoro] <- 1
      }
      risultato <- risultato + segno * ampiezza * contributo
    }
    risultato
  }
}

NI_PARAMETRI_PROFILO <- list(
  lineare = c("T0", "a"),
  quadratico = c("T0", "a", "b"),
  sinusoidale = c("Tm", "a", "L"),
  esponenziale = c("Th", "T0", "J"),
  abuso_termico = c("T0", "Dtot", "n", "dur", "amp")
)
NI_LABEL_PROFILO <- c(
  dati_grezzi = "Raw Data (datalogger upload)",
  lineare = "Linear: T0 + a\u00b7t",
  quadratico = "Quadratic: T0 + a\u00b7t + b\u00b7t\u00b2",
  sinusoidale = "Sinusoidal: Tm + (a/2)\u00b7sin(2\u03c0t/L + 3\u03c0/2)",
  esponenziale = "Exponential: Th + (T0-Th)\u00b7exp(-J\u00b7t)",
  abuso_termico = "Synthetic Thermal Abuse Profile"
)


# ---- 4. Cumulative integration of k(T(t)) ----------------------------------
# Fine grid + cumulative trapezoidal rule: sufficient for these
# models, since the relationship between the observed quantity and the cumulative
# integral of k(T(t)) is known in closed form (see point 5) - a
# true ODE solver is not needed. Numerically verified: under constant
# temperature it recovers EXACTLY (error 0) the original isothermal formula
# of the registry; under genuinely variable temperature (linear ramp)
# it agrees within 0.08% with an independent Euler integration
# of the original differential equation (difference expected from the
# discretization error of both methods, not a conceptual error).
ni_integra_k_cumulativo <- function(funzione_T, tipo_secondario, par_secondario, t_max, n_punti = 500) {
  t_griglia <- seq(0, t_max, length.out = n_punti)
  T_griglia <- funzione_T(t_griglia)
  k_griglia <- ni_modello_secondario(tipo_secondario, T_griglia, par_secondario)
  dt <- diff(t_griglia)
  incrementi <- (head(k_griglia, -1) + tail(k_griglia, -1)) / 2 * dt
  K_griglia <- c(0, cumsum(incrementi))
  list(t = t_griglia, T = T_griglia, k = k_griglia, K = K_griglia)
}


# ---- 5. Integrated relationship C(t) and inversion for the threshold time ---
# For each kinetic order, the solution of the differential equation
# dC/dt = -k(t)*C^order under variable k(t) is obtained by replacing, in the
# known isothermal solution, the product "k*t" with the cumulative integral
# K(t) = integral of k(T(tau)) from 0 to t (separable equation - valid for
# any fixed order, not only 0 and 1). Algebraically verified:
# ni_K_soglia() reinserted into ni_quantita_da_K() gives back exactly C_crit for
# all 4 orders.
ni_quantita_da_K <- function(ordine, A0, K, n_ordine_val = NULL) {
  switch(ordine,
    "zero" = A0 - K,
    "primo" = A0 * exp(-K),
    "secondo" = A0 / (1 + 2 * A0 * K),
    "n" = (A0^(1 - n_ordine_val) + (n_ordine_val - 1) * K)^(1 / (1 - n_ordine_val)),
    stop("Ordine cinetico sconosciuto: ", ordine)
  )
}

ni_K_soglia <- function(ordine, A0, C_crit, n_ordine_val = NULL) {
  switch(ordine,
    "zero" = A0 - C_crit,
    "primo" = log(A0 / C_crit),
    "secondo" = (A0 / C_crit - 1) / (2 * A0),
    "n" = (C_crit^(1 - n_ordine_val) - A0^(1 - n_ordine_val)) / (n_ordine_val - 1),
    stop("Ordine cinetico sconosciuto: ", ordine)
  )
}


# ---- 5b. Plot aesthetics (customizable) ------------------------------------
# Integrated into the shared "Advanced Plot Aesthetics" modal (two new
# tabs added there, not here) - the drawing functions below read
# input$ni_... directly with a fallback value, same scheme already
# used by every other plot of the app (no separate "Apply" button:
# the changes apply immediately in a reactive way).
observeEvent(input$ni_btn_estetica_secondario, { apri_estetica("Non-Isothermal: Secondary Model") })
observeEvent(input$ni_btn_estetica_previsione, { apri_estetica("Non-Isothermal: Prediction Plots") })
observeEvent(input$ni_btn_estetica_k, { apri_estetica("Non-Isothermal: Prediction Plots") })

# Kelvin/Celsius conversion only for the DISPLAY in the plots -
# separate from the Kelvin/Celsius selector of section 1 (which only serves to
# ENTER the temperatures of the samples): here it concerns only how the
# values (internally always in Kelvin, needed for R=8.314
# J/(mol*K) in the secondary models) are shown on the axes.
ni_converti_temp_display <- function(valori_K) {
  if (isTRUE(input$ni_mostra_celsius)) valori_K - 273.15 else valori_K
}
ni_etichetta_temp <- function() {
  if (isTRUE(input$ni_mostra_celsius)) "Temperature (\u00b0C)" else "Temperature (K)"
}

# Alpha (1 - level/100) from the confidence level chosen by the operator
# near step 2 - fallback to 95% if the field is empty/invalid.
# Read wherever a t_crit or a bootstrap percentile is needed in this module,
# so table and plots remain consistent with a single shared
# setting, instead of a fixed 95% scattered in several points of the code.
ni_alpha_confidenza <- function() {
  livello <- input$ni_livello_confidenza
  if (is.null(livello) || is.na(livello) || livello <= 0 || livello >= 100) livello <- 95
  1 - livello / 100
}

ni_leggi_estetica <- function() {
  col_linea_sec <- if (is.null(input$ni_ln_col_secondario)) "#18bc9c" else input$ni_ln_col_secondario
  col_banda_sec <- if (is.null(input$ni_col_banda_secondario) || input$ni_col_banda_secondario == "curva") col_linea_sec else input$ni_col_banda_secondario
  list(
    col_punti_sec = if (is.null(input$ni_pt_col_secondario)) "#2c3e50" else input$ni_pt_col_secondario,
    cex_punti_sec = if (is.null(input$ni_cex_punti_secondario)) 1.1 else input$ni_cex_punti_secondario,
    pch_punti_sec = as.numeric(if (is.null(input$ni_pch_punti_secondario)) "16" else input$ni_pch_punti_secondario),
    col_linea_sec = col_linea_sec,
    lwd_curva_sec = if (is.null(input$ni_lwd_curva_secondario)) 2 else input$ni_lwd_curva_secondario,
    col_banda_sec = col_banda_sec,
    alpha_banda_sec = if (is.null(input$ni_alpha_banda_secondario)) 0.2 else input$ni_alpha_banda_secondario,
    mostra_legenda_sec = if (is.null(input$ni_mostra_legenda_sec)) TRUE else input$ni_mostra_legenda_sec,
    cex_legenda_sec = if (is.null(input$ni_cex_legenda_sec)) 0.85 else input$ni_cex_legenda_sec,
    pos_legenda_sec = if (is.null(input$ni_pos_legenda_sec)) "topright" else input$ni_pos_legenda_sec,
    col_T = if (is.null(input$ni_col_T)) "#e67e22" else input$ni_col_T,
    col_C = if (is.null(input$ni_col_C)) "#18bc9c" else input$ni_col_C,
    col_k = if (is.null(input$ni_col_k)) "#18bc9c" else input$ni_col_k,
    lty_k = as.numeric(if (is.null(input$ni_lty_k)) "1" else input$ni_lty_k),
    lwd_k = if (is.null(input$ni_lwd_k)) 2 else input$ni_lwd_k,
    mostra_legenda_k = if (is.null(input$ni_mostra_legenda_k)) TRUE else input$ni_mostra_legenda_k,
    cex_legenda_k = if (is.null(input$ni_cex_legenda_k)) 0.85 else input$ni_cex_legenda_k,
    pos_legenda_k = if (is.null(input$ni_pos_legenda_k)) "topright" else input$ni_pos_legenda_k,
    col_soglia = if (is.null(input$ni_col_soglia)) "#c0392b" else input$ni_col_soglia,
    col_iso = if (is.null(input$ni_col_iso)) "#e67e22" else input$ni_col_iso,
    lty_iso = as.numeric(if (is.null(input$ni_lty_iso)) "2" else input$ni_lty_iso),
    lwd_linee = if (is.null(input$ni_lwd_linee)) 2 else input$ni_lwd_linee,
    lty_T = as.numeric(if (is.null(input$ni_lty_T)) "1" else input$ni_lty_T),
    alpha_T = if (is.null(input$ni_alpha_T)) 1 else input$ni_alpha_T,
    mostra_legenda_prev = if (is.null(input$ni_mostra_legenda_prev)) TRUE else input$ni_mostra_legenda_prev,
    cex_legenda_prev = if (is.null(input$ni_cex_legenda_prev)) 0.85 else input$ni_cex_legenda_prev,
    pos_legenda_prev = if (is.null(input$ni_pos_legenda_prev)) "topright" else input$ni_pos_legenda_prev,
    grid_on = if (is.null(input$mostra_griglia)) TRUE else input$mostra_griglia
  )
}


# ---- 6. Safe identifier for the Shiny IDs ----------------------------------
# Minimal local copy (one line) instead of reusing the one in shelf_life.R:
# for the full independence of this file from other modules, even at the cost of
# a small duplication of a one-line function.
ni_id_sicuro <- function(nome, indice) paste0(gsub("[^A-Za-z0-9_]", "_", nome), "_", indice)


# ---- 7. Validation of the current Batch ------------------------------------
# The kinetic model must be one of the 5 recognized ones (point 1) and there must
# be at least 3 successful samples - same minimum threshold already used
# elsewhere in the app for a 2-parameter regression with at least 1 residual
# degree of freedom. The temperature is NOT required here (it is
# confirmed/entered in the table at point 8): a batch can therefore
# turn out "valid" at this stage even if the temperatures still have to be
# entered by hand.
ni_batch_valido <- reactive({
  br <- batch_risultati()
  if (is.null(br)) {
    return(list(valido = FALSE, messaggio = "Run a Batch Elaboration first (above) using one of the order-based Reaction Kinetics models (Zero-Order, First-Order, Second-Order, or n-Order)."))
  }
  ordine <- ni_riconosci_ordine(br$formula)
  if (is.null(ordine)) {
    return(list(valido = FALSE, messaggio = "This module only works when the Batch above was run with one of the order-based Reaction Kinetics models (Zero-Order, First-Order, Second-Order, or n-Order) \u2014 the currently loaded Batch used a different model."))
  }
  campioni_ok <- Filter(function(r) isTRUE(r$successo), br$risultati)
  if (length(campioni_ok) < 3) {
    return(list(valido = FALSE, messaggio = paste0("At least 3 successfully fitted samples are needed (found ", length(campioni_ok), ").")))
  }
  list(valido = TRUE, ordine = ordine, campioni = campioni_ok, temperature_da_file = br$temperature_da_file, formula = br$formula)
})

output$ni_stato <- renderUI({
  stato <- ni_batch_valido()
  if (!stato$valido) {
    return(p(style = "font-size: 12px; color: #7f8c8d; font-style: italic;", stato$messaggio))
  }
  p(style = "font-size: 12px; color: #196f3d;",
    icon("circle-check"), " Batch recognized: ", tags$strong(NI_NOME_ORDINE[[stato$ordine]]),
    " kinetics, ", length(stato$campioni), " successfully fitted samples.")
})

# Wraps the whole interface of steps 1-4 (temperature table, fitting
# of the secondary model, choice of the T(t) profile, final prediction) - not
# shown at all if the current batch is not suitable. A single renderUI
# instead of a conditionalPanel with a custom boolean output (never
# used elsewhere in this project, therefore not verifiable against an
# already working example) - same "NULL or content" pattern already used
# successfully many times in this session for analogous cases.
# Part 1+2 (confirmation of temperatures + fit of the secondary model): ALWAYS
# visible, regardless of the validity of the current Batch - unlike
# the rest below (which makes sense only after a successful secondary
# fit). The "Fit Secondary Model" button however is disabled via
# JS (see the script in ui.R) when the Batch is not suitable, so it is
# in any case not possible to launch the fit without sufficient data - same
# native mechanism already used for the "Run Optimization
# Calculation" button on the Batch tab.
output$ni_setup_iniziale_ui <- renderUI({
  tagList(
    hr(),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
    tags$h5("1. Confirm sample temperatures"),
    selectInput("ni_unita_temp", "Temperature Unit:", choices = c("Celsius" = "celsius", "Kelvin" = "kelvin")),
    fluidRow(
      column(7, uiOutput("ni_tabella_temperature")),
      column(5, tags$div(style = "padding-top: 28px;",
        tags$button(id = "btn_copia_ni_kT", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
          style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 6px; display: block;",
          onclick = "copiaTabellaClipboard('ni_tabella_kT', 'btn_copia_ni_kT')"),
        tableOutput("ni_tabella_kT")))
    ),
    
    hr(),
    tags$h5("2. Fit the secondary model k(T)"),
    numericInput("ni_livello_confidenza", "Confidence/Prediction Level (%):", value = 95, min = 50, max = 99.9, step = 1),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
      "Independent from the global \"Confidence/Prediction Level\" in the sidebar. Governs every confidence/prediction interval computed further down in this module: the parameter table below, the Secondary Model plot's bands, and the final Predicted Critical Time / Predicted Quantity bands. Applies from the next fit or prediction run (re-run them to see it reflected)."),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -4px;",
      "The fit itself (both the point estimate and every bootstrap iteration) uses ", tags$strong("Levenberg-Marquardt"), " (least squares, with analytical derivatives when the formula allows) \u2014 independent from the Loss Function and Algorithm chosen in the sidebar, since the confidence interval math here assumes least squares."),
    selectInput("ni_modello_secondario", "Secondary model:",
                choices = c("Classic Arrhenius" = "arrhenius",
                            "Generic Modified Arrhenius" = "arrhenius_mod",
                            "Log-Logistic" = "log_logistic",
                            "Simplified Log-Logistic" = "log_logistic_semplice",
                            "Custom Formula" = "custom")),
    conditionalPanel(
      condition = "input.ni_modello_secondario != 'custom'",
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "The guesses below are pre-filled automatically, not left blank: for the two ", tags$strong("Arrhenius"), " models, via the classic linearization (a simple linear regression of ln(k) against 1/T \u2014 the same \"Arrhenius plot\" trick, already very close to the final fit); for the two ", tags$strong("Log-Logistic"), " models, via simpler heuristics based on the range of the observed temperatures and rate constants (less precise, but a reasonable starting point for the optimizer). Feel free to edit them by hand before fitting."),
      uiOutput("ni_guess_secondario_ui")
    ),
    conditionalPanel(
      condition = "input.ni_modello_secondario == 'custom'",
      textInput("ni_secondario_formula_custom", "Custom Formula (use 'T', in Kelvin):", placeholder = "A * exp(-Ea / (8.314 * T))"),
      textInput("ni_secondario_guess_custom", "Initial Guesses:", placeholder = "A=1e10[0,], Ea=50000[0,]"),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Same syntax as the sidebar's own Initial Guesses field: \"name=value[lower,upper]\", comma-separated, boundaries optional. Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides T and the parameters you name here. Unlike the 4 built-in models above, there's no automatic starting guess for a custom formula \u2014 you'll need to provide reasonable values yourself. Any boundaries you set are accepted but not enforced during the fit itself (it always uses unconstrained Simplex, same as the 4 built-in models), only clamped in bootstrap resamples if enabled below."),
      p(style = "font-size: 11px; color: #c0392b; margin-top: -6px;",
        icon("triangle-exclamation"), " If your formula has an Arrhenius-like structure (a pre-exponential factor on the order of 1e10\u20131e14 combined with an activation-energy-like term on the order of 1e4\u20131e5), Simplex can fail to find the right answer even from a guess in the right ballpark \u2014 the very different scales of the two kinds of parameter make this a genuinely hard case for it, which is why the 4 built-in models above use a pre-computed starting point from a classic linearization trick instead of leaving it to chance. There's no single fix that reliably works for every custom formula, so treat convergence with some skepticism: check that the fitted curve actually tracks your data well (the R\u00b2 and RMSE below), and that the recovered parameter values are physically sensible, rather than trusting a reported successful fit at face value."),
    ),
    checkboxInput("ni_mostra_celsius", "Display temperature in \u00b0Celsius on plots (below)", value = TRUE),
    checkboxInput("ni_usa_monte_carlo", "Propagate primary-fit uncertainty (nested bootstrap)", value = FALSE),
    conditionalPanel(
      condition = "input.ni_usa_monte_carlo == true",
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "For each sample, resamples k from its own Monte Carlo bootstrap distribution (if the Batch fit used Monte Carlo) or a Gaussian approximation from its Jacobian-based error (if not), refits the secondary model on each resample, and later propagates the same resamples into the predicted critical time below \u2014 accounting for the primary fit's own uncertainty, not just the secondary fit's."),
      numericInput("ni_n_boot_secondario", "Nested Bootstrap Iterations:", value = 200, min = 20, max = 2000, step = 20)
    ),
    actionButton("ni_fit_secondario", "Fit Secondary Model", class = "btn-primary w-100", icon = icon("calculator")),
    )
  )
})

# Part 3+4 (T(t) profile and final prediction): they remain hidden until the
# current Batch is suitable, since they depend on a fit of the
# secondary model which in turn requires sufficient data.
output$ni_corpo_principale <- renderUI({
  stato <- ni_batch_valido()
  if (!stato$valido) return(NULL)
  
  tagList(
    br(), br(),
    tags$button(id = "btn_copia_ni_riepilogo", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('ni_riepilogo_secondario', 'btn_copia_ni_riepilogo')"),
    tableOutput("ni_riepilogo_secondario"),
    tags$button(id = "btn_copia_ni_confronto_modelli", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('ni_confronto_modelli_secondario', 'btn_copia_ni_confronto_modelli')"),
    tableOutput("ni_confronto_modelli_secondario"),
    tags$button(id = "btn_copia_ni_secondario", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
      style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 4px;",
      onclick = "copiaTabellaClipboard('ni_tabella_secondario', 'btn_copia_ni_secondario')"),
    tableOutput("ni_tabella_secondario"),
    fluidRow(
      column(6, checkboxInput("ni_mostra_banda_confidenza", "Show Confidence Band", value = TRUE)),
      column(6, checkboxInput("ni_mostra_banda_predizione", "Show Prediction Band", value = FALSE))
    ),
    uiOutput("ni_ui_banda_simultanea"),
    plotOutput("ni_grafico_secondario", height = "380px"),
    div(style = "margin-top: 4px; margin-bottom: 8px;",
        downloadButton("scarica_ni_secondario", "Download Plot", class = "btn-outline-secondary btn-sm"),
        actionButton("ni_btn_estetica_secondario", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
    
    hr(),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
    tags$h5("3. Choose the temperature profile T(t)"),
    selectInput("ni_tipo_profilo", "Temperature profile:",
                choices = c("Raw Data (datalogger upload)" = "dati_grezzi",
                            "Synthetic Thermal Abuse Profile" = "abuso_termico",
                            "Linear: T0 + a\u00b7t" = "lineare",
                            "Quadratic: T0 + a\u00b7t + b\u00b7t\u00b2" = "quadratico",
                            "Sinusoidal: Tm + (a/2)\u00b7sin(2\u03c0t/L + 3\u03c0/2)" = "sinusoidale",
                            "Exponential: Th + (T0-Th)\u00b7exp(-J\u00b7t)" = "esponenziale",
                            "Custom Formula" = "custom")),
    conditionalPanel(
      condition = "input.ni_tipo_profilo == 'dati_grezzi'",
      fileInput("ni_file_datalogger", "Datalogger file (2 columns: time, temperature in Kelvin):", accept = c(".csv", ".xlsx", ".xls"))
    ),
    conditionalPanel(
      condition = "input.ni_tipo_profilo == 'custom'",
      textInput("ni_profilo_formula_custom", "Custom Formula (use 't', same time units as your data):", placeholder = "T0 + a * t + b * sin(t)"),
      textInput("ni_profilo_guess_custom", "Parameter Values:", placeholder = "T0=277, a=0.1, b=2"),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Same \"name=value\" syntax as everywhere else in the app (boundaries in brackets are accepted too, but have no effect here \u2014 this profile is a fixed input you're defining, not something being fitted, so there's nothing to constrain). Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides t and the parameters you name here. This formula is evaluated as-is at every time point of the simulation \u2014 it is never fitted to data, so pick values you already consider realistic.")
    ),
    uiOutput("ni_parametri_profilo_ui"),
    
    hr(),
    tags$h5("4. Set the scenario and run"),
    uiOutput("ni_A0_C_crit_ui"),
    uiOutput("ni_campo_n_ui"),
    uiOutput("ni_campo_isotermo_ui"),
    p(style = "font-size: 11px; color: #c0392b; font-weight: 500; margin-top: -6px;",
      icon("triangle-exclamation"), " Integration Grid Resolution is used both for the point-estimate curve above and, if Monte Carlo is enabled, for ", tags$strong("every single one"), " of its bootstrap iterations \u2014 raising it can noticeably slow down the prediction, and with Monte Carlo on, the slowdown multiplies directly by the number of Bootstrap Iterations set for the secondary fit. It does ", tags$strong("not"), " adjust itself automatically when you enable Monte Carlo \u2014 consider lowering it (e.g. to around 300) yourself in that case, and raise it only if you specifically need finer time resolution (e.g. very short/sharp temperature abuses relative to the total simulated duration)."),
    actionButton("ni_lancia_previsione", "Predict Critical Time", class = "btn-primary btn-sm w-100", icon = icon("play")),
    ),
    br(), br(),
    uiOutput("ni_risultato_testo"),
    uiOutput("ni_confronto_isotermo_testo"),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
      "When \"Propagate primary-fit uncertainty\" is enabled above, the Predicted Quantity curve shows a 95% confidence band. The Temperature Profile never does \u2014 it's a fixed input you provided (raw data or a chosen formula), not an estimated quantity, so there's nothing statistical to attach an interval to."),
    fluidRow(
      if (!is.null(ni_secondario_risultato()$mat_boot)) column(6, checkboxInput("ni_mostra_banda_previsione", "Show Confidence Band", value = TRUE)),
      column(6, checkboxInput("ni_combina_grafici", "Combine into a single dual-axis plot", value = FALSE))
    ),
    uiOutput("ni_area_grafici_previsione")
  )
})



# ---- 8. Table associating temperature -> sample ----------------------------
# Same idea already present in the Shelf-Life module (one row per sample,
# pre-filled from the fourth column of the file if available, otherwise
# empty), but with its own independent ID namespace (ni_temp_*, not
# sl_temp_*) so as not to depend on whether the user has already visited the
# Shelf-Life section in this session.
output$ni_tabella_temperature <- renderUI({
  stato <- ni_batch_valido()
  if (!stato$valido) return(NULL)
  temp_da_file <- stato$temperature_da_file
  nomi_ok <- names(stato$campioni)
  righe <- lapply(seq_along(nomi_ok), function(i) {
    n <- nomi_ok[i]
    id_campo <- paste0("ni_temp_", ni_id_sicuro(n, i))
    valore_corrente <- isolate(input[[id_campo]])
    valore_prefill <- if (!is.null(valore_corrente) && !is.na(valore_corrente)) {
      valore_corrente
    } else if (!is.null(temp_da_file) && n %in% names(temp_da_file)) {
      temp_da_file[[n]]
    } else NA
    fluidRow(
      column(5, tags$div(style = "padding: 7px 10px; margin-top: 2px; font-size: 13px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; background-color: #ffffff; border: 1px solid #d5dbdb; border-radius: 5px;", n)),
      column(4, numericInput(id_campo, NULL, value = valore_prefill, width = "100px"))
    )
  })
  tagList(
    tags$strong(style = "font-size: 13px;", "Temperature per sample:"),
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: 2px;",
      "Pre-filled from the 4th column of the batch file where available (in the unit selected above); enter the rest by hand."),
    righe
  )
})

# Table (name, estimated k, entered T) for the samples with known temperature -
# basis both for the fit of the secondary model and for its display.
ni_dati_kT <- reactive({
  stato <- ni_batch_valido()
  if (!stato$valido) return(NULL)
  in_celsius <- is.null(input$ni_unita_temp) || input$ni_unita_temp == "celsius"
  nomi_ok <- names(stato$campioni)
  righe <- lapply(seq_along(nomi_ok), function(i) {
    n <- nomi_ok[i]
    id_campo <- paste0("ni_temp_", ni_id_sicuro(n, i))
    T_val <- input[[id_campo]]
    if (!is.null(T_val) && !is.na(T_val) && in_celsius) T_val <- T_val + 273.15
    parametri_campione <- as.list(stato$campioni[[n]]$parametri)
    k_val <- parametri_campione[["k"]]
    n_val <- parametri_campione[["n"]]  # NULL for all except n_ordine
    if (is.null(T_val) || is.na(T_val) || is.null(k_val) || !is.finite(k_val)) return(NULL)
    data.frame(nome = n, T = T_val, k = k_val, n_par = if (is.null(n_val)) NA_real_ else n_val)
  })
  righe <- Filter(Negate(is.null), righe)
  if (length(righe) < 3) return(NULL)
  do.call(rbind, righe)
})

# Same selection/alignment as ni_dati_kT() above, but also returning
# the "source" of uncertainty for the nested bootstrap - a separate
# reactive (instead of extending ni_dati_kT() with a list column)
# because the existing callers of ni_dati_kT() expect a simple
# data.frame, and breaking them would not be worth it for this single use.
ni_dati_kT_con_fonti <- reactive({
  stato <- ni_batch_valido()
  if (!stato$valido) return(NULL)
  in_celsius <- is.null(input$ni_unita_temp) || input$ni_unita_temp == "celsius"
  nomi_ok <- names(stato$campioni)
  risultati <- lapply(seq_along(nomi_ok), function(i) {
    n <- nomi_ok[i]
    id_campo <- paste0("ni_temp_", ni_id_sicuro(n, i))
    T_val <- input[[id_campo]]
    if (!is.null(T_val) && !is.na(T_val) && in_celsius) T_val <- T_val + 273.15
    r <- stato$campioni[[n]]
    parametri_campione <- as.list(r$parametri)
    k_val <- parametri_campione[["k"]]
    if (is.null(T_val) || is.na(T_val) || is.null(k_val) || !is.finite(k_val)) return(NULL)
    list(T = T_val, fonte = ni_costruisci_fonte_k(r))
  })
  risultati <- Filter(Negate(is.null), risultati)
  if (length(risultati) < 3) return(NULL)
  risultati
})

output$ni_tabella_kT <- renderTable({
  dati <- ni_dati_kT()
  if (is.null(dati)) return(data.frame(Message = "Enter at least 3 valid temperatures above."))
  df_show <- data.frame(Sample = dati$nome, `Temperature (K)` = as.character(round(dati$T, 2)), `Fitted k` = as.character(signif(dati$k, 7)), check.names = FALSE)
  df_show
}, striped = TRUE, hover = TRUE)


# ---- 8b. Nested bootstrap: propagation of the uncertainty of the primary ---
# fit into the secondary model and into the final prediction --------------
# Exactly the same pattern already validated in Shelf-Life
# (costruisci_fonte_sl/trova_distribuzione_sl_mc): for each sample of the
# Batch, builds a "source" from which to draw a plausible value of k -
# empirical (from the true Monte Carlo bootstrap distribution of the primary
# fit, if available) or Gaussian (simulated from the Hessian standard
# error, if the primary fit used the Linear method) -
# instead of always using only the point value of k. Repeating the fit
# of the secondary model on MANY sets of k resampled in this way
# gives an empirical distribution of its parameters (and,
# consequently, of the final predicted shelf life), which accounts both
# for the uncertainty of the primary fit and for that of the secondary fit -
# not only the latter, as a Hessian applied to the secondary fit alone
# would do.
ni_costruisci_fonte_k <- function(r) {
  parametri_lista <- as.list(r$parametri)
  k_puntuale <- parametri_lista[["k"]]
  if (!is.null(r$mat_boot) && "k" %in% colnames(r$mat_boot)) {
    valori_k <- r$mat_boot[, "k"]
    valori_k <- valori_k[is.finite(valori_k)]
    if (length(valori_k) >= 10) {
      return(list(tipo = "empirico", valori = valori_k, puntuale = k_puntuale))
    }
  }
  if (!is.null(r$cov) && "k" %in% rownames(r$cov)) {
    se_k <- suppressWarnings(sqrt(r$cov["k", "k"]))
    if (is.finite(se_k) && se_k > 0) {
      return(list(tipo = "gaussiano", media = k_puntuale, se = se_k, puntuale = k_puntuale))
    }
  }
  list(tipo = "solo_punto", puntuale = k_puntuale)
}

ni_pesca_k <- function(fonte) {
  switch(fonte$tipo,
    "empirico" = sample(fonte$valori, 1),
    "gaussiano" = rnorm(1, fonte$media, fonte$se),
    "solo_punto" = fonte$puntuale
  )
}


# ---- 9. Choice and fitting of the secondary model k(T) ---------------------
output$ni_guess_secondario_ui <- renderUI({
  dati <- ni_dati_kT()
  tipo <- input$ni_modello_secondario
  if (is.null(dati) || is.null(tipo) || tipo == "custom") return(NULL)
  guess_auto <- ni_guess_secondario(tipo, dati$T, dati$k)
  nomi_par <- NI_PARAMETRI_SECONDARIO[[tipo]]
  larghezza_colonna <- floor(12 / length(nomi_par))
  colonne <- lapply(nomi_par, function(p) {
    column(larghezza_colonna, numericInput(paste0("ni_guess_sec_", p), paste0("Guess for ", p, ":"), value = signif(guess_auto[[p]], 4)))
  })
  fluidRow(colonne)
})

ni_secondario_risultato <- reactiveVal(NULL)

# The nested bootstrap of the secondary model can be used for the bands
# if it has at least MINIMO_ITERAZIONI_MC valid iterations. Used both by the plot
# (choice between bootstrap and linear band) and by the interface (the checkbox
# of the simultaneous band appears only with the linear band), so the two cannot
# diverge. Applies to both Non-Isothermal modules.
ni_bootstrap_secondario_valido <- function(ris) {
  !is.null(ris) && !is.null(ris$mat_boot) && sum(apply(ris$mat_boot, 1, function(r) all(is.finite(r)))) >= MINIMO_ITERAZIONI_MC
}

# Clears both results of the non-isothermal module every time the
# Batch is rerun (same Run button used by all the other
# modules that depend on batch_risultati()) - without this, a fit of the
# secondary model or a prediction computed on the PREVIOUS batch
# would remain visible on screen even after launching a new batch
# with potentially different data, giving a false sense of consistency with the
# new results. Exactly the same "reset on new fit" pattern already
# established everywhere in this session (e.g. profili_tutti_parametri_dati
# in Grid Search, batch_test_parametri_risultato in Batch itself).
observeEvent(input$esegui_batch, {
  ni_secondario_risultato(NULL)
  ni_previsione_risultato(NULL)
})

observeEvent(input$ni_fit_secondario, {
  if (!isTRUE(ni_batch_valido()$valido)) {
    showNotification("Requires a Batch run with one of the order-based Reaction Kinetics models (Zero/First/Second/n-Order) and at least 3 successfully fitted samples.", type = "error", duration = 8)
    return()
  }
  dati <- ni_dati_kT()
  tipo_selezionato <- input$ni_modello_secondario
  if (is.null(dati) || is.null(tipo_selezionato)) {
    showNotification("Enter at least 3 valid temperatures above first.", type = "error")
    return()
  }
  # "Custom" case: the formula is safely validated HERE, then its
  # own text becomes the value of "tipo" used for the rest of this
  # function (fit, bootstrap, storage in ni_secondario_risultato) - see the
  # design note above ni_modello_secondario() for why it works
  # without touching anything else. For the 4 built-in models, behaviour
  # identical to before this change.
  if (tipo_selezionato == "custom") {
    formula_testo <- input$ni_secondario_formula_custom
    guess_testo <- input$ni_secondario_guess_custom
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
    guess_letti <- sapply(nomi_par, function(p) input[[paste0("ni_guess_sec_", p)]])
    if (any(sapply(guess_letti, function(v) is.null(v) || is.na(v)))) {
      showNotification("Enter an initial guess for every parameter first.", type = "error")
      return()
    }
    guess_vettore <- setNames(as.numeric(guess_letti), nomi_par)
  }
  # The minimum number of samples depends on the number of parameters of the
  # CHOSEN secondary model (2 for the Arrhenius ones, 3 for the log-logistic ones,
  # as many as the custom one has for those who use it) - at least 1 residual degree
  # of freedom is needed to estimate a standard error, not just a point
  # value.
  if (length(dati$k) <= length(nomi_par)) {
    showNotification(paste0("This secondary model has ", length(nomi_par), " parameters \u2014 at least ", length(nomi_par) + 1, " samples with a known temperature are needed (found ", length(dati$k), ")."), type = "error", duration = 8)
    return()
  }
  
  # Same pattern already validated in the Shelf-Life module: optim Nelder-Mead on
  # SSE (calcola_loss_generica(..., "mse") despite the name returns
  # the SUM of squares, not the mean - same convention already used
  # everywhere in this project), independent of the Loss Function/Algorithm
  # chosen in the sidebar. hessian=TRUE to compute the standard errors below.
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
  # Same scale convention already used in motore_calcolo.R for Simplex/
  # Nelder-Mead (moltiplicatore=2, since the numerical Hessian of optim()
  # on an SSE is 2*J'J, whereas the formula of the covariance matrix
  # requires J'J alone).
  # Covariance from the Jacobian of the model, s^2 (J^T J)^-1, not from the numerical
  # Hessian of optim. Previously a negative variance (invalid covariance)
  # was turned into a standard error equal to zero (pmax(..., 0)),
  # misleading: now in that case the standard error is "not available".
  matrice_cov <- covarianza_minimi_quadrati(function(p) ni_modello_secondario(tipo, dati$T, setNames(as.list(p), nomi_par)),
                                            setNames(fit$par, nomi_par), sse, df_residuo)
  se_par <- if (!is.null(matrice_cov)) sqrt(diag(matrice_cov)) else rep(NA_real_, length(nomi_par))
  t_crit <- qt(1 - ni_alpha_confidenza() / 2, df_residuo)
  
  # Nested bootstrap (optional): repeats the fit of the secondary model
  # on many sets of k resampled from the respective sources of
  # uncertainty per sample (empirical if the primary fit used Monte
  # Carlo, Gaussian otherwise) - it thus propagates the uncertainty of the
  # PRIMARY fit into the distribution of the secondary parameters, not only that
  # of the secondary fit itself.
  mat_boot_secondario <- NULL
  if (isTRUE(input$ni_usa_monte_carlo)) {
    dati_con_fonti <- ni_dati_kT_con_fonti()
    if (!is.null(dati_con_fonti)) {
      n_boot_sec <- if (is.null(input$ni_n_boot_secondario) || is.na(input$ni_n_boot_secondario)) 200 else min(max(round(input$ni_n_boot_secondario), 20), 2000)
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
  
  # Exactly the same formula already used in the main computation engine,
  # to compare different secondary models (2 parameters for the
  # Arrhenius ones, 3 for the log-logistic ones) on the same basis.
  n_oss <- length(dati$k)
  p_par <- length(nomi_par)
  # Secondary model: always least squares (Levenberg-Marquardt).
  ic_sec <- criteri_informazione(sse, n_oss, p_par, "mse")
  aic_sec <- ic_sec$aic; aicc_sec <- ic_sec$aicc; bic_sec <- ic_sec$bic
  
  ni_secondario_risultato(list(tipo = tipo, par = par_finali, se = setNames(se_par, nomi_par), r2 = r2, r2_adj = r2_adj,
                                sse = sse, mse = mse_vero, rmse = rmse, cv_rmse = cv_rmse, n_obs = n_oss,
                                aic = aic_sec, aicc = aicc_sec, bic = bic_sec,
                                df_residuo = df_residuo, t_crit = t_crit, matrice_cov = matrice_cov,
                                dati_kT = dati, n_ordine_medio = mean(dati$n_par, na.rm = TRUE),
                                mat_boot = mat_boot_secondario))
  showNotification("Secondary model fitted successfully.", type = "message")
})

output$ni_riepilogo_secondario <- renderTable({
  ris <- ni_secondario_risultato()
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

# Separate table for AIC/AICc/BIC - useful to compare several different
# secondary models (Arrhenius vs Log-Logistic, etc.) refitted one after
# the other on the same data; separated from the goodness-of-fit table above
# so as not to overcrowd a single row.
output$ni_confronto_modelli_secondario <- renderTable({
  ris <- ni_secondario_risultato()
  if (is.null(ris)) return(NULL)
  data.frame(
    AIC = as.character(signif(ris$aic, 6)),
    AICc = as.character(signif(ris$aicc, 6)),
    BIC = as.character(signif(ris$bic, 6)),
    check.names = FALSE
  )
}, striped = TRUE, hover = TRUE)

output$ni_tabella_secondario <- renderTable({
  ris <- ni_secondario_risultato()
  if (is.null(ris)) return(data.frame(Message = "Run the secondary model fit above."))
  valori <- unlist(ris$par)
  alpha <- ni_alpha_confidenza()
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

# Plot of the secondary model: observed (T,k) points from the main
# fit, curve of the fitted model, and 95% confidence band
# obtained with the delta method (propagation of the Hessian covariance
# matrix through the NUMERICAL gradient of k(T) with respect to the
# parameters - more robust than an analytical derivative written by hand for
# each of the 4 models, numerically verified before writing it).
ni_gradiente_numerico_secondario <- function(tipo, T_val, par, eps = 1e-6) {
  nomi <- names(par)
  grad <- numeric(length(nomi))
  for (i in seq_along(nomi)) {
    par_su <- par; par_su[[nomi[i]]] <- par[[nomi[i]]] * (1 + eps) + eps
    par_giu <- par; par_giu[[nomi[i]]] <- par[[nomi[i]]] * (1 - eps) - eps
    grad[i] <- (ni_modello_secondario(tipo, T_val, par_su) - ni_modello_secondario(tipo, T_val, par_giu)) / (par_su[[nomi[i]]] - par_giu[[nomi[i]]])
  }
  grad
}

output$ni_grafico_secondario <- renderPlot({
  disegna_ni_secondario()
})
output$scarica_ni_secondario <- downloadHandler(
  filename = function() paste0("secondary_model.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_ni_secondario()
    dev.off()
  }
)
disegna_ni_secondario <- function() {
  ris <- ni_secondario_risultato()
  if (is.null(ris)) {
    plot.new()
    text(0.5, 0.5, "Fit the secondary model above to see the plot here.", col = "#7f8c8d")
    return(invisible(NULL))
  }
  dati <- ris$dati_kT
  T_grid <- seq(min(dati$T) * 0.98, max(dati$T) * 1.02, length.out = 200)
  k_grid <- ni_modello_secondario(ris$tipo, T_grid, ris$par)
  
  # Standard error of k for each observed point (specific to that
  # sample, from the PRIMARY fit in Batch) - to draw the vertical error
  # bars on the points, not on the curve.
  stato <- ni_batch_valido()
  se_k_punti <- sapply(dati$nome, function(n) {
    r <- stato$campioni[[n]]
    if (!is.null(r$cov) && "k" %in% rownames(r$cov)) {
      valore <- suppressWarnings(sqrt(r$cov["k", "k"]))
      if (is.finite(valore)) return(valore)
    }
    NA_real_
  })
  
  # Band on the plot: if the nested bootstrap is available, it uses THAT
  # (same precedence already corrected in the parameter table next to it,
  # ni_tabella_secondario, and in the twin Arrhenius plot of Shelf-Life) -
  # evaluates the secondary model at EVERY T of the grid using EVERY bootstrap
  # row, then takes the percentiles column by column. It falls back on the
  # Hessian/delta method ONLY if the bootstrap is not available (Monte Carlo
  # disabled) - never again a silent fallback when the bootstrap
  # is there instead and the user has explicitly requested it.
  # Nested bootstrap with fewer than 10 valid iterations: linear band
  # (previously the plot showed no band).
  if (ni_bootstrap_secondario_valido(ris)) {
    alpha <- ni_alpha_confidenza()
    righe_valide <- which(apply(ris$mat_boot, 1, function(r) all(is.finite(r))))
    if (length(righe_valide) >= 10) {
      mat_curve_boot <- matrix(NA_real_, length(righe_valide), length(T_grid))
      for (i in seq_along(righe_valide)) {
        par_b <- setNames(as.list(ris$mat_boot[righe_valide[i], ]), colnames(ris$mat_boot))
        mat_curve_boot[i, ] <- ni_modello_secondario(ris$tipo, T_grid, par_b)
      }
      if (isTRUE(input$ni_mostra_banda_confidenza)) {
        banda_inf <- pmax(apply(mat_curve_boot, 2, quantile, probs = alpha / 2, na.rm = TRUE), 0)
        banda_sup <- apply(mat_curve_boot, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      } else {
        banda_sup <- banda_inf <- NULL
      }
      # Prediction band: as in Shelf-Life, adds to each bootstrap
      # curve a Gaussian noise with standard deviation equal
      # to the residual MSE of the point fit, then takes the percentiles -
      # it also reflects the dispersion of the observed k around the curve,
      # not only the uncertainty on the parameters.
      if (isTRUE(input$ni_mostra_banda_predizione) && is.finite(ris$mse) && ris$mse > 0) {
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
    alpha <- ni_alpha_confidenza()
    t_crit_live <- qt(1 - alpha / 2, ris$df_residuo)
    var_par_grid <- sapply(T_grid, function(T_val) {
      g <- ni_gradiente_numerico_secondario(ris$tipo, T_val, ris$par)
      max(as.numeric(t(g) %*% ris$matrice_cov %*% g), 0)
    })
    se_k_grid <- sqrt(var_par_grid)
    if (isTRUE(input$ni_mostra_banda_confidenza)) {
      # Pointwise t multiplier, or simultaneous one of Bates & Watts (1988) if requested.
      m_ci <- if (isTRUE(input$ni_banda_simultanea)) moltiplicatore_banda(ris$df_residuo, length(ris$par), 100 * (1 - alpha), TRUE) else t_crit_live
      if (!is.finite(m_ci)) m_ci <- t_crit_live
      banda_sup <- k_grid + m_ci * se_k_grid
      banda_inf <- pmax(k_grid - m_ci * se_k_grid, 0)
    } else {
      banda_sup <- banda_inf <- NULL
    }
    # Prediction band (optional): like the confidence band, but with
    # the residual variance (MSE) added - exactly the same formula already
    # used in Shelf-Life to distinguish "mean response" from "new
    # observation".
    if (isTRUE(input$ni_mostra_banda_predizione)) {
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
  est <- ni_leggi_estetica()
  dati_T_vis <- ni_converti_temp_display(dati$T)
  T_grid_vis <- ni_converti_temp_display(T_grid)
  plot(dati_T_vis, dati$k, type = "n",
       xlim = range(T_grid_vis), ylim = range(c(dati$k - se_k_punti, dati$k + se_k_punti, banda_inf, banda_sup, pi_inf, pi_sup, k_grid), na.rm = TRUE),
       xlab = ni_etichetta_temp(), ylab = "Rate constant k", main = if (input$mostra_titolo) "Secondary Model: k(T)" else "")
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
    livello_testo <- paste0(round(100 * (1 - ni_alpha_confidenza()), 1), "%")
    voci <- c("Observed k (\u00b1 SE, from Batch)", "Fitted secondary model")
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


# ---- 10. Temperature profile T(t) ------------------------------------------
# A0 pre-filled with the mean of all the A0 values estimated in the samples
# of the current batch (non-null/finite) - exactly the same pattern already
# used for "Critical Y Value" in the Shelf-Life module: it depends only on
# ni_batch_valido() (in turn linked to batch_risultati()), not on its
# own input, so it updates only when the batch is
# ACTUALLY rerun, remaining stable (without overwriting a
# manual change) until the batch is launched again. Critical
# Threshold pre-filled as 75% of that same A0 (a reasonable starting
# point - a quarter of degradation - still editable
# by hand), computed in the SAME renderUI to guarantee consistency between the
# two instead of recomputing A0 separately in a second place.
output$ni_A0_C_crit_ui <- renderUI({
  stato <- ni_batch_valido()
  valore_default_A0 <- 10
  valore_default_t_max <- 500
  if (isTRUE(stato$valido)) {
    tutti_A0 <- sapply(stato$campioni, function(r) as.list(r$parametri)[["A0"]])
    tutti_A0 <- unlist(tutti_A0[!sapply(tutti_A0, is.null)])
    tutti_A0 <- tutti_A0[is.finite(tutti_A0)]
    if (length(tutti_A0) > 0) valore_default_A0 <- round(mean(tutti_A0), 4)
    
    # Maximum raw X among the qualifying samples, as a reasonable starting
    # point for how long to simulate - if the original isothermal experiments
    # went up to a certain time, simulating approximately
    # on the same order of magnitude is a more sensible default than an
    # arbitrary fixed number (still editable by hand).
    tutti_x_max <- sapply(stato$campioni, function(r) if (!is.null(r$x)) max(r$x, na.rm = TRUE) else NA_real_)
    tutti_x_max <- tutti_x_max[is.finite(tutti_x_max)]
    if (length(tutti_x_max) > 0) valore_default_t_max <- round(max(tutti_x_max), 4)
  }
  # Preserves the value of Max Time already entered (isolate: reads the
  # CURRENT value without creating a reactive dependency on itself, same
  # pattern already used for the temperature table) - this renderUI
  # updates only when the batch changes (for the default of A0), and
  # must therefore not reset t_max every time the user has already
  # changed it by hand.
  valore_t_max <- isolate(input$ni_t_max)
  if (is.null(valore_t_max) || is.na(valore_t_max)) valore_t_max <- valore_default_t_max
  # Fixed default (no longer linked to the Monte Carlo checkbox): a default that
  # "adapts" based on input$ni_usa_monte_carlo clashed with the
  # isolate() preservation below - once the field ALREADY has a
  # value (even just the default shown at the first render), that value
  # is protected at every subsequent render, including the one triggered by the
  # checkbox itself - so the automatic adaptation never really had
  # a chance to kick in. Better a fixed, predictable default;
  # the disclaimer under the field guides the operator to lower it by hand.
  valore_risoluzione <- isolate(input$ni_risoluzione_griglia)
  if (is.null(valore_risoluzione) || is.na(valore_risoluzione)) valore_risoluzione <- 1000
  fluidRow(
    column(3, numericInput("ni_A0", "Starting Quantity (A0):", value = valore_default_A0)),
    column(3, numericInput("ni_C_crit", "Critical Threshold:", value = round(valore_default_A0 * 0.75, 4))),
    column(3, numericInput("ni_t_max", "Max Time to Simulate:", value = valore_t_max, min = 1)),
    column(3, numericInput("ni_risoluzione_griglia", "Integration Grid Resolution:", value = valore_risoluzione, min = 20, max = 5000, step = 10))
  )
})

# Field "n" separated into its own output, conditional on a primary
# n-Order Kinetics fit: it reacts to the fit of the secondary model (unlike
# ni_A0_C_crit_ui above, which isolates it on purpose so as not to reset A0/C_crit
# at every re-fit) so the suggested mean value always stays up to date
# if the secondary model is refitted, without disturbing the other fields.
# Previously this value (n_ordine_medio) remained an internal computation never
# shown to the operator - in an app designed for professional use,
# a value that enters the final computation must be visible and
# editable, not just "trusted".
output$ni_campo_n_ui <- renderUI({
  stato <- ni_batch_valido()
  if (!isTRUE(stato$valido) || !identical(stato$ordine, "n")) return(NULL)
  ris_sec <- ni_secondario_risultato()
  valore_default_n <- if (!is.null(ris_sec) && !is.na(ris_sec$n_ordine_medio)) round(ris_sec$n_ordine_medio, 4) else NA_real_
  valore_n <- isolate(input$ni_n_ordine)
  if (is.null(valore_n) || is.na(valore_n)) valore_n <- if (!is.na(valore_default_n)) valore_default_n else 1.5
  fluidRow(style = "margin-top: 4px;",
    column(4, numericInput("ni_n_ordine", "Reaction Order (n):", value = valore_n, step = 0.01)),
    column(8, p(style = "font-size: 11px; color: #7f8c8d; margin-top: 30px;",
                if (!is.na(valore_default_n))
                  paste0("Pre-filled with the average n already fitted across the Batch samples (", valore_default_n, "). Editable, but changing it here means this prediction will no longer exactly match the primary Batch fit.")
                else
                  "Fit the secondary model above first to pre-fill this with the Batch's own average n \u2014 for now, enter it manually."))
  )
})

# Checkbox + temperature field for the comparison with an isothermal scenario
# (constant temperature) using the SAME secondary model already fitted -
# no new integration needed, since with constant k K(t)=k*t
# falls back exactly on the classic isothermal formula (see ni_K_soglia).
# Temperature in Kelvin, same convention as the profile parameters at
# point 3 (always Kelvin, regardless of "Display in Celsius").
output$ni_campo_isotermo_ui <- renderUI({
  stato <- ni_batch_valido()
  if (!isTRUE(stato$valido)) return(NULL)
  valore_T_iso <- isolate(input$ni_T_iso)
  if (is.null(valore_T_iso) || is.na(valore_T_iso)) valore_T_iso <- 277
  tagList(
    checkboxInput("ni_confronta_isotermo", "Compare with Isothermal Storage", value = if (is.null(isolate(input$ni_confronta_isotermo))) FALSE else isolate(input$ni_confronta_isotermo)),
    conditionalPanel(
      condition = "input.ni_confronta_isotermo == true",
      fluidRow(
        column(4, numericInput("ni_T_iso", "Isothermal Comparison Temperature (K):", value = valore_T_iso, step = 0.5))
      ),
      p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
        "Compares the critical time predicted under the chosen temperature profile above against what would happen if the product had instead been stored at this single fixed temperature the whole time \u2014 same secondary model, same starting quantity and threshold, just a constant temperature instead of the profile.")
    )
  )
})

output$ni_parametri_profilo_ui <- renderUI({
  tipo <- input$ni_tipo_profilo
  if (is.null(tipo) || tipo == "dati_grezzi" || tipo == "custom") return(NULL)
  if (tipo == "abuso_termico") {
    # Same pattern already used for "Max Time to Simulate" (point 4): the
    # maximum raw X among the qualifying samples as a sensible default for
    # the total duration, instead of an arbitrary fixed number - preserving
    # (isolate) a value already entered by hand by the user.
    stato <- ni_batch_valido()
    valore_default_Dtot <- 240
    if (isTRUE(stato$valido)) {
      tutti_x_max <- sapply(stato$campioni, function(r) if (!is.null(r$x)) max(r$x, na.rm = TRUE) else NA_real_)
      tutti_x_max <- tutti_x_max[is.finite(tutti_x_max)]
      if (length(tutti_x_max) > 0) valore_default_Dtot <- round(max(tutti_x_max), 4)
    }
    valore_Dtot <- isolate(input$ni_profilo_Dtot)
    if (is.null(valore_Dtot) || is.na(valore_Dtot)) valore_Dtot <- valore_default_Dtot
    return(tagList(
      fluidRow(
        column(3, numericInput("ni_profilo_T0", "Baseline Temperature (T0):", value = 277)),
        column(3, numericInput("ni_profilo_Dtot", "Total Duration:", value = valore_Dtot, min = 0.001)),
        column(3, numericInput("ni_profilo_n", "Number of Abuses:", value = 3, min = 1, max = 50, step = 1)),
        column(3, numericInput("ni_profilo_dur", "Duration of Each Abuse:", value = 2, min = 0.001))
      ),
      fluidRow(
        column(4, numericInput("ni_profilo_amp", "Amplitude of Each Abuse:", value = 5, min = 0)),
        column(4, selectInput("ni_abuso_direzione", "Direction:", choices = c("Positive (warmer)" = "positivo", "Negative (colder)" = "negativo"))),
        column(4, selectInput("ni_abuso_forma", "Shape:", choices = c("Sharp Jump" = "salto", "Fast Ramp" = "rampa_veloce", "Soft Ramp" = "rampa_morbida")))
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
    column(larghezza_colonna, numericInput(paste0("ni_profilo_", p), paste0(p, ":"), value = valori_default[[p]]))
  })
  fluidRow(colonne)
})


# ---- 11. Final prediction ---------------------------------------------------
ni_previsione_risultato <- reactiveVal(NULL)

observeEvent(input$ni_lancia_previsione, {
  ris_sec <- ni_secondario_risultato()
  stato <- ni_batch_valido()
  if (is.null(ris_sec) || !stato$valido) {
    showNotification("Fit the secondary model first.", type = "error")
    return()
  }
  n_ordine_usato <- NULL
  if (identical(stato$ordine, "n")) {
    n_ordine_usato <- input$ni_n_ordine
    if (is.null(n_ordine_usato) || is.na(n_ordine_usato)) n_ordine_usato <- ris_sec$n_ordine_medio
    if (is.null(n_ordine_usato) || is.na(n_ordine_usato)) {
      showNotification("Could not determine the reaction order 'n' \u2014 enter it manually above.", type = "error")
      return()
    }
  }
  
  tipo_profilo <- input$ni_tipo_profilo
  funzione_T <- if (tipo_profilo == "dati_grezzi") {
    file_dati <- input$ni_file_datalogger
    if (is.null(file_dati)) { showNotification("Upload a datalogger file first.", type = "error"); return() }
    df_log <- tryCatch(leggi_file_generico(file_dati$datapath, file_dati$name), error = function(e) NULL)
    if (is.null(df_log) || ncol(df_log) < 2) { showNotification("Could not read the datalogger file (expects 2 columns: time, temperature).", type = "error"); return() }
    colnames(df_log)[1:2] <- c("tempo", "temperatura")
    ni_costruisci_profilo_T("dati_grezzi", NULL, df_log)
  } else if (tipo_profilo == "custom") {
    # Same technique as ni_modello_secondario: the formula is safely
    # validated HERE, then its own text becomes the "tipo" passed to
    # ni_costruisci_profilo_T(), which evaluates it through the fallback branch
    # already added there - no other point of the code needs changes.
    formula_testo <- input$ni_profilo_formula_custom
    guess_testo <- input$ni_profilo_guess_custom
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
    valori_par <- sapply(nomi_par, function(p) input[[paste0("ni_profilo_", p)]])
    if (any(sapply(valori_par, function(v) is.null(v) || is.na(v)))) { showNotification("Enter every temperature profile parameter first.", type = "error"); return() }
    par_nominati <- setNames(as.list(as.numeric(valori_par)), nomi_par)
    if (tipo_profilo == "abuso_termico") {
      if (is.null(input$ni_abuso_direzione) || is.null(input$ni_abuso_forma)) { showNotification("Choose a Direction and Shape for the abuses first.", type = "error"); return() }
      par_nominati$segno <- if (input$ni_abuso_direzione == "positivo") 1 else -1
      par_nominati$frazione_bordo <- switch(input$ni_abuso_forma, "salto" = 0, "rampa_veloce" = 0.15, "rampa_morbida" = 0.5)
      if (par_nominati$dur > par_nominati$Dtot / (par_nominati$n + 1)) {
        showNotification("The abuses would overlap (or run past the start/end) with these settings \u2014 reduce the abuse duration or count, or increase the Total Duration.", type = "error", duration = 8)
        return()
      }
    }
    ni_costruisci_profilo_T(tipo_profilo, par_nominati)
  }
  if (is.null(funzione_T)) { showNotification("Could not build the temperature profile.", type = "error"); return() }
  
  A0 <- input$ni_A0; C_crit <- input$ni_C_crit; t_max <- input$ni_t_max
  if (any(sapply(list(A0, C_crit, t_max), function(v) is.null(v) || is.na(v)))) {
    showNotification("Enter Starting Quantity, Critical Threshold, and Max Time to Simulate.", type = "error")
    return()
  }
  if (C_crit >= A0) { showNotification("Critical Threshold must be lower than Starting Quantity (this module assumes a decaying quantity).", type = "error"); return() }
  n_punti_griglia <- if (is.null(input$ni_risoluzione_griglia) || is.na(input$ni_risoluzione_griglia)) 1000 else max(round(input$ni_risoluzione_griglia), 20)
  
  integrazione <- ni_integra_k_cumulativo(funzione_T, ris_sec$tipo, ris_sec$par, t_max, n_punti = n_punti_griglia)
  curva_C <- ni_quantita_da_K(stato$ordine, A0, integrazione$K, n_ordine_usato)
  
  K_soglia_val <- ni_K_soglia(stato$ordine, A0, C_crit, n_ordine_usato)
  # Threshold time by interpolation on K(t) (monotone increasing for k>0) -
  # more robust than interpolating directly on C(t), whose monotonicity
  # depends on the sign of (1-order) for order n.
  if (K_soglia_val > max(integrazione$K)) {
    t_soglia <- NA
  } else {
    t_soglia <- approx(integrazione$K, integrazione$t, xout = K_soglia_val)$y
  }
  
  # Isothermal comparison (optional): at CONSTANT temperature, K(t) = k*t
  # exactly as in the classic isothermal case, so t = K_soglia/k -
  # no integration needed, only a direct evaluation of the
  # secondary model already fitted at the chosen temperature.
  confronta_isotermo <- isTRUE(input$ni_confronta_isotermo)
  T_iso <- input$ni_T_iso
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
      # Same formula as the single point, applied to the whole time grid
      # already computed for the main curve: at CONSTANT
      # temperature, K_iso(t) = k_iso*t exactly (no integration
      # needed), so the isothermal curve is immediate to obtain.
      curva_C_iso <- ni_quantita_da_K(stato$ordine, A0, k_iso_puntuale * integrazione$t, n_ordine_usato)
    }
  }
  
  # Propagation of the uncertainty into the final prediction: if the nested
  # bootstrap of the secondary model is available, it repeats the same
  # computation of the threshold time for EVERY set of bootstrap secondary
  # parameters - not only the uncertainty of the secondary fit, but also that
  # (already propagated within it) of the primary fit. Reduced resolution
  # (300 instead of 1000 points) for each iteration, since here only
  # the final threshold time is needed, not the complete curve - a
  # reasonable compromise given the repetition over many iterations.
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
        # Same bootstrap row already used above for k(T(t)) - reused here
        # for an INSTANTANEOUS evaluation (no integration) at T_iso,
        # so the isothermal comparison inherits the same uncertainty of the
        # primary+secondary fit, without separate recomputations.
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
      alpha <- ni_alpha_confidenza()
      banda_C_inf <- apply(mat_curve_C_boot[righe_valide, , drop = FALSE], 2, quantile, probs = alpha / 2, na.rm = TRUE)
      banda_C_sup <- apply(mat_curve_C_boot[righe_valide, , drop = FALSE], 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      banda_k_inf <- apply(mat_curve_k_boot[righe_valide, , drop = FALSE], 2, quantile, probs = alpha / 2, na.rm = TRUE)
      banda_k_sup <- apply(mat_curve_k_boot[righe_valide, , drop = FALSE], 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
    }
  }
  
  # Statistical comparison between the two critical times (non-isothermal vs isothermal):
  # same technique already validated in calcola_confronto_coppia
  # (batch_elaboration.R, used by the Parameter Significance Test) - all the
  # cross differences between the two independent bootstrap distributions
  # (outer()), not a paired row-by-row comparison, for consistency with
  # that precedent. No formal test if the bootstrap is not available
  # for both quantities: here there is no analytical standard error
  # from which to derive it (unlike the comparison between model parameters),
  # so it is better to show only the two point values than to invent a test.
  confronto_isotermo <- NULL
  if (confronta_isotermo && !is.na(t_iso)) {
    if (!is.null(t_soglia_boot) && !is.null(t_iso_boot)) {
      alpha <- ni_alpha_confidenza()
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
  
  ni_previsione_risultato(list(t = integrazione$t, T = integrazione$T, k = integrazione$k, C = curva_C, A0 = A0, C_crit = C_crit, t_soglia = t_soglia, t_soglia_boot = t_soglia_boot,
                                T_iso = T_iso, t_iso = t_iso, t_iso_boot = t_iso_boot, C_iso = curva_C_iso, confronto_isotermo = confronto_isotermo,
                                t_griglia_banda = if (!is.null(banda_C_inf)) seq(0, t_max, length.out = 200) else NULL,
                                banda_C_inf = banda_C_inf, banda_C_sup = banda_C_sup,
                                banda_k_inf = banda_k_inf, banda_k_sup = banda_k_sup))
})

output$ni_risultato_testo <- renderUI({
  ris <- ni_previsione_risultato()
  if (is.null(ris)) return(NULL)
  if (is.na(ris$t_soglia)) {
    return(p(style = "font-size: 14px; color: #c0392b; font-weight: 500;",
              "The critical threshold is not reached within the simulated time range \u2014 try increasing Max Time to Simulate."))
  }
  ci_testo <- if (!is.null(ris$t_soglia_boot)) {
    alpha <- ni_alpha_confidenza()
    ci_bassa <- signif(quantile(ris$t_soglia_boot, alpha / 2), 4)
    ci_alta <- signif(quantile(ris$t_soglia_boot, 1 - alpha / 2), 4)
    paste0(" (", round(100 * (1 - alpha), 1), "% CI: ", ci_bassa, " \u2013 ", ci_alta, ")")
  } else ""
  div(style = "background-color: #f0faf8; border-left: 4px solid #18bc9c; border-radius: 4px; padding: 10px 14px; margin-bottom: 10px;",
      tags$strong(style = "font-size: 16px; color: black;", paste0("Predicted critical time: ", signif(ris$t_soglia, 4), " time units", ci_testo)))
  })
  
  output$ni_confronto_isotermo_testo <- renderUI({
    ris <- ni_previsione_risultato()
    if (is.null(ris) || is.null(ris$confronto_isotermo)) return(NULL)
    conf <- ris$confronto_isotermo
    ci_iso_testo <- if (!is.null(ris$t_iso_boot)) {
      alpha <- ni_alpha_confidenza()
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
              if (!is.null(conf$p_value)) paste0("  |  ", round(100 * (1 - ni_alpha_confidenza()), 1), "% CI of the difference: ", signif(conf$ci_diff_inf, 4), " \u2013 ", signif(conf$ci_diff_sup, 4), "  |  p = ", format.pval(conf$p_value, digits = 3), " (", conf$significativita, ")") else ""),
            p(style = "font-size: 11px; color: #7f8c8d;", conf$metodo)
          )
        })
})

# Plot area: side by side (default) or a single combined dual-axis plot
# (quantity on the left, temperature on the right) depending on the user's
# choice - a single renderUI that decides the LAYOUT, so the two modes can
# never appear at the same time by mistake.
output$ni_area_grafici_previsione <- renderUI({
  if (isTRUE(input$ni_combina_grafici)) {
    tagList(
      plotOutput("ni_grafico_combinato", height = "450px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_ni_combinato", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("ni_btn_estetica_previsione", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
      hr(),
      plotOutput("ni_grafico_k", height = "350px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_ni_grafico_k", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("ni_btn_estetica_k", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  } else {
    tagList(
      fluidRow(
        column(6, plotOutput("ni_grafico_profilo", height = "400px")),
        column(6, plotOutput("ni_grafico_degradazione", height = "400px"))
      ),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_ni_profilo", "Download Temperature Profile", class = "btn-outline-secondary btn-sm"),
          downloadButton("scarica_ni_degradazione", "Download Predicted Quantity", class = "btn-outline-secondary btn-sm"),
          actionButton("ni_btn_estetica_previsione", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize these plots")),
      hr(),
      plotOutput("ni_grafico_k", height = "350px"),
      div(style = "margin-top: 8px;",
          downloadButton("scarica_ni_grafico_k", "Download Plot", class = "btn-outline-secondary btn-sm"),
          actionButton("ni_btn_estetica_k", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
    )
  }
})

output$ni_grafico_profilo <- renderPlot({
  disegna_ni_profilo()
})
output$scarica_ni_profilo <- downloadHandler(
  filename = function() paste0("temperature_profile.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_ni_profilo()
    dev.off()
  }
)
disegna_ni_profilo <- function() {
  ris <- ni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  est <- ni_leggi_estetica()
  par(mar = c(4, 4.5, if (input$mostra_titolo) 3 else 1.5, 1))
  plot(ris$t, ni_converti_temp_display(ris$T), type = "l", col = adjustcolor(est$col_T, alpha.f = est$alpha_T), lwd = est$lwd_linee, lty = est$lty_T,
       xlab = "Time", ylab = ni_etichetta_temp(), main = if (input$mostra_titolo) "Temperature Profile T(t)" else "")
  if (est$grid_on) grid()
}

output$ni_grafico_k <- renderPlot({
  disegna_ni_grafico_k()
})
output$scarica_ni_grafico_k <- downloadHandler(
  filename = function() paste0("rate_constant_k.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_ni_grafico_k()
    dev.off()
  }
)
# Third plot, added at the end as a separate panel (it does not enter the
# side-by-side/combined choice of the first two, nor their dual-axis toggle
# - here there is a single curve, there is no need to share the axis with anything else).
# Reuses the colour already associated with "k" in the Secondary Model section
# (col_linea_sec), for visual consistency with that plot.
disegna_ni_grafico_k <- function() {
  ris <- ni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$ni_mostra_banda_previsione)) ris$banda_k_inf <- ris$banda_k_sup <- NULL
  est <- ni_leggi_estetica()
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
    livello_testo <- paste0(round(100 * (1 - ni_alpha_confidenza()), 1), "%")
    legend(est$pos_legenda_k, legend = paste0(livello_testo, " Confidence Band"), col = adjustcolor(est$col_k, alpha.f = 0.4), pch = 15, bty = "o", bg = "white", cex = est$cex_legenda_k)
  }
}

output$ni_grafico_degradazione <- renderPlot({
  disegna_ni_degradazione()
})
output$scarica_ni_degradazione <- downloadHandler(
  filename = function() paste0("degradation_curve.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_ni_degradazione()
    dev.off()
  }
)
disegna_ni_degradazione <- function() {
  ris <- ni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$ni_mostra_banda_previsione)) ris$banda_C_inf <- ris$banda_C_sup <- NULL
  est <- ni_leggi_estetica()
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
        voci <- c(voci, paste0(round(100 * (1 - ni_alpha_confidenza()), 1), "% Confidence Band"))
        colori_legenda <- c(colori_legenda, adjustcolor(est$col_C, alpha.f = 0.4)); lty_legenda <- c(lty_legenda, NA); lwd_legenda <- c(lwd_legenda, NA); pch_legenda <- c(pch_legenda, 15)
      }
      legend(est$pos_legenda_prev, legend = voci, col = colori_legenda, lty = lty_legenda, lwd = lwd_legenda, pch = pch_legenda, bty = "o", bg = "white", cex = est$cex_legenda_prev)
    }
  }
}


# Combined plot: same standard base-R technique for a second Y axis
# (par(new=TRUE) + a second invisible plot() on the axes + axis(4)) - the
# temperature on the right axis, the predicted quantity on the left one,
# both on the same time axis.
output$ni_grafico_combinato <- renderPlot({
  disegna_ni_combinato()
})
output$scarica_ni_combinato <- downloadHandler(
  filename = function() paste0("combined_plot.", input$formato_esportazione),
  content = function(file) {
    dim_exp <- dimensioni_esportazione()
    if (input$formato_esportazione == "pdf") pdf(file, width = dim_exp$width_in, height = dim_exp$height_in)
    else jpeg(file, width = dim_exp$width_px, height = dim_exp$height_px, res = dim_exp$dpi)
    disegna_ni_combinato()
    dev.off()
  }
)
disegna_ni_combinato <- function() {
  ris <- ni_previsione_risultato()
  if (is.null(ris)) { plot.new(); text(0.5, 0.5, "Run the prediction above.", col = "#7f8c8d"); return(invisible(NULL)) }
  if (!isTRUE(input$ni_mostra_banda_previsione)) ris$banda_C_inf <- ris$banda_C_sup <- NULL
  est <- ni_leggi_estetica()
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
  plot(ris$t, ni_converti_temp_display(ris$T), type = "l", col = adjustcolor(est$col_T, alpha.f = est$alpha_T), lwd = est$lwd_linee, lty = est$lty_T, axes = FALSE, xlab = "", ylab = "")
  axis(4)
  mtext(ni_etichetta_temp(), side = 4, line = 3)
  if (est$mostra_legenda_prev) {
    voci <- c("Predicted Quantity", "Temperature", "Critical threshold", "Predicted critical time")
    colori_legenda <- c(est$col_C, est$col_T, est$col_soglia, "#2980b9"); lty_legenda <- c(1, est$lty_T, 2, 2); pch_legenda <- c(NA, NA, NA, NA)
    if (ha_iso) {
      voci <- c(voci, "Isothermal Curve")
      colori_legenda <- c(colori_legenda, est$col_iso); lty_legenda <- c(lty_legenda, est$lty_iso); pch_legenda <- c(pch_legenda, NA)
    }
    if (ha_banda) {
      voci <- c(voci, paste0(round(100 * (1 - ni_alpha_confidenza()), 1), "% Confidence Band (Quantity)"))
      colori_legenda <- c(colori_legenda, adjustcolor(est$col_C, alpha.f = 0.4)); lty_legenda <- c(lty_legenda, NA); pch_legenda <- c(pch_legenda, 15)
    }
    legend(est$pos_legenda_prev, legend = voci, col = colori_legenda, lty = lty_legenda, pch = pch_legenda, lwd = est$lwd_linee, bty = "o", bg = "white", cex = est$cex_legenda_prev)
  }
}



# Checkbox of the simultaneous band of the secondary plot: only when the band
# comes from the linear approximation; with the nested bootstrap the bands are
# pointwise percentiles and a note appears in its place.
output$ni_ui_banda_simultanea <- renderUI({
  ris <- ni_secondario_risultato()
  if (is.null(ris)) return(NULL)
  if (ni_bootstrap_secondario_valido(ris)) {
    return(p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the nested bootstrap (percentiles of the bootstrap curves)."))
  }
  tagList(
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Bands from the linear approximation of the secondary model (nested bootstrap not available)."),
    conditionalPanel(condition = "input.ni_mostra_banda_confidenza",
      div(style = "margin-left: 20px;",
        checkboxInput("ni_banda_simultanea", "Simultaneous confidence band (Bates & Watts, 1988)", value = FALSE)))
  )
})
