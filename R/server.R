# ------------------------------------------------------------------------------
# 2. BACKEND LOGIC (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$btn_syntax, {
    showModal(modalDialog(
      title = div(icon("book"), " Math Syntax Guide for Custom Models"),
      size = "l",
      p(style = "color: #c0392b; font-weight: bold; margin-bottom: 5px;", "Write equations exactly as shown:"),
      tags$ul(
        style = "font-size: 14px; line-height: 1.6;",
        tags$li(tags$strong("Basic Operators: "), "+ (add), - (subtract), * (multiply), / (divide), ^ (power). ", tags$em("Example: A * x ^ 2")),
        tags$li(tags$strong("Exponents & Logs: "), "exp(x) [exponential e^x], log(x) [natural logarithm ln], log10(x) [base-10 log], log2(x) [base-2 log]"),
        tags$li(tags$strong("Trigonometry: "), "sin(x) [sine], cos(x) [cosine], tan(x) [tangent]"),
        tags$li(tags$strong("Inverse Trig (Arcs): "), "asin(x) [arcsine], acos(x) [arccosine], atan(x) [arctangent]"),
        tags$li(tags$strong("Reciprocal Trig: "), "1/tan(x) [cotangent], 1/cos(x) [secant], 1/sin(x) [cosecant]"),
        tags$li(tags$strong("Hyperbolic: "), "sinh(x), cosh(x), tanh(x), asinh(x), acosh(x), atanh(x)"),
        tags$li(tags$strong("Special Functions: "), "sqrt(x) [square root], abs(x) [absolute value], pi [geometric constant 3.1415...]"),
        tags$li(tags$strong("Advanced Statistics: "), "gamma(x) [Gamma function], lgamma(x) [Natural log of Gamma function]")
      ),
      easyClose = TRUE,
      footer = modalButton("Close Guide")
    ))
  })
  
  apri_guida_label <- function() {
    tabella_greche <- tags$table(
      style = "width: 100%; font-size: 13px; border-collapse: collapse;",
      tags$tr(style = "font-weight: bold; border-bottom: 1px solid #ddd;",
              tags$td("Type"), tags$td("Shows"), tags$td("Type"), tags$td("Shows"), tags$td("Type"), tags$td("Shows")),
      tags$tr(tags$td("alpha"), tags$td("\u03b1"), tags$td("eta"), tags$td("\u03b7"), tags$td("nu"), tags$td("\u03bd")),
      tags$tr(tags$td("beta"), tags$td("\u03b2"), tags$td("theta"), tags$td("\u03b8"), tags$td("xi"), tags$td("\u03be")),
      tags$tr(tags$td("gamma"), tags$td("\u03b3"), tags$td("iota"), tags$td("\u03b9"), tags$td("pi"), tags$td("\u03c0")),
      tags$tr(tags$td("delta"), tags$td("\u03b4"), tags$td("kappa"), tags$td("\u03ba"), tags$td("rho"), tags$td("\u03c1")),
      tags$tr(tags$td("epsilon"), tags$td("\u03b5"), tags$td("lambda"), tags$td("\u03bb"), tags$td("sigma"), tags$td("\u03c3")),
      tags$tr(tags$td("zeta"), tags$td("\u03b6"), tags$td("mu"), tags$td("\u03bc"), tags$td("tau"), tags$td("\u03c4")),
      tags$tr(tags$td("phi"), tags$td("\u03c6"), tags$td("chi"), tags$td("\u03c7"), tags$td("psi, omega"), tags$td("\u03c8, \u03c9"))
    )
    tabella_maiuscole <- tags$table(
      style = "width: 100%; font-size: 13px; border-collapse: collapse; margin-top: 8px;",
      tags$tr(style = "font-weight: bold; border-bottom: 1px solid #ddd;",
              tags$td("Type"), tags$td("Shows"), tags$td("Type"), tags$td("Shows"), tags$td("Type"), tags$td("Shows")),
      tags$tr(tags$td("Gamma"), tags$td("\u0393"), tags$td("Lambda"), tags$td("\u039b"), tags$td("Phi"), tags$td("\u03a6")),
      tags$tr(tags$td("Delta"), tags$td("\u0394"), tags$td("Xi"), tags$td("\u039e"), tags$td("Psi"), tags$td("\u03a8")),
      tags$tr(tags$td("Theta"), tags$td("\u0398"), tags$td("Pi, Sigma"), tags$td("\u03a0, \u03a3"), tags$td("Omega"), tags$td("\u03a9"))
    )
    showModal(modalDialog(
      title = div(icon("book"), " Label Syntax Guide (Greek Letters, Subscripts & Superscripts)"),
      size = "l",
      p(style = "font-size: 13px; color: #566573;",
        "The ", tags$strong("X Variable Name"), " and ", tags$strong("Y Variable Name"), " fields (and any other axis label in the app) accept R's ", tags$strong("plotmath"), " syntax for mathematical symbols. ",
        "If what you type isn't recognized as valid syntax, it's simply shown as plain text exactly as typed \u2014 so ordinary labels like \"Time (days)\" always work exactly as before."),
      hr(),
      tags$h5("Greek Letters (lowercase):"),
      tabella_greche,
      tags$h5(style = "margin-top: 15px;", "Greek Letters (uppercase \u2014 only the ones that look different from Latin letters):"),
      tabella_maiuscole,
      hr(),
      p(style = "color: #c0392b; font-weight: bold; margin-bottom: 5px;", "Subscripts, superscripts & combinations:"),
      tags$ul(
        style = "font-size: 14px; line-height: 1.8;",
        tags$li(tags$code("mu[max]"), " \u2192 \u03bc with subscript \"max\" (", tags$em("square brackets = subscript"), ")"),
        tags$li(tags$code("x^2"), " \u2192 x with superscript 2 (", tags$em("caret = superscript"), ")"),
        tags$li(tags$code("k[1]^2"), " \u2192 k with subscript 1, then superscript 2 (subscript and superscript can combine)"),
        tags$li(tags$code("sigma^2"), " \u2192 \u03c3\u00b2 (variance symbol)"),
        tags$li(tags$code("Delta*T"), " \u2192 \u0394T (", tags$em("* concatenates symbols with no space"), ")"),
        tags$li(tags$code("T~(degree*C)"), " \u2192 T (\u00b0C) (", tags$em("~ inserts a space; degree gives the \u00b0 symbol"), ")"),
        tags$li(tags$code("\"Concentration\"~x[i]"), " \u2192 mixes quoted plain text with a symbol (quotes needed around multi-word plain text used alongside symbols)")
      ),
      p(style = "font-size: 12px; color: #7f8c8d;",
        "This is R's native ", tags$code("plotmath"), " notation (not LaTeX). For the full reference, see ", tags$code("?plotmath"), " in R."),
      easyClose = TRUE,
      footer = modalButton("Close Guide")
    ))
  }
  
  observeEvent(input$btn_syntax_label, { apri_guida_label() })
  observeEvent(input$btn_syntax_label_simboli, { apri_guida_label() })
  
  # ---- Model selection popup panel, organized by food application ----
  # The original selectInput("tipo_modello") remains the source of truth (hidden
  # in the UI) - this popup is just a more navigable interface on top of the
  # same mechanism: a click on a card sends the model id via
  # Shiny.setInputValue(), a single observer updates the hidden selectInput
  # via updateSelectInput() and closes the modal. In this way the whole rest of the app
  # (dozens of references to input$tipo_modello) keeps working without
  # any change.
  genera_card_modello <- function(id_modello, etichetta, display_formula, selezionato = FALSE) {
    display_abbellito <- abbellisci_formula_html(id_modello)
    
    tags$div(
      style = paste0("border: 1px solid ", if (selezionato) "#005088" else "#dee2e6", 
                     "; background-color: ", if (selezionato) "#eaf2fa" else "white",
                     "; border-radius: 6px; padding: 10px 14px; margin-bottom: 8px; cursor: pointer;"),
      onclick = sprintf("Shiny.setInputValue('modello_scelto_popup', '%s', {priority: 'event'})", id_modello),
      onmouseover = "this.style.backgroundColor='#f0f4f8'",
      onmouseout = if (selezionato) "this.style.backgroundColor='#eaf2fa'" else "this.style.backgroundColor='white'",
      tags$strong(etichetta),
      tags$div(style = "font-size: 12.5px; color: #566573; margin-top: 4px; font-family: 'Cambria Math', Cambria, Georgia, serif; line-height: 1.5; word-wrap: break-word; font-style: italic;", HTML(display_abbellito))
    )
  }
  
  apri_selettore_modello <- function() {
    categorie <- costruisci_categorie_modello()
    modello_corrente <- input$tipo_modello
    # Finds the category to which the model currently in use belongs,
    # so that the pop-up opens already on the right tab instead of always
    # on the first one - e.g. with an active custom formula it opens on "Custom
    # Model", with Baranyi it opens on "Predictive Microbiology", etc.
    categoria_corrente <- NULL
    for (nome_cat in names(categorie)) {
      if (isTRUE(modello_corrente %in% categorie[[nome_cat]]$id)) {
        categoria_corrente <- nome_cat
        break
      }
    }
    showModal(modalDialog(
      title = div(icon("flask"), " Choose a Model by Application"),
      size = "l",
      do.call(tabsetPanel, c(list(id = "selettore_modello_tabs", selected = categoria_corrente), lapply(names(categorie), function(nome_categoria) {
        df_cat <- categorie[[nome_categoria]]
        tabPanel(nome_categoria,
                 br(),
                 lapply(seq_len(nrow(df_cat)), function(i) {
                   genera_card_modello(df_cat$id[i], df_cat$label[i], df_cat$display[i], 
                                      selezionato = identical(df_cat$id[i], modello_corrente))
                 })
        )
      }))),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  
  observeEvent(input$apri_selettore_modello, { apri_selettore_modello() })
  
  observeEvent(input$modello_scelto_popup, {
    updateSelectInput(session, "tipo_modello", selected = input$modello_scelto_popup)
    removeModal()
  })
  
  output$trigger_selettore_modello <- renderUI({
    info_modello <- MODEL_REGISTRY[[input$tipo_modello]]
    if (is.null(info_modello)) return(NULL)
    tags$div(style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 14px; margin-bottom: 8px; background-color: #f8f9fa;",
             tags$strong(info_modello$label),
             tags$div(style = "font-size: 11px; color: #7f8c8d; margin-top: 2px;", info_modello$categoria)
    )
  })
  
  apri_estetica <- function(tab_selezionata = NULL) {
    
    # ---- Content of the individual panels, defined as functions --------
    # Each function builds the content of ONE single tab - so it
    # can be called both to build the whole panel (generic
    # "Plot Aesthetics & Colors" button in the sidebar, which must be able to
    # navigate freely among all categories) AND on its own, when the
    # panel is opened from a specific "Customize this plot" button (27
    # of the 28 call points in the whole app) - in that case ONLY the
    # requested tab is built, not the other ~25, much faster to
    # open. The content of each function is identical, byte for
    # byte, to the content that was inline here before this refactoring.
  ce_root_parameter_names <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "the model equation display, the Parameter Estimation Results table, the Parameter Distributions plot, and every table/plot elsewhere in the app (Batch Elaboration, Model Comparison, Shelf-Life) that shows this model's parameter names."),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   "Purely cosmetic: shows a nicer symbol for a parameter wherever its name appears, without changing the actual name used in formulas/exports. Since the number of parameters depends on the chosen model, add one mapping at a time. ", tags$strong("Plotmath syntax is supported"), " (e.g. ", tags$code("mu[max]"), ")."),
                 fluidRow(style = "display: flex; align-items: flex-end;",
                   column(5, textInput("simbolo_par_nome", "Parameter Name:", placeholder = "e.g. MU")),
                   column(5, textInput("simbolo_par_display", "Display Symbol:", placeholder = "e.g. mu[max]")),
                   column(2, actionButton("aggiungi_simbolo", "Add", class = "btn-success btn-sm w-100", style = "margin-bottom: 15px;"))
                 ),
                 tableOutput("tabella_simboli_parametri"),
                 fluidRow(
                   column(6, actionButton("svuota_simboli", "Clear All Symbols", class = "btn-danger btn-sm w-100", icon = icon("trash"))),
                   column(6, actionButton("btn_syntax_label_simboli", "\U0001F4D6 Syntax Guide", class = "btn-info btn-sm w-100", style = "color: white;"))
                 )
        )
  }
  ce_single_fit_main_fit <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Main Fit Plot, and the Inverse Prediction fit plot (they share the same drawing function). The X range below is also shared with the Residuals plot."),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Leave the axis range fields below at 0 for automatic calculation based on the input data."),
                 fluidRow(
                   column(3, numericInput("range_x_min", "X Min:", value = if (is.null(input$range_x_min)) 0 else input$range_x_min)),
                   column(3, numericInput("range_x_max", "X Max:", value = if (is.null(input$range_x_max)) 0 else input$range_x_max)),
                   column(3, numericInput("range_y_min", "Y Min:", value = if (is.null(input$range_y_min)) 0 else input$range_y_min)),
                   column(3, numericInput("range_y_max", "Y Max:", value = if (is.null(input$range_y_max)) 0 else input$range_y_max))
                 ),
                 fluidRow(
                   column(4, selectInput("pt_col_fit", "Data Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$pt_col_fit)) "#005088" else input$pt_col_fit)),
                   column(4, selectInput("ln_col_fit", "Fit Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$ln_col_fit)) "#e74c3c" else input$ln_col_fit)),
                   column(4, selectInput("pt_pch_fit", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Open Square" = "0", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$pt_pch_fit)) "16" else input$pt_pch_fit))
                 ),
                 fluidRow(
                   column(4, sliderInput("pt_cex_fit", "Points Size:", min = 0.5, max = 3, value = if(is.null(input$pt_cex_fit)) 1.5 else input$pt_cex_fit, step = 0.1)),
                   column(4, sliderInput("ln_lwd_fit", "Line Width:", min = 1, max = 10, value = if(is.null(input$ln_lwd_fit)) 3 else input$ln_lwd_fit, step = 0.5)),
                   column(4, selectInput("ln_lty_fit", "Fit Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ln_lty_fit)) "1" else input$ln_lty_fit))
                 ),
                 fluidRow(
                   column(4, sliderInput("ci_alpha_fit", "Interval Opacity:", min = 0.05, max = 0.9, value = if(is.null(input$ci_alpha_fit)) 0.3 else input$ci_alpha_fit, step = 0.05)),
                   column(4, sliderInput("fnt_cex_fit", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_fit)) 1.0 else input$fnt_cex_fit, step = 0.1))
                 ),
                 fluidRow(
                   column(4, sliderInput("inv_cex_legenda", "Legend Text Size (Inverse Prediction's \"Predicted X\" label):", min = 0.5, max = 1.5, value = if(is.null(input$inv_cex_legenda)) 0.9 else input$inv_cex_legenda, step = 0.05)),
                   column(4, selectInput("inv_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$inv_pos_legenda)) "bottomleft" else input$inv_pos_legenda))
                 )
        )
  }
  ce_single_fit_simulator <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Initial Guess Simulator tab only."),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Leave the axis range fields below at 0 for automatic calculation based on the simulated curve."),
                 fluidRow(
                   column(3, numericInput("range_x_min_sim", "X Min:", value = if (is.null(input$range_x_min_sim)) 0 else input$range_x_min_sim)),
                   column(3, numericInput("range_x_max_sim", "X Max:", value = if (is.null(input$range_x_max_sim)) 0 else input$range_x_max_sim)),
                   column(3, numericInput("range_y_min_sim", "Y Min:", value = if (is.null(input$range_y_min_sim)) 0 else input$range_y_min_sim)),
                   column(3, numericInput("range_y_max_sim", "Y Max:", value = if (is.null(input$range_y_max_sim)) 0 else input$range_y_max_sim))
                 ),
                 fluidRow(
                   column(4, selectInput("pt_col_sim", "Data Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$pt_col_sim)) "#bdc3c7" else input$pt_col_sim)),
                   column(4, selectInput("ln_col_sim", "Simulated Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$ln_col_sim)) "#e74c3c" else input$ln_col_sim)),
                   column(4, selectInput("pt_pch_sim", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Open Square" = "0", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$pt_pch_sim)) "16" else input$pt_pch_sim))
                 ),
                 fluidRow(
                   column(4, sliderInput("pt_cex_sim", "Points Size:", min = 0.5, max = 3, value = if(is.null(input$pt_cex_sim)) 1.4 else input$pt_cex_sim, step = 0.1)),
                   column(4, sliderInput("ln_lwd_sim", "Line Width:", min = 1, max = 10, value = if(is.null(input$ln_lwd_sim)) 3.5 else input$ln_lwd_sim, step = 0.5)),
                   column(4, sliderInput("fnt_cex_sim", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_sim)) 1.0 else input$fnt_cex_sim, step = 0.1))
                 ),
                 checkboxInput("sim_mostra_legenda", "Show Legend", value = if(is.null(input$sim_mostra_legenda)) TRUE else input$sim_mostra_legenda),
                 checkboxInput("sim_mostra_punti_dati", "Show Raw Data Points", value = if(is.null(input$sim_mostra_punti_dati)) TRUE else input$sim_mostra_punti_dati),
                 fluidRow(
                   column(4, sliderInput("sim_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$sim_cex_legenda)) 1.0 else input$sim_cex_legenda, step = 0.05)),
                   column(4, selectInput("sim_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$sim_pos_legenda)) "topright" else input$sim_pos_legenda))
                 )
        )
  }
  ce_single_fit_residuals <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Residuals Plot and Lag Plot (same colors and symbols). X-axis range matches the Main Fit Plot (sidebar); Y-axis range is set below."),
                 fluidRow(
                   column(6, checkboxInput("res_collega_punti", "Connect residuals with a line (in order of x)",
                                           value = if (is.null(input$res_collega_punti)) !ha_repliche_correnti() else input$res_collega_punti)),
                   column(6, checkboxInput("res_mostra_medie", "Show mean residual per x (data with replicates)",
                                           value = if (is.null(input$res_mostra_medie)) TRUE else input$res_mostra_medie))
                 ),
                 checkboxInput("res_mostra_sottotitolo", "Show the lag-1 autocorrelation under the title of the Lag Plot",
                               value = if (is.null(input$res_mostra_sottotitolo)) TRUE else input$res_mostra_sottotitolo),
                 fluidRow(
                   column(4, selectInput("pt_col_res", "Residual Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$pt_col_res)) "#2c3e50" else input$pt_col_res)),
                   column(4, selectInput("ln_col_res", "Zero Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$ln_col_res)) "#e74c3c" else input$ln_col_res)),
                   column(4, selectInput("pt_pch_res", "Points Symbol Shape:", choices = c("Solid Triangle" = "17", "Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Cross" = "4"), selected = if(is.null(input$pt_pch_res)) "17" else input$pt_pch_res))
                 ),
                 fluidRow(
                   column(4, sliderInput("pt_cex_res", "Points Size:", min = 0.5, max = 3, value = if(is.null(input$pt_cex_res)) 1.3 else input$pt_cex_res, step = 0.1)),
                   column(4, selectInput("ln_lty_res", "Zero Line Type:", choices = c("Dashed" = "2", "Solid" = "1", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ln_lty_res)) "2" else input$ln_lty_res)),
                   column(4, sliderInput("ln_lwd_res", "Zero Line Width:", min = 0.5, max = 10, value = if(is.null(input$ln_lwd_res)) 2 else input$ln_lwd_res, step = 0.5))
                 ),
                 sliderInput("fnt_cex_res", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_res)) 1.0 else input$fnt_cex_res, step = 0.1),
                 fluidRow(
                   column(4, numericInput("range_y_min_res", "Min Y-axis Residual:", value = if (is.null(input$range_y_min_res)) 0 else input$range_y_min_res)),
                   column(4, numericInput("range_y_max_res", "Max Y-axis Residual:", value = if (is.null(input$range_y_max_res)) 0 else input$range_y_max_res))
                 ),
                 p("Leave both range fields at 0 for automatic scaling.", style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;")
        )
  }
  ce_single_fit_q_q_plot <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Normal Q-Q Plot of Residuals and Replicate SD vs Mean plot (same colors and symbols). Styled independently from the Residuals Plot, since they are different diagnostic views."),
                 checkboxInput("qq_mostra_sottotitolo", "Show the fitted relation or the reading guide under the title of the Variance Structure plot",
                               value = if (is.null(input$qq_mostra_sottotitolo)) TRUE else input$qq_mostra_sottotitolo),
                 fluidRow(
                   column(4, selectInput("pt_col_qq", "Q-Q Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$pt_col_qq)) "#2c3e50" else input$pt_col_qq)),
                   column(4, selectInput("ln_col_qq", "Reference Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$ln_col_qq)) "#e74c3c" else input$ln_col_qq)),
                   column(4, selectInput("pt_pch_qq", "Points Symbol Shape:", choices = c("Solid Triangle" = "17", "Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Cross" = "4"), selected = if(is.null(input$pt_pch_qq)) "17" else input$pt_pch_qq))
                 ),
                 fluidRow(
                   column(4, sliderInput("pt_cex_qq", "Points Size:", min = 0.5, max = 3, value = if(is.null(input$pt_cex_qq)) 1.3 else input$pt_cex_qq, step = 0.1)),
                   column(4, selectInput("ln_lty_qq", "Reference Line Type:", choices = c("Dashed" = "2", "Solid" = "1", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ln_lty_qq)) "2" else input$ln_lty_qq)),
                   column(4, sliderInput("ln_lwd_qq", "Reference Line Width:", min = 0.5, max = 10, value = if(is.null(input$ln_lwd_qq)) 2 else input$ln_lwd_qq, step = 0.5))
                 ),
                 sliderInput("fnt_cex_qq", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_qq)) 1.0 else input$fnt_cex_qq, step = 0.1),
                 hr(),
                 tags$h5("Display Range:"),
                 p("Leave at 0 for automatic scaling.", style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;"),
                 fluidRow(
                   column(3, numericInput("range_x_min_qq", "Min Theoretical Quantile:", value = if (is.null(input$range_x_min_qq)) 0 else input$range_x_min_qq)),
                   column(3, numericInput("range_x_max_qq", "Max Theoretical Quantile:", value = if (is.null(input$range_x_max_qq)) 0 else input$range_x_max_qq)),
                   column(3, numericInput("range_y_min_qq", "Min Sample Quantile:", value = if (is.null(input$range_y_min_qq)) 0 else input$range_y_min_qq)),
                   column(3, numericInput("range_y_max_qq", "Max Sample Quantile:", value = if (is.null(input$range_y_max_qq)) 0 else input$range_y_max_qq))
                 )
        )
  }
  ce_single_fit_distributions <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "the Parameter Distributions plot (Single Fit Analysis) and the Bootstrap Distribution of X plot (Inverse Prediction) \u2014 both share the same histogram style."),
                 sliderInput("n_bins_dist", "Number of Bins:", min = 5, max = 60, value = if(is.null(input$n_bins_dist)) 30 else input$n_bins_dist, step = 1),
                 fluidRow(
                   column(4, selectInput("bar_col_dist", "Histogram Fill Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$bar_col_dist)) "#f39c12" else input$bar_col_dist)),
                   column(4, selectInput("dens_col_dist", "Density Curve Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$dens_col_dist)) "black" else input$dens_col_dist)),
                   column(4, selectInput("vln_col_dist", "Best Fit Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$vln_col_dist)) "black" else input$vln_col_dist))
                 ),
                 fluidRow(
                   column(6, checkboxInput("mostra_densita_dist", "Show density curve (fitted distribution)", value = if(is.null(input$mostra_densita_dist)) TRUE else input$mostra_densita_dist)),
                   column(6, checkboxInput("mostra_vln_dist", "Show best-fit estimate line", value = if(is.null(input$mostra_vln_dist)) TRUE else input$mostra_vln_dist))
                 ),
                 fluidRow(
                   column(4, selectInput("vln_lty_dist", "Best Fit Line Type:", choices = c("Dashed" = "2", "Solid" = "1", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$vln_lty_dist)) "2" else input$vln_lty_dist)),
                   column(4, sliderInput("vln_lwd_dist", "Best Fit Line Width:", min = 0.5, max = 10, value = if(is.null(input$vln_lwd_dist)) 2 else input$vln_lwd_dist, step = 0.5)),
                   column(4, sliderInput("fnt_cex_dist", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_dist)) 1.0 else input$fnt_cex_dist, step = 0.1))
                 ),
                 hr(),
                 tags$h5("Display Range \u2014 Inverse Prediction's Distribution of X only:"),
                 p("Leave at 0 for automatic scaling. Not available for Parameter Distributions: it draws one panel per parameter, each on its own natural scale, so a single shared range wouldn't apply meaningfully across all of them.", 
                   style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;"),
                 fluidRow(
                   column(3, numericInput("range_x_min_distx", "X Min:", value = if (is.null(input$range_x_min_distx)) 0 else input$range_x_min_distx)),
                   column(3, numericInput("range_x_max_distx", "X Max:", value = if (is.null(input$range_x_max_distx)) 0 else input$range_x_max_distx)),
                   column(3, numericInput("range_y_min_distx", "Min Density:", value = if (is.null(input$range_y_min_distx)) 0 else input$range_y_min_distx)),
                   column(3, numericInput("range_y_max_distx", "Max Density:", value = if (is.null(input$range_y_max_distx)) 0 else input$range_y_max_distx))
                 )
        )
  }
  ce_single_fit_sensitivity <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Parameter Sensitivity Analysis plot."),
                 fluidRow(
                   column(4, sliderInput("ln_lwd_sens", "Line Width:", min = 1, max = 10, value = if(is.null(input$ln_lwd_sens)) 2.5 else input$ln_lwd_sens, step = 0.5)),
                   column(4, sliderInput("fnt_cex_sens", "Text & Axis Size:", min = 0.7, max = 2.0, value = if(is.null(input$fnt_cex_sens)) 1.0 else input$fnt_cex_sens, step = 0.1)),
                   column(4, sliderInput("sens_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$sens_cex_legenda)) 0.85 else input$sens_cex_legenda, step = 0.05))
                 ),
                 fluidRow(
                   column(6, selectInput("sens_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$sens_pos_legenda)) "topright" else input$sens_pos_legenda)),
                   column(6, selectInput("sens_palette", "Color Palette (one color per parameter):", choices = c(
                     "Fixed Palette (best for few parameters)" = "fissa", "Rainbow" = "rainbow", "Viridis" = "viridis", "Plasma" = "plasma",
                     "Cividis" = "cividis", "Spectral" = "spectral", "Heat" = "heat"
                   ), selected = if(is.null(input$sens_palette)) "fissa" else input$sens_palette))
                 ),
                 fluidRow(
                   column(6, checkboxInput("mostra_curva_sens", "Show Predicted Curve (gray, right axis)", value = if(is.null(input$mostra_curva_sens)) TRUE else input$mostra_curva_sens)),
                   column(6, checkboxInput("mostra_legenda_sens", "Show Legend", value = if(is.null(input$mostra_legenda_sens)) TRUE else input$mostra_legenda_sens))
                 )
        )
  }
  ce_single_fit_parameter_pairs <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Bootstrap Parameter Cloud (Pairwise Scatter). Monte Carlo only."),
                 fluidRow(
                   column(4, selectInput("pt_col_pairs", "Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$pt_col_pairs)) "#2980b9" else input$pt_col_pairs)),
                   column(4, sliderInput("pt_cex_pairs", "Points Size:", min = 0.3, max = 2.0, value = if(is.null(input$pt_cex_pairs)) 0.8 else input$pt_cex_pairs, step = 0.1)),
                   column(4, sliderInput("pt_alpha_pairs", "Points Opacity:", min = 0.1, max = 1.0, value = if(is.null(input$pt_alpha_pairs)) 0.4 else input$pt_alpha_pairs, step = 0.05))
                 ),
                 hr(),
                 tags$h5("Density Contour Lines:"),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   "Shown/hidden via the checkbox next to the plot; these settings control their appearance."),
                 fluidRow(
                   column(4, selectInput("col_contour_pairs", "Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_contour_pairs)) "#2c3e50" else input$col_contour_pairs)),
                   column(4, selectInput("lty_contour_pairs", "Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$lty_contour_pairs)) "1" else input$lty_contour_pairs)),
                   column(4, sliderInput("lwd_contour_pairs", "Line Width:", min = 0.5, max = 10, value = if(is.null(input$lwd_contour_pairs)) 1 else input$lwd_contour_pairs, step = 0.5))
                 )
        )
  }
  ce_grid_search_loss_surface_2d_3d <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Single Fit Analysis \u2192 Loss Surface Explorer (both the 3D and Contour plots)."),
                 tags$h5("3D Surface Plot:"),
                 fluidRow(
                   column(6, selectInput("palette_superficie_grid", "Surface Color Palette:", choices = c("Blue-Red (classic heatmap)" = "blu_rosso", "Green-Yellow-Red" = "verde_giallo", "Purple-Orange" = "viola_arancio", "Ocean Blues" = "oceano"), selected = if(is.null(input$palette_superficie_grid)) "blu_rosso" else input$palette_superficie_grid)),
                   column(6, sliderInput("ombra_grid", "Shading Intensity:", min = 0, max = 1, value = if(is.null(input$ombra_grid)) 0.4 else input$ombra_grid, step = 0.1))
                 ),
                 tags$h5(style = "margin-top: 10px;", "Sampled Grid Points:"),
                 fluidRow(
                   column(3, selectInput("col_punti_grid_3d", "Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_punti_grid_3d)) "black" else input$col_punti_grid_3d)),
                   column(3, selectInput("pch_punti_grid_3d", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$pch_punti_grid_3d)) "16" else input$pch_punti_grid_3d)),
                   column(3, sliderInput("cex_punti_grid_3d", "Points Size:", min = 0.1, max = 1.5, value = if(is.null(input$cex_punti_grid_3d)) 0.75 else input$cex_punti_grid_3d, step = 0.05)),
                   column(3, sliderInput("alpha_punti_grid_3d", "Points Opacity:", min = 0.1, max = 1.0, value = if(is.null(input$alpha_punti_grid_3d)) 0.5 else input$alpha_punti_grid_3d, step = 0.05))
                 ),
                 hr(),
                 tags$h5("Contour Plot:"),
                 fluidRow(
                   column(4, selectInput("col_linee_grid", "Contour Line Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_linee_grid)) "black" else input$col_linee_grid)),
                   column(4, sliderInput("n_livelli_grid", "Number of Contour Levels:", min = 5, max = 30, value = if(is.null(input$n_livelli_grid)) 15 else input$n_livelli_grid, step = 1)),
                   column(4, sliderInput("lwd_linee_grid", "Contour Line Width:", min = 0.5, max = 10, value = if(is.null(input$lwd_linee_grid)) 1 else input$lwd_linee_grid, step = 0.5))
                 ),
                 fluidRow(
                   column(4, sliderInput("grid_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$grid_cex_legenda)) 0.8 else input$grid_cex_legenda, step = 0.05)),
                   column(4, selectInput("grid_pos_legenda", "Legend Position (Confidence Region):", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$grid_pos_legenda)) "topright" else input$grid_pos_legenda)),
                   column(4, selectInput("col_punto_ott_grid", "Optimal Point Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_punto_ott_grid)) "#e74c3c" else input$col_punto_ott_grid))
                 ),
                 selectInput("col_regione_grid", "Confidence Region Color:", choices = c(
                   "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                   "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                   "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                   "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                   "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                   "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                   "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                 ), selected = if(is.null(input$col_regione_grid)) "#8e44ad" else input$col_regione_grid)
        )
  }
  ce_grid_search_profile_curve <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Loss Surface Explorer \u2192 the Profile Curve (Parameter vs Loss) plots. The three reference line colors (optimal value, minimum loss, CI threshold) are fixed, shown in the plot's own legend."),
                 fluidRow(
                   column(4, selectInput("profilo2d_col_linea", "Curve Line Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$profilo2d_col_linea)) "black" else input$profilo2d_col_linea)),
                   column(4, selectInput("profilo2d_col_punto", "Curve Point Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$profilo2d_col_punto)) "#aed6f1" else input$profilo2d_col_punto)),
                   column(4, selectInput("profilo2d_pch_punto", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$profilo2d_pch_punto)) "16" else input$profilo2d_pch_punto))
                 ),
                 fluidRow(
                   column(4, sliderInput("profilo2d_cex_punto", "Points Size:", min = 0.2, max = 2.5, value = if(is.null(input$profilo2d_cex_punto)) 0.6 else input$profilo2d_cex_punto, step = 0.1)),
                   column(4, sliderInput("profilo2d_fnt_cex", "Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$profilo2d_fnt_cex)) 0.9 else input$profilo2d_fnt_cex, step = 0.05)),
                   column(4, sliderInput("profilo2d_lwd_linea", "Curve Line Width:", min = 0.5, max = 5, value = if(is.null(input$profilo2d_lwd_linea)) 1.5 else input$profilo2d_lwd_linea, step = 0.25))
                 ),
                 fluidRow(
                   column(6, checkboxInput("profilo2d_mostra_legenda", "Show legend", value = if(is.null(input$profilo2d_mostra_legenda)) TRUE else input$profilo2d_mostra_legenda)),
                   column(6, selectInput("profilo2d_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$profilo2d_pos_legenda)) "topright" else input$profilo2d_pos_legenda))
                 )
        )
  }
  
  ce_grid_search_t_plot <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Loss Surface Explorer \u2192 the t-Plot (Bates & Watts, 1988) plots. The reference line colors (linear approximation, \u00b1t critical, optimal value) are fixed, shown in the plot's own legend."),
                 fluidRow(
                   column(4, selectInput("tplot_col_linea", "Curve Line Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$tplot_col_linea)) "black" else input$tplot_col_linea)),
                   column(4, selectInput("tplot_col_punto", "Curve Point Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$tplot_col_punto)) "#aed6f1" else input$tplot_col_punto)),
                   column(4, selectInput("tplot_col_retta", "Linear Approximation Color:", choices = c(
                     "Slate Gray" = "#7f8c8d", "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9",
                     "Dark Green" = "#27ae60", "Orange" = "#e67e22", "Purple" = "#8e44ad",
                     "Gray" = "#95a5a6", "Light Gray" = "#bdc3c7"
                   ), selected = if(is.null(input$tplot_col_retta)) "#7f8c8d" else input$tplot_col_retta))
                 ),
                 fluidRow(
                   column(4, selectInput("tplot_pch_punto", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$tplot_pch_punto)) "16" else input$tplot_pch_punto)),
                   column(4, sliderInput("tplot_cex_punto", "Points Size:", min = 0.2, max = 2.5, value = if(is.null(input$tplot_cex_punto)) 0.6 else input$tplot_cex_punto, step = 0.1)),
                   column(4, sliderInput("tplot_lwd_linea", "Curve Line Width:", min = 0.5, max = 5, value = if(is.null(input$tplot_lwd_linea)) 1.5 else input$tplot_lwd_linea, step = 0.25))
                 ),
                 fluidRow(
                   column(4, sliderInput("tplot_fnt_cex", "Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$tplot_fnt_cex)) 0.9 else input$tplot_fnt_cex, step = 0.05)),
                   column(4, checkboxInput("tplot_mostra_legenda", "Show legend", value = if(is.null(input$tplot_mostra_legenda)) TRUE else input$tplot_mostra_legenda)),
                   column(4, selectInput("tplot_pos_legenda", "Legend Position:", choices = c("Top Left" = "topleft", "Top Right" = "topright", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$tplot_pos_legenda)) "topleft" else input$tplot_pos_legenda))
                 ),
                 fluidRow(
                   column(4, checkboxInput("tplot_mostra_punti", "Show points", value = if(is.null(input$tplot_mostra_punti)) TRUE else input$tplot_mostra_punti)),
                   column(4, checkboxInput("tplot_mostra_soglia", "Show \u00b1t critical lines", value = if(is.null(input$tplot_mostra_soglia)) TRUE else input$tplot_mostra_soglia))
                 ),
                 hr(),
                 checkboxInput("tplot_zoom_soglia", "Zoom Y-axis around \u00b1t critical (extra option, helps judge the linear approximation near the threshold)", value = if(is.null(input$tplot_zoom_soglia)) FALSE else input$tplot_zoom_soglia),
                 conditionalPanel(condition = "input.tplot_zoom_soglia",
                   sliderInput("tplot_zoom_margine_pct", "Margin beyond \u00b1t critical (%):", min = 5, max = 100, value = if(is.null(input$tplot_zoom_margine_pct)) 25 else input$tplot_zoom_margine_pct, step = 5)
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                   "Independent of the other options above \u2014 crops the Y-axis to just beyond the \u00b1t critical lines (instead of the full range of the profiled curve), so the region that actually decides the confidence interval fills the plot. Useful for judging by eye how much the actual profile bends away from the straight line right where it matters, when the far ends of the curve would otherwise dominate the scale.")
        )
  }
  
  ce_grid_search_profile_trace <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Loss Surface Explorer \u2192 the Profile Trace (Bates & Watts, 1988) plots. The optimum marker (black cross at the origin) is fixed."),
                 fluidRow(
                   column(4, selectInput("ptrace_col1", "First Trace Color:", choices = c(
                     "Blue" = "#2980b9", "Red" = "#e74c3c", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548", "Black" = "black",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d"
                   ), selected = if(is.null(input$ptrace_col1)) "#2980b9" else input$ptrace_col1)),
                   column(4, selectInput("ptrace_col2", "Second Trace Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548", "Black" = "black",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d"
                   ), selected = if(is.null(input$ptrace_col2)) "#e74c3c" else input$ptrace_col2)),
                   column(4, sliderInput("ptrace_lwd", "Trace Line Width:", min = 0.5, max = 5, value = if(is.null(input$ptrace_lwd)) 1.8 else input$ptrace_lwd, step = 0.25))
                 ),
                 fluidRow(
                   column(4, sliderInput("ptrace_fnt_cex", "Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$ptrace_fnt_cex)) 0.9 else input$ptrace_fnt_cex, step = 0.05)),
                   column(4, checkboxInput("ptrace_mostra_riferimento", "Show reference cross-hairs at 0", value = if(is.null(input$ptrace_mostra_riferimento)) TRUE else input$ptrace_mostra_riferimento)),
                   column(4, checkboxInput("ptrace_mostra_legenda", "Show legend", value = if(is.null(input$ptrace_mostra_legenda)) TRUE else input$ptrace_mostra_legenda))
                 ),
                 selectInput("ptrace_pos_legenda", "Legend Position:", choices = c("Top Left" = "topleft", "Top Right" = "topright", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$ptrace_pos_legenda)) "topleft" else input$ptrace_pos_legenda),
                 hr(),
                 checkboxInput("ptrace_valori_nominali", "Show nominal parameter values (instead of the tau / studentized scale)", value = if(is.null(input$ptrace_valori_nominali)) TRUE else input$ptrace_valori_nominali),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                   "Default, as in the profile traces of Bates & Watts (1988, chapter 6, p. 213): each axis in the parameter's own units, with the reference cross-hairs at the fitted estimates. Unchecked, the traces are shown on the tau scale for the profiled parameter and on the studentized scale (\u03b8 \u2212 \u03b8\u0302)/SE for the readjusted one, an additional representation of the app in which, for a linear model, the traces would be straight lines through the origin whose angle reflects the correlation."),
                 conditionalPanel(condition = "!input.ptrace_valori_nominali",
                   checkboxInput("ptrace_zoom_ottimo", "Zoom around the optimum (extra option, both axes)", value = if(is.null(input$ptrace_zoom_ottimo)) FALSE else input$ptrace_zoom_ottimo),
                   conditionalPanel(condition = "input.ptrace_zoom_ottimo",
                     sliderInput("ptrace_zoom_ottimo_range", "Range around 0 (\u00b1, both axes):", min = 1, max = 20, value = if(is.null(input$ptrace_zoom_ottimo_range)) 5 else input$ptrace_zoom_ottimo_range, step = 0.5)
                   ),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                     "Independent of the other options above \u2014 crops both axes to \u00b1 this range around the optimum, instead of the full extent of the traces. Useful when the far ends of a trace stretch out and squeeze the central region that actually matters for judging linearity and correlation. Not available with nominal values, since each parameter has its own scale.")
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: 6px;",
                   "If the two traces are close to straight lines crossing near the center, the two parameters behave close to linearly together. The more they curve, or the more their crossing angle departs from what the classic correlation would predict, the less reliable the classic (Wald) confidence interval is for this pair.")
        )
  }
  ce_grid_search_ci_comparison <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Loss Surface Explorer \u2192 the Profile/Classic/Monte Carlo comparison bar charts."),
                 fluidRow(
                   column(4, selectInput("grid_ci_col_barre", "Bar Fill Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$grid_ci_col_barre)) "#f39c12" else input$grid_ci_col_barre)),
                   column(4, selectInput("grid_ci_col_bordo", "Bar Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$grid_ci_col_bordo)) "black" else input$grid_ci_col_bordo)),
                   column(4, sliderInput("grid_ci_fnt_cex", "Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$grid_ci_fnt_cex)) 0.9 else input$grid_ci_fnt_cex, step = 0.05))
                 ),
                 checkboxInput("grid_ci_mostra_titolo_x", "Show x-axis title (\"Method\")", value = if(is.null(input$grid_ci_mostra_titolo_x)) TRUE else input$grid_ci_mostra_titolo_x)
        )
  }
  ce_model_comparison_shelf_life_bar_chart <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Shelf Life Comparison (Linear/Jacobian Method)."),
                 fluidRow(
                   column(4, selectInput("mc_sl_barre_col_barre", "Bar Fill Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_barre_col_barre)) "#f39c12" else input$mc_sl_barre_col_barre)),
                   column(4, selectInput("mc_sl_barre_col_bordo_barre", "Bar Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_barre_col_bordo_barre)) "black" else input$mc_sl_barre_col_bordo_barre)),
                   column(4, sliderInput("mc_sl_barre_fnt_cex_barre", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mc_sl_barre_fnt_cex_barre)) 0.8 else input$mc_sl_barre_fnt_cex_barre, step = 0.05))
                 ),
                 checkboxInput("mc_sl_barre_ordina_barre", "Sort bars ascending", value = if(is.null(input$mc_sl_barre_ordina_barre)) FALSE else input$mc_sl_barre_ordina_barre),
                 checkboxInput("mc_sl_barre_mostra_titolo_x", "Show x-axis title (\"Fit\")", value = if(is.null(input$mc_sl_barre_mostra_titolo_x)) TRUE else input$mc_sl_barre_mostra_titolo_x)
        )
  }
  ce_model_comparison_shelf_life_boxplots <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Shelf Life Distribution Across Fits."),
                 fluidRow(
                   column(4, selectInput("mc_sl_box_tipo_grafico_box", "Plot Type:", choices = c("Boxplot" = "boxplot", "Violin Plot" = "violin"), selected = if(is.null(input$mc_sl_box_tipo_grafico_box)) "boxplot" else input$mc_sl_box_tipo_grafico_box)),
                   column(4, selectInput("mc_sl_box_col_box", "Box Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_box_col_box)) "#f39c12" else input$mc_sl_box_col_box)),
                   column(4, selectInput("mc_sl_box_col_bordo_box", "Box Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_box_col_bordo_box)) "black" else input$mc_sl_box_col_bordo_box))
                 ),
                 sliderInput("mc_sl_box_fnt_cex_box", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mc_sl_box_fnt_cex_box)) 0.8 else input$mc_sl_box_fnt_cex_box, step = 0.05),
                 fluidRow(
                   column(4, checkboxInput("mc_sl_box_ordina_box", "Sort ascending by median", value = if(is.null(input$mc_sl_box_ordina_box)) FALSE else input$mc_sl_box_ordina_box)),
                   column(4, checkboxInput("mc_sl_box_mostra_outlier_box", "Show outlier points (boxplot only)", value = if(is.null(input$mc_sl_box_mostra_outlier_box)) TRUE else input$mc_sl_box_mostra_outlier_box)),
                   column(4, checkboxInput("mc_sl_box_mostra_titolo_x", "Show x-axis title (\"Fit\")", value = if(is.null(input$mc_sl_box_mostra_titolo_x)) TRUE else input$mc_sl_box_mostra_titolo_x))
                 )
        )
  }
  ce_model_comparison_boxplots <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Parameter Distributions (Monte Carlo)."),
                 fluidRow(
                   column(4, selectInput("tipo_grafico_boxplot_confronto", "Plot Type:", choices = c("Boxplot" = "boxplot", "Violin Plot" = "violin"), selected = if(is.null(input$tipo_grafico_boxplot_confronto)) "boxplot" else input$tipo_grafico_boxplot_confronto)),
                   column(4, selectInput("col_boxplot_confronto", "Box Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_boxplot_confronto)) "#f39c12" else input$col_boxplot_confronto)),
                   column(4, selectInput("col_bordo_boxplot_confronto", "Box Border Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$col_bordo_boxplot_confronto)) "black" else input$col_bordo_boxplot_confronto))
                 ),
                 sliderInput("fnt_cex_boxplot_confronto", "Text & Axis Size:", min = 0.5, max = 1.5, value = if(is.null(input$fnt_cex_boxplot_confronto)) 0.8 else input$fnt_cex_boxplot_confronto, step = 0.1),
                 fluidRow(
                   column(4, checkboxInput("ordina_boxplot_confronto", "Sort boxes/violins ascending by median (per panel)", value = if(is.null(input$ordina_boxplot_confronto)) FALSE else input$ordina_boxplot_confronto)),
                   column(4, checkboxInput("mostra_outlier_boxplot_confronto", "Show outlier points", value = if(is.null(input$mostra_outlier_boxplot_confronto)) TRUE else input$mostra_outlier_boxplot_confronto)),
                   column(4, checkboxInput("mostra_titolo_x_boxplot_confronto", "Show x-axis title (\"Model\")", value = if(is.null(input$mostra_titolo_x_boxplot_confronto)) TRUE else input$mostra_titolo_x_boxplot_confronto))
                 )
        )
  }
  ce_model_comparison_overlaid_curves <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Overlaid Fitted Curves."),
                 fluidRow(
                   column(8, selectInput("confronto_palette", "Curve Color Palette (one color per model):", choices = SCELTE_PALETTE_SERIE,
                                         selected = if(is.null(input$confronto_palette)) "fissa" else input$confronto_palette)),
                   column(4, selectInput("confronto_col_punti", "Points Color:", choices = SCELTE_COLORE_PUNTI,
                                         selected = if(is.null(input$confronto_col_punti)) "uguale" else input$confronto_col_punti))
                 ),
                 fluidRow(
                   column(6, sliderInput("confronto_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$confronto_cex_legenda)) 0.85 else input$confronto_cex_legenda, step = 0.05)),
                   column(6, selectInput("confronto_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$confronto_pos_legenda)) "topright" else input$confronto_pos_legenda))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "The continuous palettes (Viridis, Plasma, Spectral, ...) are especially useful with many models added, where a fixed set of 8 colors would start repeating. The single-color options (Black, Dark Grey, Red) give every model the same color: meant for \"Download Single Fits (Combined Image)\", where each model has its own panel \u2014 in the overlaid plot the models would become indistinguishable."),
                 fluidRow(
                   column(4, sliderInput("confronto_lwd_curve", "Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$confronto_lwd_curve)) 2.5 else input$confronto_lwd_curve, step = 0.5)),
                   column(4, sliderInput("confronto_cex_punti", "Points Size:", min = 0.3, max = 2.5, value = if(is.null(input$confronto_cex_punti)) 1 else input$confronto_cex_punti, step = 0.1)),
                   column(4, selectInput("confronto_pch_punti", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$confronto_pch_punti)) "16" else input$confronto_pch_punti))
                 ),
                 selectInput("confronto_lty_curve", "Curve Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$confronto_lty_curve)) "1" else input$confronto_lty_curve),
                 tags$h5(style = "margin-top: 10px;", "Display Range (leave blank/0 for automatic):"),
                 fluidRow(
                   column(3, numericInput("confronto_range_x_min", "X Min:", value = if (is.null(input$confronto_range_x_min)) 0 else input$confronto_range_x_min)),
                   column(3, numericInput("confronto_range_x_max", "X Max:", value = if (is.null(input$confronto_range_x_max)) 0 else input$confronto_range_x_max)),
                   column(3, numericInput("confronto_range_y_min", "Y Min:", value = if (is.null(input$confronto_range_y_min)) 0 else input$confronto_range_y_min)),
                   column(3, numericInput("confronto_range_y_max", "Y Max:", value = if (is.null(input$confronto_range_y_max)) 0 else input$confronto_range_y_max))
                 ),
                 checkboxInput("confronto_mostra_legenda", "Show Legend", value = if(is.null(input$confronto_mostra_legenda)) TRUE else input$confronto_mostra_legenda)
        )
  }
  ce_model_comparison_shelf_life <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Shelf-Life & Kinetic Model Analysis \u2192 Shelf-Life vs Temperature Plot."),
                 fluidRow(
                   column(4, selectInput("mc_sl_col_punti", "Points/Error Bars Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_col_punti)) "#2980b9" else input$mc_sl_col_punti)),
                   column(4, sliderInput("mc_sl_cex_punti", "Points Size:", min = 0.5, max = 2.5, value = if(is.null(input$mc_sl_cex_punti)) 1.3 else input$mc_sl_cex_punti, step = 0.1)),
                   column(4, selectInput("mc_sl_pch_punti", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$mc_sl_pch_punti)) "16" else input$mc_sl_pch_punti))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Confidence/Prediction Band visibility is controlled directly above the plot, not here."),
                 fluidRow(
                   column(4, selectInput("mc_sl_col_curva", "Fitted Curve Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_col_curva)) "#e74c3c" else input$mc_sl_col_curva)),
                   column(4, sliderInput("mc_sl_lwd_curva", "Fitted Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$mc_sl_lwd_curva)) 2.5 else input$mc_sl_lwd_curva, step = 0.5)),
                   column(4, selectInput("mc_sl_col_banda", "Confidence Band Color:", choices = c(
                     "Same as Curve" = "curva",
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$mc_sl_col_banda)) "curva" else input$mc_sl_col_banda))
                 ),
                 fluidRow(
                   column(4, sliderInput("mc_sl_alpha_banda", "Confidence Band Opacity:", min = 0.05, max = 0.6, value = if(is.null(input$mc_sl_alpha_banda)) 0.15 else input$mc_sl_alpha_banda, step = 0.05)),
                   column(4, sliderInput("mc_sl_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mc_sl_cex_legenda)) 0.8 else input$mc_sl_cex_legenda, step = 0.05)),
                   column(4, selectInput("mc_sl_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$mc_sl_pos_legenda)) "topright" else input$mc_sl_pos_legenda))
                 ),
                 checkboxInput("mc_sl_mostra_legenda", "Show Legend", value = if(is.null(input$mc_sl_mostra_legenda)) TRUE else input$mc_sl_mostra_legenda)
        )
  }
  ce_model_comparison_non_isothermal_secondary_model <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Non-Isothermal Shelf-Life Prediction \u2192 Secondary Model plot."),
                 fluidRow(
                   column(4, selectInput("mcni_pt_col_secondario", "Observed Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7"
                   ), selected = if(is.null(input$mcni_pt_col_secondario)) "#2c3e50" else input$mcni_pt_col_secondario)),
                   column(4, selectInput("mcni_ln_col_secondario", "Fitted Curve Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7"
                   ), selected = if(is.null(input$mcni_ln_col_secondario)) "#18bc9c" else input$mcni_ln_col_secondario)),
                   column(4, selectInput("mcni_pch_punti_secondario", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$mcni_pch_punti_secondario)) "16" else input$mcni_pch_punti_secondario))
                 ),
                 fluidRow(
                   column(4, sliderInput("mcni_cex_punti_secondario", "Points Size:", min = 0.5, max = 2.5, value = if(is.null(input$mcni_cex_punti_secondario)) 1.1 else input$mcni_cex_punti_secondario, step = 0.1)),
                   column(4, sliderInput("mcni_lwd_curva_secondario", "Fitted Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$mcni_lwd_curva_secondario)) 2 else input$mcni_lwd_curva_secondario, step = 0.5)),
                   column(4, selectInput("mcni_col_banda_secondario", "Confidence Band Color:", choices = c(
                     "Same as Curve" = "curva",
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Teal" = "#16a085", "Orange" = "#e67e22",
                     "Purple" = "#8e44ad", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_banda_secondario)) "curva" else input$mcni_col_banda_secondario))
                 ),
                 fluidRow(
                   column(4, sliderInput("mcni_alpha_banda_secondario", "Confidence Band Opacity:", min = 0.05, max = 0.6, value = if(is.null(input$mcni_alpha_banda_secondario)) 0.2 else input$mcni_alpha_banda_secondario, step = 0.05)),
                   column(4, sliderInput("mcni_cex_legenda_sec", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mcni_cex_legenda_sec)) 0.85 else input$mcni_cex_legenda_sec, step = 0.05)),
                   column(4, selectInput("mcni_pos_legenda_sec", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$mcni_pos_legenda_sec)) "topright" else input$mcni_pos_legenda_sec))
                 ),
                 checkboxInput("mcni_mostra_legenda_sec", "Show Legend", value = if(is.null(input$mcni_mostra_legenda_sec)) TRUE else input$mcni_mostra_legenda_sec)
        )
  }
  ce_model_comparison_non_isothermal_predictions <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Model Comparison \u2192 Non-Isothermal Shelf-Life Prediction \u2192 Temperature Profile, Degradation Curve, and Combined plots."),
                 fluidRow(
                   column(3, selectInput("mcni_col_T", "Temperature Line Color:", choices = c(
                     "Orange" = "#e67e22", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_T)) "#e67e22" else input$mcni_col_T)),
                   column(3, selectInput("mcni_col_C", "Degradation Curve Color:", choices = c(
                     "Teal" = "#18bc9c", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_C)) "#18bc9c" else input$mcni_col_C)),
                   column(3, selectInput("mcni_col_soglia", "Threshold Line Color:", choices = c(
                     "Red" = "#c0392b", "Blue" = "#2980b9", "Orange" = "#e67e22",
                     "Purple" = "#8e44ad", "Black" = "black", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_soglia)) "#c0392b" else input$mcni_col_soglia)),
                   column(3, selectInput("mcni_col_iso", "Isothermal Curve Color:", choices = c(
                     "Orange" = "#e67e22", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_iso)) "#e67e22" else input$mcni_col_iso))
                 ),
                 fluidRow(
                   column(3, selectInput("mcni_lty_T", "Temperature Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$mcni_lty_T)) "1" else input$mcni_lty_T)),
                   column(3, selectInput("mcni_lty_iso", "Isothermal Curve Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$mcni_lty_iso)) "2" else input$mcni_lty_iso)),
                   column(3, sliderInput("mcni_alpha_T", "Temperature Line Opacity:", min = 0.1, max = 1.0, value = if(is.null(input$mcni_alpha_T)) 1 else input$mcni_alpha_T, step = 0.05)),
                   column(3, sliderInput("mcni_lwd_linee", "Line Width (Temperature & Degradation):", min = 1, max = 5, value = if(is.null(input$mcni_lwd_linee)) 2 else input$mcni_lwd_linee, step = 0.5))
                 ),
                 fluidRow(
                   column(6, sliderInput("mcni_cex_legenda_prev", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mcni_cex_legenda_prev)) 0.85 else input$mcni_cex_legenda_prev, step = 0.05)),
                   column(6, selectInput("mcni_pos_legenda_prev", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$mcni_pos_legenda_prev)) "topright" else input$mcni_pos_legenda_prev))
                 ),
                 checkboxInput("mcni_mostra_legenda_prev", "Show Legend", value = if(is.null(input$mcni_mostra_legenda_prev)) TRUE else input$mcni_mostra_legenda_prev),
                 hr(),
                 tags$strong("Rate Constant k(t) plot (fully independent from the plots above):"),
                 fluidRow(style = "margin-top: 6px;",
                   column(4, selectInput("mcni_col_k", "Line Color:", choices = c(
                     "Teal" = "#18bc9c", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$mcni_col_k)) "#18bc9c" else input$mcni_col_k)),
                   column(4, selectInput("mcni_lty_k", "Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$mcni_lty_k)) "1" else input$mcni_lty_k)),
                   column(4, sliderInput("mcni_lwd_k", "Line Width:", min = 1, max = 5, value = if(is.null(input$mcni_lwd_k)) 2 else input$mcni_lwd_k, step = 0.5))
                 ),
                 fluidRow(
                   column(6, sliderInput("mcni_cex_legenda_k", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$mcni_cex_legenda_k)) 0.85 else input$mcni_cex_legenda_k, step = 0.05)),
                   column(6, selectInput("mcni_pos_legenda_k", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$mcni_pos_legenda_k)) "topright" else input$mcni_pos_legenda_k))
                 ),
                 checkboxInput("mcni_mostra_legenda_k", "Show Legend", value = if(is.null(input$mcni_mostra_legenda_k)) TRUE else input$mcni_mostra_legenda_k)
        )
  }
  ce_batch_main_plot <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Overlaid Fitted Curves."),
                 fluidRow(
                   column(8, selectInput("batch_palette", "Curve Color Palette (one color per sample):", choices = SCELTE_PALETTE_SERIE,
                                         selected = if(is.null(input$batch_palette)) "fissa" else input$batch_palette)),
                   column(4, selectInput("batch_col_punti", "Points Color:", choices = SCELTE_COLORE_PUNTI,
                                         selected = if(is.null(input$batch_col_punti)) "uguale" else input$batch_col_punti))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "The continuous palettes are especially useful with many samples in the batch, where a fixed set of 8 colors would start repeating. The single-color options (Black, Dark Grey, Red) give every sample the same color: meant for \"Download Single Fits (Combined Image)\", where each sample has its own panel \u2014 in the overlaid plot the samples would become indistinguishable."),
                 fluidRow(
                   column(4, sliderInput("batch_cex_punti", "Points Size:", min = 0.3, max = 2.5, value = if(is.null(input$batch_cex_punti)) 1 else input$batch_cex_punti, step = 0.1)),
                   column(4, sliderInput("batch_lwd_linee", "Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$batch_lwd_linee)) 2.5 else input$batch_lwd_linee, step = 0.5)),
                   column(4, selectInput("batch_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$batch_pos_legenda)) "topright" else input$batch_pos_legenda))
                 ),
                 fluidRow(
                   column(4, selectInput("batch_pch_punti", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$batch_pch_punti)) "16" else input$batch_pch_punti)),
                   column(4, selectInput("batch_lty_linee", "Curve Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$batch_lty_linee)) "1" else input$batch_lty_linee)),
                   column(4, br(), checkboxInput("batch_mostra_legenda", "Show Legend", value = if(is.null(input$batch_mostra_legenda)) TRUE else input$batch_mostra_legenda))
                 ),
                 sliderInput("batch_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$batch_cex_legenda)) 0.8 else input$batch_cex_legenda, step = 0.05),
                 tags$h5(style = "margin-top: 10px;", "Display Range (leave blank/0 for automatic):"),
                 fluidRow(
                   column(3, numericInput("batch_range_x_min", "X Min:", value = if (is.null(input$batch_range_x_min)) 0 else input$batch_range_x_min)),
                   column(3, numericInput("batch_range_x_max", "X Max:", value = if (is.null(input$batch_range_x_max)) 0 else input$batch_range_x_max)),
                   column(3, numericInput("batch_range_y_min", "Y Min:", value = if (is.null(input$batch_range_y_min)) 0 else input$batch_range_y_min)),
                   column(3, numericInput("batch_range_y_max", "Y Max:", value = if (is.null(input$batch_range_y_max)) 0 else input$batch_range_y_max))
                 )
        )
  }
  ce_batch_shelf_life_bar_chart <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Shelf Life Comparison (Linear/Jacobian Method)."),
                 fluidRow(
                   column(4, selectInput("sl_barre_col_barre", "Bar Fill Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_barre_col_barre)) "#f39c12" else input$sl_barre_col_barre)),
                   column(4, selectInput("sl_barre_col_bordo_barre", "Bar Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_barre_col_bordo_barre)) "black" else input$sl_barre_col_bordo_barre)),
                   column(4, sliderInput("sl_barre_fnt_cex_barre", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$sl_barre_fnt_cex_barre)) 0.8 else input$sl_barre_fnt_cex_barre, step = 0.05))
                 ),
                 checkboxInput("sl_barre_ordina_barre", "Sort bars ascending", value = if(is.null(input$sl_barre_ordina_barre)) FALSE else input$sl_barre_ordina_barre),
                 checkboxInput("sl_barre_mostra_titolo_x", "Show x-axis title (\"Sample\")", value = if(is.null(input$sl_barre_mostra_titolo_x)) TRUE else input$sl_barre_mostra_titolo_x)
        )
  }
  ce_batch_shelf_life_boxplots <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Shelf Life Distribution Across Samples."),
                 fluidRow(
                   column(4, selectInput("sl_box_tipo_grafico_box", "Plot Type:", choices = c("Boxplot" = "boxplot", "Violin Plot" = "violin"), selected = if(is.null(input$sl_box_tipo_grafico_box)) "boxplot" else input$sl_box_tipo_grafico_box)),
                   column(4, selectInput("sl_box_col_box", "Box Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_box_col_box)) "#f39c12" else input$sl_box_col_box)),
                   column(4, selectInput("sl_box_col_bordo_box", "Box Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_box_col_bordo_box)) "black" else input$sl_box_col_bordo_box))
                 ),
                 sliderInput("sl_box_fnt_cex_box", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$sl_box_fnt_cex_box)) 0.8 else input$sl_box_fnt_cex_box, step = 0.05),
                 fluidRow(
                   column(4, checkboxInput("sl_box_ordina_box", "Sort ascending by median", value = if(is.null(input$sl_box_ordina_box)) FALSE else input$sl_box_ordina_box)),
                   column(4, checkboxInput("sl_box_mostra_outlier_box", "Show outlier points (boxplot only)", value = if(is.null(input$sl_box_mostra_outlier_box)) TRUE else input$sl_box_mostra_outlier_box)),
                   column(4, checkboxInput("sl_box_mostra_titolo_x", "Show x-axis title (\"Sample\")", value = if(is.null(input$sl_box_mostra_titolo_x)) TRUE else input$sl_box_mostra_titolo_x))
                 )
        )
  }
  ce_batch_parameter_bar_charts <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Parameter Distributions (Linear/Jacobian Method)."),
                 fluidRow(
                   column(4, selectInput("batch_col_ist_par", "Bar Fill Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$batch_col_ist_par)) "#f39c12" else input$batch_col_ist_par)),
                   column(4, selectInput("batch_col_bordo_ist_par", "Bar Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$batch_col_bordo_ist_par)) "black" else input$batch_col_bordo_ist_par)),
                   column(4, sliderInput("batch_fnt_cex_ist_par", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$batch_fnt_cex_ist_par)) 0.8 else input$batch_fnt_cex_ist_par, step = 0.05))
                 ),
                 checkboxInput("batch_ordina_ist_par", "Sort bars ascending (per panel)", value = if(is.null(input$batch_ordina_ist_par)) FALSE else input$batch_ordina_ist_par),
                 checkboxInput("batch_mostra_titolo_x_ist_par", "Show x-axis title (\"Sample\")", value = if(is.null(input$batch_mostra_titolo_x_ist_par)) TRUE else input$batch_mostra_titolo_x_ist_par)
        )
  }
  ce_batch_parameter_boxplots <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Parameter Distributions (Monte Carlo)."),
                 fluidRow(
                   column(4, selectInput("batch_tipo_grafico_box_par", "Plot Type:", choices = c("Boxplot" = "boxplot", "Violin Plot" = "violin"), selected = if(is.null(input$batch_tipo_grafico_box_par)) "boxplot" else input$batch_tipo_grafico_box_par)),
                   column(4, selectInput("batch_col_box_par", "Box Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$batch_col_box_par)) "#f39c12" else input$batch_col_box_par)),
                   column(4, selectInput("batch_col_bordo_box_par", "Box Border Color:", choices = c(
                     "Black" = "black", "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$batch_col_bordo_box_par)) "black" else input$batch_col_bordo_box_par))
                 ),
                 sliderInput("batch_fnt_cex_box_par", "Axis Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$batch_fnt_cex_box_par)) 0.8 else input$batch_fnt_cex_box_par, step = 0.05),
                 fluidRow(
                   column(4, checkboxInput("batch_mostra_outlier_box_par", "Show outlier points (boxplot only)", value = if(is.null(input$batch_mostra_outlier_box_par)) TRUE else input$batch_mostra_outlier_box_par)),
                   column(4, checkboxInput("batch_ordina_box_par", "Sort boxes/violins ascending by median (per panel)", value = if(is.null(input$batch_ordina_box_par)) FALSE else input$batch_ordina_box_par)),
                   column(4, checkboxInput("batch_mostra_titolo_x_box_par", "Show x-axis title (\"Sample\")", value = if(is.null(input$batch_mostra_titolo_x_box_par)) TRUE else input$batch_mostra_titolo_x_box_par))
                 )
        )
  }
  ce_batch_shelf_life_kinetic_model <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Shelf-Life & Kinetic Model Analysis \u2192 Shelf-Life vs Temperature Plot."),
                 fluidRow(
                   column(4, selectInput("sl_col_punti", "Points/Error Bars Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_col_punti)) "#2980b9" else input$sl_col_punti)),
                   column(4, sliderInput("sl_cex_punti", "Points Size:", min = 0.5, max = 2.5, value = if(is.null(input$sl_cex_punti)) 1.3 else input$sl_cex_punti, step = 0.1)),
                   column(4, selectInput("sl_pch_punti", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$sl_pch_punti)) "16" else input$sl_pch_punti))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Confidence/Prediction Band visibility is controlled directly above the plot, not here."),
                 fluidRow(
                   column(4, selectInput("sl_col_curva", "Fitted Curve Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_col_curva)) "#e74c3c" else input$sl_col_curva)),
                   column(4, sliderInput("sl_lwd_curva", "Fitted Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$sl_lwd_curva)) 2.5 else input$sl_lwd_curva, step = 0.5)),
                   column(4, selectInput("sl_col_banda", "Confidence Band Color:", choices = c(
                     "Same as Curve" = "curva",
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7",
                     "Light Purple" = "#d7bde2", "Light Red" = "#fadbd8"
                   ), selected = if(is.null(input$sl_col_banda)) "curva" else input$sl_col_banda))
                 ),
                 fluidRow(
                   column(4, sliderInput("sl_alpha_banda", "Confidence Band Opacity:", min = 0.05, max = 0.6, value = if(is.null(input$sl_alpha_banda)) 0.15 else input$sl_alpha_banda, step = 0.05)),
                   column(4, sliderInput("sl_cex_legenda", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$sl_cex_legenda)) 0.8 else input$sl_cex_legenda, step = 0.05)),
                   column(4, selectInput("sl_pos_legenda", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$sl_pos_legenda)) "topright" else input$sl_pos_legenda))
                 ),
                 checkboxInput("sl_mostra_legenda", "Show Legend", value = if(is.null(input$sl_mostra_legenda)) TRUE else input$sl_mostra_legenda)
        )
  }
  ce_batch_non_isothermal_secondary_model <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Non-Isothermal Shelf-Life Prediction \u2192 Secondary Model plot."),
                 fluidRow(
                   column(4, selectInput("ni_pt_col_secondario", "Observed Points Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7"
                   ), selected = if(is.null(input$ni_pt_col_secondario)) "#2c3e50" else input$ni_pt_col_secondario)),
                   column(4, selectInput("ni_ln_col_secondario", "Fitted Curve Color:", choices = c(
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Sky Blue" = "#3498db", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Yellow" = "#f1c40f", "Purple" = "#8e44ad", "Pink" = "#e91e8c", "Brown" = "#795548",
                     "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6", "Slate Gray" = "#7f8c8d",
                     "Light Gray" = "#bdc3c7", "Pale Gray" = "#ecf0f1", "White" = "white",
                     "Light Blue" = "#aed6f1", "Light Green" = "#a9dfbf", "Light Orange" = "#f5cba7"
                   ), selected = if(is.null(input$ni_ln_col_secondario)) "#18bc9c" else input$ni_ln_col_secondario)),
                   column(4, selectInput("ni_pch_punti_secondario", "Points Symbol Shape:", choices = c("Solid Circle" = "16", "Open Circle" = "1", "Solid Square" = "15", "Solid Triangle" = "17", "Cross" = "4"), selected = if(is.null(input$ni_pch_punti_secondario)) "16" else input$ni_pch_punti_secondario))
                 ),
                 fluidRow(
                   column(4, sliderInput("ni_cex_punti_secondario", "Points Size:", min = 0.5, max = 2.5, value = if(is.null(input$ni_cex_punti_secondario)) 1.1 else input$ni_cex_punti_secondario, step = 0.1)),
                   column(4, sliderInput("ni_lwd_curva_secondario", "Fitted Curve Line Width:", min = 0.5, max = 10, value = if(is.null(input$ni_lwd_curva_secondario)) 2 else input$ni_lwd_curva_secondario, step = 0.5)),
                   column(4, selectInput("ni_col_banda_secondario", "Confidence Band Color:", choices = c(
                     "Same as Curve" = "curva",
                     "Red" = "#e74c3c", "Blue" = "#2980b9", "Teal" = "#16a085", "Orange" = "#e67e22",
                     "Purple" = "#8e44ad", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_banda_secondario)) "curva" else input$ni_col_banda_secondario))
                 ),
                 fluidRow(
                   column(4, sliderInput("ni_alpha_banda_secondario", "Confidence Band Opacity:", min = 0.05, max = 0.6, value = if(is.null(input$ni_alpha_banda_secondario)) 0.2 else input$ni_alpha_banda_secondario, step = 0.05)),
                   column(4, sliderInput("ni_cex_legenda_sec", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$ni_cex_legenda_sec)) 0.85 else input$ni_cex_legenda_sec, step = 0.05)),
                   column(4, selectInput("ni_pos_legenda_sec", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$ni_pos_legenda_sec)) "topright" else input$ni_pos_legenda_sec))
                 ),
                 checkboxInput("ni_mostra_legenda_sec", "Show Legend", value = if(is.null(input$ni_mostra_legenda_sec)) TRUE else input$ni_mostra_legenda_sec)
        )
  }
  ce_batch_non_isothermal_predictions <- function() {
    tagList(
                 br(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   tags$strong("Affects: "), "Batch Elaboration \u2192 Non-Isothermal Shelf-Life Prediction \u2192 Temperature Profile, Degradation Curve, and Combined plots."),
                 fluidRow(
                   column(3, selectInput("ni_col_T", "Temperature Line Color:", choices = c(
                     "Orange" = "#e67e22", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Teal" = "#16a085", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_T)) "#e67e22" else input$ni_col_T)),
                   column(3, selectInput("ni_col_C", "Degradation Curve Color:", choices = c(
                     "Teal" = "#18bc9c", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_C)) "#18bc9c" else input$ni_col_C)),
                   column(3, selectInput("ni_col_soglia", "Threshold Line Color:", choices = c(
                     "Red" = "#c0392b", "Blue" = "#2980b9", "Orange" = "#e67e22",
                     "Purple" = "#8e44ad", "Black" = "black", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_soglia)) "#c0392b" else input$ni_col_soglia)),
                   column(3, selectInput("ni_col_iso", "Isothermal Curve Color:", choices = c(
                     "Orange" = "#e67e22", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_iso)) "#e67e22" else input$ni_col_iso))
                 ),
                 fluidRow(
                   column(3, selectInput("ni_lty_T", "Temperature Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ni_lty_T)) "1" else input$ni_lty_T)),
                   column(3, selectInput("ni_lty_iso", "Isothermal Curve Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ni_lty_iso)) "2" else input$ni_lty_iso)),
                   column(3, sliderInput("ni_alpha_T", "Temperature Line Opacity:", min = 0.1, max = 1.0, value = if(is.null(input$ni_alpha_T)) 1 else input$ni_alpha_T, step = 0.05)),
                   column(3, sliderInput("ni_lwd_linee", "Line Width (Temperature & Degradation):", min = 1, max = 5, value = if(is.null(input$ni_lwd_linee)) 2 else input$ni_lwd_linee, step = 0.5))
                 ),
                 fluidRow(
                   column(6, sliderInput("ni_cex_legenda_prev", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$ni_cex_legenda_prev)) 0.85 else input$ni_cex_legenda_prev, step = 0.05)),
                   column(6, selectInput("ni_pos_legenda_prev", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$ni_pos_legenda_prev)) "topright" else input$ni_pos_legenda_prev))
                 ),
                 checkboxInput("ni_mostra_legenda_prev", "Show Legend", value = if(is.null(input$ni_mostra_legenda_prev)) TRUE else input$ni_mostra_legenda_prev),
                 hr(),
                 tags$strong("Rate Constant k(t) plot (fully independent from the plots above):"),
                 fluidRow(style = "margin-top: 6px;",
                   column(4, selectInput("ni_col_k", "Line Color:", choices = c(
                     "Teal" = "#18bc9c", "Red" = "#e74c3c", "Blue" = "#2980b9", "Navy" = "#005088",
                     "Dark Green" = "#27ae60", "Orange" = "#e67e22", "Amber" = "#f39c12",
                     "Purple" = "#8e44ad", "Black" = "black", "Dark Slate" = "#2c3e50", "Gray" = "#95a5a6"
                   ), selected = if(is.null(input$ni_col_k)) "#18bc9c" else input$ni_col_k)),
                   column(4, selectInput("ni_lty_k", "Line Type:", choices = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3", "Dot-dash" = "4"), selected = if(is.null(input$ni_lty_k)) "1" else input$ni_lty_k)),
                   column(4, sliderInput("ni_lwd_k", "Line Width:", min = 1, max = 5, value = if(is.null(input$ni_lwd_k)) 2 else input$ni_lwd_k, step = 0.5))
                 ),
                 fluidRow(
                   column(6, sliderInput("ni_cex_legenda_k", "Legend Text Size:", min = 0.5, max = 1.5, value = if(is.null(input$ni_cex_legenda_k)) 0.85 else input$ni_cex_legenda_k, step = 0.05)),
                   column(6, selectInput("ni_pos_legenda_k", "Legend Position:", choices = c("Top Right" = "topright", "Top Left" = "topleft", "Bottom Right" = "bottomright", "Bottom Left" = "bottomleft"), selected = if(is.null(input$ni_pos_legenda_k)) "topright" else input$ni_pos_legenda_k))
                 ),
                 checkboxInput("ni_mostra_legenda_k", "Show Legend", value = if(is.null(input$ni_mostra_legenda_k)) TRUE else input$ni_mostra_legenda_k)
        )
  }
    
    # Map original-plot-name -> category/tab/function: every
    # apri_estetica() call scattered throughout the app passes the ORIGINAL name of the
    # plot (e.g. "Simulator Plot") - translated here both into the
    # navigation references (category + tab id, for the "all" case) and into the
    # function to call directly (for the "only this
    # tab" case). No existing caller needs changes.
    mappa_categorie <- list(
        "Main Fit Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Main Fit", fn = ce_single_fit_main_fit),
        "Simulator Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Simulator", fn = ce_single_fit_simulator),
        "Residuals Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Residuals", fn = ce_single_fit_residuals),
        "Q-Q Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Q-Q Plot", fn = ce_single_fit_q_q_plot),
        "Distributions Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Distributions", fn = ce_single_fit_distributions),
        "Sensitivity Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Sensitivity", fn = ce_single_fit_sensitivity),
        "Parameter Pairs Plot" = list(cat = "Single Fit", id = "estetica_tabs_single_fit", nome = "Parameter Pairs", fn = ce_single_fit_parameter_pairs),
        "Loss Surface" = list(cat = "Grid Search", id = "estetica_tabs_grid_search", nome = "Loss Surface (2D/3D)", fn = ce_grid_search_loss_surface_2d_3d),
        "Loss Surface Profile Curve" = list(cat = "Grid Search", id = "estetica_tabs_grid_search", nome = "Profile Curve", fn = ce_grid_search_profile_curve),
        "Loss Surface t-Plot" = list(cat = "Grid Search", id = "estetica_tabs_grid_search", nome = "t-Plot", fn = ce_grid_search_t_plot),
        "Loss Surface Profile Trace" = list(cat = "Grid Search", id = "estetica_tabs_grid_search", nome = "Profile Trace", fn = ce_grid_search_profile_trace),
        "Loss Surface CI Comparison" = list(cat = "Grid Search", id = "estetica_tabs_grid_search", nome = "CI Comparison", fn = ce_grid_search_ci_comparison),
        "Batch Elaboration" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Main Plot", fn = ce_batch_main_plot),
        "Batch Shelf-Life Bar Chart" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Shelf-Life Bar Chart", fn = ce_batch_shelf_life_bar_chart),
        "Batch Shelf-Life Boxplots" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Shelf-Life Boxplots", fn = ce_batch_shelf_life_boxplots),
        "Batch Parameter Bar Charts" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Parameter Bar Charts", fn = ce_batch_parameter_bar_charts),
        "Batch Parameter Boxplots" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Parameter Boxplots", fn = ce_batch_parameter_boxplots),
        "Shelf-Life & Kinetic Model" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Shelf-Life & Kinetic Model", fn = ce_batch_shelf_life_kinetic_model),
        "Non-Isothermal: Secondary Model" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Non-Isothermal: Secondary Model", fn = ce_batch_non_isothermal_secondary_model),
        "Non-Isothermal: Prediction Plots" = list(cat = "Batch", id = "estetica_tabs_batch", nome = "Non-Isothermal: Predictions", fn = ce_batch_non_isothermal_predictions),
        "Model Comparison Non-Isothermal: Secondary Model" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Non-Isothermal: Secondary Model", fn = ce_model_comparison_non_isothermal_secondary_model),
        "Model Comparison Non-Isothermal: Predictions" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Non-Isothermal: Predictions", fn = ce_model_comparison_non_isothermal_predictions),
        "Model Comparison Shelf-Life Bar Chart" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Shelf-Life Bar Chart", fn = ce_model_comparison_shelf_life_bar_chart),
        "Model Comparison Shelf-Life Boxplots" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Shelf-Life Boxplots", fn = ce_model_comparison_shelf_life_boxplots),
        "Model Comparison Boxplots" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Boxplots", fn = ce_model_comparison_boxplots),
        "Overlaid Fitted Curves" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Overlaid Curves", fn = ce_model_comparison_overlaid_curves),
        "Model Comp. Shelf-Life" = list(cat = "Model Comparison", id = "estetica_tabs_model_comparison", nome = "Shelf-Life", fn = ce_model_comparison_shelf_life)
    )
    corrispondenza <- if (!is.null(tab_selezionata)) mappa_categorie[[tab_selezionata]] else NULL
    
    if (!is.null(corrispondenza)) {
      # Fast case (27 buttons out of 28): builds ONLY the requested
      # tab, not the whole panel - the title of the modal includes
      # the name of the tab for clarity, since here no tab bar
      # is visible that would already show it.
      showModal(modalDialog(
        title = div(icon("palette"), " Advanced Plot Aesthetics \u2014 ", corrispondenza$nome),
        size = "xl",
        corrispondenza$fn(),
      easyClose = TRUE,
      footer = tagList(
        actionButton("reset_estetica", "Reset All to Defaults", icon = icon("rotate-left"), class = "btn-outline-danger btn-sm"),
        modalButton("Close & Apply")
      )
      ))
    } else {
      # Full case (generic "Plot Aesthetics & Colors" button in the
      # sidebar, designed precisely to navigate freely among all the
      # categories): builds the whole panel with all ~25 tabs,
      # exactly as this function behaved before this
      # refactoring.
      showModal(modalDialog(
        title = div(icon("palette"), " Advanced Plot Aesthetics"),
        size = "xl", 
        tabsetPanel(
          id = "estetica_tabs",
          tabPanel("Parameter Names", ce_root_parameter_names()),
          tabPanel("Single Fit",
            tabsetPanel(
              id = "estetica_tabs_single_fit",
              tabPanel("Main Fit", ce_single_fit_main_fit()),
              tabPanel("Simulator", ce_single_fit_simulator()),
              tabPanel("Residuals", ce_single_fit_residuals()),
              tabPanel("Q-Q Plot", ce_single_fit_q_q_plot()),
              tabPanel("Distributions", ce_single_fit_distributions()),
              tabPanel("Sensitivity", ce_single_fit_sensitivity()),
              tabPanel("Parameter Pairs", ce_single_fit_parameter_pairs())
            )
          ),
          tabPanel("Grid Search",
            tabsetPanel(
              id = "estetica_tabs_grid_search",
              tabPanel("Loss Surface (2D/3D)", ce_grid_search_loss_surface_2d_3d()),
              tabPanel("Profile Curve", ce_grid_search_profile_curve()),
              tabPanel("t-Plot", ce_grid_search_t_plot()),
              tabPanel("Profile Trace", ce_grid_search_profile_trace()),
              tabPanel("CI Comparison", ce_grid_search_ci_comparison())
            )
          ),
          tabPanel("Model Comparison",
            tabsetPanel(
              id = "estetica_tabs_model_comparison",
              tabPanel("Shelf-Life Bar Chart", ce_model_comparison_shelf_life_bar_chart()),
              tabPanel("Shelf-Life Boxplots", ce_model_comparison_shelf_life_boxplots()),
              tabPanel("Boxplots", ce_model_comparison_boxplots()),
              tabPanel("Overlaid Curves", ce_model_comparison_overlaid_curves()),
              tabPanel("Shelf-Life", ce_model_comparison_shelf_life()),
              tabPanel("Non-Isothermal: Secondary Model", ce_model_comparison_non_isothermal_secondary_model()),
              tabPanel("Non-Isothermal: Predictions", ce_model_comparison_non_isothermal_predictions())
            )
          ),
          tabPanel("Batch",
            tabsetPanel(
              id = "estetica_tabs_batch",
              tabPanel("Main Plot", ce_batch_main_plot()),
              tabPanel("Shelf-Life Bar Chart", ce_batch_shelf_life_bar_chart()),
              tabPanel("Shelf-Life Boxplots", ce_batch_shelf_life_boxplots()),
              tabPanel("Parameter Bar Charts", ce_batch_parameter_bar_charts()),
              tabPanel("Parameter Boxplots", ce_batch_parameter_boxplots()),
              tabPanel("Shelf-Life & Kinetic Model", ce_batch_shelf_life_kinetic_model()),
              tabPanel("Non-Isothermal: Secondary Model", ce_batch_non_isothermal_secondary_model()),
              tabPanel("Non-Isothermal: Predictions", ce_batch_non_isothermal_predictions())
            )
          )
        ),
      easyClose = TRUE,
      footer = tagList(
        actionButton("reset_estetica", "Reset All to Defaults", icon = icon("rotate-left"), class = "btn-outline-danger btn-sm"),
        modalButton("Close & Apply")
      )
      ))
      if (!is.null(tab_selezionata)) {
        # "Parameter Names" or an unrecognized name - fallback to the
        # original behaviour (selects by direct name).
        updateTabsetPanel(session, "estetica_tabs", selected = tab_selezionata)
      }
    }
  }
  
  # Main button in the sidebar: opens the modal on the first tab (default)
  observeEvent(input$btn_estetica, { apri_estetica() })
  
  # "🎨" buttons next to each plot: open the modal directly on the
  # tab of that specific plot, instead of having to navigate among the tabs.
  observeEvent(input$btn_estetica_fit, { apri_estetica("Main Fit Plot") })
  observeEvent(input$btn_estetica_fit_inv, { apri_estetica("Main Fit Plot") })
  observeEvent(input$btn_estetica_sim, { apri_estetica("Simulator Plot") })
  observeEvent(input$btn_estetica_res, { apri_estetica("Residuals Plot") })
  observeEvent(input$btn_estetica_res_lag, { apri_estetica("Residuals Plot") })
  observeEvent(input$btn_estetica_qq_sd, { apri_estetica("Q-Q Plot") })
  observeEvent(input$btn_estetica_qq, { apri_estetica("Q-Q Plot") })
  observeEvent(input$btn_estetica_dist, { apri_estetica("Distributions Plot") })
  observeEvent(input$btn_estetica_distx, { apri_estetica("Distributions Plot") })
  observeEvent(input$btn_estetica_sensibilita, { apri_estetica("Sensitivity Plot") })
  observeEvent(input$btn_estetica_pairs, { apri_estetica("Parameter Pairs Plot") })
  observeEvent(input$btn_estetica_grid_3d, { apri_estetica("Loss Surface") })
  observeEvent(input$btn_estetica_grid_contour, { apri_estetica("Loss Surface") })
  observeEvent(input$btn_estetica_batch, { apri_estetica("Batch Elaboration") })
  observeEvent(input$btn_estetica_istogrammi_batch, { apri_estetica("Batch Parameter Bar Charts") })
  observeEvent(input$btn_estetica_boxplot_batch, { apri_estetica("Batch Parameter Boxplots") })
  observeEvent(input$btn_estetica_boxplot_sl, { apri_estetica("Batch Shelf-Life Boxplots") })
  observeEvent(input$btn_estetica_boxplot_mc_sl, { apri_estetica("Model Comparison Shelf-Life Boxplots") })
  observeEvent(input$btn_estetica_profilo_2d, { apri_estetica("Loss Surface Profile Curve") })
  observeEvent(input$btn_estetica_t_plot, { apri_estetica("Loss Surface t-Plot") })
  observeEvent(input$btn_estetica_profile_trace, { apri_estetica("Loss Surface Profile Trace") })
  observeEvent(input$btn_estetica_barre_ci_grid, { apri_estetica("Loss Surface CI Comparison") })
  observeEvent(input$btn_estetica_barre_sl, { apri_estetica("Batch Shelf-Life Bar Chart") })
  observeEvent(input$btn_estetica_barre_mc_sl, { apri_estetica("Model Comparison Shelf-Life Bar Chart") })
  observeEvent(input$btn_estetica_boxplot_confronto, { apri_estetica("Model Comparison Boxplots") })
  observeEvent(input$btn_estetica_sl, { apri_estetica("Shelf-Life & Kinetic Model") })
  observeEvent(input$btn_estetica_mc_sl, { apri_estetica("Model Comp. Shelf-Life") })
  observeEvent(input$btn_estetica_confronto, { apri_estetica("Overlaid Fitted Curves") })
  
  observeEvent(input$reset_estetica, {
    # Main Fit Plot
    updateSelectInput(session, "pt_col_fit", selected = "#005088")
    updateSelectInput(session, "ln_col_fit", selected = "#e74c3c")
    updateSliderInput(session, "pt_cex_fit", value = 1.5)
    updateSliderInput(session, "ln_lwd_fit", value = 3)
    updateSelectInput(session, "pt_pch_fit", selected = "16")
    updateSelectInput(session, "ln_lty_fit", selected = "1")
    updateSliderInput(session, "ci_alpha_fit", value = 0.3)
    updateSliderInput(session, "fnt_cex_fit", value = 1.0)
    updateSliderInput(session, "inv_cex_legenda", value = 0.9)
    # Simulator Plot
    updateSelectInput(session, "pt_col_sim", selected = "#bdc3c7")
    updateSelectInput(session, "ln_col_sim", selected = "#e74c3c")
    updateSliderInput(session, "pt_cex_sim", value = 1.4)
    updateSliderInput(session, "ln_lwd_sim", value = 3.5)
    updateCheckboxInput(session, "sim_mostra_legenda", value = TRUE)
    updateCheckboxInput(session, "sim_mostra_punti_dati", value = TRUE)
    updateSliderInput(session, "sim_cex_legenda", value = 1.0)
    updateSelectInput(session, "pt_pch_sim", selected = "16")
    updateSliderInput(session, "fnt_cex_sim", value = 1.0)
    # Residuals Plot
    updateSelectInput(session, "pt_col_res", selected = "#2c3e50")
    updateSelectInput(session, "ln_col_res", selected = "#e74c3c")
    updateSliderInput(session, "pt_cex_res", value = 1.3)
    updateSelectInput(session, "pt_pch_res", selected = "17")
    updateSelectInput(session, "ln_lty_res", selected = "2")
    updateSliderInput(session, "ln_lwd_res", value = 2)
    updateSliderInput(session, "fnt_cex_res", value = 1.0)
    updateNumericInput(session, "range_y_min_res", value = 0)
    updateNumericInput(session, "range_y_max_res", value = 0)
    # Q-Q Plot
    updateSelectInput(session, "pt_col_qq", selected = "#2c3e50")
    updateSelectInput(session, "ln_col_qq", selected = "#e74c3c")
    updateSliderInput(session, "pt_cex_qq", value = 1.3)
    updateSelectInput(session, "pt_pch_qq", selected = "17")
    updateSelectInput(session, "ln_lty_qq", selected = "2")
    updateSliderInput(session, "ln_lwd_qq", value = 2)
    updateSliderInput(session, "fnt_cex_qq", value = 1.0)
    updateNumericInput(session, "range_x_min_qq", value = 0)
    updateNumericInput(session, "range_x_max_qq", value = 0)
    updateNumericInput(session, "range_y_min_qq", value = 0)
    updateNumericInput(session, "range_y_max_qq", value = 0)
    # Distributions Plot
    updateSelectInput(session, "bar_col_dist", selected = "#f39c12")
    updateSliderInput(session, "n_bins_dist", value = 30)
    updateSelectInput(session, "dens_col_dist", selected = "black")
    updateSelectInput(session, "vln_col_dist", selected = "black")
    updateCheckboxInput(session, "mostra_densita_dist", value = TRUE)
    updateCheckboxInput(session, "mostra_vln_dist", value = TRUE)
    updateCheckboxInput(session, "mc_sl_box_mostra_titolo_x", value = TRUE)
    updateCheckboxInput(session, "mostra_titolo_x_boxplot_confronto", value = TRUE)
    updateCheckboxInput(session, "sl_box_mostra_titolo_x", value = TRUE)
    updateCheckboxInput(session, "batch_mostra_titolo_x_box_par", value = TRUE)
    updateCheckboxInput(session, "mc_sl_barre_mostra_titolo_x", value = TRUE)
    updateCheckboxInput(session, "sl_barre_mostra_titolo_x", value = TRUE)
    updateCheckboxInput(session, "batch_mostra_titolo_x_ist_par", value = TRUE)
    updateCheckboxInput(session, "grid_ci_mostra_titolo_x", value = TRUE)
    updateSelectInput(session, "vln_lty_dist", selected = "2")
    updateSliderInput(session, "vln_lwd_dist", value = 2)
    updateSliderInput(session, "fnt_cex_dist", value = 1.0)
    updateNumericInput(session, "range_x_min_distx", value = 0)
    updateNumericInput(session, "range_x_max_distx", value = 0)
    updateNumericInput(session, "range_y_min_distx", value = 0)
    updateNumericInput(session, "range_y_max_distx", value = 0)
    # Sensitivity Plot
    updateSliderInput(session, "ln_lwd_sens", value = 2.5)
    updateSelectInput(session, "sens_palette", selected = "fissa")
    updateSliderInput(session, "fnt_cex_sens", value = 1.0)
    updateCheckboxInput(session, "mostra_curva_sens", value = TRUE)
    updateCheckboxInput(session, "mostra_legenda_sens", value = TRUE)
    updateSliderInput(session, "sens_cex_legenda", value = 0.85)
    # Parameter Pairs Plot
    updateSelectInput(session, "pt_col_pairs", selected = "#2980b9")
    updateSliderInput(session, "pt_cex_pairs", value = 0.8)
    updateSliderInput(session, "pt_alpha_pairs", value = 0.4)
    updateSelectInput(session, "col_contour_pairs", selected = "#2c3e50")
    updateSelectInput(session, "lty_contour_pairs", selected = "1")
    updateSliderInput(session, "lwd_contour_pairs", value = 1)
    # Loss Surface
    updateSelectInput(session, "palette_superficie_grid", selected = "blu_rosso")
    updateSelectInput(session, "col_punti_grid_3d", selected = "black")
    updateSliderInput(session, "cex_punti_grid_3d", value = 0.75)
    updateSliderInput(session, "alpha_punti_grid_3d", value = 0.5)
    updateSliderInput(session, "ombra_grid", value = 0.4)
    updateSelectInput(session, "col_linee_grid", selected = "black")
    updateSliderInput(session, "n_livelli_grid", value = 15)
    updateSelectInput(session, "col_punto_ott_grid", selected = "#e74c3c")
    updateSelectInput(session, "col_regione_grid", selected = "#8e44ad")
    updateSelectInput(session, "pch_punti_grid_3d", selected = "16")
    updateSliderInput(session, "lwd_linee_grid", value = 1)
    updateSliderInput(session, "grid_cex_legenda", value = 0.8)
    updateSliderInput(session, "batch_cex_punti", value = 1)
    updateSliderInput(session, "batch_lwd_linee", value = 2.5)
    updateSelectInput(session, "batch_pos_legenda", selected = "topright")
    updateSelectInput(session, "batch_palette", selected = "fissa")
    updateSelectInput(session, "batch_col_punti", selected = "uguale")
    updateSelectInput(session, "batch_pch_punti", selected = "16")
    updateSelectInput(session, "batch_lty_linee", selected = "1")
    updateCheckboxInput(session, "batch_mostra_legenda", value = TRUE)
    updateSliderInput(session, "batch_cex_legenda", value = 0.8)
    updateSelectInput(session, "batch_col_ist_par", selected = "#f39c12")
    updateSelectInput(session, "batch_col_bordo_ist_par", selected = "black")
    updateSliderInput(session, "batch_fnt_cex_ist_par", value = 0.8)
    updateCheckboxInput(session, "sl_barre_ordina_barre", value = FALSE)
    updateSelectInput(session, "profilo2d_col_linea", selected = "black")
    updateSelectInput(session, "profilo2d_col_punto", selected = "#aed6f1")
    updateSelectInput(session, "profilo2d_pch_punto", selected = "16")
    updateSliderInput(session, "profilo2d_cex_punto", value = 0.6)
    updateSliderInput(session, "profilo2d_fnt_cex", value = 0.9)
    updateSliderInput(session, "profilo2d_lwd_linea", value = 1.5)
    updateCheckboxInput(session, "profilo2d_mostra_legenda", value = TRUE)
    updateSelectInput(session, "tplot_col_linea", selected = "black")
    updateSelectInput(session, "tplot_col_punto", selected = "#aed6f1")
    updateSelectInput(session, "tplot_col_retta", selected = "#7f8c8d")
    updateSelectInput(session, "tplot_pch_punto", selected = "16")
    updateSliderInput(session, "tplot_cex_punto", value = 0.6)
    updateSliderInput(session, "tplot_fnt_cex", value = 0.9)
    updateSliderInput(session, "tplot_lwd_linea", value = 1.5)
    updateCheckboxInput(session, "tplot_mostra_legenda", value = TRUE)
    updateCheckboxInput(session, "tplot_mostra_punti", value = TRUE)
    updateCheckboxInput(session, "tplot_mostra_soglia", value = TRUE)
    updateCheckboxInput(session, "tplot_zoom_soglia", value = FALSE)
    updateSliderInput(session, "tplot_zoom_margine_pct", value = 25)
    updateSelectInput(session, "ptrace_col1", selected = "#2980b9")
    updateSelectInput(session, "ptrace_col2", selected = "#e74c3c")
    updateSliderInput(session, "ptrace_lwd", value = 1.8)
    updateSliderInput(session, "ptrace_fnt_cex", value = 0.9)
    updateCheckboxInput(session, "ptrace_mostra_riferimento", value = TRUE)
    updateCheckboxInput(session, "ptrace_mostra_legenda", value = TRUE)
    updateCheckboxInput(session, "ptrace_zoom_ottimo", value = FALSE)
    updateCheckboxInput(session, "ptrace_valori_nominali", value = TRUE)
    updateSliderInput(session, "ptrace_zoom_ottimo_range", value = 5)
    updateSelectInput(session, "grid_ci_col_barre", selected = "#f39c12")
    updateSelectInput(session, "grid_ci_col_bordo", selected = "black")
    updateSliderInput(session, "grid_ci_fnt_cex", value = 0.9)
    updateSelectInput(session, "sl_barre_col_barre", selected = "#f39c12")
    updateSelectInput(session, "sl_barre_col_bordo_barre", selected = "black")
    updateSliderInput(session, "sl_barre_fnt_cex_barre", value = 0.8)
    updateCheckboxInput(session, "mc_sl_barre_ordina_barre", value = FALSE)
    updateSelectInput(session, "mc_sl_barre_col_barre", selected = "#f39c12")
    updateSelectInput(session, "mc_sl_barre_col_bordo_barre", selected = "black")
    updateSliderInput(session, "mc_sl_barre_fnt_cex_barre", value = 0.8)
    updateSelectInput(session, "sl_box_tipo_grafico_box", selected = "boxplot")
    updateCheckboxInput(session, "sl_box_ordina_box", value = FALSE)
    updateSelectInput(session, "sl_box_col_box", selected = "#f39c12")
    updateCheckboxInput(session, "sl_box_mostra_outlier_box", value = TRUE)
    updateSelectInput(session, "sl_box_col_bordo_box", selected = "black")
    updateSliderInput(session, "sl_box_fnt_cex_box", value = 0.8)
    updateSelectInput(session, "mc_sl_box_tipo_grafico_box", selected = "boxplot")
    updateCheckboxInput(session, "mc_sl_box_ordina_box", value = FALSE)
    updateSelectInput(session, "mc_sl_box_col_box", selected = "#f39c12")
    updateCheckboxInput(session, "mc_sl_box_mostra_outlier_box", value = TRUE)
    updateSelectInput(session, "mc_sl_box_col_bordo_box", selected = "black")
    updateSliderInput(session, "mc_sl_box_fnt_cex_box", value = 0.8)
    updateCheckboxInput(session, "batch_ordina_ist_par", value = FALSE)
    updateSelectInput(session, "batch_tipo_grafico_box_par", selected = "boxplot")
    updateSelectInput(session, "batch_col_box_par", selected = "#f39c12")
    updateCheckboxInput(session, "batch_mostra_outlier_box_par", value = TRUE)
    updateSelectInput(session, "batch_col_bordo_box_par", selected = "black")
    updateSliderInput(session, "batch_fnt_cex_box_par", value = 0.8)
    updateCheckboxInput(session, "batch_ordina_box_par", value = FALSE)
    updateNumericInput(session, "batch_range_x_min", value = 0)
    updateNumericInput(session, "batch_range_x_max", value = 0)
    updateNumericInput(session, "batch_range_y_min", value = 0)
    updateNumericInput(session, "batch_range_y_max", value = 0)
    updateSelectInput(session, "col_boxplot_confronto", selected = "#f39c12")
    updateCheckboxInput(session, "mostra_outlier_boxplot_confronto", value = TRUE)
    updateSelectInput(session, "col_bordo_boxplot_confronto", selected = "black")
    updateSelectInput(session, "tipo_grafico_boxplot_confronto", selected = "boxplot")
    updateCheckboxInput(session, "ordina_boxplot_confronto", value = FALSE)
    updateSliderInput(session, "fnt_cex_boxplot_confronto", value = 0.8)
    updateSelectInput(session, "sl_col_punti", selected = "#2980b9")
    updateSelectInput(session, "sl_col_curva", selected = "#e74c3c")
    updateSliderInput(session, "sl_cex_punti", value = 1.3)
    updateSliderInput(session, "sl_lwd_curva", value = 2.5)
    updateSelectInput(session, "sl_col_banda", selected = "curva")
    updateSliderInput(session, "sl_alpha_banda", value = 0.15)
    updateCheckboxInput(session, "sl_mostra_legenda", value = TRUE)
    updateSelectInput(session, "sl_pch_punti", selected = "16")
    updateSliderInput(session, "sl_cex_legenda", value = 0.8)
    updateSelectInput(session, "mc_sl_col_punti", selected = "#2980b9")
    updateSelectInput(session, "mc_sl_col_curva", selected = "#e74c3c")
    updateSliderInput(session, "mc_sl_cex_punti", value = 1.3)
    updateSliderInput(session, "mc_sl_lwd_curva", value = 2.5)
    updateSelectInput(session, "mc_sl_col_banda", selected = "curva")
    updateSliderInput(session, "mc_sl_alpha_banda", value = 0.15)
    updateCheckboxInput(session, "mc_sl_mostra_legenda", value = TRUE)
    updateSelectInput(session, "mc_sl_pch_punti", selected = "16")
    updateSliderInput(session, "mc_sl_cex_legenda", value = 0.8)
    updateSelectInput(session, "confronto_palette", selected = "fissa")
    updateSelectInput(session, "confronto_col_punti", selected = "uguale")
    updateCheckboxInput(session, "confronto_mostra_legenda", value = TRUE)
    updateSliderInput(session, "confronto_lwd_curve", value = 2.5)
    updateSliderInput(session, "confronto_cex_punti", value = 1)
    updateSelectInput(session, "confronto_pch_punti", selected = "16")
    updateSelectInput(session, "confronto_lty_curve", selected = "1")
    updateNumericInput(session, "confronto_range_x_min", value = 0)
    updateNumericInput(session, "confronto_range_x_max", value = 0)
    updateNumericInput(session, "confronto_range_y_min", value = 0)
    updateNumericInput(session, "confronto_range_y_max", value = 0)
    updateSliderInput(session, "confronto_cex_legenda", value = 0.85)
    # Legend positions (added in a later round, never included in the reset)
    updateSelectInput(session, "confronto_pos_legenda", selected = "topright")
    updateSelectInput(session, "grid_pos_legenda", selected = "topright")
    updateSelectInput(session, "inv_pos_legenda", selected = "bottomleft")
    updateSelectInput(session, "mc_sl_pos_legenda", selected = "topright")
    updateSelectInput(session, "profilo2d_pos_legenda", selected = "topright")
    updateSelectInput(session, "tplot_pos_legenda", selected = "topleft")
    updateSelectInput(session, "ptrace_pos_legenda", selected = "topleft")
    updateSelectInput(session, "sens_pos_legenda", selected = "topright")
    updateSelectInput(session, "sim_pos_legenda", selected = "topright")
    updateSelectInput(session, "sl_pos_legenda", selected = "topright")
    # Non-Isothermal (Batch)
    updateSelectInput(session, "ni_pt_col_secondario", selected = "#2c3e50")
    updateSelectInput(session, "ni_ln_col_secondario", selected = "#18bc9c")
    updateSelectInput(session, "ni_pch_punti_secondario", selected = "16")
    updateSliderInput(session, "ni_cex_punti_secondario", value = 1.1)
    updateSliderInput(session, "ni_lwd_curva_secondario", value = 2)
    updateSelectInput(session, "ni_col_banda_secondario", selected = "curva")
    updateSliderInput(session, "ni_alpha_banda_secondario", value = 0.2)
    updateCheckboxInput(session, "ni_mostra_legenda_sec", value = TRUE)
    updateSliderInput(session, "ni_cex_legenda_sec", value = 0.85)
    updateSelectInput(session, "ni_pos_legenda_sec", selected = "topright")
    updateSelectInput(session, "ni_col_T", selected = "#e67e22")
    updateSelectInput(session, "ni_col_C", selected = "#18bc9c")
    updateSelectInput(session, "ni_col_soglia", selected = "#c0392b")
    updateSelectInput(session, "ni_lty_T", selected = "1")
    updateSliderInput(session, "ni_alpha_T", value = 1)
    updateSliderInput(session, "ni_lwd_linee", value = 2)
    updateCheckboxInput(session, "ni_mostra_legenda_prev", value = TRUE)
    updateSliderInput(session, "ni_cex_legenda_prev", value = 0.85)
    updateSelectInput(session, "ni_pos_legenda_prev", selected = "topright")
    # Non-Isothermal (Model Comparison)
    updateSelectInput(session, "mcni_pt_col_secondario", selected = "#2c3e50")
    updateSelectInput(session, "mcni_ln_col_secondario", selected = "#18bc9c")
    updateSelectInput(session, "mcni_pch_punti_secondario", selected = "16")
    updateSliderInput(session, "mcni_cex_punti_secondario", value = 1.1)
    updateSliderInput(session, "mcni_lwd_curva_secondario", value = 2)
    updateSelectInput(session, "mcni_col_banda_secondario", selected = "curva")
    updateSliderInput(session, "mcni_alpha_banda_secondario", value = 0.2)
    updateCheckboxInput(session, "mcni_mostra_legenda_sec", value = TRUE)
    updateSliderInput(session, "mcni_cex_legenda_sec", value = 0.85)
    updateSelectInput(session, "mcni_pos_legenda_sec", selected = "topright")
    updateSelectInput(session, "mcni_col_T", selected = "#e67e22")
    updateSelectInput(session, "mcni_col_C", selected = "#18bc9c")
    updateSelectInput(session, "mcni_col_soglia", selected = "#c0392b")
    updateSelectInput(session, "mcni_lty_T", selected = "1")
    updateSliderInput(session, "mcni_alpha_T", value = 1)
    updateSliderInput(session, "mcni_lwd_linee", value = 2)
    updateCheckboxInput(session, "mcni_mostra_legenda_prev", value = TRUE)
    updateSliderInput(session, "mcni_cex_legenda_prev", value = 0.85)
    updateSelectInput(session, "mcni_pos_legenda_prev", selected = "topright")
    showNotification("All plot aesthetics reset to their default values.", type = "message")
  })
  
  observeEvent(input$clear_data, {
    updateTextAreaInput(session, "dati_manuali", value = "")
  })
  
  observeEvent(input$reset_app, {
    session$reload()
  })
  
  observeEvent(input$esegui, {
    # Exception: if we are already on Model Comparison, do not divert
    # to Single Fit Analysis - it often happens that several fits are launched in
    # a row precisely from there (especially now that Auto-add is active by
    # default) to build the comparison, and being sent elsewhere at
    # every click would be counterproductive for that workflow.
    if (!identical(input$main_tabs, "tab_confronto")) {
      updateTabsetPanel(session, "main_tabs", selected = "tab1")
    }
    # Clears the results of the Loss Surface Explorer: they refer to the
    # previous fit (optimal parameters, original data) - without this, after a
    # new Run of the main fit, results by then inconsistent with the source
    # would remain visible, without any warning.
    grid_stato(NULL)
  })
  
  observe({
    if (input$tipo_loss != "mse" && input$algoritmo == "LM") {
      updateSelectInput(session, "algoritmo", selected = "Nelder-Mead")
      showNotification("Levenberg-Marquardt algorithm requires standard MSE Loss. Automatically switched to Simplex (Nelder-Mead).", type = "warning")
    }
  })
  
  # ---- Convergence tolerance: default value and limits by algorithm ----
  # When the algorithm changes the field returns to the default of the new algorithm,
  # with its limits (a value suitable for one algorithm may not be suitable for
  # another). Exception: a value loaded from Operator Configuration
  # together with a different algorithm is "put on hold" and used here
  # instead of the default, otherwise the change of algorithm would erase it.
  tolleranza_in_attesa <- reactiveVal(NULL)
  observeEvent(input$algoritmo, {
    l <- limiti_tolleranza(input$algoritmo)
    attesa <- tolleranza_in_attesa()
    valore <- if (!is.null(attesa)) min(max(round(attesa), l$minimo), l$massimo) else l$predefinito
    tolleranza_in_attesa(NULL)
    precedente <- isolate(input$esponente_tolleranza)
    updateNumericInput(session, "esponente_tolleranza", value = valore, min = l$minimo, max = l$massimo)
    if (is.null(attesa) && !is.null(precedente) && isTRUE(is.finite(precedente)) && precedente != valore) {
      showNotification(sprintf("Convergence tolerance reset to 10^%d, the default for the selected algorithm.", valore), type = "message", duration = 5)
    }
  }, ignoreInit = TRUE)
  
  # Warnings under the field: value outside the limits, looser or tighter
  # than the default of the algorithm actually used.
  output$avviso_tolleranza <- renderUI({
    alg <- algoritmo_effettivo(input$algoritmo, input$tipo_loss)
    l <- limiti_tolleranza(alg)
    v <- input$esponente_tolleranza
    if (is.null(v) || !isTRUE(is.finite(v))) return(NULL)
    messaggio <- if (v < l$minimo || v > l$massimo) {
      sprintf("Outside the accepted range for this algorithm: 10^%d will be used.", esponente_tolleranza(v, alg))
    } else if (v > l$predefinito) {
      "Looser than the default: the fit may stop before the minimum, giving inaccurate estimates and standard errors without any obvious sign (NIST validation with Levenberg-Marquardt: 48 of 52 runs with at least 4 correct digits at the default 10^-12, 41 at 10^-8, 30 at 10^-6, 20 at 10^-4, and no fit reported as not converged)."
    } else if (v < l$predefinito) {
      "Tighter than the default: longer computation times (especially with Monte Carlo and Batch Elaboration) and more fits reported as not converged if Max Iterations is too low."
    } else return(NULL)
    div(style = "font-size: 11px; color: #d35400; margin-top: -6px; margin-bottom: 8px;", "\u26a0\ufe0f ", messaggio)
  })
  
  output$visualizza_formula_matematica <- renderUI({
    testo_html <- abbellisci_formula_html(input$tipo_modello)
    
    div(style = "background-color: #f0faf8; border-left: 4px solid #18bc9c; border-radius: 4px; padding: 10px 14px; margin-top: -5px; margin-bottom: 10px;",
        tags$em(style = "color: #148f76; font-size: 14.5px; line-height: 1.6; font-family: 'Cambria Math', Cambria, Georgia, serif; word-wrap: break-word; display: block; font-style: italic;",
                HTML(testo_html)))
  })
  
  # Removes the limits syntax "[min,max]" from a guess string,
  # leaving only "name=value" - used when the user disables the default
  # limits that we added to some known models for numerical
  # stability (e.g. MU>0 in Baranyi). It does not touch the "$value$" syntax for
  # fixed parameters, which is an explicit choice of the user, not ours.
  rimuovi_limiti_guess <- function(guess_string) {
    gsub("\\[[^]]*\\]", "", guess_string)
  }
  
  # Replaces, in an initial guess string, the VALUE of each estimated
  # parameter with the one given in 'valori', leaving unchanged the limits
  # "[min,max]", the fixed parameters "$value$" and the spaces. Same split
  # into parts as parse_guesses() (commas outside square brackets).
  # Returns list(testo, liberi): 'liberi' are the names of the non-fixed
  # parameters found in the string, for the consistency check with the fit.
  sostituisci_valori_guess <- function(testo, valori) {
    caratteri <- strsplit(testo, "")[[1]]
    parti <- character(0); buffer <- ""; profondita <- 0
    for (ch in caratteri) {
      if (ch == "[") profondita <- profondita + 1
      if (ch == "]") profondita <- profondita - 1
      if (ch == "," && profondita == 0) { parti <- c(parti, buffer); buffer <- "" } else buffer <- paste0(buffer, ch)
    }
    parti <- c(parti, buffer)
    liberi <- character(0)
    for (k in seq_along(parti)) {
      pos <- regexpr("=", parti[k], fixed = TRUE)
      if (pos == -1) next
      sinistra <- substr(parti[k], 1, pos)                      # leading spaces + name + "="
      nome <- trimws(substr(parti[k], 1, pos - 1))
      resto <- substr(parti[k], pos + 1, nchar(parti[k]))
      if (grepl("^\\s*\\$.*\\$\\s*$", resto)) next              # fixed parameter: unchanged
      liberi <- c(liberi, nome)
      if (!nome %in% names(valori)) next
      limiti <- regmatches(resto, regexpr("\\[.*\\]\\s*$", resto))  # "[min,max]" if present
      spazi_finali <- if (length(limiti)) "" else regmatches(resto, regexpr("\\s*$", resto))
      parti[k] <- paste0(sinistra, as.character(signif(valori[[nome]], 10)), if (length(limiti)) limiti else "", spazi_finali)
    }
    list(testo = paste(parti, collapse = ","), liberi = liberi)
  }
  
  # "Use Last Fit Results as Initial Guesses" button (4. Model Definition).
  observeEvent(input$usa_ultimo_fit_come_guess, {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    if (is.null(res) || !isTRUE(res$successo) || is.null(res$parametri)) {
      showNotification("No Single Fit results available yet: run \"Run Optimization Calculation\" in the Single Fit tab first.", type = "error", duration = 6)
      return()
    }
    risultato <- sostituisci_valori_guess(if (is.null(input$guess_iniziali)) "" else input$guess_iniziali, res$parametri)
    stimati <- names(res$parametri)
    if (!setequal(risultato$liberi, stimati)) {
      showNotification(paste0("The last Single Fit optimization used different parameters (", paste(stimati, collapse = ", "),
                              ") from the current initial guesses (", paste(risultato$liberi, collapse = ", "),
                              "). Run the optimization with the current model first."), type = "error", duration = 10)
      return()
    }
    updateTextInput(session, "guess_iniziali", value = risultato$testo)
    showNotification("Initial guesses updated with the results of the last Single Fit optimization (boundaries and fixed parameters kept).", type = "message", duration = 5)
  })
  
  observeEvent(input$tipo_modello, {
    guess_registry <- MODEL_REGISTRY[[input$tipo_modello]]$guess
    nuovo_guess <- if (isTRUE(input$applica_limiti_predefiniti)) guess_registry else rimuovi_limiti_guess(guess_registry)
    updateTextInput(session, "guess_iniziali", value = nuovo_guess)
    # If the selected model has preset display symbols
    # in the registry (e.g. the microbial growth models: N0->N[0],
    # M->mu[max], L->lambda), it applies them automatically - same logic
    # already used for resetting the initial guesses. If the model has none
    # (most of them), the map is simply emptied.
    simboli_preset <- MODEL_REGISTRY[[input$tipo_modello]]$simboli
    if (!is.null(simboli_preset)) {
      simboli_parametri(as.list(simboli_preset))
    } else {
      simboli_parametri(list())
    }
  })
  
  # Resynchronizes simboli_parametri() every time a NEW fit completes
  # successfully, not only when the dropdown changes - without this, if
  # the user changes model in the sidebar WITHOUT relaunching "Run Optimization
  # Calculation", every display based on the fit already computed (res:
  # parameter tables, correlation matrix, Grid Search selectors,
  # Batch/Model Comparison plots that reuse this same map)
  # would use the symbol mapping of the NEW selected model on
  # parameter names that still belong to the OLD fit - exactly the kind
  # of misalignment that caused "L" instead of "lambda" in the model
  # selection popup, here in its most general form.
  # [BUG FIX] Exactly the same problem already solved in model_comparison.R
  # for Auto-add: ignoreInit=TRUE on an observeEvent(elaborazione(), ...)
  # causes the FIRST successful fit of the session (the first real value
  # that elaborazione() produces, since it is an eventReactive whose button
  # starts at 0 - treated by Shiny as a "null event" in the same way as
  # NULL) to be mistaken for "the initial trigger to be ignored". Result:
  # after the very first fit of the session, simboli_parametri() stayed at the
  # default value instead of aligning with the model just fitted -
  # exactly the kind of "L instead of lambda" misalignment that this
  # block should prevent, but only from the second fit onwards.
  observeEvent(elaborazione(), {
    res <- tryCatch(elaborazione(), error = function(e) NULL)
    if (!is.null(res) && isTRUE(res$successo) && !is.null(res$tipo_modello_usato)) {
      simboli_preset_fit <- MODEL_REGISTRY[[res$tipo_modello_usato]]$simboli
      simboli_parametri(if (!is.null(simboli_preset_fit)) as.list(simboli_preset_fit) else list())
    }
  })
  
  # Reapplies/removes the limits on the guess ALREADY on screen when the user
  # changes the checkbox, without waiting for a new change of model - so
  # the effect is immediate even if the model is not reselected.
  observeEvent(input$applica_limiti_predefiniti, {
    guess_registry <- MODEL_REGISTRY[[input$tipo_modello]]$guess
    nuovo_guess <- if (isTRUE(input$applica_limiti_predefiniti)) guess_registry else rimuovi_limiti_guess(guess_registry)
    updateTextInput(session, "guess_iniziali", value = nuovo_guess)
  }, ignoreInit = TRUE)
  
  # ---- Synthetic Data (for quick exploration/teaching, not real data) ----
  # Uses the current model and default guess as "true" parameters,
  # evaluates the curve on a grid of X, and adds Gaussian noise
  # proportional to the amplitude of the curve. Updated ONLY when the
  # button is clicked (not at every slider change), so the dataset stays stable
  # while the controls are adjusted before generating.
  dati_sintetici <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  observeEvent(input$genera_dati_sintetici, {
    formula_stringa <- costruisci_formula(input$tipo_modello, input$formula_custom)
    guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) NULL)
    if (is.null(guesses_raw) || !valida_formula(formula_stringa, guesses_raw$tutti_nomi)) {
      showNotification("Cannot generate synthetic data: check the model/initial guesses in section 4 first.", type = "error")
      return()
    }
    x_seq <- seq(input$synth_x_min, input$synth_x_max, length.out = input$synth_n_points)
    valori_completi <- as.list(c(guesses_raw$guess, guesses_raw$fissi))
    y_pulito <- tryCatch(valuta_formula_generica(formula_stringa, valori_completi, x_seq), error = function(e) NULL)
    if (is.null(y_pulito) || any(!is.finite(y_pulito))) {
      showNotification("The current model/initial guesses don't produce a valid curve over this X range \u2014 try adjusting them.", type = "error")
      return()
    }
    ampiezza_y <- diff(range(y_pulito))
    if (ampiezza_y == 0) ampiezza_y <- max(abs(y_pulito), 1)
    # Unlike the slider (which physically constrained the value),
    # numericInput does not prevent typing a number out of range - here
    # we explicitly limit it to 0-50.
    rumore_pct <- if (is.null(input$synth_rumore_pct) || is.na(input$synth_rumore_pct)) 5 else min(max(input$synth_rumore_pct, 0), 30)
    rumore_sd <- ampiezza_y * (rumore_pct / 100)
    
    # Simulates the loss of some intermediate experimental X points (common in
    # real data: a lost sample, an invalid reading, etc.) - the
    # first and the last point are never removed, since they define
    # the sampling range itself (X Min/X Max), not a "lost" value
    # within it. Applied to the UNIQUE X values, BEFORE generating the
    # replicates below: removing an X point thus deletes all its
    # replicates together, not one row at a time. A minimum threshold of
    # X points is always kept, so as not to leave too few data
    # for the subsequent fit.
    dropout_pct <- if (is.null(input$synth_dropout_pct) || is.na(input$synth_dropout_pct)) 0 else min(max(input$synth_dropout_pct, 0), 80)
    if (dropout_pct > 0 && length(x_seq) > 3) {
      indici_rimovibili <- 2:(length(x_seq) - 1)
      n_min_da_mantenere <- 3
      n_massimo_rimovibile <- max(0, length(x_seq) - n_min_da_mantenere)
      n_da_rimuovere <- min(round(length(x_seq) * dropout_pct / 100), n_massimo_rimovibile, length(indici_rimovibili))
      if (n_da_rimuovere > 0) {
        indici_da_rimuovere <- sample(indici_rimovibili, n_da_rimuovere)
        x_seq <- x_seq[-indici_da_rimuovere]
        y_pulito <- y_pulito[-indici_da_rimuovere]
      }
    }
    
    # Replicates: each remaining X value is repeated n_repliche times,
    # each with its own independent random error - simulates repeated
    # measurements at the same experimental point (e.g. laboratory
    # replicates), not a single sample per point. each= (not times=)
    # groups the replicates of the same X consecutively in the resulting
    # table, instead of alternating them in blocks.
    n_repliche <- if (is.null(input$synth_n_repliche) || is.na(input$synth_n_repliche)) 1 else min(max(round(input$synth_n_repliche), 1), 20)
    x_finale <- rep(x_seq, each = n_repliche)
    y_pulito_finale <- rep(y_pulito, each = n_repliche)
    y_rumoroso <- y_pulito_finale + rnorm(length(x_finale), mean = 0, sd = rumore_sd)
    
    dati_sintetici(data.frame(x = round(x_finale, 5), y = round(y_rumoroso, 5)))
    messaggio_punti <- paste0(length(x_seq), " X points", if (n_repliche > 1) paste0(" \u00d7 ", n_repliche, " replicates = ", length(x_finale), " rows") else "")
    showNotification(paste0("Synthetic data generated (", messaggio_punti, ", ", rumore_pct, "% noise", if (dropout_pct > 0) paste0(", ", dropout_pct, "% missing X points") else "", ")."), type = "message")
  })
  
  # Automatically sets the suggested model (Choose Model) when a
  # reference dataset is chosen - both when the dataset is changed from the menu,
  # and when switching to the "riferimento" method with the dataset already selected
  # by default (otherwise that first case would not trigger anything,
  # since the value of the selectInput would not change).
  # Auto-fills the X range for "Generate Synthetic Data" when a
  # statistical distribution with a specific, known domain is chosen - the generic X
  # range (default 1-10, designed for growth/kinetic curves) does not
  # respect the domain of these distributions: Beta requires 0<x<1
  # (outside it produces absurd values, not a true probability
  # density), Binomial/Poisson are discrete (x should be an
  # integer, a generic continuous range does not guarantee it). The number of
  # points is chosen on purpose to give a step of exactly 1 in the discrete ones
  # (e.g. seq(0,10,length.out=11) = 0,1,2,...,10).
  observeEvent(input$tipo_modello, {
    parametri_distribuzione <- list(
      "beta_dist" = list(min = 0.01, max = 0.99, punti = 15),
      "poisson_dist" = list(min = 0, max = 15, punti = 16),
      # Sorption isotherms: x = water activity (aw), domain (0,1)
      # by definition - many of these formulas have terms Ln(1-x) or
      # Ln(x), which would break down exactly at the edges 0/1 (hence the
      # margins 0.05-0.95 instead of a sharp 0-1).
      "gab" = list(min = 0.05, max = 0.95, punti = 15),
      "bet" = list(min = 0.05, max = 0.95, punti = 15),
      "langmuir_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "halsey_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "smith_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "henderson_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "oswin_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "ferro_fontan_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      "peleg_isoterma" = list(min = 0.05, max = 0.95, punti = 15),
      # Herschel-Bulkley: x = shear rate, which in a real rheological flow
      # curve typically starts from zero - the generic default
      # (X Min=1) excludes this point, which was seen to noticeably improve
      # the quality of the fit for this specific model.
      "herschel_bulkley" = list(min = 0, max = 10, punti = 15),
      # Arrhenius: x = absolute temperature in Kelvin - the generic
      # default (1-10) would be physically meaningless (close to absolute
      # zero), I would use instead a realistic range of 0-100 degrees Celsius.
      "arrhenius" = list(min = 273, max = 373, punti = 15)
    )
    info <- parametri_distribuzione[[input$tipo_modello]]
    if (is.null(info)) return()
    updateNumericInput(session, "synth_x_min", value = info$min)
    updateNumericInput(session, "synth_x_max", value = info$max)
    updateNumericInput(session, "synth_n_points", value = info$punti)
    showNotification(paste0("X range auto-set to this distribution's domain (", info$min, " to ", info$max, ") \u2014 the generic default doesn't fit it."), type = "message", duration = 6)
  }, ignoreInit = TRUE)
  
  # NOTE: previously there was an observeEvent here that automatically changed
  # the model (input$tipo_modello) based on
  # d$modello_suggerito every time a reference dataset
  # was chosen - removed on explicit request: the reference
  # datasets now only provide the data (x,y) and a note with the
  # values certified/published by the original source, without imposing or
  # suggesting any model or automatic reparameterization.
  
  output$info_dataset_riferimento <- renderUI({
    d <- DATASET_RIFERIMENTO[[input$dataset_riferimento_scelto]]
    if (is.null(d)) return(NULL)
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; padding: 8px 12px; border-radius: 6px; font-size: 11px; color: #21618c; margin-bottom: 8px;",
        tags$strong("Source: "), d$fonte, tags$br(), tags$br(),
        d$nota)
  })
  
  # Table of the raw data of the chosen reference dataset - read-
  # only (unlike Direct Input, which is an editable text
  # box: here the values are certified/published, not meant
  # to be altered), but it still allows seeing immediately what has
  # been loaded before launching the fit.
  output$tabella_dataset_riferimento <- renderTable({
    d <- DATASET_RIFERIMENTO[[input$dataset_riferimento_scelto]]
    if (is.null(d)) return(NULL)
    data.frame(x = d$x, y = d$y)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 4)
  
  dati_grezzi <- reactive({
    if (input$metodo_dati == "sintetico") {
      return(dati_sintetici())
    } else if (input$metodo_dati == "riferimento") {
      d <- DATASET_RIFERIMENTO[[input$dataset_riferimento_scelto]]
      if (is.null(d)) return(data.frame(x = numeric(0), y = numeric(0)))
      return(data.frame(x = d$x, y = d$y))
    } else if (input$metodo_dati == "csv") {
      infile <- input$file_dati
      if (is.null(infile)) return(data.frame(x = seq(1,6), y = 10*exp(-0.5*seq(1,6))))
      # Same logic already used in Batch Elaboration: detects the extension and
      # uses readxl for Excel files, read.csv otherwise - in both
      # cases with the same automatic detection of the header (first row
      # entirely numeric = no header).
      df_csv <- if (grepl("\\.(xlsx|xls)$", infile$name, ignore.case = TRUE)) {
        leggi_excel_auto_header(infile$datapath)
      } else {
        leggi_csv_auto_header(infile$datapath)
      }
      if (ncol(df_csv) < 2) {
        showNotification("The uploaded file must contain at least two columns (x and y).", type = "error")
        return(data.frame(x = numeric(0), y = numeric(0)))
      }
      indici_xy <- trova_colonne_xy_numeriche(df_csv)
      if (is.null(indici_xy)) {
        showNotification("Couldn't find two consecutive numeric columns to use as X and Y. If your file uses a comma as the decimal separator (e.g. \"1,5\"), make sure columns are separated by semicolons (;), not commas \u2014 that's auto-detected. Otherwise, check the file for stray text or symbols in the data columns.", type = "error", duration = 12)
        return(data.frame(x = numeric(0), y = numeric(0)))
      }
      if (!identical(indici_xy, c(1L, 2L))) {
        showNotification(paste0("Used columns \"", colnames(df_csv)[indici_xy[1]], "\" and \"", colnames(df_csv)[indici_xy[2]], "\" as X and Y (the first column(s) weren't numeric)."), type = "message", duration = 6)
      }
      df_xy <- df_csv[, indici_xy]
      colnames(df_xy) <- c("x", "y")
      x_num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df_xy$x), fixed = TRUE)))
      y_num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df_xy$y), fixed = TRUE)))
      if (all(is.na(x_num)) || all(is.na(y_num))) {
        showNotification("Couldn't read the X/Y columns as numbers. If your file uses a comma as the decimal separator (e.g. \"1,5\"), make sure columns are separated by semicolons (;), not commas \u2014 that's auto-detected. Otherwise, check the file for stray text or symbols in the data columns.", type = "error", duration = 12)
        return(data.frame(x = numeric(0), y = numeric(0)))
      }
      return(data.frame(x = x_num, y = y_num))
    } else {
      testo <- input$dati_manuali
      if (trimws(testo) == "") return(data.frame(x = numeric(0), y = numeric(0)))
      righe <- read.table(text = testo, header = FALSE, col.names = c("x", "y"), fill = TRUE,
                          colClasses = "character")
      # Normalizes comma as decimal separator (common when pasting from
      # Excel under an Italian locale), then explicitly converts to numeric.
      # Without this step, a single non-numeric cell would cause the whole
      # column to be read as text, and the is.finite() filter would silently
      # drop ALL rows (no error, no warning, just an empty plot with no
      # explanation).
      x_num <- suppressWarnings(as.numeric(gsub(",", ".", righe$x, fixed = TRUE)))
      y_num <- suppressWarnings(as.numeric(gsub(",", ".", righe$y, fixed = TRUE)))
      righe_valide <- is.finite(x_num) & is.finite(y_num)
      n_scartate <- sum(!righe_valide)
      if (n_scartate > 0) {
        showNotification(sprintf("%d row(s) with non-numeric or incomplete values were ignored.", n_scartate), type = "warning")
      }
      return(data.frame(x = x_num[righe_valide], y = y_num[righe_valide]))
    }
  })
  
  # Wraps dati_grezzi() applying the chosen Y pre-treatment (if
  # any) - a single point of application, independent of which branch
  # of dati_grezzi() produced the result, so every downstream consumer
  # (fit, plots, residuals, CI/PI, export) already sees the transformed
  # data without needing to know it.
  dati <- reactive({
    df_base <- dati_grezzi()
    # "Swap X and Y": only for data entered or uploaded by the user. The
    # synthetic data (generated by the model) and the reference datasets (NIST,
    # ComBase) are already in the correct orientation: swapping them would
    # make them wrong, even if the box had remained ticked.
    if (isTRUE(input$inverti_xy) && input$metodo_dati %in% c("manuale", "csv")) df_base <- inverti_colonne_xy(df_base)
    df_trasformato <- applica_trasformazione_y_su_df(df_base, input$trasformazione_y)
    df_densita <- converti_a_densita(df_trasformato, input$trasformazione_densita, input$densita_media_bin,
                       input$densita_confine_inf_modo, input$densita_confine_inf_valore,
                       input$densita_confine_sup_modo, input$densita_confine_sup_valore)
    aggiungi_punti_ancora(df_densita, input$densita_ancora_zero, input$densita_ancora_max, input$densita_ancora_max_valore)
  })
  
  observeEvent(dati(), {
    df_correnti <- dati()
    if (nrow(df_correnti) > 0) {
      updateNumericInput(session, "x_search_min_inversa", value = round(min(df_correnti$x, na.rm = TRUE), 4))
      updateNumericInput(session, "x_search_max_inversa", value = round(max(df_correnti$x, na.rm = TRUE), 4))
    }
  }, ignoreInit = FALSE)
  
  parse_guesses <- function(testo) {
    # Top-level split on commas, ignoring commas inside [ ] (used for boundaries)
    caratteri <- strsplit(testo, "")[[1]]
    parti <- character(0)
    buffer <- ""
    profondita <- 0
    for (ch in caratteri) {
      if (ch == "[") profondita <- profondita + 1
      if (ch == "]") profondita <- profondita - 1
      if (ch == "," && profondita == 0) {
        parti <- c(parti, buffer)
        buffer <- ""
      } else {
        buffer <- paste0(buffer, ch)
      }
    }
    if (nchar(trimws(buffer)) > 0) parti <- c(parti, buffer)
    
    vettore <- c()
    lower_v <- c()
    upper_v <- c()
    fissi_v <- c()
    ordine_nomi <- character(0)
    
    for (p in parti) {
      p_trim <- trimws(p)
      if (nchar(p_trim) == 0) next
      
      eq_pos <- regexpr("=", p_trim, fixed = TRUE)
      if (eq_pos == -1) next
      nome_par <- trimws(substr(p_trim, 1, eq_pos - 1))
      resto <- trimws(substr(p_trim, eq_pos + 1, nchar(p_trim)))
      ordine_nomi <- c(ordine_nomi, nome_par)
      
      # ---- FIXED parameter: syntax N0=$7.0125$ (value between two dollar signs) ----
      fisso_match <- regmatches(resto, regexec("^\\$(.+)\\$\\s*$", resto))[[1]]
      if (length(fisso_match) == 2) {
        valore_fisso <- suppressWarnings(as.numeric(trimws(fisso_match[2])))
        if (is.na(valore_fisso)) stop(paste0("Invalid numeric value for fixed parameter '", nome_par, "': '", trimws(fisso_match[2]), "'"))
        fissi_v[nome_par] <- valore_fisso
        next
      }
      
      bracket_match <- regmatches(resto, regexec("^(.*?)\\[(.*)\\]\\s*$", resto))[[1]]
      
      if (length(bracket_match) == 3) {
        valore_testo <- trimws(bracket_match[2])
        bounds_testo <- bracket_match[3]
        virgola_pos <- regexpr(",", bounds_testo, fixed = TRUE)
        if (virgola_pos == -1) stop(paste0("Invalid boundary syntax for parameter '", nome_par, "'. Use the format: A=10[min,max]"))
        testo_lower <- trimws(substr(bounds_testo, 1, virgola_pos - 1))
        testo_upper <- trimws(substr(bounds_testo, virgola_pos + 1, nchar(bounds_testo)))
        if (grepl(",", testo_upper, fixed = TRUE)) stop(paste0("Invalid boundary syntax for parameter '", nome_par, "'. Use the format: A=10[min,max]"))
      } else {
        valore_testo <- resto
        testo_lower <- ""
        testo_upper <- ""
      }
      
      valore_par <- suppressWarnings(as.numeric(valore_testo))
      if (is.na(valore_par)) stop(paste0("Invalid numeric value for parameter '", nome_par, "': '", valore_testo, "'"))
      
      val_lower <- if (nchar(testo_lower) == 0) -Inf else suppressWarnings(as.numeric(testo_lower))
      val_upper <- if (nchar(testo_upper) == 0) Inf else suppressWarnings(as.numeric(testo_upper))
      if (is.na(val_lower)) stop(paste0("Invalid lower boundary for parameter '", nome_par, "'"))
      if (is.na(val_upper)) stop(paste0("Invalid upper boundary for parameter '", nome_par, "'"))
      if (val_lower > val_upper) stop(paste0("Lower boundary is greater than upper boundary for parameter '", nome_par, "'"))
      if (valore_par < val_lower || valore_par > val_upper) stop(paste0("Initial guess for '", nome_par, "' (", valore_par, ") lies outside its specified boundaries [", val_lower, ", ", val_upper, "]"))
      
      vettore[nome_par] <- valore_par
      lower_v[nome_par] <- val_lower
      upper_v[nome_par] <- val_upper
    }
    if (length(vettore) == 0 && length(fissi_v) == 0) stop("No valid initial guesses were parsed. Use the format: A=10, B=0.5 (optionally A=10[min,max], or A=$10$ to fix A)")
    if (length(vettore) == 0) stop("At least one free (non-fixed) parameter is required for the optimization to run.")
    if (any(duplicated(ordine_nomi))) stop(paste0("Duplicate parameter name(s) found: ", paste(unique(ordine_nomi[duplicated(ordine_nomi)]), collapse = ", ")))
    return(list(guess = vettore, lower = lower_v, upper = upper_v, fissi = fissi_v, tutti_nomi = ordine_nomi))
  }
  
  # Textual reminder that translates the raw names typeable in the
  # Initial Guesses field (e.g. "L", "MU") into the symbols shown in the formula of the
  # sidebar (e.g. "lambda", "mu[max]") - without this, a user might
  # not associate "L" with lambda just by looking at the formatted formula.
  # Reads the CURRENT content of the field (not a static reference to the
  # registry), so it stays consistent even if the user edits the text
  # by hand. Shows only the parameters that REALLY have a special symbol
  # (different from their raw name) - if none has one, it shows nothing.
  output$legenda_simboli_guess <- renderUI({
    guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) NULL)
    if (is.null(guesses_raw) || length(guesses_raw$tutti_nomi) == 0) return(NULL)
    coppie <- Filter(Negate(is.null), lapply(guesses_raw$tutti_nomi, function(nome) {
      simbolo <- simbolo_visualizzazione(nome)
      if (identical(simbolo, nome)) return(NULL)
      paste0(nome, " = ", simbolo_a_testo_semplice(simbolo))
    }))
    if (length(coppie) == 0) return(NULL)
    p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px; margin-bottom: 8px; font-style: italic;",
      icon("circle-info"), " ", paste(unlist(coppie), collapse = "   \u00b7   "))
  })
  
  valida_formula <- function(testo, nomi_parametri) {
    espr <- tryCatch(parse(text = testo), error = function(e) NULL)
    if (is.null(espr) || length(espr) != 1) return(FALSE)
    nomi_usati <- all.names(espr)
    funzioni_consentite <- c("+", "-", "*", "/", "^", "(",
                             "exp", "log", "log10", "log2", "sqrt", "abs", "pi",
                             "sin", "cos", "tan", "asin", "acos", "atan",
                             "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
                             "gamma", "lgamma", "beta", "factorial", "choose")
    whitelist <- c(funzioni_consentite, "x", nomi_parametri)
    all(nomi_usati %in% whitelist)
  }
  
  costruisci_formula <- function(tipo_modello, formula_custom_utente) {
    if (tipo_modello == "custom") return(formula_custom_utente)
    if (is.null(MODEL_REGISTRY[[tipo_modello]])) stop(paste0("Unknown model type: '", tipo_modello, "' (not found in MODEL_REGISTRY)."))
    MODEL_REGISTRY[[tipo_modello]]$formula
  }
  
  range_personalizzato_attivo <- function(valore_min, valore_max) {
    !is.null(valore_min) && !is.null(valore_max) && is.finite(valore_min) && is.finite(valore_max) && valore_max > valore_min
  }
  
  calcola_limiti <- function(valore_min, valore_max, valori_dati) {
    if (range_personalizzato_attivo(valore_min, valore_max)) {
      c(valore_min, valore_max)
    } else {
      range(valori_dati[is.finite(valori_dati)], na.rm = TRUE)
    }
  }
  
  # Step for numerical derivatives (central finite differences), scaled to
  # the magnitude of the perturbed value instead of a fixed absolute step:
  # with parameters on very different scales (e.g. k0~1e10 in Arrhenius and
  # Ea~1e4), a small fixed step (e.g. 1e-5) can fall below numerical
  # precision for the large parameters, giving inaccurate derivatives.
  passo_derivata <- function(valore) {
    1e-6 * max(abs(valore), 1e-8)
  }
  
  # Interprets a label using R's plotmath syntax (Greek letters, superscripts,
  # subscripts: e.g. "mu[max]", "sigma^2", "Delta*T") if it's valid R syntax;
  # otherwise falls back to the plain text exactly as typed. Fully backward
  # compatible: a label like "Time (days)" isn't valid R syntax, so it just
  # returns the original text unchanged.
  interpreta_etichetta <- function(testo) {
    if (is.null(testo) || nchar(trimws(testo)) == 0) return(testo)
    espr <- tryCatch(str2expression(testo), error = function(e) NULL)
    if (!is.null(espr) && length(espr) == 1) espr[[1]] else testo
  }
  
  # Critical value for confidence/prediction intervals: uses Student's t with
  # the given residual degrees of freedom when available and valid (matching
  # R's own nls()/confint() convention for non-linear regression), which
  # automatically converges to the normal/Z critical value as df grows large
  # - no need to explicitly branch on sample size. Falls back to the normal
  # (Z) critical value only if df isn't available/valid (e.g. before any fit
  # has run, or a degenerate edge case).
  valore_critico <- function(df = NULL) {
    area_due_code <- 1 - (1 - input$livello_confidenza / 100) / 2
    if (!is.null(df) && is.finite(df) && df > 0) qt(area_due_code, df = df) else qnorm(area_due_code)
  }
  
  # Skewness and excess kurtosis: unlike the p-value of
  # a normality test (which always tends to "significant" as the
  # number of samples grows, even for tiny deviations), these are
  # effect-size measures - they do not depend on the sample size, and
  # they say HOW MUCH the distribution departs from the normal, not only WHETHER.
  # Standard moment formulas (g1, g2); no additional package
  # required, to avoid the same compatibility problems with
  # Shinylive/webR already encountered with other packages (e.g. deSolve).
  calcola_skewness <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 3) return(NA_real_)
    m <- mean(x)
    m2 <- mean((x - m)^2)
    m3 <- mean((x - m)^3)
    if (m2 <= 0) return(NA_real_)
    m3 / (m2^1.5)
  }
  
  calcola_kurtosi_eccesso <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 4) return(NA_real_)
    m <- mean(x)
    m2 <- mean((x - m)^2)
    m4 <- mean((x - m)^4)
    if (m2 <= 0) return(NA_real_)
    (m4 / (m2^2)) - 3
  }
  
  # Bivariate density estimate (Gaussian kernel on a grid), replicating the
  # same method as MASS::kde2d() but using ONLY functions already present in
  # base R (dnorm, outer, bw.nrd0 - the latter is Silverman's rule
  # for choosing the bandwidth, the same used by default by kde2d itself),
  # to avoid any risk of compatibility with a new package
  # under Shinylive/webR (same problem already encountered with deSolve).
  # Detects whether the first row of a CSV is a real header (text) or already
  # data (numeric) - read.csv() always assumes header=TRUE by default,
  # which would silently lose the first data row if the file did not
  # have one. If EVERY value of the first row converts to a number,
  # it is assumed that there is no header.
  leggi_csv_auto_header <- function(path) {
    prima_riga <- tryCatch(readLines(path, n = 1), error = function(e) NULL)
    if (is.null(prima_riga) || length(prima_riga) == 0) return(read.csv(path))
    
    # Detects the column separator: the mere PRESENCE of a
    # semicolon in the row is a reliable signal that it is the real
    # column separator (unlike the comma, a
    # semicolon never appears by chance inside a number or a header).
    # A comparative count (commas vs semicolons) would have been
    # misleading with few columns: with only 2 columns in European format
    # (e.g. "1,5;2,3"), the decimal commas (one per value, up to 2)
    # can easily outnumber the single semicolon
    # separator, leading to the wrong separator if the
    # occurrences are counted instead of only checking their presence. This is
    # exactly the kind of file that, read with the comma fixed as
    # separator, later caused an unhandled error in the computation
    # engine (the reported "crash") when the file uses the European
    # convention (semicolon as column separator, comma as
    # decimal separator).
    separatore <- if (grepl(";", prima_riga, fixed = TRUE)) ";" else ","
    
    valori <- strsplit(prima_riga, separatore, fixed = TRUE)[[1]]
    # If the separator is the semicolon, the numbers in the row probably
    # use the comma as decimal mark - I convert it to a point before
    # testing whether the row is entirely numeric (otherwise "1,5" would be
    # mistaken for text, making the header detection wrong).
    valori_normalizzati <- if (separatore == ";") gsub(",", ".", trimws(valori), fixed = TRUE) else trimws(valori)
    tutti_numerici <- length(valori) > 0 && all(!is.na(suppressWarnings(as.numeric(valori_normalizzati))))
    
    df <- read.csv(path, header = !tutti_numerici, sep = separatore)
    
    # If the file was in European format, every column that read.csv()
    # imported as text (because it contains decimal commas) must be
    # converted back to numeric - but only if the conversion does not introduce
    # MORE missing values than those already present: if a column was not
    # really numeric (e.g. a sample name), it must stay text as it was.
    if (separatore == ";") {
      for (col in seq_len(ncol(df))) {
        if (is.character(df[[col]])) {
          convertito <- suppressWarnings(as.numeric(gsub(",", ".", df[[col]], fixed = TRUE)))
          if (sum(is.na(convertito)) <= sum(is.na(df[[col]]))) df[[col]] <- convertito
        }
      }
    }
    df
  }
  
  # Same logic for Excel files: reads only the first row to decide,
  # then rereads the whole sheet with the correct setting.
  leggi_excel_auto_header <- function(path) {
    prima_riga <- tryCatch(as.character(unlist(readxl::read_excel(path, n_max = 1, col_names = FALSE))), error = function(e) NULL)
    if (is.null(prima_riga) || length(prima_riga) == 0) return(readxl::read_excel(path))
    tutti_numerici <- all(!is.na(suppressWarnings(as.numeric(trimws(prima_riga)))))
    readxl::read_excel(path, col_names = !tutti_numerici)
  }
  
  # Finds the first pair of CONSECUTIVE columns that are actually numeric
  # in a data.frame, instead of blindly assuming they are always the
  # first two - handles a common case with real data files (e.g. the
  # isotherm tables of Iglesias & Chirife): a first
  # "label"/category column (e.g. "Item", always equal to "Data") before the
  # real numeric data. Without this, the app took that text column
  # as X, causing an error later in the computation engine
  # (or, in Batch, an actual crash, since the downstream safety
  # check was also missing there). Returns c(indice_x, indice_y) or
  # NULL if it finds no valid pair.
  trova_colonne_xy_numeriche <- function(df) {
    is_colonna_numerica <- function(col) {
      if (is.numeric(col)) return(TRUE)
      convertito <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col), fixed = TRUE)))
      length(col) > 0 && sum(is.na(convertito)) <= sum(is.na(col))
    }
    if (ncol(df) < 2) return(NULL)
    numeriche <- sapply(df, is_colonna_numerica)
    for (i in seq_len(ncol(df) - 1)) {
      if (numeriche[i] && numeriche[i + 1]) return(c(i, i + 1))
    }
    NULL
  }
  
  
  # Applies the chosen transformation to a Y vector, with an explicit
  # check on the domain: log/root of a negative value or zero
  # would silently produce NaN (which would then appear as points
  # missing without explanation, much harder to diagnose than
  # a clear warning right at the moment of the transformation).
  applica_trasformazione_y <- function(y, tipo) {
    if (is.null(tipo) || tipo == "nessuna") return(list(y = y, messaggio_errore = NULL))
    switch(tipo,
      "ln" = {
        if (any(y[is.finite(y)] <= 0)) return(list(y = y, messaggio_errore = "Natural Log requires all Y values to be strictly positive (> 0). Some values in your data are zero or negative \u2014 consider \"ln(y+1)\" instead if your data can include zeros."))
        list(y = log(y), messaggio_errore = NULL)
      },
      "ln1p" = list(y = log1p(y), messaggio_errore = if (any(y[is.finite(y)] <= -1)) "ln(y+1) requires all Y values to be greater than -1." else NULL),
      "log10" = {
        if (any(y[is.finite(y)] <= 0)) return(list(y = y, messaggio_errore = "Log10 requires all Y values to be strictly positive (> 0). Some values in your data are zero or negative \u2014 consider \"Log10(y+1)\" instead if your data can include zeros."))
        list(y = log10(y), messaggio_errore = NULL)
      },
      "log101p" = list(y = log10(y + 1), messaggio_errore = if (any(y[is.finite(y)] <= -1)) "Log10(y+1) requires all Y values to be greater than -1." else NULL),
      "sqrt" = {
        if (any(y[is.finite(y)] < 0)) return(list(y = y, messaggio_errore = "Square Root requires all Y values to be non-negative (>= 0). Some values in your data are negative."))
        list(y = sqrt(y), messaggio_errore = NULL)
      },
      "exp" = list(y = exp(y), messaggio_errore = NULL),
      "pow10" = list(y = 10^y, messaggio_errore = NULL),
      # Direct conversion between logarithmic bases, for data that are ALREADY
      # on a log scale but with the wrong base (e.g. imported from a source
      # that uses ln, but log10 is needed to compare with the literature) -
      # log10(x) = ln(x) / ln(10), so it is just a multiplication by
      # a constant. No domain check needed: unlike
      # ln(y)/log10(y) on raw data (which require y>0), here the
      # input value is already a logarithm, so any real
      # number (including negative) is a valid input.
      "ln_a_log10" = list(y = y * log10(exp(1)), messaggio_errore = NULL),
      "log10_a_ln" = list(y = y * log(10), messaggio_errore = NULL),
      list(y = y, messaggio_errore = NULL)
    )
  }
  
  # Applies the transformation (if chosen) to the y column of a data.frame
  # with columns x,y - shows a clear warning if the domain is not valid
  # for the chosen transformation, without producing silent NaN.
  applica_trasformazione_y_su_df <- function(df, tipo, silenzioso = FALSE) {
    if (is.null(df) || nrow(df) == 0 || is.null(tipo) || tipo == "nessuna") return(df)
    ris <- applica_trasformazione_y(df$y, tipo)
    if (!is.null(ris$messaggio_errore)) {
      if (!silenzioso) showNotification(ris$messaggio_errore, type = "error", duration = 10)
      return(df)
    }
    df$y <- ris$y
    df
  }
  
  # Adds up to two "anchor" points to the data, BEFORE the fit but AFTER
  # any transformation (Y pre-transformation, density) - they force the
  # curve towards zero at one end or the other, useful above all with
  # few size classes where the measured data alone do not constrain
  # the tails of the distribution well. They are ADDED points, not measured:
  # with few real data they can shift the fit disproportionately,
  # so they stay disabled by default and the choice is always an explicit one
  # of the operator.
  aggiungi_punti_ancora <- function(df, aggiungi_zero, aggiungi_max, x_max_valore) {
    if (is.null(df) || nrow(df) == 0) return(df)
    punti_extra <- list()
    if (isTRUE(aggiungi_zero)) punti_extra[[length(punti_extra) + 1]] <- data.frame(x = 0, y = 0)
    if (isTRUE(aggiungi_max) && !is.null(x_max_valore) && is.finite(x_max_valore)) {
      punti_extra[[length(punti_extra) + 1]] <- data.frame(x = x_max_valore, y = 0)
    }
    if (length(punti_extra) == 0) return(df)
    colonne_extra <- setdiff(names(df), c("x", "y"))
    for (i in seq_along(punti_extra)) {
      for (col in colonne_extra) punti_extra[[i]][[col]] <- NA
    }
    df_completo <- do.call(rbind, c(list(df), punti_extra))
    df_completo[order(df_completo$x), ]
  }
  

  # distribution density - standard sieving convention: the
  # material associated with point X_i lies ONLY between X_i and the point
  # immediately larger above it (never downwards) - the
  # sieve itself is a BOUNDARY of its bin, not its centre. Only the
  # largest point has no "sieve above" (the oversize) and always needs
  # an assumed boundary or must be discarded; the smallest point already
  # correctly uses its own value as lower boundary (e.g. a "pan"
  # at X=0 already gives a fully defined bin [0, next], without the need
  # for any assumption) - the operator can still override it if
  # their smallest value does not already represent a true physical limit.
  # "tipo" governs whether the width of the bin (and therefore the unit of the
  # resulting density) is computed on a linear or logarithmic scale -
  # the latter is the most common convention in particle-size analysis, since
  # sieve series are typically in geometric progression (but the
  # computation works identically also with irregularly spaced sieves,
  # since it always uses the REAL differences/ratios between adjacent points,
  # never an assumed constant step). The final density is normalized
  # on the ORIGINAL total mass/quantity (including any points later
  # discarded), so it represents a true probability density (area
  # under the curve = 1 over the whole sample, not only over the points kept). The
  # returned X is the centre (arithmetic or geometric) of the true bin, no
  # longer the raw value of the sieve.
  # Pre-fills the value of the anchor (X, 0) with the same assumed Upper Boundary,
  # ONLY if the anchor field is still empty (it never overwrites
  # a value already chosen by the operator) and ONLY if the boundary is really
  # in "assume" mode (it makes no sense to copy a leftover value if the
  # mode is "discard"). It is a STARTING value for convenience, not a
  # constraint: the operator remains free to raise it to avoid the
  # contradiction described in the warning of the "how does this work?" box.
  sincronizza_ancora_da_confine <- function(prefisso) {
    modo <- input[[paste0(prefisso, "densita_confine_sup_modo")]]
    valore_confine <- input[[paste0(prefisso, "densita_confine_sup_valore")]]
    valore_ancora <- input[[paste0(prefisso, "densita_ancora_max_valore")]]
    if (isTRUE(input[[paste0(prefisso, "densita_ancora_max")]]) &&
        identical(modo, "assumi") &&
        !is.null(valore_confine) && !is.na(valore_confine) &&
        (is.null(valore_ancora) || is.na(valore_ancora))) {
      updateNumericInput(session, paste0(prefisso, "densita_ancora_max_valore"), value = valore_confine)
    }
  }
  observeEvent(input$densita_confine_sup_valore, { sincronizza_ancora_da_confine("") }, ignoreInit = TRUE)
  observeEvent(input$densita_ancora_max, { sincronizza_ancora_da_confine("") }, ignoreInit = TRUE)
  observeEvent(input$batch_densita_confine_sup_valore, { sincronizza_ancora_da_confine("batch_") }, ignoreInit = TRUE)
  observeEvent(input$batch_densita_ancora_max, { sincronizza_ancora_da_confine("batch_") }, ignoreInit = TRUE)
  
  converti_a_densita <- function(df, tipo, media_bin, confine_inf_modo, confine_inf_valore, confine_sup_modo, confine_sup_valore) {
    if (is.null(df) || is.null(tipo) || tipo == "nessuna") return(df)
    if (nrow(df) < 2) return(df)
    df <- df[order(df$x), ]
    n <- nrow(df)
    x <- df$x
    quantita_totale <- sum(df$y, na.rm = TRUE)
    if (!is.finite(quantita_totale) || quantita_totale <= 0) return(df)
    
    media <- function(a, b) if (media_bin == "geometrica") sqrt(a * b) else (a + b) / 2
    
    # Each point (except the last) uses its own value as lower
    # boundary and the value of the next (larger) point as upper
    # boundary - NEVER the smaller neighbour.
    bordo_inf <- x
    bordo_sup <- c(x[-1], NA)
    
    scarta <- rep(FALSE, n)
    if (confine_sup_modo == "scarta") {
      scarta[n] <- TRUE
    } else {
      bordo_sup[n] <- confine_sup_valore
    }
    if (confine_inf_modo == "scarta") {
      scarta[1] <- TRUE
    } else if (confine_inf_modo == "assumi") {
      bordo_inf[1] <- confine_inf_valore
    }
    # (default, neither of the two above: the smallest point keeps its
    # own value as lower boundary, already set above)
    
    larghezza <- if (tipo == "logaritmica") log(bordo_sup) - log(bordo_inf) else bordo_sup - bordo_inf
    x_rappresentativo <- media(bordo_inf, bordo_sup)
    
    # An invalid width (e.g. log(0) = -Inf when a boundary is zero
    # or negative with the logarithmic transformation, or a width
    # negative/zero if an assumed boundary is inconsistent with the point
    # itself) makes that bin meaningless - it must be explicitly excluded,
    # not left to produce a value that looks plausible (e.g. 0) but is
    # not.
    scarta <- scarta | !is.finite(larghezza) | larghezza <= 0 | !is.finite(x_rappresentativo)
    
    df$x <- x_rappresentativo
    df$y <- df$y / larghezza / quantita_totale
    df <- df[!scarta & is.finite(df$y) & is.finite(df$x), ]
    df
  }
  
  # Draws a violin plot from a NAMED list of numeric
  # vectors (same interface as boxplot(lista, ...), so the two
  # can be swapped without changing the rest of the calling code). Uses
  # only base R density() - no vioplot package, which would have
  # required a new dependency to be checked for compatibility with
  # webR/Shinylive, not testable live in this environment.
  disegna_violin_gruppi <- function(lista_dati, main = "", ylab = "Value", col = "#f39c12", col_bordo = "black", cex_axis = 0.8, mostra_punti_singoli = FALSE) {
    n_gruppi <- length(lista_dati)
    tutti_valori <- unlist(lista_dati)
    tutti_valori <- tutti_valori[is.finite(tutti_valori)]
    if (length(tutti_valori) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
      return(invisible(NULL))
    }
    range_y <- range(tutti_valori, na.rm = TRUE)
    margine_y <- diff(range_y) * 0.1
    if (margine_y == 0) margine_y <- abs(range_y[1]) * 0.1 + 0.1
    plot(1, type = "n", xlim = c(0.5, n_gruppi + 0.5), ylim = range_y + c(-margine_y, margine_y),
         main = main, ylab = ylab, xlab = "", xaxt = "n", las = 1)
    axis(1, at = seq_len(n_gruppi), labels = names(lista_dati), las = 2, cex.axis = cex_axis)
    larghezza_max <- 0.4
    for (i in seq_len(n_gruppi)) {
      valori <- lista_dati[[i]]
      valori <- valori[is.finite(valori)]
      if (length(valori) < 2) next
      # With very few distinct points (common, e.g. with few samples
      # per parameter) the density can fail or give a degenerate shape -
      # bw.nrd0() requires a positive variance, otherwise it falls back to a
      # simple "diamond" shape (min-max) instead of crashing.
      dens <- tryCatch({
        bw <- bw.nrd0(valori)
        if (!is.finite(bw) || bw <= 0) stop("larghezza di banda non valida")
        density(valori, bw = bw, n = 128)
      }, error = function(e) NULL)
      if (is.null(dens)) {
        # Fallback: diamond shape between min and max, median at the centre
        y_diamante <- c(min(valori), median(valori), max(valori))
        x_diamante <- i + c(0, larghezza_max, 0)
        polygon(c(x_diamante, rev(2 * i - x_diamante)), c(y_diamante, rev(y_diamante)), col = col, border = col_bordo)
        next
      }
      larghezza_scalata <- dens$y / max(dens$y) * larghezza_max
      # Closed polygon: right profile (increasing x) followed by the mirrored
      # left profile (decreasing x), same y in reverse order -
      # draws the classic symmetric "violin" outline around the
      # centre of category i.
      polygon(c(i + larghezza_scalata, rev(i - larghezza_scalata)), c(dens$x, rev(dens$x)), col = col, border = col_bordo)
      segments(i, quantile(valori, 0.25), i, quantile(valori, 0.75), lwd = 2, col = col_bordo)
      points(i, median(valori), pch = 19, col = col_bordo, cex = 0.9)
      if (mostra_punti_singoli && length(valori) <= 30) {
        set.seed(1)
        jitter_x <- i + runif(length(valori), -larghezza_max * 0.3, larghezza_max * 0.3)
        points(jitter_x, valori, pch = 16, col = adjustcolor(col_bordo, alpha.f = 0.4), cex = 0.6)
      }
    }
  }
  
  kde2d_base <- function(x, y, n_grid = 40, espansione = 0.3) {
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    if (length(x) < 10 || length(x) != length(y)) return(NULL)
    hx <- tryCatch(bw.nrd0(x), error = function(e) NA)
    hy <- tryCatch(bw.nrd0(y), error = function(e) NA)
    if (is.na(hx) || is.na(hy) || hx <= 0 || hy <= 0) return(NULL)
    rx <- range(x); ry <- range(y)
    ex <- diff(rx) * espansione; ey <- diff(ry) * espansione
    if (ex <= 0) ex <- 1; if (ey <= 0) ey <- 1
    griglia_x <- seq(rx[1] - ex, rx[2] + ex, length.out = n_grid)
    griglia_y <- seq(ry[1] - ey, ry[2] + ey, length.out = n_grid)
    dens <- matrix(0, n_grid, n_grid)
    for (i in seq_along(x)) {
      kx <- dnorm((griglia_x - x[i]) / hx)
      ky <- dnorm((griglia_y - y[i]) / hy)
      dens <- dens + outer(kx, ky)
    }
    dens <- dens / (length(x) * hx * hy)
    list(x = griglia_x, y = griglia_y, z = dens)
  }
  
  # ---- Parameter Display Symbols (cosmetic mapping: raw name -> nicer symbol) ----
  # Purely for display in the Parameter Distributions plot and the main
  # Results table; the actual parameter name used for computation/exports is
  # never touched. A named list, since the number of parameters varies by
  # model, so entries are added one at a time via the sidebar "Add" button.
  simboli_parametri <- reactiveVal(list())
  
  # ---- Initial Guess Simulator: editable cells + continuous mode ----
  # guess_celle_base stores the "starting" values (loaded from the
  # sidebar), NOT reactively linked to the input$guess_iniziali field - once
  # loaded, the user can freely edit them in the cells without
  # them being reset at every small change elsewhere. They are updated only
  # when the user explicitly presses "Load Current Sidebar Guesses" (or
  # at the very first opening of the tab, if not yet loaded).
  guess_celle_base <- reactiveVal(NULL)
  # Separate counter, incremented at EVERY click on "Load", regardless
  # of the loaded value - needed because guess_celle_base() alone is not
  # enough: if the sidebar text has not changed since the last Load (e.g.
  # you edited only the cells, not the sidebar), setting the SAME
  # value on a reactiveVal does not trigger a new update (Shiny
  # compares with the previous value and, if identical, does not propagate the
  # change again) - hence the bug whereby "Load" worked only the first
  # time. The renderUI of the cells ALSO depends on this counter, not
  # only on the value, so every click always forces a real rebuild.
  guess_celle_trigger <- reactiveVal(0)
  
  # History of the simulated curves, for the "Keep previous curves drawn" option
  # - a list of {x,y}, filled at every successful simulation ONLY if
  # the option is active. Limited to a maximum of 30 curves to prevent
  # the plot from becoming unreadable or the list from growing indefinitely
  # during a long session.
  storico_curve_simulate <- reactiveVal(list())
  
  observeEvent(simulazione_dati(), {
    if (!isTRUE(input$sim_lascia_traccia)) return()
    res <- tryCatch(simulazione_dati(), error = function(e) NULL)
    if (is.null(res) || !isTRUE(res$successo)) return()
    storico <- storico_curve_simulate()
    storico[[length(storico) + 1]] <- list(x = res$x, y = res$y)
    if (length(storico) > 30) storico <- storico[(length(storico) - 29):length(storico)]
    storico_curve_simulate(storico)
  })
  
  observeEvent(input$sim_pulisci_traccia, {
    storico_curve_simulate(list())
    showNotification("Previous curves cleared.", type = "message")
  })
  # Also clears the history when the guesses are reloaded from the sidebar, since
  # curves referring to a previous model/dataset would no longer make
  # sense mixed with the new ones.
  observeEvent(input$carica_guess_celle, { storico_curve_simulate(list()) })
  
  popola_guess_celle <- function() {
    guesses_raw <- tryCatch(parse_guesses(input$guess_iniziali), error = function(e) NULL)
    if (!is.null(guesses_raw) && !is.null(guesses_raw$guess) && length(guesses_raw$guess) > 0) {
      guess_celle_base(guesses_raw$guess)
      guess_celle_trigger(isolate(guess_celle_trigger()) + 1)
    } else {
      showNotification("Could not read the guesses from the sidebar \u2014 check the Initial Guesses field first.", type = "error")
    }
  }
  
  observeEvent(input$carica_guess_celle, { popola_guess_celle() })
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "tab_simula") && is.null(isolate(guess_celle_base()))) popola_guess_celle()
  }, ignoreInit = FALSE)
  
  # Only the very first time Batch Elaboration is entered (never again
  # afterwards, even if this tab is revisited later), moves the focus
  # from section 1 (Data Input for Single Fit, irrelevant here) to
  # section 4 (Model Definition, the first actual step for Batch) -
  # but only if Model Definition is still closed: if the user has already
  # opened it on their own initiative (perhaps together with something else), we touch
  # nothing so as not to undo an arrangement already chosen manually.
  batch_primo_accesso <- reactiveVal(TRUE)
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "tab_batch") && isTRUE(batch_primo_accesso())) {
      batch_primo_accesso(FALSE)
      pannelli_aperti <- input$sidebar_accordion
      if (!("sec4" %in% pannelli_aperti)) {
        bslib::accordion_panel_close(id = "sidebar_accordion", values = "sec1")
        bslib::accordion_panel_open(id = "sidebar_accordion", values = "sec4")
      }
    }
  }, ignoreInit = TRUE)
  
  simulatore_primo_accesso <- reactiveVal(TRUE)
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "tab_simula") && isTRUE(simulatore_primo_accesso())) {
      simulatore_primo_accesso(FALSE)
      pannelli_aperti <- input$sidebar_accordion
      if (!("sec4" %in% pannelli_aperti)) {
        bslib::accordion_panel_close(id = "sidebar_accordion", values = "sec1")
        bslib::accordion_panel_open(id = "sidebar_accordion", values = "sec4")
      }
    }
  }, ignoreInit = TRUE)
  
  # Tracks which parameter is "the animated one" through a click on the red
  # button under its slider (instead of the old drop-down selector) - a
  # generic shared input (set via JS with Shiny.setInputValue,
  # exactly the same pattern already used successfully for selecting the
  # model from the popup) instead of N distinct Shiny buttons with dynamic IDs,
  # which would require as many dynamically generated observeEvent
  # (risk of duplicated/accumulated observers at every rebuild of the
  # renderUI). The guarantee "only one animated parameter at a time" remains
  # EXACTLY the same as before (by construction: only the parameter
  # tracked here receives animate=TRUE on its slider, no JS trick
  # to "stop" the others, which would be fragile).
  parametro_animato_scelto <- reactiveVal(NULL)
  observeEvent(input$parametro_animato_click, {
    parametro_animato_scelto(input$parametro_animato_click)
    session$sendCustomMessage("avviaAnimazioneSlider", paste0("simula_val_", input$parametro_animato_click))
  })
  
  # Tracks the last value of guess_celle_trigger() already seen by this
  # renderUI - needed to distinguish a real click on "Load" (which MUST
  # reset all sliders to the sidebar values) from a simple
  # change of "which parameter to animate" or "number of steps" (which must NOT
  # touch the positions that the user may have already dragged by
  # hand - without this distinction, changing the parameter to animate
  # silently reset every slider, losing any manual
  # adjustments already made).
  ultimo_trigger_guess_visto <- reactiveVal(-1)
  
  output$celle_guess_dinamiche <- renderUI({
    trigger_attuale <- guess_celle_trigger()  # explicit dependency: forces the rebuild at every click on Load, even with an unchanged value
    base <- guess_celle_base()
    if (is.null(base)) return(p(style = "color: #7f8c8d; font-size: 13px;", "Click \u201cLoad Current Sidebar Guesses\u201d above to start."))
    nomi <- names(base)
    # One slider per parameter, which directly shows and sets the
    # ABSOLUTE VALUE (not a percentage) - the range of the slider still
    # covers ±50% around the loaded value, but the user always sees
    # at a glance the true value of the parameter, without having to do
    # mental calculations. Updates the curve automatically at every movement,
    # no "Simulate" button needed.
    #
    # Only ONE parameter at a time can be animated (play button):
    # instead of relying on a JS trick to "stop the other animations"
    # (fragile, not verifiable without a live environment), the constraint is
    # guaranteed by construction - the play slider (animate) is
    # assigned ONLY to the parameter chosen in the selector above, the other
    # sliders do not have it at all. This renderUI reads the value of the
    # selector (to decide which one to animate) but never rebuilds it,
    # so there is no risk of the loop described above.
    # Unlike the old selectInput (which ALWAYS had a
    # selection, and therefore fell back on the first parameter as default),
    # with the buttons no parameter must appear "animated" until
    # the user explicitly clicks one - otherwise the first
    # slider would already show the native Play button at the first
    # loading, without the user having asked for anything.
    parametro_animato <- parametro_animato_scelto()
    if (!is.null(parametro_animato) && !(parametro_animato %in% nomi)) parametro_animato <- NULL
    
    # Full reset (to the sidebar values) only if the trigger has
    # really changed since last time (i.e. "Load" has just been
    # clicked) - otherwise preserves the current positions of the sliders,
    # even if the renderUI is being rebuilt for another reason
    # (change of animated parameter, change of number of steps).
    reset_completo <- !identical(trigger_attuale, isolate(ultimo_trigger_guess_visto()))
    if (reset_completo) isolate(ultimo_trigger_guess_visto(trigger_attuale))
    
    # Number of animation steps: determines the "gap" between one
    # frame and the next (range divided by the number of steps) -
    # fewer steps = larger gap = the curve moves more visibly
    # at every frame, for the same time interval between frames.
    n_passi <- if (is.null(input$simula_n_passi_animazione) || is.na(input$simula_n_passi_animazione)) 50 else input$simula_n_passi_animazione
    n_passi <- min(max(round(n_passi), 5), 200)
    
    spread_pct <- if (is.null(input$simula_spread_percentuale) || is.na(input$simula_spread_percentuale)) 50 else input$simula_spread_percentuale
    spread_pct <- min(max(spread_pct, 5), 200)
    spread_frazione <- spread_pct / 100
    
    campi <- lapply(nomi, function(nome) {
      valore_base <- base[[nome]]
      if (valore_base == 0) {
        # A percentage spread of 0 is always 0 - arbitrary absolute fallback range
        limite_inf <- -1; limite_sup <- 1
      } else if (valore_base > 0) {
        limite_inf <- valore_base * (1 - spread_frazione); limite_sup <- valore_base * (1 + spread_frazione)
      } else {
        # For a negative value, *(1+spread) makes it MORE negative (not
        # larger) - they must be swapped to still have minimum < maximum.
        limite_inf <- valore_base * (1 + spread_frazione); limite_sup <- valore_base * (1 - spread_frazione)
      }
      step_s <- (limite_sup - limite_inf) / n_passi
      limite_inf_r <- signif(limite_inf, 3)
      limite_sup_r <- signif(limite_sup, 3)
      step_r <- signif(step_s, 3)
      e_animato <- identical(nome, parametro_animato)
      # At every Load (full reset), ALL sliders start from their
      # lower end - not only the one chosen for the animation. In this way
      # the "Animate this parameter" button merely enables
      # play on the chosen slider, without having to force its position anymore
      # every time the parameter to animate is changed (the original
      # annoyance: previously it brought the slider back to the left at every click on
      # Animate, even if in the meantime the user had moved it by hand).
      # Outside a reset, the current position is always preserved.
      valore_input_corrente <- isolate(input[[paste0("simula_val_", nome)]])
      valore_di_partenza <- if (!reset_completo && !is.null(valore_input_corrente) && !is.na(valore_input_corrente)) {
        valore_input_corrente
      } else {
        limite_inf_r
      }
      valore_r <- min(max(signif(valore_di_partenza, 3), limite_inf_r), limite_sup_r)
      etichetta_simbolo <- simbolo_a_testo_semplice(simbolo_visualizzazione(nome))
      etichetta_completa <- if (e_animato) paste0("\u25B6 ", etichetta_simbolo, " (animated)") else etichetta_simbolo
      div(style = "flex: 0 0 19%; max-width: 19%; padding: 0 4px; box-sizing: border-box; min-height: 90px;",
        div(style = "background-color: #f8f9fa; border: 1px solid #dfe4e6; border-radius: 8px; padding: 10px 10px 4px 10px;",
          div(style = "margin-bottom: -15px;",
            sliderInput(paste0("simula_val_", nome), etichetta_completa,
                       min = limite_inf_r, max = limite_sup_r, value = valore_r, step = step_r,
                       animate = if (e_animato) animationOptions(interval = 200, loop = FALSE) else FALSE)
          ),
          tags$button(
            "\u25B6 Animate this parameter",
            class = "btn btn-sm w-100",
            style = if (e_animato) "background-color: #c0392b; color: white; font-size: 11px; padding: 2px 4px;" else "background-color: #e74c3c; color: white; font-size: 11px; padding: 2px 4px;",
            onclick = sprintf("Shiny.setInputValue('parametro_animato_click', '%s', {priority: 'event'})", nome)
          )
        )
      )
    })
    div(style = "display: flex; flex-wrap: wrap;", campi)
  })
  
  # Values of the parameters actually to be used for the simulation,
  # derived directly from the sliders (absolute value) - the fixed
  # parameters do not pass through here, they remain those read directly from the
  # guess field of the sidebar.
  valori_guess_correnti <- reactive({
    base <- guess_celle_base()
    if (is.null(base)) return(NULL)
    nomi <- names(base)
    valori <- sapply(nomi, function(nome) {
      v <- input[[paste0("simula_val_", nome)]]
      if (is.null(v) || is.na(v)) base[[nome]] else v
    })
    setNames(valori, nomi)
  })
  
  # The main "Run Optimization Calculation" button performs the
  # SINGLE fit on the current data (section 1) - on the Batch Elaboration &
  # Shelf-life tab this can be confusing, since that tab has its own
  # "Run Batch Elaboration" button that operates on completely different data (the
  # samples loaded for the batch). The button ALWAYS remains the same
  # persistent element (never destroyed/recreated via renderUI - it was in
  # a previous version, and this reset the click counter of the
  # actionButton on returning from another tab, unintentionally
  # re-triggering elaborazione() with the state of the sidebar at that
  # moment). The actual disabling (grey button, click that
  # does not register at all) is handled client-side via JS (see tags$script
  # in ui.R) - a simple check here inside elaborazione() is NOT enough,
  # since eventReactive() updates its stored value anyway
  # with whatever its expression returns, so
  # even a "refusal" would have overwritten the previous fit with an
  # empty result - the click must be prevented BEFORE it reaches
  # Shiny, not discarded afterwards. The check below remains anyway as a
  # defensive safety net (e.g. if the JS were for some reason not
  # applied), but in normal use it should never trigger.
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("toggleEseguiBatchTab", identical(input$main_tabs, "tab_batch"))
  }, ignoreNULL = FALSE)
  
  # ---- Saving/loading the operator configuration ----
  # Uses dput()/dget() (base R, no new dependency to be checked for
  # webR compatibility) instead of a proper JSON format - the file
  # is still readable plain text, only with R syntax instead of
  # JSON. Covers the main settings; does NOT yet include the
  # per-plot aesthetic customizations (colours, line widths) - left
  # for a future extension, given the large number of controls
  # involved in that panel.
  # Categorized lists of ALL the aesthetic customization controls
  # of the app (Aesthetics panel + global show/hide switches +
  # export settings) - used by the configuration saving/loading
  # below. Organized by widget type, given the large
  # number of fields (79), instead of writing an if/update line for
  # each one: easier to extend in the future if new
  # customization controls are added.
  CONFIG_CAMPI_SELECT <- c("bar_col_dist", "batch_pos_legenda", "col_boxplot_confronto", "col_contour_pairs",
    "col_linee_grid", "col_punti_grid_3d", "col_punto_ott_grid", "col_regione_grid", "dens_col_dist",
    "ln_col_fit", "ln_col_qq", "ln_col_res", "ln_col_sim", "lty_contour_pairs", "palette_superficie_grid",
    "pt_col_fit", "pt_col_pairs", "pt_col_qq", "pt_col_res", "pt_col_sim", "pt_pch_fit", "pt_pch_qq",
    "pt_pch_res", "pt_pch_sim", "sl_col_banda", "sl_col_curva", "sl_col_punti", "vln_col_dist", "formato_esportazione", "comp_modo_layout", "comp_riempimento", "comp_stile_lettere", "comp_pos_lettere", "batch_col_punti", "confronto_col_punti",
    "mc_sl_col_banda", "mc_sl_col_curva", "mc_sl_col_punti", "confronto_palette",
    "ln_lty_fit", "ln_lty_res", "ln_lty_qq", "vln_lty_dist", "sens_palette", "pch_punti_grid_3d",
    "sl_pch_punti", "mc_sl_pch_punti", "confronto_pch_punti", "confronto_lty_curve",
    "batch_palette", "batch_pch_punti", "batch_lty_linee", "col_bordo_boxplot_confronto", "tipo_grafico_boxplot_confronto",
    "batch_col_ist_par", "batch_col_bordo_ist_par", "batch_col_box_par", "batch_col_bordo_box_par", "batch_tipo_grafico_box_par",
    "sl_box_tipo_grafico_box", "sl_box_col_box", "sl_box_col_bordo_box", "mc_sl_box_tipo_grafico_box", "mc_sl_box_col_box", "mc_sl_box_col_bordo_box",
    "sl_barre_col_barre", "sl_barre_col_bordo_barre", "mc_sl_barre_col_barre", "mc_sl_barre_col_bordo_barre",
    "grid_ci_col_barre", "grid_ci_col_bordo", "profilo2d_col_linea", "profilo2d_col_punto", "profilo2d_pch_punto",
    "tplot_col_linea", "tplot_col_punto", "tplot_col_retta", "tplot_pch_punto", "ptrace_col1", "ptrace_col2",
    "sim_pos_legenda", "sens_pos_legenda", "grid_pos_legenda", "profilo2d_pos_legenda", "tplot_pos_legenda", "ptrace_pos_legenda",
    "confronto_pos_legenda", "inv_pos_legenda", "sl_pos_legenda", "mc_sl_pos_legenda",
    "ni_pos_legenda_sec", "ni_pos_legenda_prev",
    "mcni_pos_legenda_sec", "mcni_pos_legenda_prev",
    "ni_pt_col_secondario", "ni_ln_col_secondario", "ni_pch_punti_secondario", "ni_col_banda_secondario",
    "ni_col_T", "ni_col_C", "ni_col_soglia", "ni_lty_T",
    "mcni_pt_col_secondario", "mcni_ln_col_secondario", "mcni_pch_punti_secondario", "mcni_col_banda_secondario",
    "mcni_col_T", "mcni_col_C", "mcni_col_soglia", "mcni_lty_T",
    "ni_col_iso", "ni_col_k", "ni_lty_iso", "ni_lty_k", "ni_pos_legenda_k",
    "ni_modello_secondario", "ni_tipo_profilo", "ni_unita_temp", "ni_abuso_direzione", "ni_abuso_forma",
    "mcni_col_iso", "mcni_col_k", "mcni_lty_iso", "mcni_lty_k", "mcni_pos_legenda_k",
    "mcni_modello_secondario", "mcni_tipo_profilo", "mcni_unita_temp", "mcni_abuso_direzione", "mcni_abuso_forma",
    "sl_modello_cinetico", "mc_sl_modello_cinetico",
    "trasformazione_densita", "densita_media_bin", "densita_confine_inf_modo", "densita_confine_sup_modo",
    "batch_trasformazione_densita", "batch_densita_media_bin", "batch_densita_confine_inf_modo", "batch_densita_confine_sup_modo")
  CONFIG_CAMPI_SLIDER <- c("comp_cex_lettere", "alpha_punti_grid_3d", "batch_cex_punti", "batch_lwd_linee", "cex_punti_grid_3d",
    "ci_alpha_fit", "fnt_cex_dist", "fnt_cex_fit", "fnt_cex_qq", "fnt_cex_res", "fnt_cex_sens", "fnt_cex_sim",
    "ln_lwd_fit", "ln_lwd_sens", "ln_lwd_sim", "lwd_contour_pairs", "n_livelli_grid", "ombra_grid",
    "pt_alpha_pairs", "pt_cex_fit", "pt_cex_pairs", "pt_cex_qq", "pt_cex_res", "pt_cex_sim", "sl_alpha_banda",
    "sl_cex_punti", "sl_lwd_curva", "mc_sl_alpha_banda", "mc_sl_cex_punti", "mc_sl_lwd_curva",
    "confronto_lwd_curve", "confronto_cex_punti",
    "ln_lwd_res", "ln_lwd_qq", "vln_lwd_dist", "lwd_linee_grid", "fnt_cex_boxplot_confronto",
    "sim_cex_legenda", "sens_cex_legenda", "grid_cex_legenda", "batch_cex_legenda",
    "confronto_cex_legenda", "sl_cex_legenda", "mc_sl_cex_legenda", "inv_cex_legenda",
    "batch_fnt_cex_ist_par", "batch_fnt_cex_box_par", "sl_box_fnt_cex_box", "mc_sl_box_fnt_cex_box",
    "sl_barre_fnt_cex_barre", "mc_sl_barre_fnt_cex_barre", "n_bins_dist", "grid_ci_fnt_cex", "profilo2d_fnt_cex", "profilo2d_lwd_linea", "profilo2d_cex_punto",
    "tplot_fnt_cex", "tplot_lwd_linea", "tplot_cex_punto", "tplot_zoom_margine_pct", "ptrace_lwd", "ptrace_fnt_cex", "ptrace_zoom_ottimo_range",
    "ni_cex_punti_secondario", "ni_lwd_curva_secondario", "ni_alpha_banda_secondario", "ni_cex_legenda_sec",
    "ni_alpha_T", "ni_lwd_linee", "ni_cex_legenda_prev",
    "mcni_cex_punti_secondario", "mcni_lwd_curva_secondario", "mcni_alpha_banda_secondario", "mcni_cex_legenda_sec",
    "mcni_alpha_T", "mcni_lwd_linee", "mcni_cex_legenda_prev",
    "ni_cex_legenda_k", "ni_lwd_k", "mcni_cex_legenda_k", "mcni_lwd_k")
  CONFIG_CAMPI_CHECKBOX <- c("mc_sl_box_mostra_titolo_x", "mostra_titolo_x_boxplot_confronto", "sl_box_mostra_titolo_x", "batch_mostra_titolo_x_box_par", "mc_sl_barre_mostra_titolo_x", "sl_barre_mostra_titolo_x", "batch_mostra_titolo_x_ist_par", "grid_ci_mostra_titolo_x", "mostra_densita_dist", "mostra_vln_dist", "comp_lettere", "comp_assi_condivisi", "mostra_curva_sens", "mostra_outlier_boxplot_confronto", "mostra_griglia",
    "mostra_titolo", "mostra_ci", "banda_simultanea", "mostra_pi", "residui_modificati", "nm_ripartenze", "res_collega_punti", "res_mostra_sottotitolo", "qq_mostra_sottotitolo", "res_mostra_medie", "sl_banda_simultanea", "mc_sl_banda_simultanea", "ni_banda_simultanea", "mcni_banda_simultanea", "mostra_regione_confidenza", "mostra_punti_confronto",
    "mostra_contour_pairs", "mostra_punti_grid_3d", "batch_mostra_punti",
    "accetta_non_convergenti", "batch_forza_lineare", "batch_warm_start", "confronto_mostra_legenda",
    "sim_mostra_legenda", "sim_lascia_traccia", "mostra_legenda_sens", "sl_mostra_legenda", "mc_sl_mostra_legenda",
    "batch_mostra_legenda", "auto_add_confronto", "sl_mostra_ci", "sl_mostra_pi", "mc_sl_mostra_ci", "mc_sl_mostra_pi",
    "batch_mostra_outlier_box_par", "batch_ordina_ist_par", "batch_ordina_box_par", "ordina_boxplot_confronto",
    "sl_box_ordina_box", "sl_box_mostra_outlier_box", "mc_sl_box_ordina_box", "mc_sl_box_mostra_outlier_box",
    "sl_barre_ordina_barre", "mc_sl_barre_ordina_barre", "profilo2d_mostra_legenda", "tplot_mostra_legenda",
    "tplot_mostra_punti", "tplot_mostra_soglia", "tplot_zoom_soglia", "sl_usa_bootstrap", "mc_sl_usa_bootstrap", "sim_mostra_punti_dati", "ptrace_mostra_riferimento", "ptrace_mostra_legenda", "ptrace_zoom_ottimo", "ptrace_valori_nominali",
    "ni_mostra_legenda_sec", "ni_mostra_legenda_prev", "mcni_mostra_legenda_sec", "mcni_mostra_legenda_prev",
    "ni_mostra_legenda_k", "ni_combina_grafici", "ni_usa_monte_carlo", "ni_mostra_celsius",
    "ni_mostra_banda_confidenza", "ni_mostra_banda_predizione", "ni_mostra_banda_previsione", "ni_confronta_isotermo",
    "mcni_mostra_legenda_k", "mcni_combina_grafici", "mcni_usa_monte_carlo", "mcni_mostra_celsius",
    "mcni_mostra_banda_confidenza", "mcni_mostra_banda_predizione", "mcni_mostra_banda_previsione", "mcni_confronta_isotermo",
    "densita_ancora_zero", "densita_ancora_max", "batch_densita_ancora_zero", "batch_densita_ancora_max")
  CONFIG_CAMPI_NUMERIC <- c("maxiter_ottimizzazione", "esponente_tolleranza", "range_x_max_distx", "range_x_max_qq", "range_x_min_distx", "range_x_min_qq",
    "range_y_max_distx", "range_y_max_qq", "range_y_max_res", "range_y_min_distx", "range_y_min_qq",
    "range_y_min_res", "jpeg_dpi", "export_width_px", "export_height_px", "comp_n_colonne", "comp_n_righe", "comp_pannello_w_px", "comp_pannello_h_px",
    "batch_range_x_min", "batch_range_x_max", "batch_range_y_min", "batch_range_y_max",
    "confronto_range_x_min", "confronto_range_x_max", "confronto_range_y_min", "confronto_range_y_max",
    "ni_A0", "ni_C_crit", "ni_t_max", "ni_T_iso", "ni_n_ordine", "ni_livello_confidenza", "ni_risoluzione_griglia",
    "ni_n_boot_secondario", "ni_profilo_T0", "ni_profilo_Dtot", "ni_profilo_n", "ni_profilo_dur", "ni_profilo_amp",
    "mcni_A0", "mcni_C_crit", "mcni_t_max", "mcni_T_iso", "mcni_n_ordine", "mcni_livello_confidenza", "mcni_risoluzione_griglia",
    "mcni_n_boot_secondario", "mcni_profilo_T0", "mcni_profilo_Dtot", "mcni_profilo_n", "mcni_profilo_dur", "mcni_profilo_amp",
    "sl_livello_confidenza", "mc_sl_livello_confidenza",
    "range_x_min_sim", "range_x_max_sim", "range_y_min_sim", "range_y_max_sim",
    "densita_confine_inf_valore", "densita_confine_sup_valore",
    "batch_densita_confine_inf_valore", "batch_densita_confine_sup_valore",
    "densita_ancora_max_valore", "batch_densita_ancora_max_valore")
  
  # Fifth category, for the free-text fields (textInput) that do not
  # fall into the 4 categories above - so far handled only with explicit
  # lines (formula_custom/guess_iniziali of the main registry);
  # from now on any new text field must be added here, with the
  # same generic loop as the other categories instead of lines written by hand.
  CONFIG_CAMPI_TEXT <- c(
    "ni_secondario_formula_custom", "ni_secondario_guess_custom",
    "mcni_secondario_formula_custom", "mcni_secondario_guess_custom",
    "ni_profilo_formula_custom", "ni_profilo_guess_custom",
    "mcni_profilo_formula_custom", "mcni_profilo_guess_custom",
    "sl_formula_custom", "sl_guess_custom",
    "mc_sl_formula_custom", "mc_sl_guess_custom")
  
  output$salva_configurazione <- downloadHandler(
    filename = function() {
      nome_pulito <- trimws(input$nome_file_configurazione)
      if (nchar(nome_pulito) == 0) nome_pulito <- paste0("nlr_config_", format(Sys.time(), "%Y%m%d_%H%M"))
      # Removes characters that are not safe for a file name, keeps only
      # letters/numbers/underscore/hyphen
      nome_pulito <- gsub("[^A-Za-z0-9_-]", "_", nome_pulito)
      paste0(nome_pulito, ".rconfig")
    },
    content = function(file) {
      configurazione <- list(
        tipo_modello = input$tipo_modello,
        formula_custom = input$formula_custom,
        guess_iniziali = input$guess_iniziali,
        tipo_loss = input$tipo_loss,
        ponderazione = input$ponderazione,
        ponderazione_zeta = input$ponderazione_zeta,
        batch_zeta_modo = input$batch_zeta_modo,
        algoritmo = input$algoritmo,
        metodo_errore = input$metodo_errore,
        n_boot = input$n_boot,
        tipo_rumore_mc = input$tipo_rumore_mc,
        label_x = input$label_x,
        label_y = input$label_y,
        applica_limiti_predefiniti = input$applica_limiti_predefiniti
      )
      # Adds all the aesthetic customization controls, by type
      for (id in CONFIG_CAMPI_SELECT) configurazione[[id]] <- input[[id]]
      for (id in CONFIG_CAMPI_SLIDER) configurazione[[id]] <- input[[id]]
      for (id in CONFIG_CAMPI_CHECKBOX) configurazione[[id]] <- input[[id]]
      for (id in CONFIG_CAMPI_NUMERIC) configurazione[[id]] <- input[[id]]
      for (id in CONFIG_CAMPI_TEXT) configurazione[[id]] <- input[[id]]
      dput(configurazione, file = file)
    }
  )
  
  observeEvent(input$carica_configurazione, {
    file_caricato <- input$carica_configurazione
    configurazione <- tryCatch(dget(file_caricato$datapath), error = function(e) NULL)
    if (is.null(configurazione) || !is.list(configurazione)) {
      showNotification("Could not read this configuration file \u2014 it may be corrupted or not a valid configuration.", type = "error")
      return()
    }
    # Updates each field ONLY if present in the loaded file, so a file
    # saved by a previous version of the app (with fewer fields) does not
    # cause errors on the missing fields.
    if (!is.null(configurazione$tipo_modello)) updateSelectInput(session, "tipo_modello", selected = configurazione$tipo_modello)
    if (!is.null(configurazione$formula_custom)) updateTextInput(session, "formula_custom", value = configurazione$formula_custom)
    if (!is.null(configurazione$tipo_loss)) updateSelectInput(session, "tipo_loss", selected = configurazione$tipo_loss)
    if (!is.null(configurazione$ponderazione)) updateSelectInput(session, "ponderazione", selected = configurazione$ponderazione)
    if (!is.null(configurazione$ponderazione_zeta)) updateNumericInput(session, "ponderazione_zeta", value = configurazione$ponderazione_zeta)
    if (!is.null(configurazione$batch_zeta_modo)) updateRadioButtons(session, "batch_zeta_modo", selected = configurazione$batch_zeta_modo)
    if (!is.null(configurazione$algoritmo)) updateSelectInput(session, "algoritmo", selected = configurazione$algoritmo)
    if (!is.null(configurazione$metodo_errore)) updateSelectInput(session, "metodo_errore", selected = configurazione$metodo_errore)
    if (!is.null(configurazione$n_boot)) updateNumericInput(session, "n_boot", value = configurazione$n_boot)
    if (!is.null(configurazione$tipo_rumore_mc)) updateSelectInput(session, "tipo_rumore_mc", selected = configurazione$tipo_rumore_mc)
    if (!is.null(configurazione$label_x)) updateTextInput(session, "label_x", value = configurazione$label_x)
    if (!is.null(configurazione$label_y)) updateTextInput(session, "label_y", value = configurazione$label_y)
    if (!is.null(configurazione$applica_limiti_predefiniti)) updateCheckboxInput(session, "applica_limiti_predefiniti", value = configurazione$applica_limiti_predefiniti)
    # Restores all the aesthetic customization controls, by type -
    # same "only if present in the file" logic as above.
    for (id in CONFIG_CAMPI_SELECT) if (!is.null(configurazione[[id]])) updateSelectInput(session, id, selected = configurazione[[id]])
    for (id in CONFIG_CAMPI_SLIDER) if (!is.null(configurazione[[id]])) updateSliderInput(session, id, value = configurazione[[id]])
    for (id in CONFIG_CAMPI_CHECKBOX) if (!is.null(configurazione[[id]])) updateCheckboxInput(session, id, value = configurazione[[id]])
    for (id in CONFIG_CAMPI_NUMERIC) if (!is.null(configurazione[[id]])) updateNumericInput(session, id, value = configurazione[[id]])
    if (!is.null(configurazione$esponente_tolleranza) && !is.null(configurazione$algoritmo) && !identical(configurazione$algoritmo, isolate(input$algoritmo))) {
      tolleranza_in_attesa(configurazione$esponente_tolleranza)
    }
    for (id in CONFIG_CAMPI_TEXT) if (!is.null(configurazione[[id]])) updateTextInput(session, id, value = configurazione[[id]])
    # Changing tipo_modello above triggers an automatic reset of the
    # guess field (another observer, same mechanism as the model selector) - but
    # that observer fires at the NEXT reactive cycle, not immediately: if
    # we wrote the loaded guess here, it would be overwritten AFTERWARDS by that
    # automatic reset, not before. session$onFlushed() instead guarantees that
    # our update happens AFTER the reactive cycle (including
    # that reset) has already stabilized.
    guess_da_ripristinare <- configurazione$guess_iniziali
    if (!is.null(guess_da_ripristinare)) {
      session$onFlushed(function() {
        updateTextInput(session, "guess_iniziali", value = guess_da_ripristinare)
      }, once = TRUE)
    }
    showNotification("Configuration loaded successfully.", type = "message")
  })
  
  observeEvent(input$aggiungi_simbolo, {
    nome <- trimws(input$simbolo_par_nome)
    display <- trimws(input$simbolo_par_display)
    if (nchar(nome) == 0 || nchar(display) == 0) {
      showNotification("Enter both a parameter name and a display symbol.", type = "error")
      return()
    }
    mappa <- simboli_parametri()
    mappa[[nome]] <- display
    simboli_parametri(mappa)
    updateTextInput(session, "simbolo_par_nome", value = "")
    updateTextInput(session, "simbolo_par_display", value = "")
  })
  
  observeEvent(input$svuota_simboli, {
    simboli_parametri(list())
    showNotification("All display symbols cleared.", type = "message")
  })
  
  output$tabella_simboli_parametri <- renderTable({
    mappa <- simboli_parametri()
    if (length(mappa) == 0) return(data.frame(Message = "No custom display symbols defined yet."))
    data.frame(Parameter = names(mappa), `Display Symbol` = unlist(mappa, use.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Automatic fallback for parameters WITHOUT an explicit mapping in the
  # registry (field "simboli") - still recognizes two common patterns:
  # 1) the name, made lowercase, is a known lowercase Greek letter (e.g.
  #    "MU" or "Mu" -> "mu", which plotmath automatically draws as mu)
  #    - I first check the EXACT name among the keys of GRECO_HTML, so
  #    a hypothetical parameter literally called "Gamma" (UPPERCASE Greek
  #    letter Gamma, a distinct key in GRECO_HTML) is not
  #    confused with lowercase "gamma".
  # 2) the name ends with one or more digits (e.g. "n1", "k12", "b2") -
  #    rewritten in plotmath subscript notation, e.g. "n[1]", "k[12]".
  # For very long formulas (e.g. Integrated Baranyi), instead of
  # shrinking the font more than necessary on a single line, I look for a
  # safe point to break it onto two lines: a "+"/"-" at a level
  # NOT nested inside parentheses (depth 0), as close as
  # possible to the centre of the string. I never choose a unary sign
  # (e.g. the "-" inside "exp(-MU*x)", or right after another operator) -
  # only a true binary operator, recognizable by the fact that the
  # previous non-space character is the end of a complete term
  # (letter, digit, or closing parenthesis), not another operator or "(".
  trova_punto_split_formula <- function(formula_stringa) {
    n <- nchar(formula_stringa)
    centro <- n %/% 2
    profondita <- 0
    candidati <- integer(0)
    for (i in seq_len(n)) {
      carattere <- substr(formula_stringa, i, i)
      if (carattere == "(") {
        profondita <- profondita + 1
      } else if (carattere == ")") {
        profondita <- profondita - 1
      } else if (profondita == 0 && carattere %in% c("+", "-") && i > 1 && i < n) {
        j <- i - 1
        while (j >= 1 && substr(formula_stringa, j, j) == " ") j <- j - 1
        precedente <- if (j >= 1) substr(formula_stringa, j, j) else ""
        if (precedente != "" && !(precedente %in% c("+", "-", "*", "/", "("))) {
          candidati <- c(candidati, i)
        }
      }
    }
    if (length(candidati) == 0) return(NULL)
    candidati[which.min(abs(candidati - centro))]
  }
  
  # Applies the split only beyond a length threshold (60 characters,
  # deliberately higher than the threshold of 40 used for the font
  # shrinking alone) - moderately long formulas stay
  # on one line with a slight shrinking, already acceptable; the
  # real need to break the line is felt only for the very long ones.
  # If no valid split point exists (e.g. the whole formula is
  # enclosed in a single outer function), it falls back to the single line.
  dividi_formula_lunga <- function(formula_stringa, soglia = 140) {
    if (nchar(formula_stringa) <= soglia) return(list(riga1 = formula_stringa, riga2 = NULL))
    pos <- trova_punto_split_formula(formula_stringa)
    if (is.null(pos)) return(list(riga1 = formula_stringa, riga2 = NULL))
    list(riga1 = trimws(substr(formula_stringa, 1, pos - 1)),
         riga2 = trimws(substr(formula_stringa, pos, nchar(formula_stringa))))
  }
  
  simbolo_automatico_pedice <- function(nome) {
    if (nome %in% names(GRECO_HTML)) return(nome)
    nomi_greci_minuscoli <- names(GRECO_HTML)[names(GRECO_HTML) == tolower(names(GRECO_HTML))]
    if (tolower(nome) %in% nomi_greci_minuscoli) return(tolower(nome))
    m <- regmatches(nome, regexec("^([A-Za-z]+)([0-9]+)$", nome))[[1]]
    if (length(m) == 3) {
      base <- m[2]; pedice <- m[3]
      base_finale <- if (tolower(base) %in% nomi_greci_minuscoli) tolower(base) else base
      return(paste0(base_finale, "[", pedice, "]"))
    }
    nome
  }
  
  # Resolves the display symbol for a parameter, falling back to its raw name
  # if no custom symbol has been defined for it.
  simbolo_visualizzazione <- function(nome_parametro) {
    mappa <- simboli_parametri()
    if (!is.null(mappa[[nome_parametro]])) mappa[[nome_parametro]] else simbolo_automatico_pedice(nome_parametro)
  }
  
  # Builds a plotmath legend from a vector of parameter
  # names, applying their Display Symbols (if defined). If no
  # special symbol is defined for any of them, it simply returns
  # the original text vector (no difference with respect
  # to before); otherwise it builds a true mixed expression() object
  # (plain text + plotmath notation), which legend()/plot() already know
  # how to interpret correctly element by element.
  costruisci_legenda_simboli <- function(nomi_parametri) {
    etichette <- lapply(nomi_parametri, function(n) interpreta_etichetta(simbolo_visualizzazione(n)))
    if (all(sapply(etichette, is.character))) {
      unlist(etichette)
    } else {
      do.call(expression, etichette)
    }
  }
  
  # ---- HTML rendering of parameter symbols in real tables ----
  # Plotmath (used for graphics/plots) is a DIFFERENT rendering engine than
  # HTML, and doesn't apply inside a renderTable() output - this is a
  # separate, purpose-built translator from the same "plotmath-like" typed
  # syntax (e.g. "mu[max]", "sigma^2") into safe HTML markup, used only for
  # the Parameter Estimation Results table.
  GRECO_HTML <- c(
    alpha = "\u03b1", beta = "\u03b2", gamma = "\u03b3", delta = "\u03b4", epsilon = "\u03b5",
    zeta = "\u03b6", eta = "\u03b7", theta = "\u03b8", iota = "\u03b9", kappa = "\u03ba",
    lambda = "\u03bb", mu = "\u03bc", nu = "\u03bd", xi = "\u03be", pi = "\u03c0",
    rho = "\u03c1", sigma = "\u03c3", tau = "\u03c4", upsilon = "\u03c5", phi = "\u03c6",
    chi = "\u03c7", psi = "\u03c8", omega = "\u03c9",
    Gamma = "\u0393", Delta = "\u0394", Theta = "\u0398", Lambda = "\u039b", Xi = "\u039e",
    Pi = "\u03a0", Sigma = "\u03a3", Upsilon = "\u03a5", Phi = "\u03a6", Psi = "\u03a8", Omega = "\u03a9"
  )
  
  # Escapes HTML-meaningful characters. Applied to EVERY piece of raw text
  # before it's placed inside constructed HTML, so nothing typed by the user
  # ever ends up as literal, unescaped HTML in the page - only the specific
  # <sub>/<sup> tags WE build ourselves below are ever emitted unescaped.
  escape_html <- function(testo) {
    testo <- gsub("&", "&amp;", testo, fixed = TRUE)
    testo <- gsub("<", "&lt;", testo, fixed = TRUE)
    testo <- gsub(">", "&gt;", testo, fixed = TRUE)
    testo
  }
  
  # Translates a single display-symbol string ("mu[max]", "sigma^2", "mu",
  # or plain text) into safe HTML. Anything not matching a recognized
  # pattern falls back to plain (HTML-escaped) text - same "safe by
  # default" philosophy as interpreta_etichetta() for plots.
  # Replaces EVERY parameter ACTUALLY present in the model (read
  # from its own "guess" field in the registry through parse_guesses, not a
  # generic recognition of Greek words in the text) with its
  # correct HTML symbol - uses the SAME registry mapping
  # (simbolo_visualizzazione, with the same automatic fallback for
  # Greek letters written in uppercase and numeric subscripts) already used for the
  # Fitted Model Equation, so sidebar, selection cards and equation
  # after the fit are always consistent with each other instead of using
  # different, unconnected mechanisms (the original problem: a parameter
  # such as "L", mapped to "lambda" ONLY in the registry of that specific
  # model, was never recognized by a generic
  # search for Greek words already spelled out in the text).
  abbellisci_formula_html <- function(id_modello) {
    info_modello <- MODEL_REGISTRY[[id_modello]]
    if (is.null(info_modello) || is.null(info_modello$display)) return("")
    testo <- info_modello$display
    if (is.null(info_modello$guess)) return(escape_html(testo))
    
    guesses_raw <- tryCatch(parse_guesses(info_modello$guess), error = function(e) NULL)
    if (is.null(guesses_raw) || length(guesses_raw$tutti_nomi) == 0) return(escape_html(testo))
    
    # Symbol mapping OF THIS SPECIFIC MODEL (id_modello), read
    # directly from the registry - NOT through simbolo_visualizzazione(),
    # which instead reads simboli_parametri() (the state of the model
    # CURRENTLY selected in the sidebar). For the "Choose a Model" popup,
    # which generates a card for EVERY model (not only the current one),
    # using simbolo_visualizzazione() would apply the wrong mapping
    # (or an empty one) to all the cards other than that of the model already chosen -
    # exactly the reason why "L" remained unchanged in the popup: the
    # L->lambda mapping of the CURRENT model was ignored for every
    # OTHER model of the list, which does not necessarily share that same
    # naming convention.
    # as.list() is necessary: $simboli in the registry is always a named
    # vector (created with c(...), never list()) - on a vector, [[ with an
    # absent name throws an error ("subscript out of bounds") instead of
    # returning NULL as a list would.
    mappa_propria <- as.list(info_modello$simboli)
    
    # Decreasing order of length, to avoid a shorter name
    # (e.g. "N") being replaced BEFORE a longer one that contains it
    # (e.g. "N0"), corrupting the subsequent replacement.
    nomi_parametri <- guesses_raw$tutti_nomi[order(-nchar(guesses_raw$tutti_nomi))]
    
    testo_escaped <- escape_html(testo)
    for (nome in nomi_parametri) {
      simbolo <- if (!is.null(mappa_propria) && !is.null(mappa_propria[[nome]])) mappa_propria[[nome]] else simbolo_automatico_pedice(nome)
      simbolo_html <- simbolo_a_html(simbolo)
      testo_escaped <- gsub(paste0("\\b", nome, "\\b"), simbolo_html, testo_escaped)
    }
    gsub("\\*", "&times;", testo_escaped)
  }
  
  simbolo_a_html <- function(testo) {
    if (is.null(testo) || nchar(trimws(testo)) == 0) return(testo)
    
    m_sub <- regmatches(testo, regexec("^([A-Za-z]+)\\[([^]]+)\\]$", testo))[[1]]
    if (length(m_sub) == 3) {
      base <- m_sub[2]; pedice <- m_sub[3]
      base_html <- if (base %in% names(GRECO_HTML)) GRECO_HTML[[base]] else escape_html(base)
      return(paste0(base_html, "<sub>", escape_html(pedice), "</sub>"))
    }
    
    m_sup <- regmatches(testo, regexec("^([A-Za-z]+)\\^\\{?([^}]+)\\}?$", testo))[[1]]
    if (length(m_sup) == 3) {
      base <- m_sup[2]; apice <- m_sup[3]
      base_html <- if (base %in% names(GRECO_HTML)) GRECO_HTML[[base]] else escape_html(base)
      return(paste0(base_html, "<sup>", escape_html(apice), "</sup>"))
    }
    
    if (testo %in% names(GRECO_HTML)) return(GRECO_HTML[[testo]])
    
    escape_html(testo)
  }
  
  # Translates a symbol into PLAIN Unicode text (no HTML markup, no
  # plotmath syntax) - for the contexts that support neither one nor
  # the other: persp() (official documentation: "expressions are not
  # accepted" for xlab/ylab/zlab) and HTML drop-down menus (the text inside
  # <option> never interprets markup). Greek letters become the real
  # Unicode character (no markup is needed for that); numeric subscripts
  # become Unicode subscript digits (e.g. "N[0]" -> "N\u2080"); non-numeric
  # subscripts (e.g. "max") have no real Unicode equivalent, so
  # they fall back to a readable underscore only for the letters without a
  # real dedicated Unicode subscript (e.g. "mu[b...]" with uncovered letters).
  CIFRE_PEDICE <- c("0"="\u2080","1"="\u2081","2"="\u2082","3"="\u2083","4"="\u2084",
                     "5"="\u2085","6"="\u2086","7"="\u2087","8"="\u2088","9"="\u2089")
  # Unicode has dedicated subscripts only for 17 lowercase Latin letters out of 26
  # (b,c,d,f,g,q,w,y,z are missing) - for these the fallback remains the
  # underscore. E.g. "max" (m,a,x all covered) -> real subscript "\u2098\u2090\u2093".
  LETTERE_PEDICE <- c(a="\u2090", e="\u2091", h="\u2095", i="\u1d62", j="\u2c7c",
                       k="\u2096", l="\u2097", m="\u2098", n="\u2099", o="\u2092",
                       p="\u209a", r="\u1d63", s="\u209b", t="\u209c", u="\u1d64",
                       v="\u1d65", x="\u2093")
  
  simbolo_a_testo_semplice <- function(testo) {
    if (is.null(testo) || nchar(trimws(testo)) == 0) return(testo)
    
    m_sub <- regmatches(testo, regexec("^([A-Za-z]+)\\[([^]]+)\\]$", testo))[[1]]
    if (length(m_sub) == 3) {
      base <- m_sub[2]; pedice <- m_sub[3]
      base_txt <- if (base %in% names(GRECO_HTML)) GRECO_HTML[[base]] else base
      if (grepl("^[0-9]+$", pedice)) {
        cifre <- strsplit(pedice, "")[[1]]
        return(paste0(base_txt, paste0(CIFRE_PEDICE[cifre], collapse = "")))
      }
      if (grepl("^[A-Za-z]+$", pedice)) {
        lettere <- strsplit(tolower(pedice), "")[[1]]
        # If EVERY letter has a real Unicode subscript, it uses that (e.g. "max" ->
        # real subscript); if even a single letter is not covered (e.g. "b",
        # "c"...), it falls back to the underscore for the whole word instead of
        # a "mixed" result (partly real subscript, partly not) that would be
        # visually inconsistent.
        if (all(lettere %in% names(LETTERE_PEDICE))) {
          return(paste0(base_txt, paste0(LETTERE_PEDICE[lettere], collapse = "")))
        }
      }
      return(paste0(base_txt, "_", pedice))
    }
    
    m_sup <- regmatches(testo, regexec("^([A-Za-z]+)\\^\\{?([^}]+)\\}?$", testo))[[1]]
    if (length(m_sup) == 3) {
      base <- m_sup[2]; apice <- m_sup[3]
      base_txt <- if (base %in% names(GRECO_HTML)) GRECO_HTML[[base]] else base
      return(paste0(base_txt, "^", apice))
    }
    
    if (testo %in% names(GRECO_HTML)) return(GRECO_HTML[[testo]])
    
    testo
  }
  
  # ---- Enriched, plotmath-rendered model formula ----
  # Builds an R language object (usable as a plot title/label) from the raw
  # formula string, substituting each parameter's raw name with its Display
  # Symbol (if one is defined) before parsing. Layered fallback: if the
  # substituted version fails to parse (e.g. a malformed custom symbol), try
  # the raw formula instead; if even that fails, return NULL (caller falls
  # back to plain text).
  costruisci_testo_formula_sostituita <- function(res) {
    if (is.null(res) || !isTRUE(res$successo) || is.null(res$formula)) return(NULL)
    
    nomi_parametri <- names(res$parametri)
    if (!is.null(res$fissi) && length(res$fissi) > 0) nomi_parametri <- c(nomi_parametri, names(res$fissi))
    # Sorts by decreasing length: replaces the longest names first,
    # so a name like "A0" is not spoiled by a replacement made
    # earlier on "A" alone.
    nomi_parametri <- nomi_parametri[order(-nchar(nomi_parametri))]
    
    mappa_propria <- if (!is.null(res$tipo_modello_usato)) as.list(MODEL_REGISTRY[[res$tipo_modello_usato]]$simboli) else NULL
    
    formula_sostituita <- res$formula
    # R's plotmath engine natively recognizes the words "gamma" and
    # "beta" as the corresponding Greek letters in ANY expression
    # parsed with str2expression(), regardless of the symbol
    # mapping above - a problem for the Gamma Distribution and the Beta
    # Distribution, where gamma(k)/beta(a,b) are the mathematical FUNCTIONS
    # (generalization of the factorial / Euler's Beta function), not
    # Greek letters. I enclose them in a literal string before
    # parsing, which plotmath draws as text instead of as a symbol -
    # only when the word is IMMEDIATELY followed by an opening
    # parenthesis (a true function call), so a model such as
    # Weibull, where "beta" is a genuine parameter written as a bare
    # word (never followed by a parenthesis), is not touched.
    formula_sostituita <- gsub("\\bgamma\\(", "\"Gamma\"(", formula_sostituita)
    formula_sostituita <- gsub("\\bbeta\\(", "\"Beta\"(", formula_sostituita)
    for (nome in nomi_parametri) {
      simbolo <- if (!is.null(mappa_propria) && !is.null(mappa_propria[[nome]])) mappa_propria[[nome]] else simbolo_automatico_pedice(nome)
      if (!identical(simbolo, nome)) {
        formula_sostituita <- gsub(paste0("\\b", nome, "\\b"), simbolo, formula_sostituita)
      }
    }
    formula_sostituita
  }
  source("server_chunks/motore_calcolo.R", local = TRUE)
  source("server_chunks/rendering_output.R", local = TRUE)
  source("server_chunks/export_dati.R", local = TRUE)
  source("server_chunks/funzioni_grafiche.R", local = TRUE)
  source("server_chunks/diagnostica_residui.R", local = TRUE)
  source("server_chunks/model_comparison.R", local = TRUE)
  source("server_chunks/predizione_inversa.R", local = TRUE)
  source("server_chunks/grid_search.R", local = TRUE)
  source("server_chunks/batch_elaboration.R", local = TRUE)
  source("server_chunks/diagnostica_batch.R", local = TRUE)
  source("server_chunks/shelf_life.R", local = TRUE)
  source("server_chunks/non_isotermico.R", local = TRUE)
  source("server_chunks/non_isotermico_mc.R", local = TRUE)
}