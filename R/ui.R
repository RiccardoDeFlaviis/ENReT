# ------------------------------------------------------------------------------
# 1. USER INTERFACE (UI)
# ------------------------------------------------------------------------------

# ---- ESTABLISHED CONVENTIONS (check BEFORE adding something
# new - most of the inconsistencies that arose so far come from building
# by looking only at the local context instead of comparing it with these) ----
#
# 1. VALIDATION OF THE "RUN"/"FIT"/"EXECUTE" BUTTONS: never disable a
#    button via JS/CSS based on a precondition (e.g. "a valid
#    batch is needed first"). The button ALWAYS stays clickable; the validation
#    happens INSIDE its observeEvent, with showNotification(type="error")
#    and an immediate return() if the precondition is not met.
#    This is the only pattern used throughout the app (Run Batch Elaboration,
#    Run Shelf-Life Analysis, Fit Secondary Model, etc.) - a button
#    disabled via JS was an isolated exception and had to be corrected.
#    (Legitimate JS disabling, different from this case, remains: the
#    "bottoniLenti" protection against double clicks during an elaboration
#    in progress, and hiding/showing entire sections with conditionalPanel
#    depending on the active tab - neither concerns "the operator does not
#    yet have the right condition to proceed".)
#
# 2. SIZE OF THE PRIMARY BUTTONS (CSS class): three hierarchical levels,
#    same size for the same role in every module.
#      - btn-lg:  ONLY "Run Optimization Calculation" (the central action
#                 of the whole app).
#      - normal (no modifier, only "btn-primary w-100"): MAIN action
#                 of a section - Run Batch Elaboration, Run
#                 Shelf-Life Analysis, Fit Secondary Model, Calculate X.
#      - btn-sm:  SECOND-LEVEL action, which presupposes an already existing fit/
#                 batch/comparison - Run Post-Hoc Comparison, Run
#                 Test, Predict Critical Time, Run Model Comparison, Generate
#                 Synthetic Data.
#
# 3. COLOUR OF DESTRUCTIVE BUTTONS: solid orange (background-color
#    #e67e22, white text) for "Remove Selected X" (removes ONE element);
#    btn-danger (red) for "Remove All X" (empties everything) - a deliberate
#    distinction based on the severity of the action, not an inconsistency.
#
# 4. LISTS OF SAMPLES/MODELS WITH CHECKBOXES (checkboxGroupInput): every
#    list of this type must have a pair of "Select All"/
#    "Deselect All" buttons right above it (class = "btn-outline-secondary btn-sm"),
#    which call updateCheckboxGroupInput() on the same data source
#    used to populate the choices - never an exception without a reason.
#
# 5. "CONFIDENCE/PREDICTION LEVEL" SETTINGS INDEPENDENT OF THE SIDEBAR:
#    when a module has its own level separate from the global one in the
#    sidebar ("Confidence/Prediction Level"), the label of the field must be
#    "Confidence/Prediction Level (%):" (or an explicit variant of the type
#    of threshold, such as "...for F-test (%):" in Grid Search) - "Confidence
#    Level" alone is imprecise when the field also governs the Prediction
#    Intervals, not only the Confidence Intervals - and it must always be accompanied
#    by a note explicitly stating its independence
#    from the global field, so that the operator does not wonder whether the two
#    interact with each other.
#
# 6. TABLE HEADERS: always with spaces, never with underscores (the
#    internal/export columns may keep underscores for
#    compatibility with CSV/Excel - the replacement must be done only
#    in the last step, inside the renderTable, never at the shared source).
#
# 7. OPERATOR CONFIGURATION (saving/loading settings, see
#    CONFIG_CAMPI_SELECT/SLIDER/CHECKBOX/NUMERIC/TEXT in R/server.R): every
#    NEW selectInput/sliderInput/checkboxInput/numericInput/textInput
#    added in any module must also be added to the CONFIG_CAMPI_* list
#    of its type, in the same change - not at a later time.

# Force-load the packages required for the interface
library(shiny)
library(bslib)

# Load the Model Registry RIGHT HERE (not just in app.R/global.R): with
# local = TRUE it's guaranteed to end up in the exact same environment this
# file's own code runs in, regardless of how ui.R is invoked (explicit
# app.R, Shiny's automatic loadSupport(), Shinylive, etc.) - this removes
# at the root the "could not find function costruisci_scelte_modello()"
# errors caused by nested source() calls landing in a different environment
# than expected.
source("model_registry.R", local = TRUE)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      /* Keeps the main tab bar (Initial Guess Simulator, Graphical
         Analysis & Parameters, Model Comparison, etc.) always visible while
         scrolling the active tab's content, instead of disappearing upward. */
      .nav-tabs {
        position: sticky;
        top: 0;
        z-index: 1020;
        background-color: #ffffff;
        padding-top: 6px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }
      /* Visible outline on each of the main tabs (Single Fit
         Analysis, Model Comparison, Batch Elaboration, etc.) - the
         bold alone (set via JS, see stilizzaSchedeEstetica)
         did not distinguish them enough from the rest of the page. A light
         border on all of them, stronger and with a different background on the tab
         currently active, to see at a glance where one is.
         Scoped to #main_tabs only - it does not touch the other nested tabs
         in the app (e.g. inside the customization modal). */
      #main_tabs.nav-tabs .nav-link {
        border: 1px solid #b0b8bf !important;
        border-bottom: none !important;
        border-radius: 6px 6px 0 0 !important;
        margin-right: 4px;
      }
      #main_tabs.nav-tabs .nav-link.active {
        border: 2px solid #2c3e50 !important;
        border-bottom: none !important;
        background-color: #eaf1f8 !important;
      }
      /* Makes Shiny's native 'play' button more visible for the
         animated sliders (Initial Guess Simulator, Continuous Mode) - by
         default it is a small grey triangle, easy to overlook.
         The selector is deliberately broad (element + all descendants,
         + several colour properties) because it was not possible to verify
         live in this environment which exact mechanism is used to
         draw the triangle (font icon, CSS border, or SVG) - it covers
         all three possibilities to maximize the probability that
         at least one hits the right element. */
      .slider-animate-container, .slider-animate-container *,
      .slider-animate-button, .slider-animate-button * {
        font-size: 26px !important;
        color: #c0392b !important;
        border-left-color: #c0392b !important;
        border-color: #c0392b !important;
        fill: #c0392b !important;
        font-weight: bold !important;
        opacity: 1 !important;
      }
      /* Slightly coloured background for the headers of the collapsible
         panels (accordion), to visually distinguish them from the content
         below - .accordion-button is the standard class of
         Bootstrap 5, used by bslib for every panel. */
      .accordion-button {
        background-color: #eaf2f8 !important;
      }
      .accordion-button:not(.collapsed) {
        background-color: #d6e9f7 !important;
      }
      /* Redraws the sliders (ionRangeSlider, the library behind
         Shiny's sliderInput) with an accent colour instead of the default
         grey - thicker bar, larger handle with a
         coloured border, value label in coloured bold.
         Applied globally: it improves the look of every slider
         of the app, not only those of the Initial Guess Simulator. */
      .irs-bar {
        background: #2980b9 !important;
        border-top: 1px solid #2980b9 !important;
        border-bottom: 1px solid #2980b9 !important;
        height: 8px !important;
      }
      .irs-line {
        height: 8px !important;
        border-radius: 4px !important;
        background: #e0e6e8 !important;
      }
      .irs-slider {
        background: #ffffff !important;
        border: 2px solid #2980b9 !important;
        box-shadow: 0 1px 4px rgba(0,0,0,0.35) !important;
        width: 18px !important;
        height: 18px !important;
        top: 22px !important;
        border-radius: 50% !important;
      }
      .irs-single {
        background: #2980b9 !important;
        font-weight: 700 !important;
      }
      .irs-min, .irs-max {
        color: #7f8c8d !important;
      }
      /* Defensive fix for the text of radio buttons/checkboxes that wrapped
         too early despite the available space - the app uses
         Bootstrap 5 (bs_theme) but Shiny's base functions generate HTML
         in Bootstrap 3 style for these controls, a mismatch that
         can cause unexpected width behaviour. I force a
         flex layout on the label (fixed-width checkbox/dot,
         text that fills all the remaining space), covering both the
         Bootstrap 3 and the Bootstrap 5/bslib class names, since it
         was not possible to verify live which one is actually
         generated in this environment. */
      /* Shiny limits .shiny-input-container by default to 300px of
         width (designed for text boxes) - but the same
         class also wraps checkboxes and radio buttons, where that limit has no
         reason to exist and cuts the space available for the text
         much earlier than seems necessary. The two previous
         attempts modified only the inner label, not this
         outer container, which is probably the real cause. */
      .checkbox.shiny-input-container, .radio.shiny-input-container,
      div.shiny-input-container:has(> .checkbox), div.shiny-input-container:has(> .radio) {
        width: 100% !important;
        max-width: 100% !important;
      }
      .checkbox label, .radio label, .form-check {
        display: flex !important;
        align-items: flex-start !important;
        white-space: normal !important;
        width: 100% !important;
      }
      .checkbox label input, .radio label input, .form-check-input {
        flex-shrink: 0 !important;
        margin-right: 8px !important;
        margin-top: 3px !important;
      }
      /* Arranges the Input Method options (single fit, short labels)
         in 2 columns instead of stacking them vertically - column-count
         fills the first column from top to bottom before moving to the
         next. NOT applied to Batch: its labels are much
         longer and would wrap onto several lines in a narrow
         column. */
      .radio-due-colonne, .radio-due-colonne .shiny-options-group, .radio-due-colonne .shiny-input-container {
        width: 100% !important;
        max-width: 100% !important;
      }
      .radio-due-colonne .shiny-options-group {
        column-count: 2;
        column-gap: 10px;
      }
    ")),
    tags$script(HTML("
      (function() {
        // The server-side workaround removes the 'download' attribute from
        // download buttons to work around a Shinylive Service Worker bug on
        // Chrome. Safari, however, requires that attribute to recognize the
        // link as a file to save (otherwise it tries to 'navigate' to the
        // URL and fails, saving the interface's HTML instead of the actual
        // file). This script restores it, but ONLY on Safari, leaving
        // Chrome unaffected.
        var ua = navigator.userAgent;
        var isSafari = /^((?!chrome|android).)*safari/i.test(ua);
        if (!isSafari) return;
        
        function ripristinaAttributoDownload() {
          document.querySelectorAll('a.shiny-download-link').forEach(function(link) {
            if (!link.hasAttribute('download')) {
              link.setAttribute('download', '');
            }
          });
        }
        
        var observer = new MutationObserver(ripristinaAttributoDownload);
        observer.observe(document.documentElement, { childList: true, subtree: true });
        document.addEventListener('DOMContentLoaded', ripristinaAttributoDownload);
      })();
    ")),
    tags$script(HTML("
      (function() {
        // Disables the 'Run Optimization Calculation' button as soon as it is
        // clicked, to prevent multiple clicks in quick succession
        // (especially before a heavy Monte Carlo modelling has
        // finished) from launching several overlapping/repeated computations. It is
        // re-enabled automatically when Shiny returns 'idle' (it has finished
        // all pending reactive work): since R/webR are
        // single-threaded, this always coincides with the real end of the
        // computation, never earlier.
        $(document).on('click', '#esegui', function() {
          $(this).prop('disabled', true);
        });
        // Same protection extended to the other potentially slow 'Run' buttons
        // added in this session (batch, Grid Search Rigorous,
        // Shelf-Life nested bootstrap in Batch and Model Comparison,
        // Non-Isothermal secondary-model fit and prediction in both
        // modules - all loops over bootstrap replicates) - not only the main
        // fit.
        var bottoniLenti = ['#esegui_batch', '#esegui_grid_rigoroso', '#sl_esegui_analisi',
                             '#mc_sl_esegui_analisi', '#ni_fit_secondario', '#mcni_fit_secondario',
                             '#ni_lancia_previsione', '#mcni_lancia_previsione'];
        bottoniLenti.forEach(function(idBottone) {
          $(document).on('click', idBottone, function() {
            $(this).prop('disabled', true);
          });
        });
        $(document).on('shiny:idle', function() {
          $('#esegui').prop('disabled', false);
          bottoniLenti.forEach(function(idBottone) {
            $(idBottone).prop('disabled', false);
          });
        });
      })();
    ")),
    tags$script(HTML("
      (function() {
        // REALLY disables (native HTML attribute, not just a
        // server-side warning) the 'Run Optimization Calculation' button
        // when on the Batch Elaboration tab, where
        // that button makes no sense (that tab has its own 'Run Batch
        // Elaboration' on completely different data). A click on a button
        // disabled via the native 'disabled' attribute does not reach
        // Shiny at all (the value of input$esegui does not increase) -
        // it is the only reliable way to prevent an accidental click from
        // overwriting the single fit already computed, since a
        // server-side check INSIDE the eventReactive would not be enough (it would
        // still update its stored value with an empty result).
        var suTabBatch = false;
        Shiny.addCustomMessageHandler('toggleEseguiBatchTab', function(valore) {
          suTabBatch = valore;
          aggiornaBottoneEsegui();
        });
        // Automatically starts the animation of the parameter just chosen,
        // without the need for a second click on the native play button of the
        // slider - the slider with animate=TRUE does not yet exist in the DOM
        // when this message arrives (the server-side renderUI must
        // first rebuild and send back the updated HTML), so it
        // retries at intervals until the button appears (up to 2
        // seconds), then clicks it only once.
        Shiny.addCustomMessageHandler('avviaAnimazioneSlider', function(idSlider) {
          var tentativi = 0;
          var intervallo = setInterval(function() {
            tentativi++;
            var contenitore = document.getElementById(idSlider);
            var bottonePlay = contenitore ? contenitore.closest('.form-group').querySelector('.slider-animate-button') : null;
            if (bottonePlay) {
              clearInterval(intervallo);
              bottonePlay.click();
            } else if (tentativi > 20) {
              clearInterval(intervallo);
            }
          }, 100);
        });
        // Copies the text content of a table (Shiny renderTable,
        // already present in the DOM) to the clipboard, tab-separated -
        // when pasted into Excel/Sheets the columns align by themselves. It reads
        // the already rendered HTML directly, no round trip to the server.
        window.copiaTabellaClipboard = function(idTabella, idBottone) {
          var contenitore = document.getElementById(idTabella);
          if (!contenitore) { console.warn('copiaTabellaClipboard: contenitore non trovato:', idTabella); return; }
          var tabellaVera = contenitore.querySelector('table') || contenitore;
          var righe = tabellaVera.querySelectorAll('tr');
          if (righe.length === 0) { console.warn('copiaTabellaClipboard: nessuna riga trovata in', idTabella); return; }
          var testo = Array.from(righe).map(function(riga) {
            var celle = riga.querySelectorAll('th, td');
            return Array.from(celle).map(function(cella) { return cella.innerText.trim(); }).join('\\t');
          }).join('\\n');
          
          function segnalaCopiaRiuscita() {
            var bottone = document.getElementById(idBottone);
            if (!bottone) return;
            var testoOriginale = bottone.innerHTML;
            bottone.innerHTML = '\\u2713 Copied!';
            setTimeout(function() { bottone.innerHTML = testoOriginale; }, 1500);
          }
          
          // navigator.clipboard.writeText() requires a secure context
          // (HTTPS) and browser permissions - if even one of
          // these conditions is missing it fails SILENTLY (the Promise is
          // rejected, no visible error), leaving the clipboard
          // unchanged with whatever was there before. If available,
          // it is tried first anyway (modern, preferred method);
          // otherwise - or if it fails - it falls back on the older
          // but much more compatible technique: a hidden text area,
          // selected and copied with document.execCommand('copy').
          function copiaConMetodoClassico() {
            var area = document.createElement('textarea');
            area.value = testo;
            area.style.position = 'fixed';
            area.style.left = '-9999px';
            document.body.appendChild(area);
            area.focus();
            area.select();
            try {
              var riuscito = document.execCommand('copy');
              if (riuscito) segnalaCopiaRiuscita();
              else console.warn('copiaTabellaClipboard: execCommand copy ha restituito false');
            } catch (erroreClassico) {
              console.error('copiaTabellaClipboard: fallito anche il metodo classico:', erroreClassico);
            }
            document.body.removeChild(area);
          }
          
          if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(testo).then(segnalaCopiaRiuscita).catch(function(errore) {
              console.warn('copiaTabellaClipboard: Clipboard API fallita, uso il metodo classico:', errore);
              copiaConMetodoClassico();
            });
          } else {
            copiaConMetodoClassico();
          }
        };
        function aggiornaBottoneEsegui() {
          var btn = document.getElementById('esegui');
          if (!btn) return;
          if (suTabBatch) {
            btn.disabled = true;
            btn.classList.remove('btn-primary');
            btn.classList.add('btn-secondary');
            btn.title = 'This runs a single fit on the data in section 1, not the batch samples. Use \u2018Run Batch Elaboration\u2019 below instead.';
          } else {
            btn.classList.remove('btn-secondary');
            btn.classList.add('btn-primary');
            btn.title = '';
          }
        }
        // The 'shiny:idle' handler registered EARLIER (above) always re-enables
        // the button at every end of reactive work, whatever its
        // cause (not only the end of a fit) - since this
        // handler is registered LATER, it runs afterwards on the
        // same event and can therefore 'correct' that unconditional
        // re-enabling if we are still on the Batch tab.
        $(document).on('shiny:idle', function() {
          if (suTabBatch) aggiornaBottoneEsegui();
        });
      })();
    ")),
    tags$script(HTML("
      (function() {
        // Shiny hides the real <input type=file> by positioning it at
        // EXTREME NEGATIVE coordinates (off screen, e.g. top:-99999px),
        // not with a simple display:none - a common technique to
        // remain clickable/accessible while showing in its place
        // the styled 'Browse' label. The problem: when that field
        // receives the focus (by clicking on its label), the browser
        // tries to scroll the page to bring it 'into view', and since
        // it lies well ABOVE the start of the page, it stops at the top -
        // exactly the reported jump. The root solution (not
        // an attempt to correct the jump after it has already happened)
        // is to reposition the element with a hiding technique that
        // keeps it in the natural flow of the document (minimal
        // size and transparency, not off-screen coordinates) - the
        // browser no longer has any need to scroll to bring it into
        // view, since it is already where it is.
        function correggiPosizioneCampiFile() {
          document.querySelectorAll('input[type=\"file\"]').forEach(function(campo) {
            if (campo.dataset.posizioneCorretta) return;
            campo.style.position = 'absolute';
            campo.style.top = '0';
            campo.style.left = '0';
            campo.style.width = '1px';
            campo.style.height = '1px';
            campo.style.opacity = '0';
            campo.style.overflow = 'hidden';
            campo.dataset.posizioneCorretta = 'true';
          });
        }
        correggiPosizioneCampiFile();
        // The file fields inside a collapsible panel (accordion) or in
        // a tab not yet visited are created AFTER the
        // initial loading of the page - a MutationObserver applies
        // the same fix to these too, not only to those already
        // present at startup.
        var osservatore = new MutationObserver(correggiPosizioneCampiFile);
        osservatore.observe(document.documentElement, { childList: true, subtree: true });
      })();
    ")),
    tags$script(HTML("
      (function() {
        // Direct approach: I use the EXPLICIT and ALREADY KNOWN IDs of the 4
        // inner tabsetPanels (one per category) and of the outer one,
        // instead of guessing the structure of the DOM by searching for generic
        // patterns such as 'ul.nav' - it completely removes the ambiguity that
        // made the previous attempts fail. These IDs are defined
        // explicitly in the tabsetPanel(id = ...) calls in this
        // same file.
        var ID_ESTERNO = 'estetica_tabs';
        var ID_INTERNI = ['estetica_tabs_single_fit', 'estetica_tabs_grid_search', 'estetica_tabs_batch', 'estetica_tabs_model_comparison'];
        
        // The ID is assigned by Shiny to the <ul> tag itself or to the div
        // that wraps it, depending on the version - this function
        // handles both cases, always returning the real navigation <ul>
        // element.
        function trovaUl(id) {
          var el = document.getElementById(id);
          if (!el) return null;
          if (el.tagName === 'UL') return el;
          return el.querySelector('ul.nav') || el.querySelector('ul');
        }
        
        function imposta(ul, dimensione, peso, colore) {
          if (!ul) return;
          ul.querySelectorAll('a').forEach(function(link) {
            link.style.setProperty('font-size', dimensione, 'important');
            link.style.setProperty('font-weight', peso, 'important');
            link.style.setProperty('color', colore, 'important');
            link.querySelectorAll('*').forEach(function(figlio) {
              figlio.style.setProperty('font-size', dimensione, 'important');
              figlio.style.setProperty('font-weight', peso, 'important');
              figlio.style.setProperty('color', colore, 'important');
            });
          });
        }
        
        function stilizzaSchedeEstetica() {
          imposta(trovaUl(ID_ESTERNO), '13.5px', 'bold', 'black');
          ID_INTERNI.forEach(function(id) {
            imposta(trovaUl(id), '13.5px', 'normal', '#7f8c8d');
          });
          // Main tabs of the app (Initial Guess Simulator, Graphical
          // Analysis & Parameters, etc.) - same explicit ID and same
          // imposta() function already used above, for visual consistency.
          imposta(trovaUl('main_tabs'), '14px', 'bold', 'black');
          // Positions the modal on the left instead of centred, so
          // the plot being edited in real time remains
          // partially visible. It recognizes the modal by its
          // title ('Advanced Plot Aesthetics', present both in the branch
          // that builds a single tab and in the one that builds
          // all of them - #estetica_tabs exists ONLY in the second
          // case, so searching for it left out 27 of the 28 buttons
          // that open a single tab), adding a dedicated class
          // to its .modal-dialog instead of guessing a
          // global CSS selector.
          document.querySelectorAll('.modal-title').forEach(function(titolo) {
            if (titolo.textContent.indexOf('Advanced Plot Aesthetics') === -1) return;
            var dialogo = titolo.closest('.modal-dialog');
            if (dialogo && !dialogo.classList.contains('estetica-dialogo-sinistra')) {
              dialogo.classList.add('estetica-dialogo-sinistra');
              dialogo.style.setProperty('margin-left', '2vw', 'important');
              dialogo.style.setProperty('margin-right', 'auto', 'important');
            }
          });
        }
        stilizzaSchedeEstetica();
        var osservatoreEstetica = new MutationObserver(stilizzaSchedeEstetica);
        osservatoreEstetica.observe(document.documentElement, { childList: true, subtree: true });
      })();
    ")),
    tags$style(HTML("
      /* General reduction of the size of text and controls
         inside the 'Advanced Plot Aesthetics' modal (anchored to the
         class assigned via JS only to ITS .modal-dialog, not to all
         the other modals of the app) - it covers labels, numeric inputs,
         selects, sliders and checkboxes in all tabs in one go,
         instead of having to act tab by tab. */
      /* Width reduced by a quarter with respect to the standard 'xl' size
         of Bootstrap (1140px -> 855px) - since every inner tab
         uses percentage column()/fluidRow(), all the content
         shrinks automatically and proportionally, without touching
         the order or the structure of any of the customization
         tabs. */
      .estetica-dialogo-sinistra {
        max-width: 800px !important;
      }
      .estetica-dialogo-sinistra label {
        font-size: 12.5px !important;
        margin-bottom: 2px !important;
      }
      .estetica-dialogo-sinistra .form-control,
      .estetica-dialogo-sinistra .selectize-input,
      .estetica-dialogo-sinistra select.form-select {
        font-size: 12.5px !important;
        padding: 4px 8px !important;
        min-height: 30px !important;
      }
      .estetica-dialogo-sinistra .irs--shiny .irs-min,
      .estetica-dialogo-sinistra .irs--shiny .irs-max,
      .estetica-dialogo-sinistra .irs--shiny .irs-from,
      .estetica-dialogo-sinistra .irs--shiny .irs-to,
      .estetica-dialogo-sinistra .irs--shiny .irs-single {
        font-size: 10.5px !important;
      }
      .estetica-dialogo-sinistra .checkbox label,
      .estetica-dialogo-sinistra .form-check-label {
        font-size: 12.5px !important;
      }
      .estetica-dialogo-sinistra .form-group,
      .estetica-dialogo-sinistra .shiny-input-container {
        margin-bottom: 8px !important;
      }
    "))
  ),
  

div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; padding: 8px 0 6px 0; border-bottom: 1px solid #dee2e6; margin-bottom: 8px;",
      div(style = "display: flex; align-items: center; gap: 10px;",
          tags$img(src = "data:image/png;base64,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", style = "height: 55px; width: auto;", alt = "ENReT logo"),
          tags$h4(style = "margin: 0; font-weight: bold; color: #2c3e50;",
                  "Easy Non-Linear Regression Tool ",
                  tags$span(style = "font-size: 13px; color: #95a5a6; font-weight: normal;", "v1.0.0"),
                  tags$span(style = "color: #18bc9c; font-style: italic; margin-left: 6px;", "for food technologists"))),
      tags$div(style = "font-size: 12px; color: black; text-align: right;",
               tags$strong(style = "font-size: 18px;", "Riccardo De Flaviis, PhD"))
  ),
  

  sidebarLayout(
    sidebarPanel(
      width = 4,
      style = "padding-right: 15px;",
      
      actionButton("reset_app", "Reset App", class = "btn-warning btn-sm w-100", icon = icon("sync")),
      div(style = "margin-bottom: 4px;"),
      
      div(style = "max-height: 75vh; overflow-y: auto; padding-right: 5px;",
      
      accordion(
        id = "sidebar_accordion",
        open = "sec1",
        multiple = TRUE,
        
        accordion_panel(
          "1. Data Input for Single Fit", value = "sec1", icon = icon("table"),
          
          div(class = "radio-due-colonne",
            radioButtons("metodo_dati", "Input Method:",
                         choices = c("Direct Input (Table)" = "manuale", "Upload CSV/Excel File" = "csv", "Generate Synthetic Data" = "sintetico", "Reference/Bibliographic Data" = "riferimento"),
                         selected = "manuale")
          ),
          conditionalPanel(
            condition = "input.metodo_dati == 'manuale' || input.metodo_dati == 'csv'",
            checkboxInput("inverti_xy", "Swap X and Y (data entered or loaded as y, x)", value = FALSE)
          ),
          
          conditionalPanel(
            condition = "input.metodo_dati == 'riferimento'",
            p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 6px;",
              "Datasets with certified or published parameter values, useful for validating the app's results against an independent, known reference (e.g. the NIST Statistical Reference Datasets)."),
            selectInput("dataset_riferimento_scelto", "Dataset:", choices = costruisci_scelte_dataset_riferimento()),
            uiOutput("info_dataset_riferimento"),
            div(style = "max-height: 220px; overflow-y: auto; border: 1px solid #dfe4e6; border-radius: 4px; padding: 4px 8px;",
                tags$button(id = "btn_copia_dataset_riferimento", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                  style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 6px;",
                  onclick = "copiaTabellaClipboard('tabella_dataset_riferimento', 'btn_copia_dataset_riferimento')"),
                tableOutput("tabella_dataset_riferimento"))
          ),
          
          conditionalPanel(
            condition = "input.metodo_dati == 'csv'",
            p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 6px;",
              "The file needs two columns: X values in the first, Y values in the second (any extra columns are ignored). A header row is optional \u2014 it's auto-detected: if the first row is entirely numeric, it's read as data, not as column names."),
            fileInput("file_dati", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls"))
          ),
          
          conditionalPanel(
            condition = "input.metodo_dati == 'manuale'",
            textAreaInput("dati_manuali", "Data (x y):", 
                          value = "1.0  6.5\n2.0  4.2\n3.0  2.8\n4.0  1.9\n5.0  1.2\n6.0  0.8", 
                          rows = 6),
            fluidRow(
              column(12, actionButton("clear_data", "Clear Data", class = "btn-danger btn-sm w-100", icon = icon("trash")))
            )
          ),
          
          conditionalPanel(
            condition = "input.metodo_dati == 'sintetico'",
            div(style = "background-color: #fff3cd; border: 1px solid #ffe69c; padding: 8px 10px; border-radius: 4px; margin-bottom: 10px; font-size: 11px; color: #664d03;",
                icon("triangle-exclamation"), " For quick exploration/teaching only \u2014 not real data. Uses the model and initial guesses currently set in section 4 as the \"true\" parameters."),
            fluidRow(
              column(6, numericInput("synth_x_min", "X Min:", value = 1)),
              column(6, numericInput("synth_x_max", "X Max:", value = 10))
            ),
            fluidRow(
              column(6, numericInput("synth_n_points", "Number of X Points:", value = 15, min = 5, max = 200, step = 5)),
              column(6, numericInput("synth_n_repliche", "Replicates per X Point:", value = 1, min = 1, max = 20, step = 1))
            ),
            fluidRow(
              column(6, numericInput("synth_rumore_pct", "Noise Level (% of Y range):", min = 0, max = 30, value = 5, step = 1)),
              column(6, numericInput("synth_dropout_pct", "Missing X Points (%):", min = 0, max = 80, value = 0, step = 5))
            ),
            actionButton("genera_dati_sintetici", "Generate Synthetic Data", class = "btn-primary btn-sm w-100", icon = icon("dice"))
          ),
          hr(),
          selectInput("trasformazione_y", "Y Pre-transformation (applied before fitting/plotting):", choices = costruisci_scelte_trasformazione_y(), selected = "nessuna"),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            "Transforms the actual Y data \u2014 the fit, plots, residuals, and confidence/prediction intervals all work in the transformed scale (not just the loss function). Common in predictive microbiology (e.g. fitting ln(CFU) instead of raw CFU). ", tags$strong("The Y axis label is not adjusted automatically"), " \u2014 update \"Y Variable Name\" above if you use a transform, e.g. to \"ln(CFU/mL)\"."),
          selectInput("trasformazione_densita", "Distribution Density Transformation:",
                      choices = c("None" = "nessuna", "Arithmetic (linear bin width)" = "aritmetica", "Logarithmic (bin width in ln space)" = "logaritmica"),
                      selected = "nessuna"),
          costruisci_box_aiuto_densita(),
          conditionalPanel(
            condition = "input.trasformazione_densita != 'nessuna'",
            selectInput("densita_media_bin", "Bin Center (X shown on the density curve):",
                        choices = c("Arithmetic mean" = "aritmetica", "Geometric mean" = "geometrica"),
                        selected = "aritmetica"),
            fluidRow(
              column(6,
                selectInput("densita_confine_inf_modo", "Lower Boundary (smallest X):", choices = c("Use this point's own value" = "proprio", "Assume a different value" = "assumi", "Discard this point" = "scarta"), selected = "proprio"),
                conditionalPanel(condition = "input.densita_confine_inf_modo == 'assumi'",
                  numericInput("densita_confine_inf_valore", "Assumed Lower Boundary:", value = 0))
              ),
              column(6,
                selectInput("densita_confine_sup_modo", "Upper Boundary (largest X):", choices = c("Assume a value" = "assumi", "Discard this point" = "scarta"), selected = "assumi"),
                conditionalPanel(condition = "input.densita_confine_sup_modo == 'assumi'",
                  numericInput("densita_confine_sup_valore", "Assumed Upper Boundary:", value = NA))
              )
            ),
            fluidRow(
              column(6, checkboxInput("densita_ancora_zero", "Add (0, 0) anchor point", value = FALSE)),
              column(6,
                checkboxInput("densita_ancora_max", "Add (X, 0) anchor point", value = FALSE),
                conditionalPanel(condition = "input.densita_ancora_max",
                  numericInput("densita_ancora_max_valore", "X value:", value = NA))
              )
            ),
          ),
        ),
        
        accordion_panel(
          "2. Axis Labels & Style", value = "sec2", icon = icon("paintbrush"),
          
          fluidRow(
            column(6, textInput("label_x", "X Variable Name (Independent):", value = "X")),
            column(6, textInput("label_y", "Y Variable Name (Response):", value = "Y"))
          ),
          actionButton("btn_syntax_label", "\U0001F4D6 Label Syntax Guide", class = "btn-info btn-sm w-100", style = "margin-top: 5px; margin-bottom: 10px; color: white;"),
          
          checkboxInput("mostra_titolo", "Show Plot Titles (uncheck for publications)", value = TRUE),
          checkboxInput("mostra_griglia", "Show Plot Background Grid", value = TRUE),
          
          actionButton("btn_estetica", "🎨 Plot Aesthetics & Colors", class = "btn-outline-primary btn-sm w-100", style="margin-top: 5px; margin-bottom: 5px;")
        ),
        
        accordion_panel(
          "3. Plot Export", value = "sec3", icon = icon("expand"),
          
          selectInput("formato_esportazione", "Plot Export Format:",
                      choices = c("PDF" = "pdf", "JPEG" = "jpeg")),
          
          conditionalPanel(
            condition = "input.formato_esportazione == 'jpeg'",
            numericInput("jpeg_dpi", "JPEG Resolution (DPI):", value = 200, min = 72, max = 600, step = 10),
            p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", 
              "Higher DPI = sharper image but larger file size. Not applicable to PDF (vector format, always sharp).")
          ),
          
          fluidRow(
            column(6, numericInput("export_width_px", "Export Width (px):", value = 2000, min = 200, max = 10000, step = 100)),
            column(6, numericInput("export_height_px", "Export Height (px):", value = 1400, min = 200, max = 10000, step = 100))
          ),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            "Applies to every single-plot download in the app (PDF and JPEG). For PDF, pixels are converted to inches using the DPI above. Combined images don't use these two values: their size comes from Panel width/height below (format and DPI above still apply)."),
          hr(),
          tags$strong(style = "font-size: 13px;", "Combined Image (multi-panel) Layout:"),
          p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 6px;",
            "Used by every \"Download Combined Image\" button: several plots arranged in a grid inside a single image (same format and DPI as above)."),
          fluidRow(
            column(6, selectInput("comp_modo_layout", "Arrangement:",
                                  choices = c("Auto (as square as possible)" = "auto", "Fixed number of columns" = "colonne", "Fixed number of rows" = "righe"),
                                  selected = "auto")),
            column(6,
              conditionalPanel(condition = "input.comp_modo_layout == 'colonne'",
                numericInput("comp_n_colonne", "Columns:", value = 3, min = 1, max = 20, step = 1)),
              conditionalPanel(condition = "input.comp_modo_layout == 'righe'",
                numericInput("comp_n_righe", "Rows:", value = 2, min = 1, max = 20, step = 1)))
          ),
          selectInput("comp_riempimento", "Fill order:",
                      choices = c("Row by row (left to right, then down)" = "righe", "Column by column (top to bottom, then right)" = "colonne"),
                      selected = "righe"),
          fluidRow(
            column(6, numericInput("comp_pannello_w_px", "Panel width (px):", value = 1000, min = 100, max = 5000, step = 50)),
            column(6, numericInput("comp_pannello_h_px", "Panel height (px):", value = 800, min = 100, max = 5000, step = 50))
          ),
          checkboxInput("comp_lettere", "Label panels (A, B, C, ...)", value = FALSE),
          conditionalPanel(condition = "input.comp_lettere",
            fluidRow(
              column(6, selectInput("comp_stile_lettere", "Label style:",
                                    choices = c("A, B, C \u2014 uppercase, bold" = "maiusc_grassetto",
                                                "A, B, C \u2014 uppercase, plain" = "maiusc_normale",
                                                "a, b, c \u2014 lowercase, italic" = "minusc_corsivo",
                                                "(a), (b), (c) \u2014 italic" = "parentesi_corsivo",
                                                "(a), (b), (c) \u2014 plain" = "parentesi_normale",
                                                "a), b), c) \u2014 italic" = "parentesi_dx_corsivo",
                                                "a), b), c) \u2014 plain" = "parentesi_dx_normale"),
                                    selected = "maiusc_grassetto")),
              column(6, selectInput("comp_pos_lettere", "Label position:",
                                    choices = c("Outside the plot (panel's top-left corner)" = "fuori",
                                                "Inside the plot, top-left" = "dentro_sx",
                                                "Inside the plot, top-right" = "dentro_dx"),
                                    selected = "fuori"))
            ),
            sliderInput("comp_cex_lettere", "Label size:", min = 0.8, max = 3, value = 1.5, step = 0.1)
          ),
          checkboxInput("comp_assi_condivisi", "Same axis ranges in every panel (Batch / Model Comparison single fits only)", value = TRUE),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            "Panel width/height replace Export Width/Height above for combined images: the full image is columns \u00d7 panel width by rows \u00d7 panel height, so it grows with the number of plots and each panel keeps the size (and text size) of a single plot. It is scaled down proportionally if a side would exceed 10000 px. An incomplete last row (or last column, when filling column by column) is centered. The axis option applies only to \"Download Single Fits (Combined Image)\" in Batch and Model Comparison, not to the other combined images (t-Plots, Profile Traces, Profile Curves, CI bars), where each panel shows a different parameter with its own units and keeps its own axes. For single fits, shared ranges make samples directly comparable by eye, individual ranges show each fit in more detail. A custom axis range set for the overlaid plot, if any, is applied to every panel.")
        ),
        
        accordion_panel(
          "4. Model Definition", value = "sec4", icon = icon("square-root-variable"),
          
          actionButton("btn_syntax", "📖 Math Syntax Guide", class = "btn-info btn-sm w-100", style="margin-bottom: 15px; color: white;"),
          
          # The original selectInput remains present but HIDDEN: the whole
          # rest of the app reads input$tipo_modello directly (dozens of
          # references), so keeping it intact and updating it via
          # updateSelectInput() from the popup avoids having to touch any
          # other point of the code - the popup is just a different interface
          # on top of the same mechanism.
          div(style = "display: none;",
            selectInput("tipo_modello", "Choose Model:", 
                        choices = costruisci_scelte_modello(),
                        selected = "custom")
          ),
          tags$label("Choose Model:", style = "font-weight: 600;"),
          uiOutput("trigger_selettore_modello"),
          actionButton("apri_selettore_modello", "\U0001F50D Browse Models by Application...", class = "btn-outline-primary btn-sm w-100", style = "margin-bottom: 10px;"),
          checkboxInput("applica_limiti_predefiniti", 
                        HTML("Apply built-in parameter boundaries <span style='font-size:11px;color:#7f8c8d;'>(recommended \u2014 prevents numerical errors on some models; uncheck to fit without any constraint)</span>"), 
                        value = TRUE),
          
          uiOutput("visualizza_formula_matematica"),
          
          conditionalPanel(
            condition = "input.tipo_modello == 'custom'",
            textInput("formula_custom", "Custom Formula (use 'x'):", value = "A * exp(-B * x^C)")
          ),
          
          textInput("guess_iniziali", "Initial Guesses (e.g., A=10, B=0.5, C=1):", value = "A=10, B=0.5, C=1"),
          uiOutput("legenda_simboli_guess"),
          actionButton("usa_ultimo_fit_come_guess", "Use Last Single Fit Results as Initial Guesses", class = "btn-outline-primary btn-sm w-100",
                       icon = icon("rotate-left"), style = "margin-bottom: 4px;"),
          p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 10px;",
            "Replaces the value of each estimated parameter with its estimate from the last Single Fit optimization (not from Batch Elaboration), keeping boundaries [min,max] and fixed parameters $value$. Useful to restart from the optimum with another algorithm or loss function. Note that these initial guesses are also the starting point of Batch Elaboration."),
          
          tags$details(
            style = "margin-bottom: 10px;",
            tags$summary(style = "cursor: pointer; color: #e67e22; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                         "\u26a0\ufe0f Important \u2014 syntax, boundaries, fixed parameters (click to expand)"),
            div(style = "background-color: #fef5e7; border: 1px solid #f5cba7; padding: 10px 15px; border-radius: 6px; margin-top: 6px; font-size: 12px; color: #935116;",
                "The order of parameters must follow the sequence defined in your initial guesses. Always use a period (.) as the decimal separator (e.g., 0.5 instead of 0,5).", br(), br(),
                tags$strong("Optional Boundaries: "), "constrain a parameter by adding square brackets right after its value, e.g. ", tags$code("A=10[0,50]"), ". Leave one side empty for an open-ended bound (e.g. ", tags$code("A=10[0,]"), " = no upper limit). Boundaries are honored only by the ", tags$strong("Levenberg-Marquardt"), " and ", tags$strong("L-BFGS-B"), " algorithms; Simplex (Nelder-Mead) and standard BFGS will run unconstrained and ignore them.", br(), br(),
                tags$strong("Fixed Parameters: "), "fix a parameter to a known value (excluded from optimization) by wrapping it in dollar signs, e.g. ", tags$code("N0=$7.0125$"), ". At least one parameter must remain free.", br(), br(),
                tags$strong("Parameter Names: "), "keep parameter names plain ASCII text (e.g. MU, k1) \u2014 they are used internally as computation variables and export column names, so Greek letters or symbols there could behave unreliably. A nicer display symbol for the Parameter Distributions plot (and everywhere else this model's parameter names appear) can be set in ", tags$strong("Advanced Plot Aesthetics \u2192 Parameter Names"), "."
            )
          ),
        ),
        
        accordion_panel(
          "5. Loss Function, Algorithm & Parameter Estimation", value = "sec6", icon = icon("gears"),
          
          selectInput("tipo_loss", "Select Loss Function:",
                      choices = c("Standard MSE (Least Squares)" = "mse",
                                  "MAE (Absolute Error - Robust)" = "mae",
                                  "Huber Loss (Robust Smooth)" = "huber",
                                  "Log10Cosh Loss (Log-Hyperbolic)" = "log10cosh",
                                  "RMSLE (Root Mean Squared Log Error)" = "rmsle")),
          conditionalPanel(condition = "input.tipo_loss != 'mse'",
            p(style = "font-size: 11px; color: #d35400; margin-top: -8px;",
              "With this loss, classic (Jacobian-based) standard errors are not computed: that formula is valid only for least squares (MSE). Select the Monte Carlo method below to obtain standard errors and confidence intervals.")),
          conditionalPanel(condition = "input.tipo_loss == 'rmsle'",
            p(style = "font-size: 11px; color: #d35400; margin-top: -4px;",
              "RMSLE is defined only for non-negative values: observed or predicted values below zero are treated as 0.")),
          # Weighted regression (least squares only): weights w = 1/yhat^zeta.
          conditionalPanel(condition = "input.tipo_loss == 'mse'",
            selectInput("ponderazione", "Weighting (least squares):",
                        choices = c("None" = "nessuna",
                                    "1/\u0177 (variance proportional to the mean, \u03b6 = 1)" = "zeta1",
                                    "1/\u0177\u00b2 (constant coefficient of variation, \u03b6 = 2)" = "zeta2",
                                    "1/\u0177^\u03b6 with \u03b6 set below" = "personalizzato",
                                    "1/\u0177^\u03b6 with \u03b6 = 2b from the replicates" = "repliche"),
                        selected = "nessuna"),
            conditionalPanel(condition = "input.ponderazione == 'personalizzato'",
              numericInput("ponderazione_zeta", "\u03b6 (exponent of the variance function):", value = 2, min = 0, max = 4, step = 0.1)),
            conditionalPanel(condition = "input.ponderazione != 'nessuna'",
              p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                "Weights w = 1/\u0177^\u03b6 for the variance model var(y) = \u03c3\u00b2\u00b7\u0177^\u03b6 (Carroll & Ruppert, 1988), estimated by iteratively reweighted least squares; all predicted values must be positive. ",
                "The residual plots then show weighted residuals \u221aw\u00b7(y \u2212 \u0177), which should have a constant spread if the weights are right. ",
                "With \u03b6 = 2b, b is the slope of the Replicate SD vs Mean regression (at least three x values with replicates are needed); the uncertainty of \u03b6 is not propagated to the standard errors. ",
                "In Batch Elaboration the same weighting is applied to each sample."))),
          
          selectInput("algoritmo", "Algorithm:", 
                      choices = c("Levenberg-Marquardt (MSE only)" = "LM", 
                                  "Simplex (Nelder-Mead)" = "Nelder-Mead", 
                                  "Quasi-Newton (BFGS)" = "BFGS",
                                  "Bounded Quasi-Newton (L-BFGS-B)" = "L-BFGS-B")),
          
          selectInput("metodo_errore", "Parameter Error Method:", 
                      choices = c("Linear Approximation (Jacobian)" = "lineare", "Monte Carlo (Bootstrap)" = "monte_carlo")),
          
          conditionalPanel(
            condition = "input.metodo_errore == 'lineare' && input.tipo_loss != 'mse'",
            p(style = "color:#e67e22; font-size:12px; margin-top:-8px; margin-bottom:10px;",
              tags$em("Warning: the Jacobian-based error approximation is rigorously valid only for MSE loss. For MAE, Huber, Log10Cosh or RMSLE, Monte Carlo (Bootstrap) is recommended."))
          ),
          
          conditionalPanel(
            condition = "input.metodo_errore == 'monte_carlo'",
            numericInput("n_boot", "Monte Carlo Iterations (Cycles):", value = 100, min = 10, max = 5000, step = 10),
            p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
              "Accepted range: 10\u20135000 (values outside this range are automatically capped). Higher values give a smoother, more stable estimate of parameter uncertainty but take longer to compute \u2014 ", tags$strong("computation time scales roughly linearly"), " (no parallel processing is available in this browser-based environment), so 5000 iterations take about 5\u00d7 as long as 1000. Keep it low (e.g. 100) for quick exploration, and raise it only when you need a final, publication-quality result. Note: at very high values, the Shapiro-Wilk normality diagnostic (Single Fit Analysis tab) can become sensitive enough to flag even tiny, practically irrelevant deviations from normality as \"significant\"."),
            selectInput("tipo_rumore_mc", "Noise Generation Method:",
                        choices = c("Non-parametric (Resample Residuals)" = "bootstrap",
                                    "Parametric (Gaussian, known σ)" = "gaussiano")),
            p(style = "font-size: 11px; color: #16a085; margin-top: -8px; margin-bottom: 10px; line-height: 1.4;",
              tags$strong("Tip: "), "with few experimental points (roughly < 10), the Parametric (Gaussian) option is often more reliable, since resampling residuals from a very small sample can poorly represent the true error distribution."),
            conditionalPanel(
              condition = "input.tipo_rumore_mc == 'bootstrap'",
              checkboxInput("residui_modificati", "Leverage-corrected residuals (Davison & Hinkley, 1997)", value = TRUE),
              p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px; margin-bottom: 10px;",
                "Raw residuals are smaller than the true errors (variance \u03c3\u00b2(1 \u2212 h), h = leverage). Each residual is divided by \u221a(1 \u2212 h), with leverages from the linear approximation of the model, and the residuals are centred before resampling (Davison & Hinkley, 1997, sections 6.2 and 7.4); with losses other than MSE only the centring is applied. Uncheck to resample the raw residuals, as in earlier versions.")
            ),
            conditionalPanel(
              condition = "input.tipo_rumore_mc == 'gaussiano'",
              numericInput("sigma_gaussiano", "Known Standard Deviation (σ):", value = 0, min = 0, step = 0.01),
              p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;", "Leave at 0 to automatically use the fit's RMSE as σ.")
            )
          ),
          
          numericInput("livello_confidenza", "Confidence/Prediction Level (%):", value = 95, min = 50, max = 99.9, step = 1),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -10px;", 
            "Note: This dynamically adjusts both Linear (Z-score) and Monte Carlo (empirical quantiles) bounds."),
          
          checkboxInput("mostra_ci", "Show Confidence Intervals", value = TRUE),
          # Sub-option of the confidence band: visible only with the band
          # active and the linear method (with Monte Carlo it has no effect).
          conditionalPanel(condition = "input.mostra_ci && input.metodo_errore != 'monte_carlo'",
            div(style = "margin-left: 20px;",
              checkboxInput("banda_simultanea", "Simultaneous confidence band (Bates & Watts, 1988)", value = FALSE),
              conditionalPanel(condition = "input.banda_simultanea",
                p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                  "The band holds for the whole curve at once (1\u2212\u03b1 approximate inference band, Bates & Watts, 1988, chapter 2): the t multiplier of the pointwise band is replaced by \u221a(p\u00b7F(p, n\u2212p; \u03b1)), which makes it wider. The prediction band remains pointwise.")))),
          checkboxInput("mostra_pi", "Show Prediction Intervals", value = TRUE),
          hr(),
          tags$strong(style = "font-size: 13px;", "Advanced Settings:"),
          p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 8px;", "Change these only if needed: they can strongly affect both the results and the computation time."),
          numericInput("maxiter_ottimizzazione", "Max Iterations (Optimizer):", value = 200, min = 50, max = 2000, step = 50),
          conditionalPanel(condition = "input.algoritmo == 'Nelder-Mead'",
            p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
              "Nelder-Mead counts function evaluations rather than iterations: 10 \u00d7 this value is used (2000 with the default)."),
            checkboxInput("nm_ripartenze", "Restart Nelder-Mead from its solution until no further improvement (up to 10 restarts)", value = TRUE),
            p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
              "Recommended: the simplex can stall before the minimum and still report convergence; restarting from the solution with a new simplex continues the search, and stops at once if the minimum has been reached. It takes longer, especially with Monte Carlo error estimation, where every iteration is refitted.")),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            "Accepted range: 50\u20132000 (values outside this range are automatically capped). With Levenberg-Marquardt the effective maximum is 1024, a limit of the minpack.lm library. Applies to both the main fit and every Monte Carlo bootstrap iteration. Increase it for models that are slow to converge."),
          numericInput("esponente_tolleranza", "Convergence Tolerance (exponent, 10^x):", value = -12, min = -15, max = -4, step = 1),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            tags$strong("Advanced setting: changing it can strongly affect both the results and the computation time."),
            " The optimizer stops when the relative improvement falls below 10^x. The default and the accepted range depend on the algorithm and are reset when the algorithm changes (Levenberg-Marquardt, BFGS and Nelder-Mead: default \u221212, range \u221215 to \u22124; L-BFGS-B: default \u22129, range \u221211 to \u22124). Applies to the main fit, every Monte Carlo iteration and Batch Elaboration; the Profile Likelihood and the secondary models of Shelf-Life and Non-Isothermal always use the default high precision."),
          uiOutput("avviso_tolleranza"),
          
          checkboxInput("accetta_non_convergenti", "Accept non-converged optimization results", value = FALSE),
          p(style = "font-size: 11px; color: #c0392b; margin-top: -8px; line-height: 1.4;",
            tags$strong("Warning: "), "enabling this shows the best estimate found even if the optimizer did NOT truly converge within the iteration limit \u2014 these parameters may not represent a real minimum of the loss function and could be misleading. Any result affected by this will be clearly flagged wherever it's shown. Only enable this for a quick exploratory look at a genuinely difficult model, e.g. under time pressure; always try increasing Max Iterations, changing initial guesses, adding boundaries, or switching algorithm first.")
        ),
        
        accordion_panel(
          "Model Comparison (Quick Add)", value = "sec7", icon = icon("layer-group"),
          
          p(style = "font-size: 12px; color: #566573; margin-bottom: 10px;",
            "After a successful fit, add it to the comparison list to compare it against other models fitted on the same data. The full comparison table, parameter estimates, significance test, and overlay plot are all in the ", tags$strong("\"Model Comparison\""), " tab."),
          textInput("etichetta_confronto", "Comparison Label (optional):", value = "", placeholder = "e.g. Weibull - bounded"),
          actionButton("aggiungi_confronto", "Add Current Fit to Comparison", class = "btn-success btn-sm w-100", icon = icon("plus")),
          hr(),
          checkboxInput("auto_add_confronto", "Auto-add every successful fit to comparison", value = TRUE),
          p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
            "If enabled, every successful \"Run Optimization Calculation\" is added automatically, without needing to click the button above each time.")
        ),
        
        accordion_panel(
          "Operator Configuration", value = "sec8", icon = icon("floppy-disk"),
          
          p(style = "font-size: 12px; color: #566573;",
            "Save the current model, formula, loss function, algorithm, initial guesses, axis labels, and all plot color/style customizations to a file, to reload them instantly next time \u2014 useful if you (or your lab) repeat the same kind of analysis often."),
          textInput("nome_file_configurazione", "File name:", value = "my_configuration", placeholder = "e.g. lab_standard_settings"),
          downloadButton("salva_configurazione", "Save Configuration", class = "btn-outline-secondary btn-sm w-100", style = "margin-bottom: 10px;"),
          fileInput("carica_configurazione", "Load Configuration:", accept = ".rconfig", buttonLabel = "Browse...", placeholder = "No file selected")
        )
      )
      
      ),  # closing of the scrollable div "max-height: 75vh"
      
      tags$div(
        style = "padding-top: 12px; border-top: 1px solid #e9ecef;",
        actionButton("esegui", "Run Optimization Calculation", class = "btn-primary btn-lg w-100")
      )
    ),
    
    mainPanel(
      width = 8,
      style = "max-height: 85vh; overflow-y: auto; padding-left: 20px;",
      
      tags$details(
        style = "margin-bottom: 15px;",
        tags$summary(style = "cursor: pointer; color: #c0392b; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                     "\u26a0\ufe0f Disclaimer & Use Notice (click to expand)"),
        div(style = "border: 1px solid #e74c3c; background-color: #fdf2f2; padding: 12px; border-radius: 6px; margin-top: 6px;",
            div(style = "color: #c0392b; font-size: 12px; line-height: 1.5; font-weight: 500;",
              tags$strong("DISCLAIMER: "), 
              "This software was developed with the support of Gemini AI and Claude AI within the R statistical environment. It started as a simple, free tool for educational purposes, but has since evolved into a broader non-linear regression and predictive modeling platform. It is primarily optimized for ", tags$strong("Food Science and Technology"), " applications (reaction kinetics, microbial growth and shelf-life, sorption isotherms, rheology), but its general-purpose model library, custom formula support, and statistical tools make it usable across other research fields that rely on non-linear modeling.",
              tags$br(), tags$br(),
              tags$strong("Main features:"),
              tags$ul(style = "margin-top: 4px; margin-bottom: 8px; padding-left: 20px;",
                tags$li("A library of 62 built-in models across 9 categories (microbial growth, microbial inactivation and Weibull models, reaction and enzyme kinetics, mass transfer, rheology and texture, water activity and sorption isotherms, statistical distributions), plus 42 models for the reference datasets (26 NIST StRD, 15 Bates & Watts and 1 Carroll & Ruppert) and support for fully custom formulas."),
                tags$li("Flexible data input: direct entry, CSV/Excel upload (single-sample and multi-sample formats), synthetic data generation, or 67 built-in reference datasets (26 from ComBase, 26 certified NIST StRD nonlinear regression datasets, 14 from Bates & Watts, Nonlinear Regression Analysis and Its Applications, and 1 from Carroll & Ruppert, Transformation and Weighting in Regression, for weighted regression), plus 2 real example datasets for Batch Elaboration (Nisha et al., 2004, 2005)."),
                tags$li("Multiple optimization algorithms (Levenberg-Marquardt with analytical derivatives, BFGS, L-BFGS-B, Nelder-Mead), loss functions, weighted least squares (weights 1/\u0177^\u03b6, with \u03b6 fixed or estimated from the replicates, in Single Fit and Batch Elaboration), and error estimation methods: classic standard errors from the linear approximation, Monte Carlo intervals (residual bootstrap or Gaussian noise), and profile-likelihood intervals."),
tags$li("Least-squares fitting validated against the ", tags$strong("NIST Statistical Reference Datasets"), " for nonlinear regression: with Levenberg-Marquardt, 48 of 52 runs reproduce the certified parameters, standard errors and residual sum of squares to at least 4 significant digits (the remaining four are explained by a typographical error in a NIST certificate and by the limits of double-precision arithmetic)."),
                tags$li("An ", tags$strong("Initial Guess Simulator"), " tab to visually adjust and animate initial guesses before running the main optimization \u2014 available above to help you find good starting values."),
                tags$li("A ", tags$strong("Profile Likelihood"), " section with exact profile confidence intervals, t-plots and profile traces for every parameter, and a ", tags$strong("Loss Surface Explorer"), " with 2D/3D grid search and rigorous confidence regions."),
                tags$li("A ", tags$strong("Batch Elaboration"), " tab for fitting many samples at once, with parameter and shelf-life comparison plots, significance testing, and post-hoc analysis."),
                tags$li("A ", tags$strong("Model Comparison"), " tab to compare different models \u2014 or the same model with different settings \u2014 fitted individually on the same dataset, including cross-model shelf-life estimation."),
                tags$li("A ", tags$strong("Non-Isothermal Kinetic Prediction"), " module (in both Batch and Model Comparison) for reaction-kinetics models: fits a secondary model relating the rate constant to temperature, then predicts the critical time under a time-varying temperature profile instead of a single fixed storage temperature, with uncertainty propagated from the primary fit via nested bootstrap."),
                tags$li("Extensive plot customization (colors, fonts, ranges, legends) for every graph in the app, plus export to Excel, CSV, and downloadable plots (JPEG/PDF), combined multi-panel images, or full ZIP archives."),
                tags$li("An Operator Configuration panel to save and reload your preferred display and computation settings.")
              ),
              "For any suggestions or questions, please contact me: enret.app@gmail.com"
            )
        )
      ),
            tabsetPanel(
        id = "main_tabs",
        selected = "tab1",
        
        tabPanel(title = "Initial Guess Simulator", value = "tab_simula",
                 br(),
                 p(style = "color: #2c3e50; font-size: 14px;",
                   "This control panel allows you to simulate and plot the mathematical curve using the model's parameters, completely separate from the fitting algorithm. Use it to check if your guesses are reasonable before running the actual optimization."),
                 fluidRow(
                   column(3, actionButton("carica_guess_celle", "\u21BB Load Current Sidebar Guesses", class = "btn-info w-100 text-white", style = "margin-top: 33px; height: 36px; padding-top: 4px; padding-bottom: 4px;")),
                   column(3, numericInput("simula_spread_percentuale", "Slider Range (\u00b1% around loaded value):", value = 50, min = 5, max = 200, step = 5)),
                   column(3, numericInput("simula_n_passi_animazione", "Animation steps across the range:", value = 50, min = 5, max = 200, step = 5)),
                   column(3, div(style = "margin-top: 33px;", checkboxInput("sim_lascia_traccia", "Keep previous curves drawn (trail)", value = FALSE)))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px; margin-bottom: 4px;",
                   "Loads the guesses currently set in the sidebar into the sliders below (fixed parameters, marked with $...$ in the sidebar, are not editable here and stay fixed). Drag a slider to update the plot immediately \u2014 no need to click anything else."),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Click the \u25B6 button under a parameter's slider to make it the animated one (only one at a time) \u2014 its own native Play button then appears on that slider, starting from the low end of its range; click that to start."),
                 uiOutput("celle_guess_dinamiche"),
                 plotOutput("grafico_simulazione", height = "420px"),
                 div(style = "margin-top: 8px;", downloadButton("scarica_grafico_simulazione", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_sim", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"), actionButton("sim_pulisci_traccia", "Clear previous curves", class = "btn-outline-secondary btn-sm")),
                 br(),
                 verbatimTextOutput("log_errore_simulazione")
        ),
        tabPanel(title = "Single Fit Analysis", value = "tab1",
                 br(),
                 conditionalPanel(
                   condition = "input.metodo_dati == 'sintetico'",
                   div(style = "background-color: #fff3cd; border: 1px solid #ffe69c; padding: 10px 15px; border-radius: 6px; margin-bottom: 15px; font-size: 13px; color: #664d03; text-align: center;",
                       icon("triangle-exclamation"), tags$strong(" Synthetic data is active"), " \u2014 these results are generated for exploration/teaching, not from real measurements.")
                 ),
                 plotOutput("grafico_fit", height = "380px"),
                 div(style = "margin-top: 8px;", downloadButton("scarica_grafico_fit", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_fit", "🎨", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                 
                 div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 6px; margin-top: 15px; margin-bottom: 15px; max-width: 50%;",
                     tags$h5(style = "margin-top: 0; color: #2c3e50;", icon("file-export"), " Export Data Tables and Figures"),
                     p(style = "font-size: 13px; color: #7f8c8d;", 
                       "CSV/Excel: Model Info, Parameter Estimates (with CI), Fixed Parameters, Goodness-of-Fit Diagnostics, Predictions & Residuals, 1000-Point Dense Curve, Monte Carlo Bootstrap Sets, Normality Diagnostics, Correlation Matrix, Sensitivity Analysis, and Loss Surface Explorer results (whichever of these are currently available). ZIP: every plot shown in this tab, as individual image files."),
                     downloadButton("scarica_csv", "Download as CSV", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_excel", "Download as Excel (.xlsx)", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_tutti_grafici_zip", "Download All Plots (ZIP)", class = "btn-outline-secondary btn-sm")
                 ),
                 br(),
                 fluidRow(
                   column(6, plotOutput("grafico_residui", height = "380px"),
                          div(style = "margin-top: 8px;", downloadButton("scarica_grafico_residui", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_res", "🎨", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))),
                   column(6, plotOutput("grafico_qq", height = "380px"),
                          div(style = "margin-top: 8px;", downloadButton("scarica_grafico_qq", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_qq", "🎨", class = "btn-outline-secondary btn-sm", title = "Customize this plot")))
                 ),
                 fluidRow(
                   column(6, plotOutput("grafico_lag", height = "380px"),
                          div(style = "margin-top: 8px;", downloadButton("scarica_grafico_lag", "Download Plot", class = "btn-outline-secondary btn-sm"),
                              actionButton("btn_estetica_res_lag", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))),
                   column(6, plotOutput("grafico_sd_media", height = "380px"),
                          div(style = "margin-top: 8px;", downloadButton("scarica_grafico_sd_media", "Download Plot", class = "btn-outline-secondary btn-sm"),
                              actionButton("btn_estetica_qq_sd", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")))
                 ),
                 uiOutput("diagnostica_repliche_ui"),
                 br(),
                 tags$h5(style = "text-align: center; color: black; margin-bottom: 2px;", "Fitted Model Equation:"),
                 uiOutput("grafico_formula_modello_contenitore"),
                 verbatimTextOutput("info_risultati"),
                 br(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Parameter Estimation Results:"),
                   tags$button(id = "btn_copia_parametri_stimati", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_parametri_stimati', 'btn_copia_parametri_stimati')")
                 ),
                 tableOutput("tabella_parametri_stimati"),
                 tableOutput("tabella_parametri_fissi"),
                 br(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Goodness-of-Fit Diagnostics:"),
                   tags$button(id = "btn_copia_diagnostica", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_diagnostica', 'btn_copia_diagnostica')")
                 ),
                 tableOutput("tabella_diagnostica"),
                 verbatimTextOutput("messaggi_errore"),
                 
                 br(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Model Predictions and Residual Analysis Table:"),
                   tags$button(id = "btn_copia_pred_residui", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_pred_residui', 'btn_copia_pred_residui')")
                 ),
                 p("Detailed summary of observed values, model predictions, residuals, and relative errors.", style = "color: #7f8c8d; font-size: 13px;"),
                 tableOutput("tabella_pred_residui"),
                 
                 br(),
                 uiOutput("sezione_dist_parametri_ui"),
                 
                 conditionalPanel(
                   condition = "input.metodo_errore == 'monte_carlo'",
                   tags$div(style = "display: flex; align-items: center; gap: 10px;",
                     tags$h5(style = "margin: 0;", "Bootstrap Distribution Summary & Normality Check:"),
                     tags$button(id = "btn_copia_normalita", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                       style = "padding-top: 2px; padding-bottom: 2px;",
                       onclick = "copiaTabellaClipboard('tabella_normalita', 'btn_copia_normalita')")
                   ),
                   p(style = "font-size: 12px; color: #7f8c8d;",
                     "The confidence intervals shown in the Results table above (and the pairwise significance test in Model Comparison) assume a roughly symmetric, normal-shaped distribution around each estimate. The plot bands (CI/PI) and this histogram instead use the ", tags$strong("actual empirical shape"), " of the bootstrap distribution, so they remain valid even if it isn't normal. ",
                     tags$strong("If a parameter below shows p < 0.05,"), " its bootstrap distribution significantly deviates from normality \u2014 for that parameter, trust the empirical quantile-based bands over the symmetric interval in the Results table. ",
                     tags$strong("Skewness and Excess Kurtosis"), " are shown alongside the p-value because, unlike it, they don't grow more \"significant\" just from having more bootstrap iterations \u2014 they measure how large the deviation actually is. As a rule of thumb: |Skewness| < 0.5 and |Excess Kurtosis| < 0.5 indicate the distribution is close to normal in practice, even if the p-value is small."),
                   p(style = "font-size: 12px; color: #7f8c8d;",
                     tags$strong("Mean, Median and Bootstrap SE"), " describe each parameter's bootstrap distribution: the Bootstrap SE (its standard deviation) can be compared with the classic Std Error in the Results table \u2014 if they differ markedly, the linear approximation behind the classic error is poor for that parameter and the empirical intervals are more reliable. ",
                     tags$strong("Bias"), " is Mean \u2212 Estimate; as a rule of thumb (Efron & Tibshirani, 1993, chapter 10) it can be ignored when |Bias| is below 0.25 Bootstrap SE, and it is flagged with \u26a0\ufe0f otherwise."),
                   div(style = "overflow-x: auto;", tableOutput("tabella_normalita"))
                 ),
                 
                 hr(),
                 tags$h4("Parameter Sensitivity Analysis:"),
                 p(style = "font-size: 12px; color: #7f8c8d;",
                   "Shows how sensitive the model's prediction is to each parameter, at every X: a normalized value of \u00b11 means a 1% change in that parameter changes the prediction by roughly 1%. Available regardless of error method (linear approximation around the optimal estimate)."),
                 plotOutput("grafico_sensibilita", height = "380px"),
                 div(style = "margin-top: 8px; margin-bottom: 20px;", downloadButton("scarica_grafico_sensibilita", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_sensibilita", "🎨", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                 
                 hr(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Parameter Correlation Matrix:"),
                   tags$button(id = "btn_copia_correlazione", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_correlazione', 'btn_copia_correlazione')")
                 ),
                 p(style = "font-size: 12px; color: #7f8c8d;",
                   "Shows how strongly the estimated parameters are correlated with each other (from the Jacobian with the Linear method, or from the bootstrap draws with Monte Carlo). High correlations (close to \u00b11) suggest the parameters are hard to estimate independently from this data \u2014 a common cause of unstable fits."),
                 tableOutput("tabella_correlazione"),
                 
                 conditionalPanel(
                   condition = "input.metodo_errore == 'monte_carlo'",
                   hr(),
                   tags$h4("Bootstrap Parameter Cloud (Pairwise Scatter):"),
                   p(style = "font-size: 12px; color: #7f8c8d;",
                     "Every Monte Carlo bootstrap draw, plotted as a point for each pair of parameters \u2014 shows the actual shape of their joint uncertainty (e.g. a curved \"banana\" shape indicates a non-linear relationship between two parameters that a simple correlation number wouldn't fully capture)."),
                   checkboxInput("mostra_contour_pairs", "Show density contour lines", value = TRUE),
                   plotOutput("grafico_pairs_bootstrap", height = "450px"),
                   div(style = "margin-top: 8px; margin-bottom: 20px;", downloadButton("scarica_grafico_pairs", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_pairs", "🎨", class = "btn-outline-secondary btn-sm", title = "Customize this plot"))
                 ),
                 
                 hr(),
                 accordion(
                   id = "profile_likelihood_accordion", open = FALSE,
                   accordion_panel(
                     "Profile Likelihood", icon = icon("chart-line"),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: 0; line-height: 1.4;",
                       "Computed for ", tags$strong("every parameter"), " of the model, independent of the 2D grid below (no need to run Rigorous Profile first \u2014 this analysis is entirely self-contained). ",
                       tags$strong("Profile likelihood:"), " for each value of one parameter, every OTHER free parameter is continuously re-optimized, and the resulting fit is compared against the best fit through an F-based threshold \u2014 reflects the actual curvature of the loss surface (can be asymmetric, unlike the other two). ",
                       tags$strong("Classic:"), " the standard estimate \u00b1 t\u00d7SE interval, at the same confidence level chosen here. ",
                       tags$strong("Monte Carlo:"), " percentiles taken directly from the bootstrap distribution, shown only if this fit actually used Monte Carlo (not Jacobian/Linear). The three are complementary, not interchangeable \u2014 seeing them side by side is a useful check: if the Profile and Classic results disagree noticeably, the loss surface is probably asymmetric around that parameter, which the Classic interval (always symmetric by construction) can't capture."),
                     p(style = "font-size: 11px; color: #e67e22; margin-top: -6px; line-height: 1.4;",
                       icon("triangle-exclamation"), " ", tags$strong("Changing Confidence Level does not update the Profile Likelihood bounds by itself."),
                       " The Profile Likelihood bounds are computed once when \"Run Profile Likelihood\" is clicked, using whatever confidence level was set at that moment \u2014 click it again after changing the level to refresh them. The Classic and Monte Carlo bounds, instead, update immediately with the confidence level, no re-run needed."),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px; line-height: 1.4;",
                       tags$strong("Re-optimization algorithm:"), " at every point of each profile, the other free parameters are re-optimized by ", tags$strong("Levenberg-Marquardt"), " (least squares, with analytical derivatives when the formula allows) ",
                       tags$strong("regardless of the Algorithm chosen in the sidebar"), " for the main fit, and ", tags$strong("without applying parameter boundaries"), "."),
                     uiOutput("profile_ci_spread_dinamica_ui"),
                     fluidRow(
                       column(6, numericInput("profile_ci_livello_confidenza", "Confidence Level (%):", value = 95, min = 80, max = 99.9, step = 0.5)),
                       column(6, numericInput("profile_ci_risoluzione", "Number of points (this analysis only):", value = 100, min = 5, max = 1000, step = 5))
                     ),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                       "The confidence limits are located exactly where the profile crosses the threshold, not on the grid points: the number of points only sets how finely the profile curves are drawn. If a limit lies beyond the explored Min/Max range, it is flagged in the table."),
                     actionButton("esegui_profile_ci", "Run Profile Likelihood", class = "btn-primary btn-sm w-100", icon = icon("calculator")),
                     div(style = "margin-top: 14px;"),
                     tags$button(id = "btn_copia_ci_marginali_grid", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                       style = "padding-top: 2px; padding-bottom: 2px; margin-bottom: 6px;",
                       onclick = "copiaTabellaClipboard('tabella_ci_marginali_grid', 'btn_copia_ci_marginali_grid')"),
                     tableOutput("tabella_ci_marginali_grid"),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                       "F-based threshold for the profile likelihood (Bates & Watts, 1988): the SSE threshold below which a single parameter is compatible with the data, computed from the F-distribution."),
                     verbatimTextOutput("info_soglia_profile_ci"),
                     br(),
                     uiOutput("sezione_profilo_2d_dinamica"),
                     br(),
                     uiOutput("sezione_t_plot_dinamica"),
                     br(),
                     uiOutput("sezione_profile_trace_dinamica"),
                     br(),
                     uiOutput("sezione_barre_ci_grid_dinamica")
                   )
                 ),
                 hr(),
                 accordion(
                   id = "grid_search_accordion", open = FALSE,
                   accordion_panel(
                     "Loss Surface Explorer (Grid Search)", icon = icon("border-all"),
                   p(style = "font-size: 12px; color: #7f8c8d;",
                     "Explore how the loss function changes as two parameters vary, holding the others fixed \u2014 useful to sanity-check that your solution sits in a well-defined minimum (not a local one), to find a better initial guess for a difficult model, and (with the rigorous profile below) to get a statistically valid confidence region for a pair of parameters."),
                   fluidRow(
                     column(4, uiOutput("selettore_grid_par1_ui"), uiOutput("grid_range_par1_ui")),
                     column(4, uiOutput("selettore_grid_par2_ui"), uiOutput("grid_range_par2_ui")),
                     column(4, numericInput("grid_risoluzione", "Grid Resolution (points per side):", value = 25, min = 10, max = 50, step = 5))
                   ),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                     "For a parameter whose optimal value is strictly positive (or negative), the range is automatically kept from crossing zero, since many formulas (log, sqrt, fractional powers) aren't defined there."),
                   fluidRow(
                     column(6, 
                            actionButton("esegui_grid_search", "Run Grid Search (Fast \u2014 conditional likelihood)", class = "btn-primary w-100", icon = icon("border-all")),
                            p(style = "font-size: 11px; color: #7f8c8d; margin-top: 4px;",
                              "Evaluates the loss once per grid point, holding every other free parameter fixed at its optimal value. Good for a quick look; not a statistically rigorous confidence region.")
                     ),
                     column(6, 
                            actionButton("esegui_grid_rigoroso", "Run Rigorous Profile (profile likelihood)", class = "btn-warning w-100", icon = icon("triangle-exclamation")),
                            p(style = "font-size: 11px; color: #c0392b; margin-top: 4px; line-height: 1.4;",
                              "Re-optimizes every OTHER free parameter at every grid point instead of holding them fixed \u2014 the statistically correct surface for a valid confidence region. Independent of the Fast button: always uses the parameter pair, range and resolution currently shown above. Uses ", tags$strong("BFGS"), " for this re-optimization, independent from the Algorithm chosen in the sidebar. ",
                              tags$strong("Meaningfully slower"), " (\u2248 as many optimizations as grid points, e.g. 25\u00d725 \u2248 625, run one after another). With the MSE loss it uses the same re-optimization as the Marginal Confidence Intervals above (Levenberg-Marquardt, independent of the sidebar Algorithm, no parameter boundaries); with other losses, BFGS with a Nelder-Mead refinement.")
                     )
                   ),
                   br(),
                   tags$strong(textOutput("stato_grid_search", inline = TRUE)),
                   br(), br(),
                   tags$h5("3D Surface:"),
                   fluidRow(
                     column(7, plotOutput("grafico_grid_3d", width = "480px", height = "480px")),
                     column(5, style = "padding-top: 30px;",
                       sliderInput("grid_3d_theta", "Rotate (horizontal angle):", min = -180, max = 180, value = 30, step = 5),
                       sliderInput("grid_3d_phi", "Rotate (vertical angle):", min = -90, max = 90, value = 25, step = 5),
                       p(style = "font-size: 11px; color: #7f8c8d;",
                         "Move the sliders to redraw the surface from a different angle (no drag-to-rotate available in this browser-based environment)."),
                       checkboxInput("mostra_punti_grid_3d", "Show sampled grid points on the surface", value = TRUE)
                     )
                   ),
                   div(style = "margin-top: 8px; margin-bottom: 20px;", downloadButton("scarica_grid_3d", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_grid_3d", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                 
                   tags$h5("Contour View:"),
                   fluidRow(
                     column(7, plotOutput("grafico_grid_contour", width = "480px", height = "480px")),
                     column(5, style = "padding-top: 30px;",
                       numericInput("grid_livello_confidenza", "Confidence Level (%):", value = 95, min = 80, max = 99.9, step = 0.5),
                       p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                         "Independent from the global \"Confidence/Prediction Level\" in the sidebar \u2014 lets you explore a different confidence level here without changing it everywhere else in the app."),
                       checkboxInput("mostra_regione_confidenza", "Show confidence region contour (requires Rigorous Profile, MSE loss only)", value = TRUE)
                     )
                   ),
                   div(style = "margin-top: 8px;", downloadButton("scarica_grid_contour", "Download Plot", class = "btn-outline-secondary btn-sm"), actionButton("btn_estetica_grid_contour", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                   tags$h5(style = "margin-top: 15px;", "Confidence Region Calculation:"),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                     "Joint profile-likelihood confidence region (Bates & Watts, 1988): the SSE threshold below which the two chosen parameters are jointly compatible with the data, computed from the F-distribution."),
                   verbatimTextOutput("info_soglia_confidenza")
                   )
                 )
        ),
        tabPanel(title = "Model Comparison", value = "tab_confronto",
                 br(),
                 p(style = "color: #2c3e50; font-size: 14px;",
                   "Every time you click ", tags$strong("\"Add Current Fit to Comparison\""), " in the sidebar (after a successful optimization), a snapshot of that fit is saved here. Use this to compare different models -- or the same model with different settings -- fitted on the same dataset, run inverse predictions on any of them, and estimate shelf life across several of them (each individually optimized, unlike the Batch Elaboration tab's automated approach)."),
                 
                 hr(),
                 accordion(
                   id = "confronto_accordion", open = TRUE,
                 accordion_panel("Overlaid Fitted Curves", icon = icon("chart-area"),
                 checkboxInput("mostra_punti_confronto", "Show each model's own data points", value = TRUE),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Each model's points are shown in the same color as its curve. If all models were fitted on the same dataset, points will overlap; uncheck this to see only the curves."),
                 plotOutput("grafico_confronto", height = "420px"),
                 div(style = "margin-top: 8px;", 
                     downloadButton("scarica_grafico_confronto", "Download Plot", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_confronto_fit_singoli", "Download Single Fits (Combined Image)", class = "btn-outline-secondary btn-sm"),
                     actionButton("btn_estetica_confronto", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                 )
                 ),
                 
                 hr(),
                 accordion(
                   id = "confronto_gestione_accordion", open = TRUE,
                 accordion_panel("Manage & Compare Saved Models", icon = icon("list-check"),
                 div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
                 tags$h4("Manage Models:"),
                 p(style = "font-size: 12px; color: #566573; margin-bottom: 4px;", tags$strong("Rename a saved model:")),
                 fluidRow(style = "margin-bottom: 8px;",
                   column(4, uiOutput("selezione_rinomina_ui")),
                   column(4, textInput("nuovo_nome_modello", NULL, placeholder = "New label...")),
                   column(4, actionButton("rinomina_modello", "\u270F\uFE0F Rename Selected Model", class = "btn-warning btn-sm w-100"))
                 ),
                 p(style = "font-size: 12px; color: #566573; margin-bottom: 4px;", tags$strong("Remove models:")),
                 fluidRow(style = "margin-bottom: 8px;",
                   column(4, uiOutput("selezione_rimozione_ui")),
                   column(4, actionButton("rimuovi_modello", "\U0001F5D1\uFE0F Remove Selected Model", class = "btn-sm w-100", style = "background-color: #e67e22; border-color: #e67e22; color: white;")),
                   column(4, actionButton("svuota_confronto", "\U0001F5D1\uFE0F Remove All the Models", class = "btn-danger btn-sm w-100"))
                 ),
                 ),
                 hr(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Comparison Table:"),
                   tags$button(id = "btn_copia_confronto", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_confronto', 'btn_copia_confronto')")
                 ),
                 fluidRow(
                   column(6, selectInput("comp_sort_by", "Sort By:",
                                         choices = c("AICc (recommended)" = "AICc", "AIC" = "AIC", "BIC" = "BIC", "RMSE" = "RMSE",
                                                     "R-squared" = "R2", "R-squared Adjusted" = "R2_Adj",
                                                     "N. Parameters" = "N_Param", "Label" = "Label"))),
                   column(6, selectInput("comp_sort_dir", "Order:", choices = c("Ascending" = "asc", "Descending" = "desc")))
                 ),
                 p(style = "font-size: 11px; color: #e67e22; line-height: 1.4;",
                   tags$strong("Note: "), "AIC, AICc and BIC assume least-squares (maximum-likelihood) estimates: they are computed only for models fitted with the MSE loss and shown as N/A for the other losses. They are comparable only across models fitted to the same dataset."),
                 tableOutput("tabella_confronto"),
                 p(style = "font-size: 11px; color: #7f8c8d; line-height: 1.4; margin-top: -6px;",
                   tags$strong("Runs test: "), "standard normal deviate for too few runs of residuals with the same sign (ordered by x; mean residual per x with replicates) and its exact one-sided p-value (van Boekel, 2008, section 7.8). A small p-value indicates systematic patterns in the residuals, i.e. a model that does not follow the shape of the data. Use it as a check, not to choose the model: models with more parameters tend to leave more random residuals; among the models without systematic patterns, prefer the one with the lowest AICc."),
                 div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 6px; margin-top: 10px; margin-bottom: 25px; max-width: 50%;",
                     tags$h5(style = "margin-top: 0; color: #2c3e50;", icon("file-export"), " Export Data Tables"),
                     p(style = "font-size: 13px; color: #7f8c8d;", 
                       "Comparison Table (Goodness-of-Fit), Parameter Estimates Comparison, Parameter Significance Test, the Pairwise Evidence Ratio Matrix, and the Post-Hoc Comparison below (whichever have been run), all in a single file."),
                     downloadButton("scarica_confronto_csv", "Download as CSV", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_confronto_excel", "Download as Excel (.xlsx)", class = "btn-outline-secondary btn-sm")
                 ),
                 uiOutput("sezione_matrice_evidenza_ui"),
                 uiOutput("sezione_parametri_confronto_ui"),
                 uiOutput("sezione_test_parametri_ui"),
                 uiOutput("sezione_posthoc_intestazione_ui"),
                 uiOutput("sezione_boxplot_confronto_ui"),
                 )
                 ),
                 
                 hr(),
                 accordion(
                   id = "confronto_inversa_accordion", open = FALSE,
                 accordion_panel("Inverse Prediction", icon = icon("arrow-right-arrow-left"),
                 tags$details(
                   style = "margin-bottom: 15px;",
                   tags$summary(style = "cursor: pointer; color: #21618c; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                                "\u2139\ufe0f Inverse Prediction vs. Shelf-Life \u2014 which one do I need? (click to expand)"),
                   div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; padding: 10px 15px; border-radius: 6px; margin-top: 6px; font-size: 12px; color: #21618c;",
                       "Both start from the same idea (find X for a given Y), but answer different questions. Use ", tags$strong("Inverse Prediction"), 
                       " when you want to solve that for ", tags$strong("one fit at a time"), " and see it visually (two diagnostic plots: the fit itself with a crosshair, and \u2014 with Monte Carlo \u2014 the full distribution of X). Use ", tags$strong("Shelf-Life & Kinetic Model Analysis"), 
                       " when you need that X value (e.g. a shelf life) for ", tags$strong("several fits at once"), 
                       ", each at a different condition (typically temperature), and then want to relate those X values to that condition through a second, ", tags$strong("kinetic"), 
                       " model \u2014 which also lets you predict at a new, untested condition. In short: Inverse Prediction answers \u201cwhat X, for this one curve?\u201d; Shelf-Life answers \u201cwhat X at each condition, and how does X change across conditions?\u201d")
                 ),
                 uiOutput("ip_selettore_modello_ui"),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Choose the current sidebar fit, or any model already added to the comparison above."),
                 uiOutput("intro_inversa"),
                 fluidRow(
                   column(2, numericInput("y_target_inversa", "Target Y:", value = 0)),
                   column(2, numericInput("x_search_min_inversa", "X Search Range Min:", value = 0)),
                   column(2, numericInput("x_search_max_inversa", "X Search Range Max:", value = 0)),
                   column(6, selectInput("tipo_intervallo_inversa", "Uncertainty Type:",
                                         choices = c("New Observation (incl. residual var.)" = "predizione",
                                                     "Mean Response Only" = "media")))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Pre-filled with the min/max X of your raw data whenever the dataset changes, since that's the range where root-finding is most reliable. Widen it manually if the target Y falls outside the model's output in that interval \u2014 but note that solutions found well outside the original data range are extrapolations."),
                 actionButton("calcola_inversa", "Calculate X", class = "btn-primary w-100"),
                 uiOutput("sezione_risultato_inversa_ui")
                 )
                 ),
                 
                 hr(),
                 accordion(
                   id = "confronto_sl_accordion", open = FALSE,
                 accordion_panel("Shelf-Life & Kinetic Model Analysis", icon = icon("hourglass-half"),
                 tags$details(
                   style = "margin-bottom: 15px;",
                   tags$summary(style = "cursor: pointer; color: #21618c; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                                "\u2139\ufe0f Inverse Prediction vs. Shelf-Life \u2014 which one do I need? (click to expand)"),
                   div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; padding: 10px 15px; border-radius: 6px; margin-top: 6px; font-size: 12px; color: #21618c;",
                       "Both start from the same idea (find X for a given Y), but answer different questions. Use ", tags$strong("Inverse Prediction"), 
                       " when you want to solve that for ", tags$strong("one fit at a time"), " and see it visually (two diagnostic plots: the fit itself with a crosshair, and \u2014 with Monte Carlo \u2014 the full distribution of X). Use ", tags$strong("Shelf-Life & Kinetic Model Analysis"), 
                       " when you need that X value (e.g. a shelf life) for ", tags$strong("several fits at once"), 
                       ", each at a different condition (typically temperature), and then want to relate those X values to that condition through a second, ", tags$strong("kinetic"), 
                       " model \u2014 which also lets you predict at a new, untested condition. In short: Inverse Prediction answers \u201cwhat X, for this one curve?\u201d; Shelf-Life answers \u201cwhat X at each condition, and how does X change across conditions?\u201d",
                       tags$br(), tags$br(),
                       "This module runs the same kind of analysis as the Shelf-Life section in Batch Elaboration, but using the individually-optimized fits added to the comparison above instead of a batch-processed CSV upload \u2014 useful when you've carefully tuned each fit's guesses/algorithm on its own, rather than relying on the batch's automated, one-size-fits-all approach. For each fit above, finds the time (X) at which Y crosses a critical threshold (shelf life), then relates it to storage temperature via a ", tags$strong("non-linear"), " fit of the kinetic model chosen below. Only the rigorous, nested-bootstrap version is offered here. The fit itself (both the point estimate and every bootstrap iteration) uses ", tags$strong("Levenberg-Marquardt"), " (least squares, with analytical derivatives when the formula allows) \u2014 independent from the Loss Function and Algorithm chosen in the sidebar.")
                 ),
                 p(style = "font-size: 12px; color: #c0392b; font-weight: 500;",
                   icon("triangle-exclamation"), " Requires at least 3 fits added above, each successfully optimized."),
                 
                 div(style = "background-color: #f4f6f7; border: 1px solid #d5dbdb; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
                 tags$h5("1. Kinetic Model:"),
                 selectInput("mc_sl_modello_cinetico", "Model:", choices = c(
                   "Arrhenius (recommended)" = "arrhenius",
                   "Q10 (Temp. Quotient)" = "q10",
                   "Eyring (Transition State)" = "eyring",
                   "Custom Formula" = "custom"
                 )),
                 conditionalPanel(
                   condition = "input.mc_sl_modello_cinetico == 'custom'",
                   textInput("mc_sl_formula_custom", "Custom Formula (use 'x', in Kelvin):", placeholder = "A * exp(-Ea / (8.314 * x))"),
                   textInput("mc_sl_guess_custom", "Initial Guesses:", placeholder = "A=1e10[0,], Ea=50000[0,]"),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                     "Same syntax as the sidebar's own Initial Guesses field: \"name=value[lower,upper]\", comma-separated, boundaries optional. Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides x and the parameters you name here. There's no automatic starting guess for a custom formula \u2014 you'll need to provide reasonable values yourself. Any boundaries you set are accepted but not enforced during the fit itself (it always uses unconstrained Simplex, same as the 3 built-in models), only clamped in bootstrap resamples."),
                   p(style = "font-size: 11px; color: #c0392b; margin-top: -6px;",
                     icon("triangle-exclamation"), " If your formula has an Arrhenius-like structure (a pre-exponential factor on the order of 1e10\u20131e14 combined with an activation-energy-like term on the order of 1e4\u20131e5), Simplex can fail to find the right answer even from a guess in the right ballpark \u2014 the very different scales of the two kinds of parameter make this a genuinely hard case for it, which is why the 3 built-in models above are parameterized to avoid it. There's no single fix that reliably works for every custom formula, so treat convergence with some skepticism: check that the fitted curve actually tracks your data well rather than trusting a reported successful fit at face value.")
                 ),
                 div(style = "background-color: white; border: 1px solid #bdc3c7; border-radius: 6px; padding: 4px;",
                     uiOutput("mc_sl_grafico_formula_contenitore")),
                 br(),
                 
                 fluidRow(
                   column(4,
                     tags$h5("2. Critical Threshold & Search Range:"),
                     uiOutput("mc_sl_y_critico_ui"),
                     numericInput("mc_sl_x_min", "X Search Range Min:", value = 0.01),
                     uiOutput("mc_sl_x_max_ui"),
                     selectInput("mc_sl_tipo_intervallo", "Uncertainty Type (Linear method fits):",
                                choices = c("New Observation (incl. residual var.)" = "predizione",
                                            "Mean Response Only" = "media"))
                   ),
                   column(4,
                     tags$h5("3. Temperature per Fit:"),
                     selectInput("mc_sl_unita_temp", "Temperature Unit:", choices = c("Celsius" = "celsius", "Kelvin" = "kelvin")),
                     uiOutput("mc_sl_tabella_temperature")
                   ),
                   column(4,
                     tags$h5("4. Guesses & Run:"),
                     uiOutput("mc_sl_guess_dinamici"),
                     checkboxInput("mc_sl_usa_bootstrap", "Propagate uncertainty (nested bootstrap)", value = FALSE),
                     conditionalPanel(condition = "input.mc_sl_usa_bootstrap",
                       numericInput("mc_sl_n_boot", "Nested Bootstrap Iterations:", value = 1000, min = 100, max = 5000, step = 100)
                     ),
                     numericInput("mc_sl_livello_confidenza", "Confidence/Prediction Level (%):", value = 95, min = 50, max = 99.9, step = 1),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                       "Independent from the global \"Confidence/Prediction Level\" in the sidebar \u2014 governs every confidence/prediction interval in this Shelf-Life module specifically.")
                   )
                 ),
                 actionButton("mc_sl_esegui_analisi", "Run Shelf-Life Analysis", class = "btn-primary w-100", icon = icon("hourglass-half"))
                 ),
                 
                 uiOutput("sezione_mc_sl_risultati_ui")
                 )
                 ),
                 
                 hr(),
                 accordion(
                   id = "confronto_ni_accordion", open = FALSE,
                 accordion_panel(
                   "Non-Isothermal Kinetic Prediction (only for Zero/First/Second/n-Order kinetics)", icon = icon("temperature-half"),
                   p(style = "font-size: 12px; color: #566573;",
                     "Same non-isothermal engine as the Batch tab, applied to fits accumulated with \"Add Current Fit to Comparison\" instead of batch samples loaded via CSV. Unlike Batch (where every sample shares one model), fits here can each use a ", tags$strong("different"), " model \u2014 this only works when at least 3 accumulated fits share the SAME order-based Reaction Kinetics model (Zero/First/Second/n-Order)."),
                   uiOutput("mcni_stato"),
                   uiOutput("mcni_setup_iniziale_ui"),
                   uiOutput("mcni_corpo_principale")
                 )
                 )
        ),
        tabPanel(title = "Batch Elaboration", value = "tab_batch",
                 br(),
                 p(style = "color: #2c3e50; font-size: 14px;",
                   "Applies the model and settings currently configured in the sidebar (\"4. Model Definition\", \"5. Loss Function, Algorithm & Parameter Estimation\") to ", tags$strong("multiple datasets at once"), " \u2014 useful for replicate samples fitted with the same model. Output is intentionally minimal: one overlaid plot and two summary tables, not the full diagnostic set of a single fit."),
                 
                 fluidRow(
                   column(6,
                     div(class = "radio-due-colonne",
                       radioButtons("batch_metodo_input", "Input Method:",
                                    choices = c("Single File \u2014 Long Format (by ID)" = "lungo",
                                                "Single File \u2014 Wide Format (x,y pairs)" = "largo",
                                                "Single File \u2014 Wide Format (x,y1,y2,y3,...)" = "largo_x_condivisa",
                                                "Paste Data (Long Format)" = "incolla",
                                                "Multiple CSV Files (1 file = 1 sample)" = "multi_csv",
                                                "Example Datasets (real data)" = "esempio"))
                     ),
                     checkboxInput("batch_inverti_xy", "Swap X and Y in every sample (data loaded as y, x)", value = FALSE),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'multi_csv'",
                       fileInput("batch_file_multi", "Choose CSV files", multiple = TRUE, accept = ".csv")
                     ),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'lungo'",
                       fileInput("batch_file_lungo", "Choose CSV or Excel file", accept = c(".csv", ".xlsx", ".xls")),
                       uiOutput("batch_selezione_colonna_id"),
                       p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                         "Example: a \"sample_id\" column plus x and y columns, with all samples' rows stacked one under another. ",
                         tags$strong("Optional 4th column:"), " a per-sample temperature (one value per sample \u2014 the first row's value is used for each group), pre-filling the Temperature table in Shelf-Life & Kinetic Model Analysis further down. In the unit selected there (Celsius by default). If omitted, temperatures can still be entered by hand in that table after running the batch. This optional column is only supported with Long Format \u2014 the other input methods always require entering temperatures by hand."),
                       uiOutput("batch_selezione_campioni_lungo")
                     ),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'largo'",
                       fileInput("batch_file_largo", "Choose CSV or Excel file", accept = c(".csv", ".xlsx", ".xls")),
                       p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                         "Columns are paired two by two in the order they appear (columns 1-2 = sample 1, columns 3-4 = sample 2, etc.). If a column is named like \"SampleName_x\"/\"SampleName_y\", that name is used to identify the sample; otherwise samples are numbered.")
                     ),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'largo_x_condivisa'",
                       fileInput("batch_file_largo_x_condivisa", "Choose CSV or Excel file", accept = c(".csv", ".xlsx", ".xls")),
                       p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                         "The first column is x, shared by every sample; each of the other columns is one sample's y values (same x's, paired automatically with each). Every non-first column becomes a sample, named after its own header \u2014 or \"Sample_2\", \"Sample_3\", etc. if a column has no header. Rows where a given sample's y is blank/non-numeric are simply skipped for that sample (other samples keep their own valid rows), so samples don't all need the same number of points.")
                     ),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'incolla'",
                       textAreaInput("batch_dati_incollati", "Data (sample_id x y):", 
                                     value = "A  1.0  6.5\nA  2.0  4.2\nA  3.0  2.8\nA  4.0  1.9\nB  1.0  7.1\nB  2.0  5.0\nB  3.0  3.3\nB  4.0  2.1", 
                                     rows = 8),
                       p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                         "One row per point: sample ID, then x, then y, separated by spaces or tabs \u2014 same paste-friendly format as \"Direct Input (Table)\" in section 1 (comma as decimal separator is also accepted), with an extra ID column at the start. Each sample needs more points than the model's free parameters (e.g. 4+ points for the default 3-parameter Custom model) \u2014 fewer than that, and the fit for that sample will fail.")
                     ),
                     conditionalPanel(
                       condition = "input.batch_metodo_input == 'esempio'",
                       selectInput("batch_dataset_esempio", "Example Dataset:", choices = costruisci_scelte_dataset_batch(), width = "100%"),
                       uiOutput("batch_info_dataset_esempio"),
                       uiOutput("batch_selezione_campioni_esempio")
                     )
                   ),
                   column(6,
                     checkboxInput("batch_forza_lineare", "Use Linear error method for this batch (recommended for speed), even if Monte Carlo is selected in the sidebar", value = TRUE),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                       "Monte Carlo multiplies the cost by its iteration count for every single sample in the batch \u2014 with no parallel processing available in this browser-based environment, this can add up to several minutes even for a modest batch. Uncheck only if you specifically need per-sample Monte Carlo uncertainty (a bootstrap standard error will be computed for each sample, using the sidebar's Monte Carlo Iterations setting)."),
                     checkboxInput("batch_warm_start", "Warm start: use each sample's result as the starting guess for the next", value = FALSE),
                     checkboxInput("batch_retry_convergenza", "Auto-retry with randomized guesses if a sample doesn't converge", value = FALSE),
                     costruisci_box_aiuto_retry(),
                     conditionalPanel(
                       condition = "input.batch_retry_convergenza == true",
                       numericInput("batch_retry_max_tentativi", "Max Retry Attempts:", value = 25, min = 1, max = 100, step = 1),
                       uiOutput("batch_retry_parametri_ui"),
                       checkboxInput("batch_retry_range_adattivo", "Adaptive retry range (avoid wasted attempts near a parameter's boundary)", value = TRUE),
                       checkboxInput("batch_retry_scarta_errore_alto", "Also discard a fit if any parameter's standard error exceeds a chosen % of the parameter's own value", value = FALSE),
                       conditionalPanel(
                         condition = "input.batch_retry_scarta_errore_alto == true",
                         numericInput("batch_retry_soglia_errore_pct", "Max Acceptable Relative Standard Error (%):", value = 100, min = 1, max = 1000, step = 5)
                       )
                     ),
                     hr(),
                     selectInput("batch_trasformazione_y", "Y Pre-transformation (applied before fitting/plotting, all samples):", choices = costruisci_scelte_trasformazione_y(), selected = "nessuna"),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                       "Same as the sidebar's Y Pre-transformation for the single fit \u2014 transforms the actual Y data for every sample before anything else touches it. If any sample has a value outside the chosen transform's valid domain (e.g. zero/negative for Ln), the whole batch is stopped with a clear message rather than transforming samples inconsistently."),
                     selectInput("batch_trasformazione_densita", "Distribution Density Transformation (all samples):",
                                 choices = c("None" = "nessuna", "Arithmetic (linear bin width)" = "aritmetica", "Logarithmic (bin width in ln space)" = "logaritmica"),
                                 selected = "nessuna"),
                     costruisci_box_aiuto_densita(),
                     conditionalPanel(
                       condition = "input.batch_trasformazione_densita != 'nessuna'",
                       selectInput("batch_densita_media_bin", "Bin Center (X shown on the density curve):",
                                   choices = c("Arithmetic mean" = "aritmetica", "Geometric mean" = "geometrica"),
                                   selected = "aritmetica"),
                       fluidRow(
                         column(6,
                           selectInput("batch_densita_confine_inf_modo", "Lower Boundary (smallest X):", choices = c("Use this point's own value" = "proprio", "Assume a different value" = "assumi", "Discard this point" = "scarta"), selected = "proprio"),
                           conditionalPanel(condition = "input.batch_densita_confine_inf_modo == 'assumi'",
                             numericInput("batch_densita_confine_inf_valore", "Assumed Lower Boundary:", value = 0))
                         ),
                         column(6,
                           selectInput("batch_densita_confine_sup_modo", "Upper Boundary (largest X):", choices = c("Assume a value" = "assumi", "Discard this point" = "scarta"), selected = "assumi"),
                           conditionalPanel(condition = "input.batch_densita_confine_sup_modo == 'assumi'",
                             numericInput("batch_densita_confine_sup_valore", "Assumed Upper Boundary:", value = NA))
                         )
                       ),
                       fluidRow(
                         column(6, checkboxInput("batch_densita_ancora_zero", "Add (0, 0) anchor point", value = FALSE)),
                         column(6,
                           checkboxInput("batch_densita_ancora_max", "Add (X, 0) anchor point", value = FALSE),
                           conditionalPanel(condition = "input.batch_densita_ancora_max",
                             numericInput("batch_densita_ancora_max_valore", "X value:", value = NA))
                         )
                       ),
                     ),
                   )
                 ),
                 
                 hr(),
                 p(style = "font-size: 11px; color: #7f8c8d;",
                   "Uses the Algorithm and Loss Function selected in the sidebar, for every sample \u2014 the sidebar's \"Accept non-converged optimization results\" checkbox, however, does ", tags$strong("not"), " apply here: this table always reports each sample's true convergence status honestly (see the \"Converged\" column below), regardless of that checkbox."),
                 
                 conditionalPanel(condition = "input.tipo_loss == 'mse' && input.ponderazione != 'nessuna'",
                   p(style = "font-size: 12px; color: #2c3e50; margin-bottom: 4px;", "The weighting selected in the sidebar (w = 1/\u0177^\u03b6) is applied to each sample."),
                 conditionalPanel(condition = "input.ponderazione == 'repliche'",
                   radioButtons("batch_zeta_modo", "\u03b6 = 2b from the replicates:",
                                choices = c("Common to all samples (replicates of all samples pooled; recommended)" = "comune", "Estimated for each sample from its own replicates" = "campione"), selected = "comune"),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px; margin-bottom: 8px;",
                     "The common \u03b6 assumes that all samples share the same variance structure, as when they are measured with the same analytical method, and is much more stable. A \u03b6 for each sample needs at least three x values with replicates in every sample and, with two to four replicates per x, is imprecise."))),
                 actionButton("esegui_batch", "Run Batch Elaboration", class = "btn-primary w-100", icon = icon("layer-group")),
                 
                 br(), br(),
                 tags$strong(textOutput("stato_batch", inline = TRUE)),
                 br(), br(),
                 uiOutput("batch_info_impostazioni"),
                 tags$h4("Overlaid Fitted Curves (all samples):"),
                 uiOutput("batch_selezione_campioni_overlay"),
                 checkboxInput("batch_mostra_punti", "Show each sample's own data points", value = TRUE),
                 plotOutput("grafico_batch_overlay", height = "420px"),
                 div(style = "margin-top: 8px; margin-bottom: 20px;", 
                     downloadButton("scarica_batch_grafico", "Download Plot", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_batch_fit_singoli", "Download Single Fits (Combined Image)", class = "btn-outline-secondary btn-sm"),
                     actionButton("btn_estetica_batch", "\U0001F3A8", class = "btn-outline-secondary btn-sm", title = "Customize this plot")),
                 
                 div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
                 tags$h4("Manage Samples:"),
                 p(style = "font-size: 12px; color: #566573; margin-bottom: 4px;", tags$strong("Rename a sample:")),
                 fluidRow(style = "margin-bottom: 8px;",
                   column(4, uiOutput("batch_selezione_rinomina_ui")),
                   column(4, textInput("batch_nuovo_nome_campione", NULL, placeholder = "New label...")),
                   column(4, actionButton("batch_rinomina_campione", "\u270F\uFE0F Rename Selected Sample", class = "btn-warning btn-sm w-100"))
                 ),
                 p(style = "font-size: 12px; color: #566573; margin-bottom: 4px;", tags$strong("Remove a sample:")),
                 fluidRow(style = "margin-bottom: 8px;",
                   column(4, uiOutput("batch_selezione_rimozione_ui")),
                   column(4, actionButton("batch_rimuovi_campione", "\U0001F5D1\uFE0F Remove Selected Sample", class = "btn-sm w-100", style = "background-color: #e67e22; border-color: #e67e22; color: white;"))
                 ),
                 ),
                 hr(style = "border-top: 1px solid #999999;"),
                 
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Parameter Estimation Results (all samples):"),
                   tags$button(id = "btn_copia_batch_parametri", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_batch_parametri', 'btn_copia_batch_parametri')")
                 ),
                 fluidRow(
                   column(6, uiOutput("batch_par_sort_choices_ui")),
                   column(6, selectInput("batch_par_sort_dir", "Order:", choices = c("Ascending" = "asc", "Descending" = "desc")))
                 ),
                 p(style = "font-size: 11px; color: #7f8c8d; margin-top: -8px;",
                   "Tip: sort by \"Converged\" (ascending) to bring failed or non-converged samples to the top."),
                 tableOutput("tabella_batch_parametri"),
                 br(),
                 uiOutput("sezione_istogrammi_batch_ui"),
                 br(),
                 uiOutput("sezione_boxplot_batch_ui"),
                 br(),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                   tags$h4(style = "margin: 0;", "Goodness-of-Fit Diagnostics (all samples):"),
                   tags$button(id = "btn_copia_batch_diagnostica", "\U0001F4CB Copy to Clipboard", class = "btn btn-outline-secondary btn-sm",
                     style = "padding-top: 2px; padding-bottom: 2px;",
                     onclick = "copiaTabellaClipboard('tabella_batch_diagnostica', 'btn_copia_batch_diagnostica')")
                 ),
                 fluidRow(
                   column(6, selectInput("batch_diag_sort_by", "Sort By:",
                                         choices = c("Sample" = "Sample", "AICc" = "AICc", "AIC" = "AIC", "BIC" = "BIC", "RMSE" = "RMSE",
                                                     "R-squared" = "R2", "R-squared Adjusted" = "R2_Adj"))),
                   column(6, selectInput("batch_diag_sort_dir", "Order:", choices = c("Ascending" = "asc", "Descending" = "desc")))
                 ),
                 tags$style(HTML("
                   #tabella_batch_diagnostica { overflow-x: auto; }
                   #tabella_batch_diagnostica table td { white-space: nowrap; }
                 ")),
                 tableOutput("tabella_batch_diagnostica"),
                 uiOutput("batch_nota_criteri"),
                 uiOutput("bdr_sezione"),
                 
                 uiOutput("sezione_batch_test_parametri_ui"),
                 uiOutput("sezione_batch_posthoc_intestazione_ui"),
                 
                 div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 6px; margin-top: 15px; margin-bottom: 25px; max-width: 50%;",
                     tags$h5(style = "margin-top: 0; color: #2c3e50;", icon("file-export"), " Export Data Tables"),
                     p(style = "font-size: 13px; color: #7f8c8d;", "Parameter Estimation Results, Diagnostics (one row per sample), the Significance Test, and the Post-Hoc Comparison above (whichever have been run), in a single file."),
                     downloadButton("scarica_batch_csv", "Download as CSV", class = "btn-outline-secondary btn-sm"),
                     downloadButton("scarica_batch_excel", "Download as Excel (.xlsx)", class = "btn-outline-secondary btn-sm")
                 ),
                 
                 hr(),
                 accordion(
                   id = "sl_accordion", open = FALSE,
                   accordion_panel(
                     "Shelf-Life & Kinetic Model Analysis", icon = icon("hourglass-half"),
                     p(style = "font-size: 12px; color: #566573;",
                   "For each sample above, finds the time (X) at which Y crosses a critical threshold (shelf life), then relates it to storage temperature via a ", tags$strong("non-linear"), " fit of the kinetic model chosen below (not a linearized regression). Only the rigorous, nested-bootstrap version is offered here: it would be inconsistent to throw away the precision of a per-sample Monte Carlo run at this last step. The fit itself (both the point estimate and every bootstrap iteration) uses ", tags$strong("Levenberg-Marquardt"), " (least squares, with analytical derivatives when the formula allows) \u2014 independent from the Loss Function and Algorithm chosen in the sidebar, since the confidence interval math here assumes least squares."),
                 p(style = "font-size: 12px; color: #c0392b; font-weight: 500;",
                   icon("triangle-exclamation"), " Rate constants typically vary by 10\u201330\u00d7 across a realistic temperature range \u2014 a single fixed initial guess can fail badly on the coldest/slowest samples (verified while building this). Strongly recommend enabling ", tags$strong("Warm start"), " above before running the batch, so each sample's fit starts from the previous (warmer, easier) one's result."),
                 
                 div(style = "background-color: #f4f6f7; border: 1px solid #d5dbdb; border-radius: 8px; padding: 15px 18px; margin-bottom: 15px;",
                 tags$h5("1. Kinetic Model:"),
                 selectInput("sl_modello_cinetico", "Model:", choices = c(
                   "Arrhenius (recommended)" = "arrhenius",
                   "Q10 (Temp. Quotient)" = "q10",
                   "Eyring (Transition State)" = "eyring",
                   "Custom Formula" = "custom"
                 )),
                 conditionalPanel(
                   condition = "input.sl_modello_cinetico == 'custom'",
                   textInput("sl_formula_custom", "Custom Formula (use 'x', in Kelvin):", placeholder = "A * exp(-Ea / (8.314 * x))"),
                   textInput("sl_guess_custom", "Initial Guesses:", placeholder = "A=1e10[0,], Ea=50000[0,]"),
                   p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                     "Same syntax as the sidebar's own Initial Guesses field: \"name=value[lower,upper]\", comma-separated, boundaries optional. Basic math only (+, -, *, /, ^, exp, log, sqrt, trig functions, etc.) \u2014 no other functions or variables besides x and the parameters you name here. There's no automatic starting guess for a custom formula \u2014 you'll need to provide reasonable values yourself. Any boundaries you set are accepted but not enforced during the fit itself (it always uses unconstrained Simplex, same as the 3 built-in models), only clamped in bootstrap resamples."),
                   p(style = "font-size: 11px; color: #c0392b; margin-top: -6px;",
                     icon("triangle-exclamation"), " If your formula has an Arrhenius-like structure (a pre-exponential factor on the order of 1e10\u20131e14 combined with an activation-energy-like term on the order of 1e4\u20131e5), Simplex can fail to find the right answer even from a guess in the right ballpark \u2014 the very different scales of the two kinds of parameter make this a genuinely hard case for it, which is why the 3 built-in models above are parameterized to avoid it. There's no single fix that reliably works for every custom formula, so treat convergence with some skepticism: check that the fitted curve actually tracks your data well rather than trusting a reported successful fit at face value.")
                 ),
                 div(style = "background-color: white; border: 1px solid #bdc3c7; border-radius: 6px; padding: 4px;",
                     uiOutput("sl_grafico_formula_contenitore")),
                 br(),
                 
                 fluidRow(
                   column(4,
                     tags$h5("2. Critical Threshold & Search Range:"),
                     uiOutput("sl_y_critico_ui"),
                     numericInput("sl_x_min", "X Search Range Min:", value = 0.01),
                     uiOutput("sl_x_max_ui"),
                     selectInput("sl_tipo_intervallo", "Uncertainty Type (Linear method samples):",
                                choices = c("New Observation (incl. residual var.)" = "predizione",
                                            "Mean Response Only" = "media"))
                   ),
                   column(4,
                     tags$h5("3. Temperature per Sample:"),
                     selectInput("sl_unita_temp", "Temperature Unit:", choices = c("Celsius" = "celsius", "Kelvin" = "kelvin")),
                     uiOutput("sl_tabella_temperature")
                   ),
                   column(4,
                     tags$h5("4. Guesses & Run:"),
                     uiOutput("sl_guess_dinamici"),
                     checkboxInput("sl_usa_bootstrap", "Propagate uncertainty (nested bootstrap)", value = FALSE),
                     conditionalPanel(condition = "input.sl_usa_bootstrap",
                       numericInput("sl_n_boot_arrhenius", "Nested Bootstrap Iterations:", value = 1000, min = 100, max = 5000, step = 100)
                     ),
                     numericInput("sl_livello_confidenza", "Confidence/Prediction Level (%):", value = 95, min = 50, max = 99.9, step = 1),
                     p(style = "font-size: 11px; color: #7f8c8d; margin-top: -6px;",
                       "Independent from the global \"Confidence/Prediction Level\" in the sidebar \u2014 governs every confidence/prediction interval in this Shelf-Life module specifically.")
                   )
                 ),
                 actionButton("sl_esegui_analisi", "Run Shelf-Life Analysis", class = "btn-primary w-100", icon = icon("hourglass-half"))
                 ),
                 
                 uiOutput("sezione_sl_risultati_ui")
                   )
                 ),
                 
                 hr(),
                 accordion(
                   id = "ni_accordion", open = FALSE,
                   accordion_panel(
                     "Non-Isothermal Kinetic Prediction (only for Zero/First/Second/n-Order kinetics)", icon = icon("temperature-half"),
                     p(style = "font-size: 12px; color: #566573;",
                       "Predicts the critical time under a ", tags$strong("time-varying"), " temperature profile, instead of a single fixed storage temperature. Only works when the Batch above was fitted with one of the order-based Reaction Kinetics models (Zero/First/Second/n-Order) \u2014 not with microbial growth models, which would need a full differential-equation solver not available in this browser-based environment. First builds a secondary model relating the fitted rate constant k to temperature (using the same samples as above, each at its own storage temperature), then integrates that rate over your chosen temperature-vs-time profile to find when a critical threshold is crossed."),
                     uiOutput("ni_stato"),
                     uiOutput("ni_setup_iniziale_ui"),
                     uiOutput("ni_corpo_principale")
                   )
                 )
        ),
        tabPanel(title = "References and R Packages", value = "tab_ref",
                 br(),
                 tags$h4("Scientific References"),
                 tags$ul(style = "line-height: 1.8; font-size: 14px;",
                         tags$li("Barbosa-Cánovas, G. V., Fontana Jr, A. J., Schmidt, S. J., & Labuza, T. P. (2020). Water activity in foods: fundamentals and applications. John Wiley & Sons."),
                         tags$li("Bates, D. M., & Watts, D. G. (1988). Nonlinear regression analysis and its applications. New York: Wiley."),
                         tags$li("Bourne, M. (2002). Food texture and viscosity: concept and measurement. Elsevier."),
                         tags$li("Burnham, K. P., & Anderson, D. R. (1998). Model selection and inference: a practical information-theoretic approach. New York: Springer-Verlag."),
                         tags$li("Carroll, R. J., & Ruppert, D. (1988). Transformation and weighting in regression. New York: Chapman and Hall."),
                         tags$li("Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application. Cambridge: Cambridge University Press."),
                         tags$li("Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New York: Chapman & Hall."),
                         tags$li("Gardini, F., & Parente, E. (2013). Manuale di microbiologia predittiva. Italia: Springer-Verlag."),
                         tags$li("Kilcast, D., & Subramaniam, P. (2011). Food and beverage stability and shelf life. Elsevier."),
                         tags$li("Motulsky, H., & Christopoulos, A. (2004). Fitting models to biological data using linear and nonlinear regression: a practical guide to curve fitting. Oxford University Press."),
                         tags$li("Neter, J., Kutner, M. H., Nachtsheim, C. J., & Wasserman, W. (1996). Applied linear statistical models."),
                         tags$li("Peleg, M. (2006). Advanced quantitative microbiology for foods and biosystems. Contemporary Food Science. CRC Taylor & Francis."),
                         tags$li("van Boekel, M. A. J. S. (2008). Kinetic modeling of reactions in foods. CRC press.")
                 ),
                 hr(),
                 tags$h4("R Packages"),
                 tags$ul(style = "line-height: 1.8; font-size: 14px;",
                         tags$li(paste0("R (v", getRversion(), ")")),
                         tags$li(paste0("shiny (v", packageVersion("shiny"), ")")),
                         tags$li(paste0("minpack.lm (v", packageVersion("minpack.lm"), ")")),
                         tags$li(paste0("bslib (v", packageVersion("bslib"), ")")),
                         tags$li(paste0("writexl (v", packageVersion("writexl"), ")")),
                         tags$li(paste0("readxl (v", packageVersion("readxl"), ")")),
                         tags$li(paste0("zip (v", packageVersion("zip"), ")"))
                 )
        )
      )
    )
  )
)