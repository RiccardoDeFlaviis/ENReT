# Intentionally empty file.
#
# Its mere presence in this folder tells Shiny (the R package, from its
# version 1.5.0 onward; not a version of ENReT)
# NOT to run the automatic "loadSupport()" loading - i.e. NOT to
# automatically source global.R and every .R file inside R/ (including
# ui.R and server.R) BEFORE app.R's own body starts executing.
#
# Why this is needed: loadSupport() sources the files inside R/ into a
# SEPARATE environment from the one app.R runs in, and it does so BEFORE
# app.R gets the chance to run its own explicit source() calls (global.R,
# then model_registry.R, then R/ui.R, then R/server.R). The result used to
# be an error: "could not find function costruisci_scelte_modello()" - R/ui.R
# was being loaded automatically by loadSupport() before model_registry.R
# had ever been sourced by anyone.
#
# With this file present, loading the app depends 100% on the explicit,
# hand-written source() calls in app.R - which is exactly what we want,
# since app.R is already written to do this in the correct order.
#
# Official documentation: ?shiny::loadSupport
