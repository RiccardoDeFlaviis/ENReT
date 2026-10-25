# ==============================================================================
# EASY NON-LINEAR REGRESSION TOOL v1.0.0 - MAIN
# ==============================================================================

library(shiny)
library(bslib)
library(minpack.lm)
library(writexl)
library(zip)
library(readxl)

# 1. LOAD THE GLOBAL FILE FIRST (Essential!)
# This will load the libraries and apply the downloadButton workaround
source("global.R")

# 2. LOAD THE UI AND SERVER FROM THE R FOLDER
# (R/ui.R loads model_registry.R itself with local=TRUE right before using
# it, so it no longer needs to be sourced here)
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)
