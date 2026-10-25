# ==============================================================================
# EASY NON-LINEAR REGRESSION TOOL v1.0.0 - GLOBAL
# ==============================================================================
# This file is executed by Shiny BEFORE loading the R/ folder (which contains
# ui.R and server.R). All library() calls and any definition that ui.R/server.R
# need to find already available when they are read should go here.

library(shiny)
library(minpack.lm)
library(bslib)
library(writexl)
library(zip)
library(readxl)

# CRUCIAL WORKAROUND FOR SHINYLIVE
# Must stay here (and not in app.R) because ui.R is built BEFORE app.R is
# executed: if the workaround stayed in app.R, ui.R would still use Shiny's
# original downloadButton(), not the corrected one.
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
