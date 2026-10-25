# ENReT — Easy Non-linear Regression Tool

**ENReT** is an interactive application for nonlinear regression, designed for food technologists and food scientists. It fits built-in or custom models to experimental data and provides parameter estimates with their uncertainty, model diagnostics and model comparison, without requiring programming.

**Before using the application, reading the [user guide](docs/ENReT_guide.pdf) is strongly recommended**: it explains how the application was conceived and developed, its theoretical basis, and how to use it, with guided examples.

Version: **1.0.0**, the first public release (see `CHANGELOG.md` for the version history).

## Main features

- **Model library**: 62 built-in models in 9 categories (microbial growth, microbial inactivation and Weibull models, reaction and enzyme kinetics, mass transfer, rheology and texture, water activity and sorption isotherms, statistical distributions), 42 models for the reference datasets, and fully custom formulas.
- **Fitting**: Levenberg-Marquardt with analytical derivatives, plus BFGS, L-BFGS-B and Nelder-Mead (with optional restarts); robust loss functions; weighted least squares with the power-of-the-mean variance model.
- **Uncertainty**: linear approximation, profile likelihood (with profile t plots and profile traces as in Bates & Watts, 1988) and Monte Carlo (residual bootstrap with leverage-corrected residuals, or Gaussian noise); confidence and prediction bands.
- **Diagnostics**: residual plots, normal Q-Q plot, lag plot, variance structure, runs test and, with replicates, lack-of-fit and variance homogeneity tests.
- **Model comparison**: information criteria, F tests and post-hoc comparisons.
- **Batch Elaboration**: the same model fitted to many samples, with statistical comparisons between samples and residual diagnostics for each sample.
- **Applications**: shelf-life estimation, non-isothermal predictions and inverse prediction.
- **Data**: direct entry, CSV/Excel upload, synthetic data, and 67 reference datasets (ComBase, NIST StRD, Bates & Watts, Carroll & Ruppert).
- **Exports**: results in CSV and Excel, plots in PDF and JPEG, combined images and ZIP archives.

## Use online

ENReT runs in the browser, with no installation: **https://enret.netlify.app/**

The web version runs R in the browser (Shinylive / webR). It is suitable for everyday analyses; for intensive computations (for example Monte Carlo with many iterations and Nelder-Mead with restarts, or Batch Elaboration with many samples) the local version in R is faster and has no browser memory limits.

## Run locally in R

1. Install R (version 4.1 or later).
2. Install the required packages:

   ```r
   install.packages(c("shiny", "bslib", "minpack.lm", "readxl", "writexl", "zip"))
   ```

3. Download this repository (green **Code** button → **Download ZIP**), unzip it and start the app:

   ```r
   shiny::runApp("path/to/ENReT")
   ```

### Repository structure

| Path | Content |
|---|---|
| `app.R`, `global.R`, `model_registry.R` | entry point, global settings, model and dataset registry |
| `R/` | user interface (`ui.R`) and server (`server.R`) |
| `R/_disable_autoload.R` | disables the automatic loading of the `R/` folder by Shiny, so that the files are loaded in the order set in `app.R` (required) |
| `server_chunks/` | computational modules (fitting engine, diagnostics, Batch Elaboration, model comparison, shelf-life, non-isothermal predictions, exports) |
| `app_deploy.R` | single-file version of the app used for the web version (Shinylive), generated from the files above: changes should be made to the source files |
| `docs/` | user guide, statistical details and validation, validation results |

Code comments are in English; internal function and variable names are in Italian.

## Documentation

- **User guide**: [`docs/ENReT_guide.pdf`](docs/ENReT_guide.pdf) - an introduction to the application, its structure and two guided examples (single fit and batch elaboration). Start here.
- **Statistical details and validation**: [`docs/ENReT_statistical_details_v1.0.0.pdf`](docs/ENReT_statistical_details_v1.0.0.pdf) - the R functions used for the computations, the noteworthy statistical and numerical choices of the application, with their sources, and the numerical validation.
- **Validation results**: [`docs/ENReT_v1.0.0_validation_results.xlsx`](docs/ENReT_v1.0.0_validation_results.xlsx) - workbook with the complete results of the validation (NIST, Bates & Watts, Carroll & Ruppert, ComBase).
- **Version history**: [`CHANGELOG.md`](CHANGELOG.md).

## Validation

The numerical results were validated against:

- the 26 nonlinear regression datasets of the **NIST Statistical Reference Datasets**, with certified values: at least 5 correct significant digits for all parameters and residual sums of squares, apart from two known issues of the certified values (Roszman1, Lanczos1);
- the datasets of **Bates & Watts (1988)**, *Nonlinear Regression Analysis and Its Applications*;
- the esterase assay data of **Carroll & Ruppert (1988)**, *Transformation and Weighting in Regression*, for weighted least squares;
- the parameters estimated by **ComBase** for 26 microbial growth curves (Baranyi & Roberts model).

Details are given in the statistical document and in the validation workbook in `docs/`.

## Use of AI tools

The source code of ENReT was developed with the assistance of large language models: initially Google Gemini Pro 3.1, then Claude (Anthropic), first with Claude Sonnet 5 and later with Claude Opus 5.5. These tools were used to write, refactor and test code and to draft parts of the documentation. The author designed the application, defined the statistical methods and their sources, checked the cited literature, reviewed and tested the code, and is responsible for its content. The numerical results were validated independently against certified and published reference data (NIST StRD, Bates & Watts, Carroll & Ruppert, ComBase), as described in the statistical document.

## Citation

A citation will be added here when the accompanying article is available.

## License

Copyright (C) 2026 Riccardo De Flaviis

ENReT is free software, distributed under the **GNU General Public License v3.0** (see `LICENSE`). It is provided without any warranty.

The documentation in the `docs/` folder is licensed under the **Creative Commons Attribution 4.0 International License (CC BY 4.0)**: it can be shared and adapted, provided that the author is credited and any changes are indicated (https://creativecommons.org/licenses/by/4.0/).

The reference datasets included in the application remain the property of their respective sources (ComBase, NIST, Bates & Watts, Carroll & Ruppert, and the cited authors) and are not covered by the license of the code; they are included, with full citation, for educational and validation purposes.

## Contact

Suggestions and questions: **enret.app@gmail.com**

Author: Riccardo De Flaviis, PhD
