# Changelog

All notable changes to ENReT are documented in this file. Versions follow [semantic versioning](https://semver.org/): MAJOR.MINOR.PATCH, where PATCH versions fix errors, MINOR versions add features compatible with previous versions, and MAJOR versions introduce incompatible changes.

## [1.0.1] — 2026

Corrections and small improvements. The least-squares results of the validation described in `docs/` (NIST and Bates & Watts datasets) are unchanged.

### Fixed
- Levenberg-Marquardt with an active bound: when a parameter ended on one of its bounds, the other parameters could stop at a non-optimal point while convergence was reported. The fit is now refined from that point; results without active bounds are unchanged (single fit, weighted regression, Monte Carlo and Batch Elaboration).
- Confidence and prediction bands (linear method), parameter sensitivity, Inverse Prediction and Shelf-Life (delta method) when a parameter is equal to zero (e.g. a lag on its bound): the bands could be very wide and irregular. The derivatives with respect to the parameters are now analytical.
- Inverse Prediction and Shelf-Life: the search range is scanned for every crossing of the target value, so non-monotone curves are handled; with more than one solution the smallest X is returned and the user is warned. In the Monte Carlo method each iteration uses the solution corresponding to the main one. Inverse Prediction requires a valid search range (pre-filled with the X range of the data). Shelf-Life solutions are computed with a relative tolerance.
- Shelf-Life with Monte Carlo fits: the "New Observation" choice now also applies to these samples (previously only to the linear method).
- Inverse Prediction with Monte Carlo fits: the noise of a new observation uses the same leverage-corrected residuals as the prediction band; the prediction band drawn for models saved in Model Comparison is generated as in the single fit.
- NIST validation statements in the Disclaimer and in the tolerance warning: 50 of 52 runs with at least 4 correct digits (the remaining two, on Lanczos1, are due to the limits of double-precision arithmetic).

### Removed
- Huber loss: its threshold was recomputed from the residuals at every evaluation, so the minimized function was not the loss of the standard Huber estimator. Operator configurations saved with it switch to least squares, with a warning.

### Added
- Warnings for parameters on a bound (standard error and confidence interval not reliable), with "(on bound)" in the parameter table and an On_Bound column in the exports and in Batch Elaboration; warning when BFGS or Nelder-Mead, which ignore bounds, give estimates outside them; warning and note when the MAE loss is used with BFGS or L-BFGS-B.
- Note under the formula of the Baranyi models: they are written for the natural logarithm of the counts, with instructions for log10 data.
- How to cite ENReT (article and software), in the Disclaimer and in the References tab.
- Line types of the confidence and prediction bands in the customization of the main plot.
- `Start_ENReT.bat`, to start the local version with a double click on Windows (available on the Releases page).

### Changed
- Sidebar with all sections closed at start.
- Band controls moved under the main plot, shown after the first fit; Export box at full width.
- E-mail address in the header.

## [1.0.0] — 2026

First public release.

The validation described in `docs/` applies to this release.

Main features of this release:

- nonlinear regression with 62 built-in models in 9 categories, 42 models for the reference datasets and custom formulas;
- Levenberg-Marquardt with analytical derivatives, BFGS, L-BFGS-B and Nelder-Mead with optional restarts; robust loss functions; weighted least squares with the power-of-the-mean variance model;
- uncertainty by linear approximation, profile likelihood (profile t plots and profile traces) and Monte Carlo (residual bootstrap with leverage-corrected residuals, or Gaussian noise), with confidence and prediction bands;
- residual diagnostics (residual plots, normal Q-Q plot, lag plot, variance structure, runs test, lack-of-fit and variance homogeneity tests);
- model comparison, Batch Elaboration with statistical comparisons and residual diagnostics for each sample, shelf-life estimation, non-isothermal predictions and inverse prediction;
- 67 reference datasets (ComBase, NIST StRD, Bates & Watts, Carroll & Ruppert);
- validation against the NIST Statistical Reference Datasets, Bates & Watts (1988), Carroll & Ruppert (1988) and ComBase.
