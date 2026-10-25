# Changelog

All notable changes to ENReT are documented in this file. Versions follow [semantic versioning](https://semver.org/): MAJOR.MINOR.PATCH, where PATCH versions fix errors, MINOR versions add features compatible with previous versions, and MAJOR versions introduce incompatible changes.

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
