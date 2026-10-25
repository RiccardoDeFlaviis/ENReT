# Changelog

All notable changes to ENReT are documented in this file. Versions follow [semantic versioning](https://semver.org/): MAJOR.MINOR.PATCH, where PATCH versions fix errors, MINOR versions add features compatible with previous versions, and MAJOR versions introduce incompatible changes.

## [1.0.0] — 2026

First public release.

The computations are identical to those of the last development version, 0.11.1, so the validation described in `docs/` applies to this release.

Earlier versions were internal development versions. They were originally numbered x.y (for example, 10.4 and 11.1) and were renumbered 0.x.y (0.10.4, 0.11.1) at the first public release, following [semantic versioning](https://semver.org/); version 1.0.0 corresponds to development version 0.11.1, formerly 11.1.

Main features of this release:

- nonlinear regression with 62 built-in models in 9 categories, 42 models for the reference datasets and custom formulas;
- Levenberg-Marquardt with analytical derivatives, BFGS, L-BFGS-B and Nelder-Mead with optional restarts; robust loss functions; weighted least squares with the power-of-the-mean variance model;
- uncertainty by linear approximation, profile likelihood (profile t plots and profile traces) and Monte Carlo (residual bootstrap with leverage-corrected residuals, or Gaussian noise), with confidence and prediction bands;
- residual diagnostics (residual plots, normal Q-Q plot, lag plot, variance structure, runs test, lack-of-fit and variance homogeneity tests);
- model comparison, Batch Elaboration with statistical comparisons and residual diagnostics for each sample, shelf-life estimation, non-isothermal predictions and inverse prediction;
- 67 reference datasets (ComBase, NIST StRD, Bates & Watts, Carroll & Ruppert);
- validation against the NIST Statistical Reference Datasets, Bates & Watts (1988), Carroll & Ruppert (1988) and ComBase.
