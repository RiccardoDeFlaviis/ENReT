# ==============================================================================
# MODEL REGISTRY - Single source of truth for all primary models
# ==============================================================================
# Before this file, the same information about each model (fitting formula,
# text shown to the user, default guess, dropdown grouping) was scattered
# across 4 different places between ui.R and server.R, which had to be kept
# manually in sync - a source of bugs (e.g. the "peleg_pos_k1k2" and
# "peleg_neg_k1k2" models, removed from the dropdown in the past but
# forgotten in the switches - definitively removed here since they were no
# longer reachable from anywhere).
#
# To add/modify/validate a model, this is the ONLY file to touch: change
# "formula" here (and "display" if you want the text shown to the user to
# reflect the change) and the fix automatically applies to the fit, the
# on-screen formula, the default guess, and the dropdown.
#
# Structure of each entry:
#   group   - name of the group in the dropdown menu (models sharing the
#             same "group", in the order they appear below, are grouped
#             automatically under the same sub-heading in the menu)
#   label   - label shown in the dropdown menu
#   formula - R string actually used for fitting (must use "x" for the
#             independent variable and the exact parameter names)
#   display - text shown below the menu when the model is selected
#   guess   - default string for the "Initial Guesses" field when this
#             model is selected
# ==============================================================================

MODEL_REGISTRY <- list(
  
  "custom" = list(
    group = "General", categoria = "Custom Model", label = "Custom Model...",
    formula = NA,  # handled separately: uses the formula written by the user
    display = "Equation freely defined by the user in the text box below.",
    guess = "A=10, B=0.5, C=1"
  ),
  
  # ---- Weibull Family ----
  "weibull1" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (+, from C0 to Ce)",
    formula = "C0 + (Ce - C0) * (1 - exp(-((x / alpha)^beta)))",
    display = "Active function: y = C0 + (Ce - C0) * (1 - exp(-((x / alpha)^beta)))",
    guess = "C0=1[0,], Ce=10[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(C0 = "C[0]", Ce = "C[e]")
  ),
  "weibull2" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (+, from 0 to Ce)",
    formula = "Ce * (1 - exp(-((x / alpha)^beta)))",
    display = "Active function: y = Ce * (1 - exp(-((x / alpha)^beta)))",
    guess = "Ce=10[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(Ce = "C[e]")
  ),
  "weibull3" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (+, from 0 to 1)",
    formula = "1 - exp(-((x / alpha)^beta))",
    display = "Active function: y = 1 - exp(-((x / alpha)^beta))",
    guess = "alpha=2[0.001,], beta=1.5[0.001,]"
  ),
  "weibull_dec1" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (-, from C0 to Ce)",
    # Numerically verified: at x=0 it equals C0, for x->infinity it tends to Ce
    # (assuming C0>Ce, a true decay). The form "C0 - (Ce-C0)*(1-exp(...))"
    # initially proposed did NOT converge to Ce (sign error) - corrected
    # by using exp(...) directly (equal to 1 at x=0, 0 for x->infinity) instead
    # of (1-exp(...)).
    formula = "Ce + (C0 - Ce) * exp(-((x / alpha)^beta))",
    display = "Active function: y = Ce + (C0 - Ce) * exp(-((x / alpha)^beta))",
    guess = "C0=10[0,], Ce=1[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(C0 = "C[0]", Ce = "C[e]")
  ),
  "weibull_dec2" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (-, from C0 to 0)",
    # Same correction as the previous one: C0*exp(...) starts from C0 and decays to
    # 0, numerically verified - the form "C0*(1-exp(...))" initially
    # proposed did exactly the opposite (0 -> C0).
    formula = "C0 * exp(-((x / alpha)^beta))",
    display = "Active function: y = C0 * exp(-((x / alpha)^beta))",
    guess = "C0=10[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(C0 = "C[0]")
  ),
  "weibull_dec3" = list(
    group = "Weibull Family", categoria = "Weibull models", label = "Weibull Model (-, from 1 to 0)",
    formula = "exp(-((x / alpha)^beta))",
    display = "Active function: y = exp(-((x / alpha)^beta))",
    guess = "alpha=2[0.001,], beta=1.5[0.001,]"
  ),
  "weibull_dist" = list(
    group = "Weibull Family", categoria = "Statistical Distributions", label = "Weibull Distribution",
    # CORRECTED: the previous version had alpha and beta swapped with respect to
    # the standard convention and to all the other Weibull models of this
    # registry (alpha=scale, beta=shape) - numerically verified
    # by comparison with the standard Weibull PDF: the old formula
    # corresponded exactly to the standard one but with alpha/beta inverted
    # (both integrated to 1, but the roles were swapped).
    formula = "((beta / (alpha^beta)) * (x^(beta - 1))) * exp(-((x / alpha)^beta))",
    display = "Active function: y = ((beta / (alpha^beta)) * (x^(beta - 1))) * exp(-((x / alpha)^beta))",
    guess = "alpha=2[0.001,], beta=3[0.001,]"
  ),
  "weibull_micro" = list(
    group = "Weibull Family", categoria = "Microbial Growth", label = "Weibull for Microbial Growth",
    formula = "N0 + A * (1 - exp(-((x / alpha)^beta)))",
    display = "Active function: y = N0 + A * (1 - exp(-((x / alpha)^beta)))",
    guess = "N0=1[0,], A=7[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(N0 = "N[0]")
  ),
  
  # ---- Logistic Family ----
  "log" = list(
    group = "Logistic Family", categoria = "Microbial Growth", label = "Logistic Growth",
    formula = "L / (1 + exp(-k * (x - x0)))",
    display = "Active function: y = L / (1 + exp(-k * (x - x0)))",
    guess = "L=10[0,], k=0.5[0,], x0=2",
    simboli = c(x0 = "x[0]")
  ),
  "log_riparam" = list(
    group = "Logistic Family", categoria = "Microbial Growth", label = "Reparameterized Logistic",
    formula = "N0 + A / (1 + exp(2 + (4 * MU * (L - x)) / A))",
    display = "Active function: y = N0 + A / (1 + exp(1 + (4 * MU * (L - x)) / A))",
    guess = "N0=1, A=7[0.001,], MU=0.5[0,], L=1.5[0,]",
    simboli = c(N0 = "N[0]", MU = "mu[max]", L = "lambda")
  ),
  
  # ---- Gompertz Family ----
  "gompertz" = list(
    group = "Gompertz Family", categoria = "Microbial Growth", label = "Gompertz Growth",
    formula = "A * exp(-B * exp(-C * x))",
    display = "Active function: y = A * exp(-B * exp(-C * x))",
    guess = "A=10[0,], B=2[0,], C=0.5[0,]"
  ),
  "gompertz_riparam" = list(
    group = "Gompertz Family", categoria = "Microbial Growth", label = "Reparameterized Gompertz",
    formula = "N0 + A * exp(-exp((MU * exp(1)) * (L - x) / A + 1))",
    display = "Active function: y = N0 + A * exp(-exp((MU * exp(1)) * (L - x) / A + 1))",
    guess = "N0=1, A=7[0.001,], MU=0.5[0,], L=1.5[0,]",
    simboli = c(N0 = "N[0]", MU = "mu[max]", L = "lambda")
  ),
  
  # ---- Advanced Growth Models ----
  "richards_originale" = list(
    group = "Advanced Growth Models", categoria = "Microbial Growth", label = "Richards Growth",
    formula = "a * (1 + NU * exp(k * (tau - x)))^(-1/NU)",
    display = "Active function: y = a * (1 + NU * exp(k * (tau - x)))^(-1/NU)",
    guess = "a=10[0,], NU=0.5[0.001,], k=0.5[0,], tau=2",
    simboli = c(NU = "nu")
  ),
  "richards_riparam" = list(
    group = "Advanced Growth Models", categoria = "Microbial Growth", label = "Reparameterized Richards",
    formula = "N0 + A * ((1 + (NU * exp(1 + NU)) * exp((MU / A) * ((1 + NU)^(1 + (1 / NU))) * (L - x)))^(-(1 / NU)))",
    display = "Active function: y = N0 + A * ((1 + (NU * exp(1 + NU)) * exp((MU / A) * ((1 + NU)^(1 + (1 / NU))) * (L - x)))^(-(1 / NU)))",
    guess = "N0=1, A=7[0.001,], NU=0.5[0.001,], MU=0.5[0,], L=1.5[0,]",
    simboli = c(N0 = "N[0]", MU = "mu[max]", NU = "nu", L = "lambda")
  ),
  "baranyi_integrato" = list(
    group = "Advanced Growth Models", categoria = "Microbial Growth", label = "Integrated Baranyi Model",
    # Original integrated form of Baranyi & Roberts (1994), not the
    # reparameterization below - verified against a reliable technical
    # source and corrected with respect to the formula originally
    # proposed (a piece was missing inside the logarithm of A(x), which
    # made it algebraically degenerate). N0 and Nmax are BOTH on the
    # LOG scale (consistent with how they are used in the Reparameterized Baranyi below),
    # which is why the ratio Nmax/N0 becomes exp(Nmax - N0) - numerically
    # verified to give results identical to the version with raw data.
    formula = "N0 + MU*(x + (1/MU)*log(exp(-MU*x) + exp(-h0) - exp(-MU*x-h0))) - log(1 + (exp(MU*(x + (1/MU)*log(exp(-MU*x) + exp(-h0) - exp(-MU*x-h0))))-1)/exp(Nmax-N0))",
    display = "Active function: y = N0 + MU*A(x) - Ln(1 + (exp(MU*A(x))-1)/exp(Nmax-N0)), with A(x) = x + (1/MU)*Ln(exp(-MU*x) + exp(-h0) - exp(-MU*x-h0))   [N0 and Nmax are in Log scale]",
    # Guesses with limits: MU>0 and h0>=0 are not just convenient but
    # mathematically NECESSARY - verified on 10000 random combinations
    # that with these constraints the expression inside the logarithm always stays
    # positive; without constraints, the optimizer (Nelder-Mead, unconstrained)
    # can explore negative values during the search, producing NaN
    # (and MU=0 would in any case give a division by zero in "1/MU"). The lower
    # limit of MU is 0.001 (not exactly 0) precisely for this reason.
    guess = "N0=1, MU=0.5[0.001,], h0=1[0,], Nmax=8",
    simboli = c(N0 = "N[0]", MU = "mu[max]", h0 = "h[0]", Nmax = "N[max]")
  ),
  "baranyi_riparam" = list(
    group = "Advanced Growth Models", categoria = "Microbial Growth", label = "Reparameterized Baranyi",
    formula = "N0 + A + log((-1 + exp(MU * L) + exp(MU * x)) / (-1 + exp(MU * x) + exp(MU * L + A)))",
    display = "Active function: y = N0 + A + log((-1 + exp(MU * L) + exp(MU * x)) / (-1 + exp(MU * x) + exp(MU * L + A)))",
    guess = "N0=1, A=7[0.001,], MU=0.8[0,], L=1.5[0,]",
    simboli = c(N0 = "N[0]", MU = "mu[max]", L = "lambda")
  ),
  "shifted_logistic" = list(
    group = "Advanced Growth Models", categoria = "Microbial Growth", label = "Shifted Logistic Growth",
    # Verified: at x=0 it equals exactly N0 (the two terms cancel out),
    # for x->infinity it tends to N0 + A*(1 - 1/(1+exp(k*tc))) - unlike
    # the classic logistic, this "shifted" version starts exactly
    # from N0 at time 0, not only asymptotically.
    formula = "N0 + A * (1/(1+exp(k*(tc-x))) - 1/(1+exp(k*tc)))",
    display = "Active function: y = N0 + A * (1/(1+exp(k*(tc-x))) - 1/(1+exp(k*tc)))",
    guess = "N0=1, A=7[0.001,], k=0.5[0,], tc=5",
    simboli = c(N0 = "N[0]", tc = "t[c]")
  ),
  
  # ---- Classical Kinetic Models ----
  "mm" = list(
    group = "Classical Kinetic Models", categoria = "Enzyme Kinetics", label = "Michaelis-Menten",
    formula = "(Vmax * x) / (Km + x)",
    display = "Active function: y = (Vmax * x) / (Km + x)",
    guess = "Vmax=10[0,], Km=2[0,]",
    simboli = c(Vmax = "V[max]", Km = "K[m]")
  ),
  
  # ---- Kinetic Reactions ----
  "arrhenius" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "Arrhenius Equation",
    # Classic form of the Arrhenius equation (reaction rate as a
    # function of the absolute temperature), not the "shelf-life" form
    # (storage time, which increases as the temperature DECREASES,
    # hence with the opposite sign in the exponent) used in the Shelf
    # Life module - the two forms serve different purposes and must not be confused.
    # R = gas constant, fixed, the same already used in the Shelf Life module.
    formula = "A * exp(-Ea / (8.314462618 * x))",
    display = "Active function: y = A * exp(-Ea / (R*T)), with R = 8.314462618 J/(mol*K)   [x = T, absolute temperature in Kelvin]",
    guess = "A=100000000[0,], Ea=50000[0,]",
    simboli = c(Ea = "E[a]")
  ),
  "zero_order" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "Zero-Order Kinetics",
    formula = "A0 - k * x",
    display = "Active function: y = A0 - k * x",
    guess = "A0=10[0,], k=0.5[0,]",
    simboli = c(A0 = "A[0]")
  ),
  "primo_ordine_ln" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "First-Order Kinetics (Linearized, ln y)",
    formula = "log(A0) - k * x",
    display = "Active function: y = log(A0) - k * x   [use with ln(y) data]",
    guess = "A0=10[0,], k=0.5[0,]",
    simboli = c(A0 = "A[0]")
  ),
  "primo_ordine_exp" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "First-Order Kinetics (Exponential Form)",
    formula = "A0 * exp(-k * x)",
    display = "Active function: y = A0 * exp(-k * x)",
    guess = "A0=10[0,], k=0.5[0,]",
    simboli = c(A0 = "A[0]")
  ),
  "secondo_ordine" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "Second-Order Kinetics",
    formula = "A0 / (1 + 2 * A0 * k * x)",
    display = "Active function: y = A0 / (1 + 2 * A0 * k * x)",
    guess = "A0=10[0,], k=0.1[0,]",
    simboli = c(A0 = "A[0]")
  ),
  "n_ordine" = list(
    group = "Kinetic Reactions", categoria = "Reaction Kinetics", label = "n-Order Kinetics",
    formula = "((A0^(1 - n)) + (n - 1) * k * x)^(1 / (1 - n))",
    display = "Active function: y = ((A0^(1 - n)) + (n - 1) * k * x)^(1 / (1 - n))",
    guess = "A0=10[0,], n=1.5[0,], k=0.1[0,]",
    simboli = c(A0 = "A[0]")
  ),
  
  # ---- Diffusion Reactions ----
  "peleg_pos_k" = list(
    group = "Diffusion Reactions", categoria = "Mass Transfer Kinetics", label = "Peleg Model (+)",
    formula = "C0 + (Ce - C0) * (x / (k + x))",
    display = "Active function: y = C0 + (Ce - C0) * (x / (k + x))",
    guess = "C0=1[0,], Ce=10[0,], k=2",
    simboli = c(C0 = "C[0]", Ce = "C[e]")
  ),
  "peleg_neg_k" = list(
    group = "Diffusion Reactions", categoria = "Mass Transfer Kinetics", label = "Peleg Model (-)",
    formula = "C0 - (Ce - C0) * (x / (k + x))",
    display = "Active function: y = C0 - (Ce - C0) * (x / (k + x))",
    guess = "C0=10[0,], Ce=1[0,], k=2",
    simboli = c(C0 = "C[0]", Ce = "C[e]")
  ),
  "becker_pos" = list(
    group = "Diffusion Reactions", categoria = "Mass Transfer Kinetics", label = "Becker Model (+)",
    formula = "C0 + (Ce - C0) * (M0 + k * sqrt(x))",
    display = "Active function: y = C0 + (Ce - C0) * (M0 + k * sqrt(x))",
    guess = "C0=1[0,], Ce=10[0,], M0=0.1[0,], k=0.5",
    simboli = c(C0 = "C[0]", Ce = "C[e]", M0 = "M[0]")
  ),
  "becker_neg" = list(
    group = "Diffusion Reactions", categoria = "Mass Transfer Kinetics", label = "Becker Model (-)",
    formula = "C0 - (Ce - C0) * (M0 + k * sqrt(x))",
    display = "Active function: y = C0 - (Ce - C0) * (M0 + k * sqrt(x))",
    guess = "C0=10[0,], Ce=1[0,], M0=0.1[0,], k=0.5",
    simboli = c(C0 = "C[0]", Ce = "C[e]", M0 = "M[0]")
  ),
  
  # ---- Rheology ----
  "power_law" = list(
    group = "Rheology", categoria = "Rheology & Texture", label = "Power Law Model",
    formula = "k * x^n",
    display = "Active function: y = k * x^n",
    guess = "k=1[0,], n=0.5[0,]"
  ),
  "herschel_bulkley" = list(
    group = "Rheology", categoria = "Rheology & Texture", label = "Herschel-Bulkley Model",
    formula = "S0 + k * x^n",
    display = "Active function: y = S0 + k * x^n",
    guess = "S0=1[0,], k=1[0,], n=0.7[0,]",
    simboli = c(S0 = "S[0]")
  ),
  "casson" = list(
    group = "Rheology", categoria = "Rheology & Texture", label = "Casson Model",
    formula = "(K0 + K * sqrt(x))^2",
    display = "Active function: y = (K0 + K * sqrt(x))^2",
    guess = "K0=1[0,], K=0.5[0,]",
    simboli = c(K0 = "K[0]")
  ),
  
  # ---- Theoretical Models ----
  "gab" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "GAB Isotherm Model",
    formula = "(m0 * k0 * c * x) / ((1 - k0 * x) * (1 + (c - 1) * k0 * x))",
    display = "Active function: y = (m0 * k0 * c * x) / ((1 - k0 * x) * (1 + (c - 1) * k0 * x))",
    guess = "m0=0.15[0,], k0=0.8[0,], c=15[0,]",
    simboli = c(m0 = "m[0]", k0 = "k[0]")
  ),
  "bet" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "BET Isotherm Model",
    formula = "(x * M0 * C) / ((1 - x) * (1 + (C - 1) * x))",
    display = "Active function: y = (x * M0 * C) / ((1 - x) * (1 + (C - 1) * x))",
    guess = "M0=0.1[0,], C=10[0,]",
    simboli = c(M0 = "M[0]")
  ),
  "langmuir_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Langmuir Isotherm",
    formula = "X0 * (C * x) / (1 + C * x)",
    display = "Active function: y = X0 * (C * x) / (1 + C * x)   [best suited for Type I isotherms]",
    guess = "X0=10[0,], C=5[0,]",
    simboli = c(X0 = "X[0]")
  ),
  "halsey_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Halsey Isotherm",
    formula = "(-A / log(x))^(1/B)",
    display = "Active function: y = (-A / Ln(x))^(1/B)   [best suited for Type II isotherms]",
    guess = "A=1[0,], B=1[0.001,]"
  ),
  "smith_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Smith Isotherm",
    formula = "A + (B * log(1 - x))",
    display = "Active function: y = A + (B * Ln(1 - x))   [best suited for Type II isotherms]",
    guess = "A=5, B=-3"
  ),
  "henderson_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Henderson Isotherm",
    formula = "((-log(1 - x)) / A)^(1/B)",
    display = "Active function: y = ((-Ln(1 - x)) / A)^(1/B)   [best suited for Type II and III isotherms]",
    guess = "A=1[0.001,], B=1[0.001,]"
  ),
  "oswin_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Oswin Isotherm",
    formula = "A * (x / (1 - x))^B",
    display = "Active function: y = A * (x / (1 - x))^B   [best suited for Type II and III isotherms]",
    guess = "A=10[0,], B=0.5[0,]"
  ),
  "ferro_fontan_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Ferro-Fontan Isotherm",
    formula = "(gamma / log(alpha / x))^(1/gamma)",
    display = "Active function: y = (gamma / Ln(alpha / x))^(1/gamma)",
    guess = "gamma=2[0.001,], alpha=2[0,]"
  ),
  "peleg_isoterma" = list(
    group = "Theoretical Models", categoria = "Water Activity & Sorption Isotherms", label = "Peleg Isotherm",
    # Different from Peleg's models for microbial inactivation (separate
    # category) - this is Peleg's flexible two-term model
    # (1993) for sorption isotherms, often used as a simpler
    # alternative to GAB for sigmoidal curves.
    formula = "K1 * x^n1 + K2 * x^n2",
    display = "Active function: y = K1 * x^n1 + K2 * x^n2",
    guess = "K1=10, n1=0.5, K2=5, n2=2",
    simboli = c(K1 = "K[1]", n1 = "n[1]", K2 = "K[2]", n2 = "n[2]")
  ),
  
  # ---- Microbial Inactivation ----
  # Note: some of these models are expressed in logarithmic form
  # (Log10) or as a survival ratio (y/N0) - they require the loaded
  # Y data to be already in the corresponding form (indicated in "display"),
  # the same convention already used for "First-Order Kinetics (Linearized, ln y)".
  "weibull_inact1" = list(
    group = "Weibull Family", categoria = "Microbial Inactivation", label = "Weibull Model (from 1 to 0)",
    # Identical to "Weibull Model (-, from 1 to 0)" in the Weibull category -
    # repeated here on purpose for the convenience of those working specifically on
    # microbial inactivation.
    formula = "exp(-((x / alpha)^beta))",
    display = "Active function: y = exp(-((x / alpha)^beta))",
    guess = "alpha=2[0.001,], beta=1.5[0.001,]"
  ),
  "weibull_inact2" = list(
    group = "Weibull Family", categoria = "Microbial Inactivation", label = "Weibull Model (from N0 to 0)",
    formula = "N0 * exp(-((x / alpha)^beta))",
    display = "Active function: y = N0 * exp(-((x / alpha)^beta))",
    guess = "N0=10[0,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(N0 = "N[0]")
  ),
  "weibull_inact_log" = list(
    group = "Weibull Family", categoria = "Microbial Inactivation", label = "Weibull Model, Log Form",
    # Algebraically verified: it is the exact log10 of the previous model
    # (N0 * exp(-((x/alpha)^beta))) - log10(e) = 1/ln(10) = 1/2.303.
    formula = "log10(N0) - (1/2.303) * ((x / alpha)^beta)",
    display = "Active function: y = Log10(N0) - (1/2.303) * ((x / alpha)^beta)   [use with Log10(N) data]",
    guess = "N0=10[0.001,], alpha=2[0.001,], beta=1.5[0.001,]",
    simboli = c(N0 = "N[0]")
  ),
  "weibull_inact_log_ratio" = list(
    group = "Weibull Family", categoria = "Microbial Inactivation", label = "Weibull Model, Log Survival Ratio",
    # Derived by subtracting log10(N0) from the previous model (Log Form) -
    # algebraically verified to coincide exactly.
    formula = "-(1/2.303) * ((x / alpha)^beta)",
    display = "Active function: y = -(1/2.303) * ((x / alpha)^beta)   [use with Log10(N/N0) data]",
    guess = "alpha=2[0.001,], beta=1.5[0.001,]"
  ),
  "weibullian" = list(
    group = "Weibullian Inactivation", categoria = "Microbial Inactivation", label = "Weibullian Model (Log N)",
    # The "power law"/Weibullian model of Peleg & Cole (1998), among the most
    # cited in predictive microbiology for non log-linear survival
    # curves - confirmed in the literature.
    formula = "log10(N0) - b * x^n",
    display = "Active function: y = Log10(N0) - b * x^n   [use with Log10(N) data]",
    guess = "N0=10[0.001,], b=0.5[0.001,], n=1[0.001,]",
    simboli = c(N0 = "N[0]")
  ),
  "weibullian_log_ratio" = list(
    group = "Weibullian Inactivation", categoria = "Microbial Inactivation", label = "Weibullian Model, Log Survival Ratio",
    formula = "-b * x^n",
    display = "Active function: y = -b * x^n   [use with Log10(N/N0) data]",
    guess = "b=0.5[0.001,], n=1[0.001,]"
  ),
  "spore_survival_ratio" = list(
    group = "Nonlinear Spore Survival", categoria = "Microbial Inactivation", label = "Nonlinear Spore Survival, Log Ratio",
    formula = "b1 * x^n1 - b2 * x^n2",
    display = "Active function: y = b1 * x^n1 - b2 * x^n2   [use with Log10(N/N0) data]",
    guess = "b1=0.015[0.001,], n1=1.8[0.001,], b2=0.008[0.001,], n2=3[0.001,]",
    simboli = c(b1 = "b[1]", n1 = "n[1]", b2 = "b[2]", n2 = "n[2]")
  ),
  "peleg_survival_ratio" = list(
    group = "Peleg Survival Model", categoria = "Microbial Inactivation", label = "Peleg Survival Model, Log Ratio",
    # Interpretation [Ln(1+exp(b3*x))]^n3 (not Ln[(1+exp(b3*x))^n3]) -
    # original source (van Boekel) with a missing parenthesis, this is the
    # most plausible reading (if the intent had been n3*Ln(...), it would
    # have been written that way directly, with no need for the exponent outside
    # the logarithm). To be corrected if the exact source is found.
    formula = "(x * (1 - log(1 + exp(b3 * x))^n3)) / (k1 + k2 * x)",
    display = "Active function: y = (x * (1 - Ln(1 + exp(b3 * x))^n3)) / (k1 + k2 * x)   [use with Log10(N/N0) data]",
    guess = "b3=0.5, n3=1, k1=1, k2=0.1",
    simboli = c(b3 = "b[3]", n3 = "n[3]", k1 = "k[1]", k2 = "k[2]")
  ),
  
  # ---- Bigelow Model ----
  # The classic log-linear model for thermal inactivation, in
  # terms of the D-value (decimal reduction time): a straight line on the
  # Log10 scale, slope -1/D.
  "bigelow_ratio" = list(
    group = "Bigelow Model", categoria = "Microbial Inactivation", label = "Bigelow Model, Log Survival Ratio",
    formula = "-x/D",
    display = "Active function: y = -x/D   [use with Log10(N/N0) data]",
    guess = "D=5[0.001,]"
  ),
  "bigelow_n0" = list(
    group = "Bigelow Model", categoria = "Microbial Inactivation", label = "Bigelow Model (Log N, with N0)",
    formula = "N0 - x/D",
    display = "Active function: y = N0 - x/D   [use with Log10(N) data]",
    guess = "N0=6, D=5[0.001,]",
    simboli = c(N0 = "N[0]")
  ),
  
  # ---- Fermi Inactivation Model ----
  # Sigmoidal curve (shoulder then steep decline around t_c), often
  # used to capture an initial shoulder followed by rapid
  # inactivation around the critical time t_c.
  "fermi_n" = list(
    group = "Fermi Inactivation Model", categoria = "Microbial Inactivation", label = "Fermi Model (raw N)",
    formula = "N0/(1+exp(k*(x-t_c)))",
    display = "Active function: y = N0 / (1 + exp(k*(x - t_c)))   [use with raw N data, not log-transformed]",
    guess = "N0=1000000[0,], k=1[0,], t_c=5",
    simboli = c(N0 = "N[0]", t_c = "t[c]")
  ),
  "fermi_ln_ratio" = list(
    group = "Fermi Inactivation Model", categoria = "Microbial Inactivation", label = "Fermi Model, Log Survival Ratio (Ln)",
    formula = "-log(1+exp(k*(x-t_c)))",
    display = "Active function: y = -Ln(1 + exp(k*(x - t_c)))   [use with Ln(N/N0) data, natural log \u2014 not Log10]",
    guess = "k=1[0,], t_c=5",
    simboli = c(t_c = "t[c]")
  ),
  
  # ---- Type A Inactivation Models ----
  # Curves with a vertical asymptote (the denominator vanishes when x
  # approaches the parameter a3/a4) - designed to capture a tail that
  # tends to a very rapid inactivation beyond a certain time. The initial
  # guess of a3/a4 must stay above the maximum value of x in the
  # data, otherwise the denominator vanishes within the observed range.
  "type_a_1" = list(
    group = "Type A Inactivation Models", categoria = "Microbial Inactivation", label = "Type A Model 1, Log Survival Ratio",
    formula = "-(a1*x)/((1+a2*x)*(a3-x))",
    display = "Active function: y = -(a1*x) / ((1+a2*x)*(a3-x))   [use with Log10(N/N0) data]",
    guess = "a1=0.5, a2=0.1, a3=20",
    simboli = c(a1 = "a[1]", a2 = "a[2]", a3 = "a[3]")
  ),
  "type_a_2" = list(
    group = "Type A Inactivation Models", categoria = "Microbial Inactivation", label = "Type A Model 2, Log Survival Ratio",
    formula = "-((a1*x)/(a2+x))-((a3*x)/(a4-x))",
    display = "Active function: y = -((a1*x)/(a2+x)) - ((a3*x)/(a4-x))   [use with Log10(N/N0) data]",
    guess = "a1=0.5, a2=1, a3=0.5, a4=15",
    simboli = c(a1 = "a[1]", a2 = "a[2]", a3 = "a[3]", a4 = "a[4]")
  ),
  
  # ---- Type B Inactivation Models ----
  "type_b_1" = list(
    group = "Type B Inactivation Models", categoria = "Microbial Inactivation", label = "Type B Model 1, Log Survival Ratio",
    formula = "-(a1*x^m)/(a2+x^m)",
    display = "Active function: y = -(a1*x^m) / (a2+x^m)   [use with Log10(N/N0) data]",
    guess = "a1=1, a2=1, m=1",
    simboli = c(a1 = "a[1]", a2 = "a[2]")
  ),
  "type_b_2" = list(
    group = "Type B Inactivation Models", categoria = "Microbial Inactivation", label = "Type B Model 2, Log Survival Ratio",
    formula = "-(a1*x^m1)*(1-exp(a2*x^m2))",
    display = "Active function: y = -(a1*x^m1) * (1-exp(a2*x^m2))   [use with Log10(N/N0) data]",
    guess = "a1=0.5, m1=1, a2=-0.1, m2=1",
    simboli = c(a1 = "a[1]", m1 = "m[1]", a2 = "a[2]", m2 = "m[2]")
  ),
  
  # ---- Strong Tailing Model ----
  "strong_tailing" = list(
    group = "Strong Tailing Model", categoria = "Microbial Inactivation", label = "Strong Tailing Model, Log Survival Ratio",
    formula = "-(a*x)/(b+x)",
    display = "Active function: y = -(a*x) / (b+x)   [use with Log10(N/N0) data]",
    guess = "a=2, b=5"
  ),
  
  # ---- Product-of-Exponentials Model ----
  # Explicitly confirmed as a product (not a sum) even though,
  # algebraically, the formula reduces to a single exponential with a
  # constant coefficient a*(1-a) in front (verified with sympy:
  # a*(1-a)*exp(k1+k2-2x)) - the three parameters are therefore not all
  # separately identifiable from the fit (infinitely many combinations of
  # a/k1/k2 give the same curve), only the product a*(1-a) and the sum
  # k1+k2 are. Scale not explicitly specified: assumed to be the
  # raw survival fraction N/N0 (not log-transformed), since
  # the exponential form naturally decays towards zero - to be confirmed.
  "prodotto_esponenziali" = list(
    group = "Product-of-Exponentials Model", categoria = "Microbial Inactivation", label = "Biphasic Model (Sum of Exponentials)",
    # HISTORICAL NOTE: originally a true "product" of exponentials
    # (a*exp(k1-x))*(1-a)*exp(k2-x) - but exp(A)*exp(B) = exp(A+B) for
    # any value of A, B (basic property of exponentials),
    # so that formula ALWAYS collapsed algebraically to a single
    # exponential with a fixed rate of -2 (a(1-a)*exp(k1+k2-2x)) - only one
    # real degree of freedom out of 3 nominal parameters, verified with
    # sympy. Replaced with a SUM of exponentials (Cerf, 1977 and
    # literature on biphasic models of microbial inactivation): it does not
    # collapse algebraically (verified with sympy), it represents two
    # subpopulations with different resistance (one sensitive at rate k1,
    # one resistant/tail at rate k2), a true model with 3 free
    # parameters provided that k1 != k2.
    formula = "a*exp(-k1*x) + (1-a)*exp(-k2*x)",
    display = "Active function: y = a*exp(-k1*x) + (1-a)*exp(-k2*x)   [use with raw N/N0 survival fraction data, not log-transformed]",
    guess = "a=0.5[0,1], k1=1[0,], k2=0.1[0,]",
    simboli = c(k1 = "k[1]", k2 = "k[2]")
  ),
  
  # ---- Statistical Distributions ----
  # Useful for example to fit PSD (Particle Size Distribution) or other
  # density/probability histograms. All numerically verified:
  # every PDF integrates to 1, every PMF sums to 1 over its own domain.
  "gaussiana_dist" = list(
    group = "Statistical Distributions", categoria = "Statistical Distributions", label = "Gaussian (Normal) Distribution",
    formula = "(1 / (sigma * sqrt(2 * pi))) * exp(-((x - mu)^2) / (2 * sigma^2))",
    display = "Active function: y = (1 / (sigma * sqrt(2*pi))) * exp(-((x - mu)^2) / (2*sigma^2))",
    guess = "mu=5, sigma=1.5[0.001,]"
  ),
  "gamma_dist" = list(
    group = "Statistical Distributions", categoria = "Statistical Distributions", label = "Gamma Distribution",
    # gamma(k) here is the mathematical Gamma function (generalization of the
    # factorial), not the name of the shape parameter - available in R
    # as a base function, no additional package needed.
    formula = "(1 / (gamma(k) * theta^k)) * x^(k - 1) * exp(-x / theta)",
    display = "Active function: y = (1 / (Gamma(k) * theta^k)) * x^(k-1) * exp(-x/theta)",
    guess = "k=2[0.001,], theta=2[0.001,]"
  ),
  "beta_dist" = list(
    group = "Statistical Distributions", categoria = "Statistical Distributions", label = "Beta Distribution",
    # Domain x in (0,1) - beta(a,b) is the mathematical Beta function,
    # available in R as a base function.
    formula = "(x^(a - 1) * (1 - x)^(b - 1)) / beta(a, b)",
    display = "Active function: y = (x^(a-1) * (1-x)^(b-1)) / Beta(a,b)   [domain: 0 < x < 1]",
    guess = "a=2[0.001,], b=5[0.001,]"
  ),
  "poisson_dist" = list(
    group = "Statistical Distributions", categoria = "Statistical Distributions", label = "Poisson Distribution",
    # Same note as for the Binomial: discrete distribution, x = number of
    # events (typically an integer).
    formula = "(lambda^x * exp(-lambda)) / factorial(x)",
    display = "Active function: y = (lambda^x * exp(-lambda)) / x!   [discrete distribution: x should be integer counts]",
    guess = "lambda=3[0.001,]"
  ),
  # ---- Models for Reference Data (NIST StRD and Bates & Watts) ----
  # One model for each reference dataset, named after the dataset: NIST
  # certified formulas (NIST Start 1 initial values) and formulas of the book by Bates &
  # Watts verified against the text (initial values derived from the data with the graphical
  # methods of the book, rounded to 2 digits). No automatic link with
  # the datasets: same name, manual pairing.
  "ref_nist_misra1a" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Misra1a",
    formula = "b1*(1-exp(-b2*x))",
    display = "NIST StRD Misra1a, certified model: y = b1*(1-exp(-b2*x)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=250, b2=0.0005.",
    guess = "b1=500, b2=0.0001"
  ),
  "ref_nist_chwirut2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Chwirut2",
    formula = "exp(-b1*x)/(b2+b3*x)",
    display = "NIST StRD Chwirut2, certified model: y = exp(-b1*x)/(b2+b3*x). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.15, b2=0.008, b3=0.01.",
    guess = "b1=0.1, b2=0.01, b3=0.02"
  ),
  "ref_nist_chwirut1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Chwirut1",
    formula = "exp(-b1*x)/(b2+b3*x)",
    display = "NIST StRD Chwirut1, certified model: y = exp(-b1*x)/(b2+b3*x). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.15, b2=0.008, b3=0.01.",
    guess = "b1=0.1, b2=0.01, b3=0.02"
  ),
  "ref_nist_lanczos3" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Lanczos3",
    formula = "b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x)",
    display = "NIST StRD Lanczos3, certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.5, b2=0.7, b3=3.6, b4=4.2, b5=4, b6=6.3.",
    guess = "b1=1.2, b2=0.3, b3=5.6, b4=5.5, b5=6.5, b6=7.6"
  ),
  "ref_nist_gauss1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Gauss1",
    formula = "b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2)",
    display = "NIST StRD Gauss1, certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=94, b2=0.0105, b3=99, b4=63, b5=25, b6=71, b7=180, b8=20.",
    guess = "b1=97, b2=0.009, b3=100, b4=65, b5=20, b6=70, b7=178, b8=16.5"
  ),
  "ref_nist_gauss2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Gauss2",
    formula = "b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2)",
    display = "NIST StRD Gauss2, certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=98, b2=0.0105, b3=103, b4=105, b5=20, b6=73, b7=150, b8=20.",
    guess = "b1=96, b2=0.009, b3=103, b4=106, b5=18, b6=72, b7=151, b8=18"
  ),
  "ref_nist_danwood" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — DanWood",
    formula = "b1*x^b2",
    display = "NIST StRD DanWood, certified model: y = b1*x^b2. Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.7, b2=4.",
    guess = "b1=1, b2=5"
  ),
  "ref_nist_misra1b" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Misra1b",
    formula = "b1*(1-(1+b2*x/2)^(-2))",
    display = "NIST StRD Misra1b, certified model: y = b1*(1-(1+b2*x/2)^(-2)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=300, b2=0.0002.",
    guess = "b1=500, b2=0.0001"
  ),
  "ref_nist_kirby2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Kirby2",
    formula = "(b1 + b2*x + b3*x^2)/(1 + b4*x + b5*x^2)",
    display = "NIST StRD Kirby2, certified model: y = (b1 + b2*x + b3*x^2)/(1 + b4*x + b5*x^2). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=1.5, b2=-0.15, b3=0.0025, b4=-0.0015, b5=2e-05.",
    guess = "b1=2, b2=-0.1, b3=0.003, b4=-0.001, b5=1e-05"
  ),
  "ref_nist_hahn1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Hahn1",
    formula = "(b1+b2*x+b3*x^2+b4*x^3)/(1+b5*x+b6*x^2+b7*x^3)",
    display = "NIST StRD Hahn1, certified model: y = (b1+b2*x+b3*x^2+b4*x^3)/(1+b5*x+b6*x^2+b7*x^3). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=1, b2=-0.1, b3=0.005, b4=-1e-06, b5=-0.005, b6=0.0001, b7=-1e-07.",
    guess = "b1=10, b2=-1, b3=0.05, b4=-1e-05, b5=-0.05, b6=0.001, b7=-1e-06"
  ),
  "ref_nist_mgh17" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — MGH17",
    formula = "b1 + b2*exp(-x*b4) + b3*exp(-x*b5)",
    display = "NIST StRD MGH17, certified model: y = b1 + b2*exp(-x*b4) + b3*exp(-x*b5). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.5, b2=1.5, b3=-1, b4=0.01, b5=0.02.",
    guess = "b1=50, b2=150, b3=-100, b4=1, b5=2"
  ),
  "ref_nist_lanczos1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Lanczos1",
    formula = "b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x)",
    display = "NIST StRD Lanczos1, certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.5, b2=0.7, b3=3.6, b4=4.2, b5=4, b6=6.3.",
    guess = "b1=1.2, b2=0.3, b3=5.6, b4=5.5, b5=6.5, b6=7.6"
  ),
  "ref_nist_lanczos2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Lanczos2",
    formula = "b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x)",
    display = "NIST StRD Lanczos2, certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.5, b2=0.7, b3=3.6, b4=4.2, b5=4, b6=6.3.",
    guess = "b1=1.2, b2=0.3, b3=5.6, b4=5.5, b5=6.5, b6=7.6"
  ),
  "ref_nist_gauss3" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Gauss3",
    formula = "b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2)",
    display = "NIST StRD Gauss3, certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=96, b2=0.0096, b3=80, b4=110, b5=25, b6=74, b7=139, b8=25.",
    guess = "b1=94.9, b2=0.009, b3=90.1, b4=113, b5=20, b6=73.8, b7=140, b8=20"
  ),
  "ref_nist_misra1c" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Misra1c",
    formula = "b1*(1-(1+2*b2*x)^(-0.5))",
    display = "NIST StRD Misra1c, certified model: y = b1*(1-(1+2*b2*x)^(-0.5)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=600, b2=0.0002.",
    guess = "b1=500, b2=0.0001"
  ),
  "ref_nist_misra1d" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Misra1d",
    formula = "b1*b2*x*((1+b2*x)^(-1))",
    display = "NIST StRD Misra1d, certified model: y = b1*b2*x*((1+b2*x)^(-1)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=450, b2=0.0003.",
    guess = "b1=500, b2=0.0001"
  ),
  "ref_nist_roszman1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Roszman1",
    formula = "b1 - b2*x - atan(b3/(x-b4))/pi",
    display = "NIST StRD Roszman1, certified model: y = b1 - b2*x - atan(b3/(x-b4))/pi. Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.2, b2=-5e-06, b3=1200, b4=-150.",
    guess = "b1=0.1, b2=-1e-05, b3=1000, b4=-100"
  ),
  "ref_nist_enso" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — ENSO",
    formula = "b1 + b2*cos(2*pi*x/12) + b3*sin(2*pi*x/12) + b5*cos(2*pi*x/b4) + b6*sin(2*pi*x/b4) + b8*cos(2*pi*x/b7) + b9*sin(2*pi*x/b7)",
    display = "NIST StRD ENSO, certified model: y = b1 + b2*cos(2*pi*x/12) + b3*sin(2*pi*x/12) + b5*cos(2*pi*x/b4) + b6*sin(2*pi*x/b4) + b8*cos(2*pi*x/b7) + b9*sin(2*pi*x/b7). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=10, b2=3, b3=0.5, b4=44, b5=-1.5, b6=0.5, b7=26, b8=-0.1, b9=1.5.",
    guess = "b1=11, b2=3, b3=0.5, b4=40, b5=-0.7, b6=-1.3, b7=25, b8=-0.3, b9=1.4"
  ),
  "ref_nist_mgh09" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — MGH09",
    formula = "b1*(x^2+x*b2)/(x^2+x*b3+b4)",
    display = "NIST StRD MGH09, certified model: y = b1*(x^2+x*b2)/(x^2+x*b3+b4). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.25, b2=0.39, b3=0.415, b4=0.39.",
    guess = "b1=25, b2=39, b3=41.5, b4=39"
  ),
  "ref_nist_thurber" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Thurber",
    formula = "(b1+b2*x+b3*x^2+b4*x^3)/(1+b5*x+b6*x^2+b7*x^3)",
    display = "NIST StRD Thurber, certified model: y = (b1+b2*x+b3*x^2+b4*x^3)/(1+b5*x+b6*x^2+b7*x^3). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=1300, b2=1500, b3=500, b4=75, b5=1, b6=0.4, b7=0.05.",
    guess = "b1=1000, b2=1000, b3=400, b4=40, b5=0.7, b6=0.3, b7=0.03"
  ),
  "ref_nist_boxbod" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — BoxBOD",
    formula = "b1*(1-exp(-b2*x))",
    display = "NIST StRD BoxBOD, certified model: y = b1*(1-exp(-b2*x)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=100, b2=0.75.",
    guess = "b1=1, b2=1"
  ),
  "ref_nist_rat42" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Rat42",
    formula = "b1/(1+exp(b2-b3*x))",
    display = "NIST StRD Rat42, certified model: y = b1/(1+exp(b2-b3*x)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=75, b2=2.5, b3=0.07.",
    guess = "b1=100, b2=1, b3=0.1"
  ),
  "ref_nist_mgh10" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — MGH10",
    formula = "b1*exp(b2/(x+b3))",
    display = "NIST StRD MGH10, certified model: y = b1*exp(b2/(x+b3)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=0.02, b2=4000, b3=250.",
    guess = "b1=2, b2=400000, b3=25000"
  ),
  "ref_nist_eckerle4" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Eckerle4",
    formula = "(b1/b2)*exp(-0.5*((x-b3)/b2)^2)",
    display = "NIST StRD Eckerle4, certified model: y = (b1/b2)*exp(-0.5*((x-b3)/b2)^2). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=1.5, b2=5, b3=450.",
    guess = "b1=1, b2=10, b3=500"
  ),
  "ref_nist_rat43" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Rat43",
    formula = "b1/((1+exp(b2-b3*x))^(1/b4))",
    display = "NIST StRD Rat43, certified model: y = b1/((1+exp(b2-b3*x))^(1/b4)). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=700, b2=5, b3=0.75, b4=1.3.",
    guess = "b1=100, b2=10, b3=1, b4=1"
  ),
  "ref_nist_bennett5" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "NIST — Bennett5",
    formula = "b1*(b2+x)^(-1/b3)",
    display = "NIST StRD Bennett5, certified model: y = b1*(b2+x)^(-1/b3). Initial values: NIST Start 1 (the official, more difficult starting point); NIST Start 2: b1=-1500, b2=45, b3=0.85.",
    guess = "b1=-2000, b2=50, b3=0.8"
  ),
  "ref_bw_puromycin_treated" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Puromycin, treated cells",
    formula = "theta1*x/(theta2+x)",
    display = "Bates & Watts (1988) model for the Puromycin, treated cells data set: y = theta1*x/(theta2+x). Initial values derived from the data with the graphical methods described in the book (theta1 = maximum observed y, theta2 = x at half of that maximum); rounded to two significant digits.",
    guess = "theta1=210, theta2=0.06"
  ),
  "ref_bw_puromycin_untreated" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Puromycin, untreated cells",
    formula = "theta1*x/(theta2+x)",
    display = "Bates & Watts (1988) model for the Puromycin, untreated cells data set: y = theta1*x/(theta2+x). Initial values derived from the data with the graphical methods described in the book (theta1 = maximum observed y, theta2 = x at half of that maximum); rounded to two significant digits.",
    guess = "theta1=160, theta2=0.051"
  ),
  "ref_bw_bod" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — BOD",
    formula = "theta1*(1-exp(-theta2*x))",
    display = "Bates & Watts (1988) model for the BOD data set: y = theta1*(1-exp(-theta2*x)). Initial values derived from the data with the graphical methods described in the book (theta1 = maximum observed y, theta2 = ln(2) / time to reach half of it); rounded to two significant digits.",
    guess = "theta1=20, theta2=0.39"
  ),
  "ref_bw_bod2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — BOD2",
    formula = "theta1*(1-exp(-theta2*x))",
    display = "Bates & Watts (1988) model for the BOD2 data set: y = theta1*(1-exp(-theta2*x)). Initial values derived from the data with the graphical methods described in the book (theta1 = maximum observed y, theta2 = ln(2) / time to reach half of it); rounded to two significant digits.",
    guess = "theta1=2.2, theta2=0.25"
  ),
  "ref_bw_chloride" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Chloride ion transport",
    formula = "theta1*(1-theta2*exp(-theta3*x))",
    display = "Bates & Watts (1988) model for the Chloride ion transport data set: y = theta1*(1-theta2*exp(-theta3*x)). Initial values derived from the data with the graphical methods described in the book (theta1 = maximum observed y, theta2 = 1 - minimum/maximum, theta3 = ln(2) / time to go halfway); rounded to two significant digits.",
    guess = "theta1=29, theta2=0.41, theta3=0.32"
  ),
  "ref_bw_leaves" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Growth of leaves",
    formula = "theta1/(1+theta2*exp(-theta3*x))^(1/theta4)",
    display = "Bates & Watts (1988) model for the Growth of leaves data set: y = theta1/(1+theta2*exp(-theta3*x))^(1/theta4). Initial values derived from the data with the graphical methods described in the book (theta4 = 1 (logistic start), theta1 = maximum observed y, theta3 from the time between 25% and 75% of it, theta2 from the inflection point); rounded to two significant digits.",
    guess = "theta1=21, theta2=120, theta3=0.74, theta4=1"
  ),
  "ref_bw_rumford" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Count Rumford's cooling experiment",
    formula = "60 + 70*exp(-theta*x)",
    display = "Bates & Watts (1988) model for the Count Rumford's cooling experiment data set: y = 60 + 70*exp(-theta*x). Initial values derived from the data with the graphical methods described in the book (theta from the linearization ln((y-60)/70) versus x); rounded to two significant digits.",
    guess = "theta=0.0092"
  ),
  "ref_bw_sulfi" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Pharmacokinetics of sulfisoxazole",
    formula = "theta1*exp(-theta2*x) + theta3*exp(-theta4*x)",
    display = "Bates & Watts (1988) model for the Pharmacokinetics of sulfisoxazole data set: y = theta1*exp(-theta2*x) + theta3*exp(-theta4*x). Initial values derived from the data with the graphical methods described in the book (curve peeling: slow phase from the last four points, fast phase from the early residuals); rounded to two significant digits.",
    guess = "theta1=90, theta2=1, theta3=150, theta4=0.15"
  ),
  "ref_bw_tetra" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Pharmacokinetics of tetracycline",
    formula = "theta3*(exp(-theta1*(x-theta4)) - exp(-theta2*(x-theta4)))",
    display = "Bates & Watts (1988) model for the Pharmacokinetics of tetracycline data set: y = theta3*(exp(-theta1*(x-theta4)) - exp(-theta2*(x-theta4))). Initial values derived from the data with the graphical methods described in the book (curve peeling (terminal and absorption phases), theta4 = 0); rounded to two significant digits.",
    guess = "theta1=0.12, theta2=1.6, theta3=2.1, theta4=0"
  ),
  "ref_bw_saccharin" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Pharmacokinetics of saccharin",
    formula = "(theta3/theta1)*exp(-theta1*x)*(1-exp(-theta1*((x + 20 - abs(x - 10))/2))) + (theta4/theta2)*exp(-theta2*x)*(1-exp(-theta2*((x + 20 - abs(x - 10))/2)))",
    display = "Bates & Watts (1988) model for the Pharmacokinetics of saccharin data set: y = (theta3/theta1)*exp(-theta1*x)*(1-exp(-theta1*((x + 20 - abs(x - 10))/2))) + (theta4/theta2)*exp(-theta2*x)*(1-exp(-theta2*((x + 20 - abs(x - 10))/2))). Initial values derived from the data with the graphical methods described in the book (curve peeling on the collection rates; the interval length dx is written as (x + 20 - abs(x - 10))/2, valid for this collection schedule (5, 10, then 15 min)); rounded to two significant digits.",
    guess = "theta1=0.024, theta2=0.096, theta3=280, theta4=1400"
  ),
  "ref_bw_pcb" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — PCB in Lake Cayuga trout",
    formula = "beta1 + beta2*x^(1/3)",
    display = "Bates & Watts (1988) model for the PCB in Lake Cayuga trout data set: y = beta1 + beta2*x^(1/3). Initial values derived from the data with the graphical methods described in the book (any starting point works for this linear model. IMPORTANT: use it with the Y pre-transformation ln(y), as in the book; without it the estimates do not match the book); rounded to two significant digits.",
    guess = "beta1=0, beta2=1"
  ),
  "ref_bw_nitrite_day1_m1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Nitrite utilization, day 1 (model 1)",
    formula = "theta1*x/(theta2+x+theta3*x^2)",
    display = "Bates & Watts (1988) model for the Nitrite utilization, day 1 data set: y = theta1*x/(theta2+x+theta3*x^2). Initial values derived from the data with the graphical methods described in the book (from the initial slope and the position and height of the maximum); rounded to two significant digits.",
    guess = "theta1=260000, theta2=590, theta3=0.066"
  ),
  "ref_bw_nitrite_day1_m2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Nitrite utilization, day 1 (model 2)",
    formula = "theta1*(exp(-theta3*x)-exp(-theta2*x))",
    display = "Bates & Watts (1988) model for the Nitrite utilization, day 1 data set: y = theta1*(exp(-theta3*x)-exp(-theta2*x)). Initial values derived from the data with the graphical methods described in the book (from the initial slope, the position of the maximum and the final slope); rounded to two significant digits.",
    guess = "theta1=6400, theta2=0.07, theta3=0.0001"
  ),
  "ref_bw_nitrite_day2_m1" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Nitrite utilization, day 2 (model 1)",
    formula = "theta1*x/(theta2+x+theta3*x^2)",
    display = "Bates & Watts (1988) model for the Nitrite utilization, day 2 data set: y = theta1*x/(theta2+x+theta3*x^2). Initial values derived from the data with the graphical methods described in the book (from the initial slope and the position and height of the maximum); rounded to two significant digits.",
    guess = "theta1=120000, theta2=260, theta3=0.029"
  ),
  "ref_bw_nitrite_day2_m2" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Bates & Watts — Nitrite utilization, day 2 (model 2)",
    formula = "theta1*(exp(-theta3*x)-exp(-theta2*x))",
    display = "Bates & Watts (1988) model for the Nitrite utilization, day 2 data set: y = theta1*(exp(-theta3*x)-exp(-theta2*x)). Initial values derived from the data with the graphical methods described in the book (from the initial slope, the position of the maximum and the final slope); rounded to two significant digits.",
    guess = "theta1=6400, theta2=0.07, theta3=0.0001"
  ),
  "ref_cr_esterase" = list(
    group = "Reference Data Models", categoria = "Models for Reference Data", label = "Carroll & Ruppert \u2014 Esterase assay",
    formula = "b0 + b1*x",
    display = "Carroll & Ruppert (1988) model for the Esterase assay data set: y = b0 + b1*x. The book fits it with the power-of-the-mean variance model with constant coefficient of variation: use weighting w = 1/y^2 (zeta = 2) in section 5. Initial values from the data.",
    guess = "b0=-20, b1=17"
  )
)

# Automatically builds the grouped list (selectInput "choices" format with
# optgroups) from the registry, preserving the order and groups defined
# above. Used by ui.R: no more separate manual copy of the dropdown menu.
costruisci_scelte_modello <- function() {
  gruppi <- list()
  ordine_gruppi <- character(0)
  for (id in names(MODEL_REGISTRY)) {
    g <- MODEL_REGISTRY[[id]]$group
    if (is.null(gruppi[[g]])) {
      gruppi[[g]] <- c()
      ordine_gruppi <- c(ordine_gruppi, g)
    }
    valore <- id
    names(valore) <- MODEL_REGISTRY[[id]]$label
    gruppi[[g]] <- c(gruppi[[g]], valore)
  }
  gruppi[ordine_gruppi]
}

# Same idea as costruisci_scelte_modello(), but grouped by
# food application (field "categoria") instead of by mathematical family
# (field "group") - used by the model selection popup panel. Each
# element of the returned list is a data.frame with id/label/display,
# one element for each category, in the order in which they appear in the
# registry.
costruisci_categorie_modello <- function() {
  gruppi <- list()
  ordine_gruppi <- character(0)
  for (id in names(MODEL_REGISTRY)) {
    cat <- MODEL_REGISTRY[[id]]$categoria
    if (is.null(cat)) cat <- "General"
    if (is.null(gruppi[[cat]])) {
      gruppi[[cat]] <- data.frame(id = character(0), label = character(0), display = character(0), stringsAsFactors = FALSE)
      ordine_gruppi <- c(ordine_gruppi, cat)
    }
    gruppi[[cat]] <- rbind(gruppi[[cat]], data.frame(
      id = id, label = MODEL_REGISTRY[[id]]$label, display = MODEL_REGISTRY[[id]]$display, stringsAsFactors = FALSE
    ))
  }
  gruppi[ordine_gruppi]
}

# ==============================================================================
# REFERENCE / BIBLIOGRAPHIC DATASETS (NIST StRD and others)
# ==============================================================================
# Datasets with certified or published values, usable to validate the app
# by comparing the estimated parameters with a known, independent reference.
# To add a new dataset: just add a new entry with the
# same schema (id, nome, fonte, x, y, nota) - no
# other change to the code is needed, the rest of the app reads it from here.
DATASET_RIFERIMENTO <- list(
  "combase_salmonella_b092_25" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_25, 10\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_25. Growth curve, Salmonella spp., 10\u00b0C, 20 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 50.88, 73.02, 103.96, 123.9, 145.01, 193.87, 218.88, 243.9, 313.86, 337.02, 362.88, 385.02, 408, 435.9, 460.92, 480, 504, 528, 554),
    y = c(2.67, 2.91, 2.87, 3.69, 3.98, 4.25, 4.89, 5.26, 5.47, 6.48, 6.91, 7.12, 7.58, 8.13, 8.44, 8.39, 8.57, 8.42, 8.57, 8.49),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 10\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.597\u00b10.144, lag=48.0058\u00b115.139 h, mu_max=0.0153\u00b10.000586 log CFU/h, MPD=8.658\u00b10.104 log CFU/g (R\u00b2=0.994, SE of fit=0.166)."
  ),
  "combase_salmonella_b092_45" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_45, 10\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_45. Growth curve, Salmonella spp., 10\u00b0C, 21 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 72.85, 192.85, 217.87, 240.85, 242.88, 246.95, 310.99, 317.93, 334.98, 341.93, 357.97, 384, 406.98, 413.94, 478.99, 502.98, 533.94, 552, 552.85, 576),
    y = c(3.53, 3.46, 5.15, 5.39, 5.63, 5.45, 5.61, 6.45, 6.57, 5.75, 7.02, 7.11, 7.38, 8.39, 8.34, 9.71, 8.56, 8.1, 9.85, 7.6, 7.98),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 10\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.515\u00b10.509, lag=119.611\u00b156.452 h, mu_max=0.0157\u00b10.00314 log CFU/h, MPD=8.645\u00b10.298 log CFU/g (R\u00b2=0.872, SE of fit=0.631)."
  ),
  "combase_salmonella_b092_4" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_4, 10\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_4. Growth curve, Salmonella spp., 10\u00b0C, 26 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 26.58, 31.08, 49.58, 54, 57.42, 62.58, 71.83, 75.08, 79.33, 98.08, 108.66, 122.08, 133.08, 145.5, 156.75, 167.75, 177.25, 193.5, 216, 288, 387, 456.25, 505.25, 553.5, 629.5),
    y = c(3.33, 3.26, 3.31, 3.43, 3.28, 3.38, 3.29, 3.39, 3.65, 3.62, 4.06, 4.16, 4.27, 4.49, 3.79, 4.76, 5.19, 5.27, 5.27, 5.94, 6.77, 7.55, 7.59, 7.76, 7.79, 7.74),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 10\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.194\u00b10.119, lag=68.295\u00b114.223 h, mu_max=0.0173\u00b10.00142 log CFU/h, MPD=7.695\u00b10.0979 log CFU/g (R\u00b2=0.984, SE of fit=0.212)."
  ),
  "combase_salmonella_b092_32" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_32, 10\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_32. Growth curve, Salmonella spp., 10\u00b0C, 23 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 25.01, 50.88, 73.01, 123.9, 145.01, 150.94, 169.02, 193.86, 218.88, 243.9, 265.86, 289.01, 314.88, 337.01, 360, 387.9, 412.92, 432, 456, 480, 506.88, 528),
    y = c(2.78, 2.74, 2.86, 3.74, 3.91, 5.25, 4.48, 4.21, 4.98, 4.98, 6.24, 6.33, 6.59, 6.91, 7.72, 7.42, 7.67, 8.37, 7.77, 8.23, 8.2, 8.32, 8.32),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 10\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.687\u00b10.262, lag=38.413\u00b128.303 h, mu_max=0.0158\u00b10.00139 log CFU/h, MPD=8.283\u00b10.177 log CFU/g (R\u00b2=0.97, SE of fit=0.342)."
  ),
  "combase_salmonella_b092_17" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_17, 10\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_17. Growth curve, Salmonella spp., 10\u00b0C, 27 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 10, 13.9, 14.91, 24, 27.9, 34, 35.87, 39.87, 45.96, 52.92, 56.99, 96, 101.93, 104.98, 120, 129.99, 147.9, 155.87, 168, 192, 192, 194.88, 218.88, 242.88, 266.88, 409.86),
    y = c(3.48, 3.61, 3.44, 4.62, 4.29, 3.88, 3.97, 3.66, 4.02, 4.13, 4.19, 4.42, 4.55, 4.61, 5.23, 4.98, 5.68, 6.07, 6.31, 6.52, 6.86, 7.37, 8.03, 8.33, 8.64, 9.2, 8.83),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 10\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.906\u00b10.108, lag=79.273\u00b19.57 h, mu_max=0.0313\u00b10.00277 log CFU/h, MPD=9.0401\u00b10.253 log CFU/g (R\u00b2=0.969, SE of fit=0.323)."
  ),
  "combase_salmonella_b146_1" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B146_1, 11\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B146_1. Growth curve, Salmonella spp., 11\u00b0C, 9 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 24, 48, 72, 144, 192, 240, 288, 360),
    y = c(4.04, 3.99, 3.87, 4, 8.04, 8.15, 8.32, 8.56, 8.58),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 11\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.967\u00b10.0985, lag=90.373\u00b133.0445 h, mu_max=0.0808\u00b10.0499 log CFU/h, MPD=8.403\u00b10.0843 log CFU/g (R\u00b2=0.995, SE of fit=0.169)."
  ),
  "combase_salmonella_b146_2" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B146_2, 11\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B146_2. Growth curve, Salmonella spp., 11\u00b0C, 6 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(24, 48, 72, 144, 192, 240),
    y = c(4.04, 3.73, 3.8, 8.04, 8.49, 8.36),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 11\u00b0C. ComBase's own fit used the Baranyi model WITHOUT an asymptote term for this curve (the data does not reach a stationary phase): N0=3.518\u00b11.948, lag=32.316\u00b1109.105 h, mu_max=0.0279\u00b10.00985 log CFU/h (R\u00b2=0.766, SE of fit=1.179). No comparable asymptote (MPD) value is available from ComBase for this curve; this app's Reparameterized Baranyi model still requires and will estimate one from the data."
  ),
  "combase_salmonella_b132_1" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B132_1, 11\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B132_1. Growth curve, Salmonella spp., 11\u00b0C, 8 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 24, 48, 72, 144, 192, 240, 288),
    y = c(3.93, 3.66, 3.75, 3.92, 7.52, 9.32, 9.2, 8.99),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 11\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.789\u00b10.0964, lag=88.879\u00b115.712 h, mu_max=0.0684\u00b10.0187 log CFU/h, MPD=9.174\u00b10.0957 log CFU/g (R\u00b2=0.996, SE of fit=0.161)."
  ),
  "combase_salmonella_b132_2" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B132_2, 11\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B132_2. Growth curve, Salmonella spp., 11\u00b0C, 6 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(24, 48, 72, 144, 192, 240),
    y = c(3.76, 3.92, 3.53, 7.73, 9.41, 9.28),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 11\u00b0C. ComBase's own fit used the Baranyi model WITHOUT an asymptote term for this curve (the data does not reach a stationary phase): N0=3.404\u00b11.37, lag=43.924\u00b169.318 h, mu_max=0.0348\u00b10.00959 log CFU/h (R\u00b2=0.863, SE of fit=1.0519). No comparable asymptote (MPD) value is available from ComBase for this curve; this app's Reparameterized Baranyi model still requires and will estimate one from the data."
  ),
  "combase_salmonella_b419_12" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B419_12, 12\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B419_12. Growth curve, Salmonella spp., 12\u00b0C, 10 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 23.2, 49.4, 75.7, 96.3, 123.9, 144.5, 167.5, 195.25, 219.3),
    y = c(2.32, 2.63, 3.93, 5.12, 6.1, 7.61, 8.41, 8.66, 8.58, 8.62),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 12\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.294\u00b10.0987, lag=23.119\u00b13.52 h, mu_max=0.0539\u00b10.00198 log CFU/h, MPD=8.659\u00b10.0649 log CFU/g (R\u00b2=0.998, SE of fit=0.108)."
  ),
  "combase_salmonella_b301_75" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B301_75, 12\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B301_75. Growth curve, Salmonella spp., 12\u00b0C, 16 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 19.83, 24.38, 43.45, 48.4, 67.33, 71.93, 91.58, 97.22, 115.35, 140.63, 163.4, 192.22, 211.22, 238.8, 287.98),
    y = c(3.2, 3.6, 3.74, 4.56, 4.94, 5.6, 5.9, 6.95, 6.89, 6.7, 8.48, 8.44, 8.47, 8.28, 8.39, 8.57),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 12\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.117\u00b10.273, lag=7.0645\u00b110.788 h, mu_max=0.0409\u00b10.00388 log CFU/h, MPD=8.462\u00b10.132 log CFU/g (R\u00b2=0.979, SE of fit=0.286)."
  ),
  "combase_salmonella_b301_79" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B301_79, 12\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B301_79. Growth curve, Salmonella spp., 12\u00b0C, 16 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 19.78, 24.38, 43.38, 48.37, 67.3, 71.92, 91.57, 97.28, 115.3, 146.78, 163.5, 188.03, 211.32, 238.82, 287.53),
    y = c(3.33, 3.55, 3.6, 4.65, 4.84, 5.48, 5.83, 6.65, 6.89, 7.65, 8.14, 8.24, 8.18, 8.11, 8.31, 8.36),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 12\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.244\u00b10.0956, lag=17.224\u00b13.552 h, mu_max=0.0465\u00b10.00173 log CFU/h, MPD=8.237\u00b10.0446 log CFU/g (R\u00b2=0.997, SE of fit=0.105)."
  ),
  "combase_salmonella_b301_82" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B301_82, 12\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B301_82. Growth curve, Salmonella spp., 12\u00b0C, 20 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 25.07, 47.52, 69.9, 96.2, 117.45, 140.68, 163.62, 188.02, 211.42, 238.95, 287.2, 310.88, 336.07, 357.35, 382.47, 403.82, 429.12, 499.2, 523.12),
    y = c(3.32, 3.24, 3.5, 4.36, 4.88, 5.36, 6.03, 6.71, 7.56, 8.15, 8.11, 7.86, 7.84, 8.03, 7.87, 7.98, 7.88, 7.79, 7.86, 7.69),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 12\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.251\u00b10.13, lag=46.876\u00b18.359 h, mu_max=0.0313\u00b10.00211 log CFU/h, MPD=7.92\u00b10.0533 log CFU/g (R\u00b2=0.991, SE of fit=0.17)."
  ),
  "combase_salmonella_b301_81" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B301_81, 12\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B301_81. Growth curve, Salmonella spp., 12\u00b0C, 15 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 19.77, 24.38, 43.4, 48.33, 70.27, 71.93, 91.58, 97.27, 117.42, 146.78, 163.58, 192.17, 211.4, 238.9),
    y = c(3.39, 3.71, 4.37, 5.2, 5.49, 6.62, 6.76, 7.95, 8.42, 8.47, 8.41, 8.51, 8.47, 8.39, 8.35),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 12\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.402\u00b10.148, lag=15.312\u00b14.0493 h, mu_max=0.0621\u00b10.00338 log CFU/h, MPD=8.461\u00b10.0645 log CFU/g (R\u00b2=0.993, SE of fit=0.156)."
  ),
  "combase_salmonella_b092_26" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_26, 15\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_26. Growth curve, Salmonella spp., 15\u00b0C, 19 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 25.86, 28.91, 34, 39.87, 45.97, 49.87, 51.91, 54.95, 56.99, 72, 73.87, 75.9, 77.93, 79.96, 80.99, 103.96, 146.88, 150.95),
    y = c(2.69, 3.61, 3.8, 4, 4.56, 4.98, 5.5, 5.78, 5.94, 6.02, 6.67, 7.97, 6.41, 8.15, 8.12, 8.23, 10.8, 9.72, 9),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 15\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.816\u00b10.523, lag=22.853\u00b18.862 h, mu_max=0.0923\u00b10.0106 log CFU/h, MPD=9.759\u00b10.37 log CFU/g (R\u00b2=0.936, SE of fit=0.565)."
  ),
  "combase_salmonella_b092_30" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_30, 15\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_30. Growth curve, Salmonella spp., 15\u00b0C, 19 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 28.91, 34, 39.87, 45.96, 49.87, 51.9, 53.93, 55.96, 56.98, 72, 73.87, 75.9, 77.93, 79.96, 80.98, 103.96, 146.88, 150.95),
    y = c(2.64, 3.86, 4.03, 4.78, 5.36, 5.59, 5.59, 6.02, 6.18, 6.44, 7.54, 7.8, 7.86, 8.45, 8.28, 8.45, 10.8, 8.89, 8.66),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 15\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.691\u00b10.449, lag=20.877\u00b16.601 h, mu_max=0.0992\u00b10.00931 log CFU/h, MPD=9.403\u00b10.279 log CFU/g (R\u00b2=0.95, SE of fit=0.463)."
  ),
  "combase_salmonella_b092_43" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_43, 15\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_43. Growth curve, Salmonella spp., 15\u00b0C, 14 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 6.95, 23.99, 30.95, 49.01, 56.98, 70.98, 78.95, 96, 143.99, 145.86, 148.91, 150.94, 166.98),
    y = c(3.57, 3.55, 3.42, 3.58, 3.35, 3.58, 5.2, 5.44, 6.06, 8.19, 8.13, 8.29, 8.31, 8.26),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 15\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.439\u00b10.135, lag=53.258\u00b15.686 h, mu_max=0.0685\u00b10.011 log CFU/h, MPD=8.25\u00b10.125 log CFU/g (R\u00b2=0.985, SE of fit=0.265)."
  ),
  "combase_salmonella_b092_14" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_14, 15\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_14. Growth curve, Salmonella spp., 15\u00b0C, 27 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 4, 7, 13.2, 19, 22, 24, 26, 30.5, 32, 49, 50.6, 53.5, 56.8, 59.1, 62, 64, 71.2, 74.4, 78.65, 97.4, 102.4, 107.8, 121.4, 132.4, 144.9, 167.15),
    y = c(3.4, 3.39, 3.26, 3.18, 3.66, 3.28, 3.46, 3.39, 3.58, 3.48, 4.36, 4.49, 4.67, 5.78, 3.95, 4.04, 5.3, 5.38, 5.54, 5.73, 7.23, 7.2, 7.83, 8.31, 8.38, 8.58, 8.38),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 15\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.325\u00b10.148, lag=37.178\u00b15.275 h, mu_max=0.0625\u00b10.00602 log CFU/h, MPD=8.519\u00b10.241 log CFU/g (R\u00b2=0.96, SE of fit=0.378)."
  ),
  "combase_salmonella_b092_56" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_56, 15\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_56. Growth curve, Salmonella spp., 15\u00b0C, 16 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 7.96, 23.99, 28.91, 31.96, 49.01, 50.88, 56.98, 71.99, 76.91, 80.98, 143.99, 146.88, 148.91, 150.95, 241.01),
    y = c(3.29, 3.23, 3.47, 3.46, 3.77, 4.54, 4.65, 5, 5.58, 5.67, 6.09, 8.29, 8.41, 8.28, 8.35, 9.88),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 15\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.056\u00b10.147, lag=13.778\u00b15.931 h, mu_max=0.0405\u00b10.00157 log CFU/h, MPD=9.875\u00b10.203 log CFU/g (R\u00b2=0.992, SE of fit=0.193)."
  ),
  "combase_salmonella_b092_108" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_108, 20\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_108. Growth curve, Salmonella spp., 20\u00b0C, 30 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 1.68, 3.92, 5.95, 7.87, 10.17, 13.8, 14.78, 15.7, 20.17, 22, 23.67, 25.3, 28.03, 29.92, 31.55, 47.82, 49.92, 51.77, 53.67, 55.58, 71.57, 73.43, 75.55, 77.62, 79.37, 168.72, 171.27, 175.32, 191.6),
    y = c(2.86, 2.69, 2.86, 2.65, 2.78, 2.78, 2.85, 2.81, 2.78, 3.02, 2.91, 3.05, 2.9, 3.23, 3.26, 3.35, 4.56, 4.53, 4.59, 4.66, 4.87, 6.02, 6.06, 6.48, 6.63, 6.59, 8.12, 8.26, 8.37, 8.04),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 20\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.729\u00b10.0381, lag=27.309\u00b11.349 h, mu_max=0.0758\u00b10.00212 log CFU/h, MPD=8.198\u00b10.0519 log CFU/g (R\u00b2=0.997, SE of fit=0.104)."
  ),
  "combase_salmonella_b092_109" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_109, 20\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_109. Growth curve, Salmonella spp., 20\u00b0C, 28 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 1.68, 3.92, 5.95, 7.87, 9.17, 13.8, 14.78, 15.7, 20.17, 22, 23.77, 25.3, 28.03, 29.92, 31.55, 47.82, 49.92, 51.77, 53.67, 55.58, 71.57, 73.43, 75.55, 77.62, 79.37, 168.72, 191.6),
    y = c(2.81, 2.91, 2.73, 2.98, 2.66, 2.83, 2.88, 2.69, 3, 2.77, 3.28, 3.26, 3.38, 3.42, 3.59, 3.66, 4.86, 5.27, 5.41, 5.57, 5.7, 7.02, 7.43, 7.65, 7.69, 7.96, 8.29, 8.33),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 20\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.793\u00b10.0459, lag=24.732\u00b11.204 h, mu_max=0.0963\u00b10.00261 log CFU/h, MPD=8.34\u00b10.0876 log CFU/g (R\u00b2=0.996, SE of fit=0.126)."
  ),
  "combase_salmonella_b092_27" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_27, 20\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_27. Growth curve, Salmonella spp., 20\u00b0C, 17 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 12.89, 14.92, 17.9, 19.94, 22.99, 25.86, 27.9, 29.94, 31.97, 32.98, 34, 35.01, 38.92, 45.97, 48, 54.95),
    y = c(2.64, 4.32, 4.69, 5.55, 5.55, 6.24, 6.83, 7.4, 7.93, 8.21, 8.37, 8.45, 8.47, 8.77, 9.15, 8.89, 9.05),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 20\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.65\u00b10.13, lag=5.566\u00b10.877 h, mu_max=0.215\u00b10.00682 log CFU/h, MPD=9.0228\u00b10.0718 log CFU/g (R\u00b2=0.995, SE of fit=0.131)."
  ),
  "combase_salmonella_b092_16" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_16, 20\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_16. Growth curve, Salmonella spp., 20\u00b0C, 25 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 4, 5, 5.8, 8, 9.75, 12.8, 18.75, 20.25, 21.75, 24.25, 25.75, 27.75, 30.25, 31.75, 48.75, 50.45, 52, 53.25, 55.1, 56.6, 59, 61.95, 63.86, 71),
    y = c(3.39, 3.34, 3.36, 3.32, 3.19, 3.19, 3.33, 4.15, 4.25, 4.29, 4.57, 4.49, 4.89, 5.25, 5.61, 8.09, 8, 7.83, 7.88, 8.16, 8.29, 8.32, 8.52, 8.48, 8.47),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 20\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.253\u00b10.0701, lag=15.545\u00b11.0414 h, mu_max=0.14\u00b10.00592 log CFU/h, MPD=8.448\u00b10.0869 log CFU/g (R\u00b2=0.995, SE of fit=0.159)."
  ),
  "combase_salmonella_b092_10" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B092_10, 20\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B092_10. Growth curve, Salmonella spp., 20\u00b0C, 21 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 1.95, 2.78, 3.78, 4.8, 5.7, 7.93, 9.7, 12.78, 19.12, 20.62, 22.1, 23.2, 24.1, 26.1, 27.1, 28.2, 29.1, 30.62, 32.2, 49.1),
    y = c(3.36, 3.4, 3.44, 3.31, 3.39, 3.65, 3.82, 4.18, 4.82, 6.44, 6.49, 6.78, 7.09, 7.3, 7.47, 7.68, 7.96, 8.1, 8.16, 8.46, 8.49),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 20\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=3.278\u00b10.06, lag=6.0798\u00b10.606 h, mu_max=0.222\u00b10.00619 log CFU/h, MPD=8.492\u00b10.0813 log CFU/g (R\u00b2=0.997, SE of fit=0.105)."
  ),
  "combase_salmonella_b104" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B104, 30\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B104. Growth curve, Salmonella spp., 30\u00b0C, 17 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    y = c(1.48, 1, 1.48, 2.23, 2.8, 3.45, 4.15, 4.72, 5.32, 5.89, 6.5, 7.74, 7.62, 8.17, 8.66, 9.02, 9.2),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 30\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=1.106\u00b10.18, lag=1.697\u00b10.475 h, mu_max=0.67\u00b10.0296 log CFU/h, MPD=9.253\u00b10.233 log CFU/g (R\u00b2=0.993, SE of fit=0.233)."
  ),
  "combase_salmonella_b130" = list(
    nome = "ComBase \u2014 Salmonella spp. Growth (Record B130, 30\u00b0C)",
    fonte = "ComBase: A Combined Database for Predictive Microbiology (USDA Agricultural Research Service, UK Institute of Food Research, Australian Food Safety Centre of Excellence). https://combase.errc.ars.usda.gov/ Record ID: B130. Growth curve, Salmonella spp., 30\u00b0C, 14 observations. Y = Log10(CFU/g), X = Time (h).",
    x = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 21, 22, 24),
    y = c(2.18, 2.9, 3.61, 4.39, 5.08, 5.77, 6.49, 7.32, 7.97, 8.72, 9.15, 9.04, 9.2, 9.2),
    nota = "Downloaded from the ComBase Browser. Isothermal growth curve at 30\u00b0C. ComBase's own fit (Baranyi and Roberts Model, complete): N0=2.214\u00b10.0841, lag=0.508\u00b10.366 h, mu_max=0.383\u00b10.00859 log CFU/h, MPD=9.332\u00b10.069 log CFU/g (R\u00b2=0.999, SE of fit=0.0896)."
  )
,
  # --- NIST StRD Nonlinear Least Squares Regression Datasets ---------
  # https://www.itl.nist.gov/div898/strd/nls/nls_main.shtml - 27 datasets
  # in total (Rat42/Rat43 already above). No suggested model, no
  # reparameterization: only the data as published and the NIST
  # formula with its certified values, in the note, as requested.
  "nist_rat42" = list(
    nome = "NIST StRD \u2014 Rat42 (Ratkowsky2)",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Rat42 (source: Ratkowsky, D.A. (1983), Nonlinear Regression Modeling, Marcel Dekker, p. 61). Higher Level of Difficulty, 9 observations. https://www.itl.nist.gov/div898/strd/nls/data/ratkowsky2.shtml",
    x = c(9.000, 14.000, 21.000, 28.000, 42.000, 57.000, 63.000, 70.000, 79.000),
    y = c(8.930, 10.800, 18.590, 22.330, 39.350, 56.110, 61.730, 64.620, 67.080),
    nota = "NIST certified model: y = b1 / (1 + exp(b2 - b3*x)). Certified values: b1=72.4622376, b2=2.6180768, b3=0.0673592 (RSS=8.0565229, 6 d.f.)."
  ),
  "nist_rat43" = list(
    nome = "NIST StRD \u2014 Rat43 (Ratkowsky3)",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Rat43 (source: Ratkowsky, D.A. (1983), Nonlinear Regression Modeling, Marcel Dekker, p. 62). Higher Level of Difficulty, 15 observations. https://www.itl.nist.gov/div898/strd/nls/data/ratkowsky3.shtml",
    x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
    y = c(16.08, 33.83, 65.80, 97.20, 191.55, 326.20, 386.87, 520.53, 590.03, 651.92, 724.93, 699.56, 689.96, 637.56, 717.41),
    nota = "NIST certified model: y = b1 / (1 + exp(b2 - b3*x))^(1/b4). Certified values: b1=699.641513, b2=5.2771253, b3=0.7596294, b4=1.2792484 (RSS=8786.4049; NIST's own materials disagree on the degrees of freedom \u2014 the Certified Values page states 11, matching the standard n-p convention (15 obs. - 4 param.), while the raw .dat file's header states 9)."
  ),
  "nist_misra1a" = list(
    nome = "NIST StRD \u2014 Misra1a",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Misra1a (source: Misra, D., NIST (1978), Dental Research Monomolecular Adsorption Study). Lower Level of Difficulty, 14 observations. https://www.itl.nist.gov/div898/strd/nls/data/misra1a.shtml",
    x = c(77.6, 114.9, 141.1, 190.8, 239.9, 289.0, 332.8, 378.4, 434.8, 477.3, 536.8, 593.1, 689.1, 760.0),
    y = c(10.07, 14.73, 17.94, 23.93, 29.61, 35.18, 40.02, 44.82, 50.76, 55.05, 61.01, 66.40, 75.47, 81.78),
    nota = "NIST certified model: y = b1*(1-exp(-b2*x)). Certified values: b1=238.94212918, b2=0.00055015643181 (RSS=0.12455138894, 12 d.f.)."
  ),
  "nist_chwirut2" = list(
    nome = "NIST StRD \u2014 Chwirut2",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Chwirut2 (source: Chwirut, D., NIST, Ultrasonic Reference Block Study). Lower Level of Difficulty, 54 observations. https://www.itl.nist.gov/div898/strd/nls/data/chwirut2.shtml",
    x = c(0.5,1.0,1.75,3.75,5.75,0.875,2.25,3.25,5.25,0.75,1.75,2.75,4.75,0.625,1.25,2.25,4.25,0.5,3.0,0.75,3.0,1.5,6.0,3.0,6.0,1.5,3.0,0.5,2.0,4.0,0.75,2.0,5.0,0.75,2.25,3.75,5.75,3.0,0.75,2.5,4.0,0.75,2.5,4.0,0.75,2.5,4.0,0.5,6.0,3.0,0.5,2.75,0.5,1.75),
    y = c(92.9,57.1,31.05,11.5875,8.025,63.6,21.4,14.25,8.475,63.8,26.8,16.4625,7.125,67.3,41.0,21.15,8.175,81.5,13.12,59.9,14.62,32.9,5.44,12.56,5.44,32.0,13.95,75.8,20.0,10.42,59.5,21.67,8.55,62.0,20.2,7.76,3.75,11.81,54.7,23.7,11.55,61.3,17.7,8.74,59.2,16.3,8.62,81.0,4.87,14.62,81.7,17.17,81.3,28.9),
    nota = "NIST certified model: y = exp(-b1*x)/(b2+b3*x). Certified values: b1=0.16657666537, b2=0.0051653291286, b3=0.012150007096 (RSS=513.04802941, 51 d.f.)."
  ),
  "nist_chwirut1" = list(
    nome = "NIST StRD \u2014 Chwirut1",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Chwirut1 (source: Chwirut, D., NIST, Ultrasonic Reference Block Study). Lower Level of Difficulty, 214 observations. https://www.itl.nist.gov/div898/strd/nls/data/chwirut1.shtml",
    x = c(0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 1.75, 2.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 1.75, 2.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 1.75, 2.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 1.75, 2.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 0.5, 0.75, 1.5, 3.0, 3.0, 3.0, 6.0, 0.5, 0.75, 1.5, 3.0, 3.0, 3.0, 6.0, 0.5, 0.75, 1.5, 3.0, 3.0, 3.0, 6.0, 0.5, 0.75, 1.5, 3.0, 6.0, 3.0, 3.0, 6.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 2.25, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 0.5, 0.75, 1.0, 1.5, 2.0, 2.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 3.0, 0.5, 0.75, 1.5, 3.0, 6.0, 3.0, 6.0, 3.0, 3.0, 3.0, 1.75, 1.75, 0.5, 0.75, 1.75, 1.75, 2.75, 3.75, 1.75, 1.75, 0.5, 0.75, 2.75, 3.75, 1.75, 1.75),
    y = c(92.9, 78.7, 64.2, 64.9, 57.1, 43.3, 31.1, 23.6, 31.05, 23.775, 17.7375, 13.8, 11.5875, 9.4125, 7.725, 7.35, 8.025, 90.6, 76.9, 71.6, 63.6, 54.0, 39.2, 29.3, 21.4, 29.175, 22.125, 17.5125, 14.25, 9.45, 9.15, 7.9125, 8.475, 6.1125, 80.0, 79.0, 63.8, 57.2, 53.2, 42.5, 26.8, 20.4, 26.85, 21.0, 16.4625, 12.525, 10.5375, 8.5875, 7.125, 6.1125, 5.9625, 74.1, 67.3, 60.8, 55.5, 50.3, 41.0, 29.4, 20.4, 29.3625, 21.15, 16.7625, 13.2, 10.875, 8.175, 7.35, 5.9625, 5.625, 81.5, 62.4, 32.5, 12.41, 13.12, 15.56, 5.63, 78.0, 59.9, 33.2, 13.84, 12.75, 14.62, 3.94, 76.8, 61.0, 32.9, 13.87, 11.81, 13.31, 5.44, 78.0, 63.5, 33.8, 12.56, 5.63, 12.75, 13.12, 5.44, 76.8, 60.0, 47.8, 32.0, 22.2, 22.57, 18.82, 13.95, 11.25, 9.0, 6.67, 75.8, 62.0, 48.8, 35.2, 20.0, 20.32, 19.31, 12.75, 10.42, 7.31, 7.42, 70.5, 59.5, 48.5, 35.8, 21.0, 21.67, 21.0, 15.64, 8.17, 8.55, 10.12, 78.0, 66.0, 62.0, 58.0, 47.7, 37.8, 20.2, 21.07, 13.87, 9.67, 7.76, 5.44, 4.87, 4.01, 3.75, 24.19, 25.76, 18.07, 11.81, 12.07, 16.12, 70.8, 54.7, 48.0, 39.8, 29.8, 23.7, 29.62, 23.81, 17.7, 11.55, 12.07, 8.74, 80.7, 61.3, 47.5, 29.0, 24.0, 17.7, 24.56, 18.67, 16.24, 8.74, 7.87, 8.51, 66.7, 59.2, 40.8, 30.7, 25.7, 16.3, 25.99, 16.95, 13.35, 8.62, 7.2, 6.64, 13.69, 81.0, 64.5, 35.5, 13.31, 4.87, 12.94, 5.06, 15.19, 14.62, 15.64, 25.5, 25.95, 81.7, 61.6, 29.8, 29.81, 17.17, 10.39, 28.4, 28.69, 81.3, 60.9, 16.65, 10.05, 28.9, 28.95),
    nota = "NIST certified model: y = exp(-b1*x)/(b2+b3*x). Certified values: b1=0.1902781837, b2=0.0061314004477, b3=0.010530908399 (RSS=2384.4771393, 211 d.f.)."
  ),
  "nist_lanczos3" = list(
    nome = "NIST StRD \u2014 Lanczos3",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Lanczos3 (source: Lanczos, C. (1956), Applied Analysis, Prentice Hall, pp. 272-280). Lower Level of Difficulty, 24 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/lanczos3.shtml",
    x = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.05,1.1,1.15),
    y = c(2.5134,2.0443,1.6684,1.3664,1.1232,0.9269,0.7679,0.6389,0.5338,0.4479,0.3776,0.3197,0.2720,0.2325,0.1997,0.1723,0.1493,0.1301,0.1138,0.1000,0.0883,0.0783,0.0698,0.0624),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Certified values: b1=0.086816414977, b2=0.95498101505, b3=0.84400777463, b4=2.9515951832, b5=1.5825685901, b6=4.9863565084 (RSS=1.6117193594e-08, 18 d.f.)."
  ),
  "nist_gauss1" = list(
    nome = "NIST StRD \u2014 Gauss1",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Gauss1 (source: Rust, B., NIST (1996)). Lower Level of Difficulty, 250 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/gauss1.shtml",
    x = 1:250,
    y = c(97.62227,97.80724,96.62247,92.59022,91.23869,95.32704,90.35040,89.46235,91.72520,89.86916,86.88076,85.94360,87.60686,86.25839,80.74976,83.03551,88.25837,82.01316,82.74098,83.30034,81.27850,81.85506,80.75195,80.09573,81.07633,78.81542,78.38596,79.93386,79.48474,79.95942,76.10691,78.39830,81.43060,82.48867,81.65462,80.84323,88.68663,84.74438,86.83934,85.97739,91.28509,97.22411,93.51733,94.10159,101.91760,98.43134,110.4214,107.6628,111.7288,116.5115,120.7609,123.9553,124.2437,130.7996,133.2960,130.7788,132.0565,138.6584,142.9252,142.7215,144.1249,147.4377,148.2647,152.0519,147.3863,149.2074,148.9537,144.5876,148.1226,148.0144,143.8893,140.9088,143.4434,139.3938,135.9878,136.3927,126.7262,124.4487,122.8647,113.8557,113.7037,106.8407,107.0034,102.46290,96.09296,94.57555,86.98824,84.90154,81.18023,76.40117,67.09200,72.67155,68.10848,67.99088,63.34094,60.55253,56.18687,53.64482,53.70307,48.07893,42.21258,45.65181,41.69728,41.24946,39.21349,37.71696,36.68395,37.30393,37.43277,37.45012,32.64648,31.84347,31.39951,26.68912,32.25323,27.61008,33.58649,28.10714,30.26428,28.01648,29.11021,23.02099,25.65091,28.50295,25.23701,26.13828,33.53260,29.25195,27.09847,26.52999,25.52401,26.69218,24.55269,27.71763,25.20297,25.61483,25.06893,27.63930,24.94851,25.86806,22.48183,26.90045,25.39919,17.90614,23.76039,25.89689,27.64231,22.86101,26.47003,23.72888,27.54334,30.52683,28.07261,34.92815,28.29194,34.19161,35.41207,37.09336,40.98330,39.53923,47.80123,47.46305,51.04166,54.58065,57.53001,61.42089,62.79032,68.51455,70.23053,74.42776,76.59911,81.62053,83.42208,79.17451,88.56985,85.66525,86.55502,90.65907,84.27290,85.72220,83.10702,82.16884,80.42568,78.15692,79.79691,77.84378,74.50327,71.57289,65.88031,65.01385,60.19582,59.66726,52.95478,53.87792,44.91274,41.09909,41.68018,34.53379,34.86419,33.14787,29.58864,27.29462,21.91439,19.08159,24.90290,19.82341,16.75551,18.24558,17.23549,16.34934,13.71285,14.75676,13.97169,12.42867,14.35519,7.703309,10.234410,11.78315,13.87768,4.535700,10.059280,8.424824,10.533120,9.602255,7.877514,6.258121,8.899865,7.877754,12.51191,10.66205,6.035400,6.790655,8.783535,4.600288,8.400915,7.216561,10.017410,7.331278,6.527863,2.842001,10.325070,4.790995,8.377101,6.264445,2.706213,8.362329,8.983658,3.362571,1.182746,4.875359),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Certified values: b1=98.778210871, b2=0.010497276517, b3=100.48990633, b4=67.481111276, b5=23.129773360, b6=71.994503004, b7=178.99805021, b8=18.389389025 (RSS=1315.8222432, 242 d.f.)."
  ),
  "nist_gauss2" = list(
    nome = "NIST StRD \u2014 Gauss2",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Gauss2 (source: Rust, B., NIST (1996)). Lower Level of Difficulty, 250 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/gauss2.shtml",
    x = 1:250,
    y = c(97.58776,97.76344,96.56705,92.52037,91.15097,95.21728,90.21355,89.29235,91.51479,89.60966,86.56187,85.55316,87.13054,85.67940,80.04851,82.18925,87.24081,80.79407,81.28570,81.56940,79.22715,79.43275,77.90195,76.75468,77.17377,74.27348,73.11900,73.84826,72.47870,71.92292,66.92176,67.93835,69.56207,69.07066,66.53983,63.87883,69.71537,63.60588,63.37154,60.01835,62.67481,65.80666,59.14304,56.62951,61.21785,54.38790,62.93443,56.65144,57.13362,58.29689,58.91744,58.50172,55.22885,58.30375,57.43237,51.69407,49.93132,53.70760,55.39712,52.89709,52.31649,53.98720,53.54158,56.45046,51.32276,53.11676,53.28631,49.80555,54.69564,56.41627,54.59362,54.38520,60.15354,59.78773,60.49995,65.43885,60.70001,63.71865,67.77139,64.70934,70.78193,70.38651,77.22359,79.52665,80.13077,85.67823,85.20647,90.24548,93.61953,95.86509,93.46992,105.8137,107.8269,114.0607,115.5019,118.5110,119.6177,122.1940,126.9903,125.7005,123.7447,130.6543,129.7168,131.8240,131.8759,131.9994,132.1221,133.4414,133.8252,133.6695,128.2851,126.5182,124.7550,118.4016,122.0334,115.2059,118.7856,110.7387,110.2003,105.17290,103.44720,94.54280,94.40526,94.57964,88.76605,87.28747,92.50443,86.27997,82.44307,80.47367,78.36608,78.74307,76.12786,79.13108,76.76062,77.60769,77.76633,81.28220,79.74307,81.97964,80.02952,85.95232,85.96838,79.94789,87.17023,90.50992,93.23373,89.14803,93.11492,90.34337,93.69421,95.74256,91.85105,96.74503,87.60996,90.47012,88.11690,85.70673,85.01361,78.53040,81.34148,75.19295,72.66115,69.85504,66.29476,63.58502,58.33847,57.50766,52.80498,50.79319,47.03490,46.47090,43.09016,34.11531,39.28235,32.68386,30.44056,31.98932,23.63330,23.69643,20.26812,19.07074,17.59544,16.08785,18.94267,18.61354,17.25800,16.62285,13.48367,15.37647,13.47208,15.96188,12.32547,16.33880,10.438330,9.628715,13.12268,8.772417,11.76143,12.55020,11.33108,11.20493,7.816916,6.800675,14.26581,10.66285,8.911574,11.56733,11.58207,11.59071,9.730134,11.44237,11.22912,10.172130,12.50905,6.201493,9.019605,10.80607,13.09625,3.914271,9.567886,8.038448,10.231040,9.367410,7.695971,6.118575,8.793207,7.796692,12.45065,10.61601,6.001003,6.765098,8.764653,4.586418,8.390783,7.209202,10.012090,7.327461,6.525136,2.840065,10.323710,4.790035,8.376431,6.263980,2.705892,8.362109,8.983507,3.362469,1.182678,4.875312),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Certified values: b1=99.018328406, b2=0.010994945399, b3=101.88022528, b4=107.03095519, b5=23.578584029, b6=72.045589471, b7=153.27010194, b8=19.525972636 (RSS=1247.5282092, 242 d.f.)."
  ),
  "nist_danwood" = list(
    nome = "NIST StRD \u2014 DanWood",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset DanWood (source: Daniel, C. and F.S. Wood (1980), Fitting Equations to Data, 2nd ed., Wiley, pp. 428-431). Lower Level of Difficulty, 6 observations. https://www.itl.nist.gov/div898/strd/nls/data/daniel_wood.shtml",
    x = c(1.309, 1.471, 1.490, 1.565, 1.611, 1.680),
    y = c(2.138, 3.421, 3.597, 4.340, 4.882, 5.660),
    nota = "NIST certified model: y = b1*x^b2 (energy radiated from a carbon filament lamp vs. absolute temperature in 1000 K). Certified values: b1=0.76886226176, b2=3.8604055871 (RSS=0.0043173084083, 4 d.f.)."
  ),
  "nist_misra1b" = list(
    nome = "NIST StRD \u2014 Misra1b",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Misra1b (source: Misra, D., NIST (1978), Dental Research Monomolecular Adsorption Study). Lower Level of Difficulty, 14 observations. https://www.itl.nist.gov/div898/strd/nls/data/misra1b.shtml",
    x = c(77.6, 114.9, 141.1, 190.8, 239.9, 289.0, 332.8, 378.4, 434.8, 477.3, 536.8, 593.1, 689.1, 760.0),
    y = c(10.07, 14.73, 17.94, 23.93, 29.61, 35.18, 40.02, 44.82, 50.76, 55.05, 61.01, 66.40, 75.47, 81.78),
    nota = "NIST certified model: y = b1*(1-(1+b2*x/2)^(-2)). Certified values: b1=337.99746163, b2=0.00039039091287 (RSS=0.075464681533, 12 d.f.)."
  )
,
  "nist_kirby2" = list(
    nome = "NIST StRD \u2014 Kirby2",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Kirby2 (source: Kirby, R., NIST, Scanning Electron Microscope Line Width Standards). Average Level of Difficulty, 151 observations. https://www.itl.nist.gov/div898/strd/nls/data/kirby2.shtml",
    x = c(9.65, 10.74, 11.81, 12.88, 14.06, 15.28, 16.63, 18.19, 19.88, 21.84, 24.0, 26.25, 28.86, 31.85, 35.79, 40.18, 44.74, 49.53, 53.94, 58.29, 62.63, 67.03, 71.25, 75.22, 79.33, 83.56, 87.75, 91.93, 96.1, 100.28, 104.46, 108.66, 112.71, 116.88, 121.33, 125.79, 125.79, 128.74, 130.27, 133.33, 134.79, 137.93, 139.33, 142.46, 143.9, 146.91, 148.51, 151.41, 153.17, 155.97, 157.76, 160.56, 162.3, 165.21, 166.9, 169.92, 170.32, 171.54, 173.79, 174.57, 176.25, 177.34, 179.19, 181.02, 182.08, 183.88, 185.75, 186.8, 188.63, 190.45, 191.48, 193.35, 195.22, 196.23, 198.05, 199.97, 201.06, 202.83, 204.69, 205.86, 207.58, 209.5, 210.65, 212.33, 215.43, 217.16, 220.21, 221.98, 225.06, 226.79, 229.92, 231.69, 234.77, 236.6, 239.63, 241.5, 244.48, 246.4, 249.35, 251.32, 254.22, 256.24, 259.11, 261.18, 264.02, 266.13, 268.94, 271.09, 273.87, 276.08, 278.83, 281.08, 283.81, 286.11, 288.81, 291.08, 293.75, 295.99, 298.64, 300.84, 302.02, 303.48, 305.65, 308.27, 310.41, 313.01, 315.12, 317.71, 319.79, 322.36, 324.42, 326.98, 329.01, 331.56, 333.56, 336.1, 338.08, 340.6, 342.57, 345.08, 347.02, 349.52, 351.44, 353.93, 355.83, 358.32, 360.2, 362.67, 364.53, 367.0, 371.3),
    y = c(0.0082, 0.0112, 0.0149, 0.0198, 0.0248, 0.0324, 0.042, 0.0549, 0.0719, 0.0963, 0.1291, 0.171, 0.2314, 0.3227, 0.4809, 0.7084, 1.022, 1.458, 1.952, 2.541, 3.223, 3.999, 4.852, 5.732, 6.727, 7.835, 9.025, 10.267, 11.578, 12.944, 14.377, 15.856, 17.331, 18.885, 20.575, 22.32, 22.303, 23.46, 24.06, 25.272, 25.853, 27.11, 27.658, 28.924, 29.511, 30.71, 31.35, 32.52, 33.23, 34.33, 35.06, 36.17, 36.84, 38.01, 38.67, 39.87, 40.03, 40.5, 41.37, 41.67, 42.31, 42.73, 43.46, 44.14, 44.55, 45.22, 45.92, 46.3, 47.0, 47.68, 48.06, 48.74, 49.41, 49.76, 50.43, 51.11, 51.5, 52.12, 52.76, 53.18, 53.78, 54.46, 54.83, 55.4, 56.43, 57.03, 58.0, 58.61, 59.58, 60.11, 61.1, 61.65, 62.59, 63.12, 64.03, 64.62, 65.49, 66.03, 66.89, 67.42, 68.23, 68.77, 69.59, 70.11, 70.86, 71.43, 72.16, 72.7, 73.4, 73.93, 74.6, 75.16, 75.82, 76.34, 76.98, 77.48, 78.08, 78.6, 79.17, 79.62, 79.88, 80.19, 80.66, 81.22, 81.66, 82.16, 82.59, 83.14, 83.5, 84.0, 84.4, 84.89, 85.26, 85.74, 86.07, 86.54, 86.89, 87.32, 87.65, 88.1, 88.43, 88.83, 89.12, 89.54, 89.85, 90.25, 90.55, 90.93, 91.2, 91.55, 92.2),
    nota = "NIST certified model: y = (b1 + b2*x + b3*x^2) / (1 + b4*x + b5*x^2). Certified values: b1=1.6745063063, b2=-0.13927397867, b3=0.0025961181191, b4=-0.0017241811870, b5=0.000021664802578 (RSS=3.9050739624, 146 d.f.)."
  ),
  "nist_mgh17" = list(
    nome = "NIST StRD \u2014 MGH17",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset MGH17 (source: Osborne, M.R. (1972), in Numerical Methods for Nonlinear Optimization, Lootsma (Ed.), Academic Press, pp. 171-189). Average Level of Difficulty, 33 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/mgh17.shtml",
    x = seq(0, 320, by = 10),
    y = c(0.844,0.908,0.932,0.936,0.925,0.908,0.881,0.850,0.818,0.784,0.751,0.718,0.685,0.658,0.628,0.603,0.580,0.558,0.538,0.522,0.506,0.490,0.478,0.467,0.457,0.448,0.438,0.431,0.424,0.420,0.414,0.411,0.406),
    nota = "NIST certified model: y = b1 + b2*exp(-x*b4) + b3*exp(-x*b5). Certified values: b1=0.37541005211, b2=1.9358469127, b3=-1.4646871366, b4=0.012867534640, b5=0.022122699662 (RSS=5.4648946975e-05, 28 d.f.)."
  ),
  "nist_lanczos1" = list(
    nome = "NIST StRD \u2014 Lanczos1",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Lanczos1 (source: Lanczos, C. (1956), Applied Analysis, Prentice Hall, pp. 272-280). Average Level of Difficulty, 24 observations, Generated Data (14-digit accuracy). https://www.itl.nist.gov/div898/strd/nls/data/lanczos1.shtml",
    x = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.05,1.1,1.15),
    y = c(2.513400000000,2.044333373291,1.668404436564,1.366418021208,1.123232487372,0.9268897180037,0.7679338563728,0.6388775523106,0.5337835317402,0.4479363617347,0.3775847884350,0.3197393199326,0.2720130773746,0.2324965529032,0.1996589546065,0.1722704126914,0.1493405660168,0.1300700206922,0.1138119324644,0.1000415587559,0.08833209084540,0.07833544019350,0.06976693743449,0.06239312536719),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Certified values: b1=0.095100000027, b2=1.0000000001, b3=0.86070000013, b4=3.0000000002, b5=1.5575999998, b6=5.0000000001 (RSS=1.4307867721e-25, 18 d.f.)."
  ),
  "nist_lanczos2" = list(
    nome = "NIST StRD \u2014 Lanczos2",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Lanczos2 (source: Lanczos, C. (1956), Applied Analysis, Prentice Hall, pp. 272-280). Average Level of Difficulty, 24 observations, Generated Data (6-digit accuracy). https://www.itl.nist.gov/div898/strd/nls/data/lanczos2.shtml",
    x = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.05,1.1,1.15),
    y = c(2.51340,2.04433,1.66840,1.36642,1.12323,0.926890,0.767934,0.638878,0.533784,0.447936,0.377585,0.319739,0.272013,0.232497,0.199659,0.172270,0.149341,0.130070,0.113812,0.100042,0.0883321,0.0783354,0.0697669,0.0623931),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-b4*x) + b5*exp(-b6*x). Certified values: b1=0.096251029939, b2=1.0057332849, b3=0.86424689056, b4=3.0078283915, b5=1.5529016879, b6=5.0028798100 (RSS=2.2299428125e-11, 18 d.f.)."
  ),
  "nist_gauss3" = list(
    nome = "NIST StRD \u2014 Gauss3",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Gauss3 (source: Rust, B., NIST (1996)). Average Level of Difficulty, 250 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/gauss3.shtml",
    x = 1:250,
    y = c(97.58776,97.76344,96.56705,92.52037,91.15097,95.21728,90.21355,89.29235,91.51479,89.60965,86.56187,85.55315,87.13053,85.67938,80.04849,82.18922,87.24078,80.79401,81.28564,81.56932,79.22703,79.43259,77.90174,76.75438,77.17338,74.27296,73.11830,73.84732,72.47746,71.92128,66.91962,67.93554,69.55841,69.06592,66.53371,63.87094,69.70526,63.59295,63.35509,59.99747,62.64843,65.77345,59.10141,56.57750,61.15313,54.30767,62.83535,56.52957,56.98427,58.11459,58.69576,58.23322,54.90490,57.91442,56.96629,51.13831,49.27123,52.92668,54.47693,51.81710,51.05401,52.51731,51.83710,54.48196,49.05859,50.52315,50.32755,46.44419,50.89281,52.13203,49.78741,49.01637,54.18198,53.17456,53.20827,57.43459,51.95282,54.20282,57.46687,53.60268,58.86728,57.66652,63.71034,65.24244,65.10878,69.96313,68.85475,73.32574,76.21241,78.06311,75.37701,87.54449,89.50588,95.82098,97.48390,100.86070,102.48510,105.7311,111.3489,111.0305,110.1920,118.3581,118.8086,122.4249,124.0953,125.9337,127.8533,131.0361,133.3343,135.1278,131.7113,131.9151,132.1107,127.6898,133.2148,128.2296,133.5902,127.2539,128.3482,124.8694,124.6031,117.0648,118.1966,119.5408,114.7946,114.2780,120.3484,114.8647,111.6514,110.1826,108.4461,109.0571,106.5308,109.4691,106.8709,107.3192,106.9000,109.6526,107.1602,108.2509,104.96310,109.3601,107.6696,99.77286,104.96440,106.1376,106.5816,100.12860,101.66910,96.44254,97.34169,96.97412,90.73460,93.37949,82.12331,83.01657,78.87360,74.86971,72.79341,65.14744,67.02127,60.16136,57.13996,54.05769,50.42265,47.82430,42.85748,42.45495,38.30808,36.95794,33.94543,34.19017,31.66097,23.56172,29.61143,23.88765,22.49812,24.86901,17.29481,18.09291,15.34813,14.77997,13.87832,12.88891,16.20763,16.29024,15.29712,14.97839,12.11330,14.24168,12.53824,15.19818,11.70478,15.83745,10.035850,9.307574,12.86800,8.571671,11.60415,12.42772,11.23627,11.13198,7.761117,6.758250,14.23375,10.63876,8.893581,11.55398,11.57221,11.58347,9.724857,11.43854,11.22636,10.170150,12.50765,6.200494,9.018902,10.80557,13.09591,3.914033,9.567723,8.038338,10.230960,9.367358,7.695937,6.118552,8.793192,7.796682,12.45064,10.61601,6.001000,6.765096,8.764652,4.586417,8.390782,7.209201,10.012090,7.327461,6.525136,2.840065,10.323710,4.790035,8.376431,6.263980,2.705892,8.362109,8.983507,3.362469,1.182678,4.875312),
    nota = "NIST certified model: y = b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2) + b6*exp(-(x-b7)^2/b8^2). Certified values: b1=98.940368970, b2=0.010945879335, b3=100.69553078, b4=111.63619459, b5=23.300500029, b6=73.705031418, b7=147.76164251, b8=19.668221230 (RSS=1244.4846360, 242 d.f.)."
  ),
  "nist_misra1c" = list(
    nome = "NIST StRD \u2014 Misra1c",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Misra1c (source: Misra, D., NIST (1978), Dental Research Monomolecular Adsorption). Average Level of Difficulty, 14 observations. https://www.itl.nist.gov/div898/strd/nls/data/misra1c.shtml",
    x = c(77.6, 114.9, 141.1, 190.8, 239.9, 289.0, 332.8, 378.4, 434.8, 477.3, 536.8, 593.1, 689.1, 760.0),
    y = c(10.07, 14.73, 17.94, 23.93, 29.61, 35.18, 40.02, 44.82, 50.76, 55.05, 61.01, 66.40, 75.47, 81.78),
    nota = "NIST certified model: y = b1*(1-(1+2*b2*x)^(-0.5)). Certified values: b1=636.42725809, b2=0.00020813627256 (RSS=0.040966836971, 12 d.f.)."
  ),
  "nist_misra1d" = list(
    nome = "NIST StRD \u2014 Misra1d",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Misra1d (source: Misra, D., NIST (1978), Dental Research Monomolecular Adsorption Study). Average Level of Difficulty, 14 observations. https://www.itl.nist.gov/div898/strd/nls/data/misra1d.shtml",
    x = c(77.6, 114.9, 141.1, 190.8, 239.9, 289.0, 332.8, 378.4, 434.8, 477.3, 536.8, 593.1, 689.1, 760.0),
    y = c(10.07, 14.73, 17.94, 23.93, 29.61, 35.18, 40.02, 44.82, 50.76, 55.05, 61.01, 66.40, 75.47, 81.78),
    nota = "NIST certified model: y = b1*b2*x/(1+b2*x). Certified values: b1=437.36970754, b2=0.00030227324449 (RSS=0.056419295283, 12 d.f.)."
  ),
  "nist_roszman1" = list(
    nome = "NIST StRD \u2014 Roszman1",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Roszman1 (source: Roszman, L., NIST, Quantum Defects for Sulfur I Atom). Average Level of Difficulty, 25 observations. https://www.itl.nist.gov/div898/strd/nls/data/roszman1.shtml",
    x = c(-4868.68,-4868.09,-4867.41,-3375.19,-3373.14,-3372.03,-2473.74,-2472.35,-2469.45,-1894.65,-1893.40,-1497.24,-1495.85,-1493.41,-1208.68,-1206.18,-1206.04,-997.92,-996.61,-996.31,-834.94,-834.66,-710.03,-530.16,-464.17),
    y = c(0.252429,0.252141,0.251809,0.297989,0.296257,0.295319,0.339603,0.337731,0.333820,0.389510,0.386998,0.438864,0.434887,0.427893,0.471568,0.461699,0.461144,0.513532,0.506641,0.505062,0.535648,0.533726,0.568064,0.612886,0.624169),
    nota = "NIST certified model: y = b1 - b2*x - atan(b3/(x-b4))/pi (quantum defects, argument to arctan in radians). Certified values: b1=0.20196866396, b2=-0.0000061953516256, b3=1204.4556708, b4=-181.34269537 (RSS=0.00049484847331, 21 d.f.). Note: the certified value for b1 as published by NIST is written \"1.20196866396E-0\" in some reproductions of the certified-values table \u2014 that appears to be a typographical rendering of 0.20196866396 (i.e. E-01, not E-0); this app uses 0.20196866396, which matches the residual sum of squares reported."
  ),
  "nist_enso" = list(
    nome = "NIST StRD \u2014 ENSO",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset ENSO (source: Kahaner, D., C. Moler, and S. Nash (1989), Numerical Methods and Software, Prentice Hall, pp. 441-445). Average Level of Difficulty, 168 observations. https://www.itl.nist.gov/div898/strd/nls/data/enso.shtml",
    x = 1:168,
    y = c(12.90000,11.30000,10.60000,11.20000,10.90000,7.500000,7.700000,11.70000,12.90000,14.30000,10.90000,13.70000,17.10000,14.00000,15.30000,8.500000,5.700000,5.500000,7.600000,8.600000,7.300000,7.600000,12.70000,11.00000,12.70000,12.90000,13.00000,10.90000,10.400000,10.200000,8.000000,10.90000,13.60000,10.500000,9.200000,12.40000,12.70000,13.30000,10.100000,7.800000,4.800000,3.000000,2.500000,6.300000,9.700000,11.60000,8.600000,12.40000,10.500000,13.30000,10.400000,8.100000,3.700000,10.70000,5.100000,10.400000,10.90000,11.70000,11.40000,13.70000,14.10000,14.00000,12.50000,6.300000,9.600000,11.70000,5.000000,10.80000,12.70000,10.80000,11.80000,12.60000,15.70000,12.60000,14.80000,7.800000,7.100000,11.20000,8.100000,6.400000,5.200000,12.00000,10.200000,12.70000,10.200000,14.70000,12.20000,7.100000,5.700000,6.700000,3.900000,8.500000,8.300000,10.80000,16.70000,12.60000,12.50000,12.50000,9.800000,7.200000,4.100000,10.60000,10.100000,10.100000,11.90000,13.60000,16.30000,17.60000,15.50000,16.00000,15.20000,11.20000,14.30000,14.50000,8.500000,12.00000,12.70000,11.30000,14.50000,15.10000,10.400000,11.50000,13.40000,7.500000,0.6000000,0.3000000,5.500000,5.000000,4.600000,8.200000,9.900000,9.200000,12.50000,10.90000,9.900000,8.900000,7.600000,9.500000,8.400000,10.70000,13.60000,13.70000,13.70000,16.50000,16.80000,17.10000,15.40000,9.500000,6.100000,10.100000,9.300000,5.300000,11.20000,16.60000,15.60000,12.00000,11.50000,8.600000,13.80000,8.700000,8.600000,8.600000,8.700000,12.80000,13.20000,14.00000,13.40000,14.80000),
    nota = "NIST certified model: y = b1 + b2*cos(2*pi*x/12) + b3*sin(2*pi*x/12) + b5*cos(2*pi*x/b4) + b6*sin(2*pi*x/b4) + b8*cos(2*pi*x/b7) + b9*sin(2*pi*x/b7) (monthly atmospheric pressure difference, Easter Island vs Darwin, Australia). Certified values: b1=10.510749193, b2=3.0762128085, b3=0.53280138227, b4=44.311088700, b5=-1.6231428586, b6=0.52554493756, b7=26.887614440, b8=0.21232288488, b9=1.4966870418 (RSS=788.53978668, 159 d.f.)."
  ),
  "nist_hahn1" = list(
    nome = "NIST StRD \u2014 Hahn1",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Hahn1 (source: Hahn, T., NIST, Copper Thermal Expansion Study). Average Level of Difficulty, 236 observations. https://www.itl.nist.gov/div898/strd/nls/data/hahn1.shtml",
    x = c(24.41, 34.82, 44.09, 45.07, 54.98, 65.51, 70.53, 75.7, 89.57, 91.14, 96.4, 97.19, 114.26, 120.25, 127.08, 133.55, 133.61, 158.67, 172.74, 171.31, 202.14, 220.55, 221.05, 221.39, 250.99, 268.99, 271.8, 271.97, 321.31, 321.69, 330.14, 333.03, 333.47, 340.77, 345.65, 373.11, 373.79, 411.82, 419.51, 421.59, 422.02, 422.47, 422.61, 441.75, 447.41, 448.7, 472.89, 476.69, 522.47, 522.62, 524.43, 546.75, 549.53, 575.29, 576.0, 625.55, 20.15, 28.78, 29.57, 37.41, 39.12, 50.24, 61.38, 66.25, 73.42, 95.52, 107.32, 122.04, 134.03, 163.19, 163.48, 175.7, 179.86, 211.27, 217.78, 219.14, 262.52, 268.01, 268.62, 336.25, 337.23, 339.33, 427.38, 428.58, 432.68, 528.99, 531.08, 628.34, 253.24, 273.13, 273.66, 282.1, 346.62, 347.19, 348.78, 351.18, 450.1, 450.35, 451.92, 455.56, 552.22, 553.56, 555.74, 652.59, 656.2, 14.13, 20.41, 31.3, 33.84, 39.7, 48.83, 54.5, 60.41, 72.77, 75.25, 86.84, 94.88, 96.4, 117.37, 139.08, 147.73, 158.63, 161.84, 192.11, 206.76, 209.07, 213.32, 226.44, 237.12, 330.9, 358.72, 370.77, 372.72, 396.24, 416.59, 484.02, 495.47, 514.78, 515.65, 519.47, 544.47, 560.11, 620.77, 18.97, 28.93, 33.91, 40.03, 44.66, 49.87, 55.16, 60.9, 72.08, 85.15, 97.06, 119.63, 133.27, 143.84, 161.91, 180.67, 198.44, 226.86, 229.65, 258.27, 273.77, 339.15, 350.13, 362.75, 371.03, 393.32, 448.53, 473.78, 511.12, 524.7, 548.75, 551.64, 574.02, 623.86, 21.46, 24.33, 33.43, 39.22, 44.18, 55.02, 94.33, 96.44, 118.82, 128.48, 141.94, 156.92, 171.65, 190.0, 223.26, 223.88, 231.5, 265.05, 269.44, 271.78, 273.46, 334.61, 339.79, 349.52, 358.18, 377.98, 394.77, 429.66, 468.22, 487.27, 519.54, 523.03, 612.99, 638.59, 641.36, 622.05, 631.5, 663.97, 646.9, 748.29, 749.21, 750.14, 647.04, 646.89, 746.9, 748.43, 747.35, 749.27, 647.61, 747.78, 750.51, 851.37, 845.97, 847.54, 849.93, 851.61, 849.75, 850.98, 848.23),
    y = c(0.591, 1.547, 2.902, 2.894, 4.703, 6.307, 7.03, 7.898, 9.47, 9.484, 10.072, 10.163, 11.615, 12.005, 12.478, 12.982, 12.97, 13.926, 14.452, 14.404, 15.19, 15.55, 15.528, 15.499, 16.131, 16.438, 16.387, 16.549, 16.872, 16.83, 16.926, 16.907, 16.966, 17.06, 17.122, 17.311, 17.355, 17.668, 17.767, 17.803, 17.765, 17.768, 17.736, 17.858, 17.877, 17.912, 18.046, 18.085, 18.291, 18.357, 18.426, 18.584, 18.61, 18.87, 18.795, 19.111, 0.367, 0.796, 0.892, 1.903, 2.15, 3.697, 5.87, 6.421, 7.422, 9.944, 11.023, 11.87, 12.786, 14.067, 13.974, 14.462, 14.464, 15.381, 15.483, 15.59, 16.075, 16.347, 16.181, 16.915, 17.003, 16.978, 17.756, 17.808, 17.868, 18.481, 18.486, 19.09, 16.062, 16.337, 16.345, 16.388, 17.159, 17.116, 17.164, 17.123, 17.979, 17.974, 18.007, 17.993, 18.523, 18.669, 18.617, 19.371, 19.33, 0.08, 0.248, 1.089, 1.418, 2.278, 3.624, 4.574, 5.556, 7.267, 7.695, 9.136, 9.959, 9.957, 11.6, 13.138, 13.564, 13.871, 13.994, 14.947, 15.473, 15.379, 15.455, 15.908, 16.114, 17.071, 17.135, 17.282, 17.368, 17.483, 17.764, 18.185, 18.271, 18.236, 18.237, 18.523, 18.627, 18.665, 19.086, 0.214, 0.943, 1.429, 2.241, 2.951, 3.782, 4.757, 5.602, 7.169, 8.92, 10.055, 12.035, 12.861, 13.436, 14.167, 14.755, 15.168, 15.651, 15.746, 16.216, 16.445, 16.965, 17.121, 17.206, 17.25, 17.339, 17.793, 18.123, 18.49, 18.566, 18.645, 18.706, 18.924, 19.1, 0.375, 0.471, 1.504, 2.204, 2.813, 4.765, 9.835, 10.04, 11.946, 12.596, 13.303, 13.922, 14.44, 14.951, 15.627, 15.639, 15.814, 16.315, 16.334, 16.43, 16.423, 17.024, 17.009, 17.165, 17.134, 17.349, 17.576, 17.848, 18.09, 18.276, 18.404, 18.519, 19.133, 19.074, 19.239, 19.28, 19.101, 19.398, 19.252, 19.89, 20.007, 19.929, 19.268, 19.324, 20.049, 20.107, 20.062, 20.065, 19.286, 19.972, 20.088, 20.743, 20.83, 20.935, 21.035, 20.93, 21.074, 21.085, 20.935),
    nota = "NIST certified model: y = (b1+b2*x+b3*x^2+b4*x^3) / (1+b5*x+b6*x^2+b7*x^3) (coefficient of thermal expansion of copper vs. temperature in kelvin). Certified values: b1=1.0776351733, b2=-0.12269296921, b3=0.0040863750610, b4=-0.0000014262662514, b5=-0.0057609940901, b6=0.00024053735503, b7=-0.00000012314450199 (RSS=1.5324382854, 229 d.f.)."
  ),
  "nist_mgh09" = list(
    nome = "NIST StRD \u2014 MGH09",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset MGH09 (source: Kowalik, J.S. and M.R. Osborne (1978), Methods for Unconstrained Optimization Problems, Elsevier North-Holland). Higher Level of Difficulty, 11 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/mgh09.shtml",
    x = c(4, 2, 1, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625),
    y = c(0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 0.0323, 0.0235, 0.0246),
    nota = "NIST certified model: y = b1*(x^2+x*b2) / (x^2+x*b3+b4). Certified values: b1=0.19280693458, b2=0.19128232873, b3=0.12305650693, b4=0.13606233068 (RSS=0.00030750560385, 7 d.f.). Known to have a local minimum elsewhere in parameter space with RSS≈0.00102734, distinct from the global minimum certified here."
  ),
  "nist_boxbod" = list(
    nome = "NIST StRD — BoxBOD",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset BoxBOD (source: Box, G.E.P., W.G. Hunter, and J.S. Hunter (1978), Statistics for Experimenters, Wiley, pp. 483-487). Higher Level of Difficulty, 6 observations. https://www.itl.nist.gov/div898/strd/nls/data/boxbod.shtml",
    x = c(1, 2, 3, 5, 7, 10),
    y = c(109, 149, 149, 191, 213, 224),
    nota = "NIST certified model: y = b1*(1-exp(-b2*x)) (biochemical oxygen demand in mg/l vs. incubation time in days). Certified values: b1=213.80940889, b2=0.54723748542 (RSS=1168.0088766, 4 d.f.)."
  ),
  "nist_mgh10" = list(
    nome = "NIST StRD \u2014 MGH10",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset MGH10 (source: Meyer, R.R. (1970), in Nonlinear Programming, Rosen, Mangasarian and Ritter (Eds.), Academic Press, pp. 465-486). Higher Level of Difficulty, 16 observations, Generated Data. https://www.itl.nist.gov/div898/strd/nls/data/mgh10.shtml",
    x = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125),
    y = c(34780, 28610, 23650, 19630, 16370, 13720, 11540, 9744, 8261, 7030, 6005, 5147, 4427, 3820, 3307, 2872),
    nota = "NIST certified model: y = b1*exp(b2/(x+b3)). Certified values: b1=0.0056096364710, b2=6181.3463463, b3=345.22363462 (RSS=87.945855171, 13 d.f.)."
  ),
  "nist_eckerle4" = list(
    nome = "NIST StRD \u2014 Eckerle4",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Eckerle4 (source: Eckerle, K., NIST, Circular Interference Transmittance Study). Higher Level of Difficulty, 35 observations. https://www.itl.nist.gov/div898/strd/nls/data/eckerle4.shtml",
    x = c(400.000000,405.000000,410.000000,415.000000,420.000000,425.000000,430.000000,435.000000,436.500000,438.000000,439.500000,441.000000,442.500000,444.000000,445.500000,447.000000,448.500000,450.000000,451.500000,453.000000,454.500000,456.000000,457.500000,459.000000,460.500000,462.000000,463.500000,465.000000,470.000000,475.000000,480.000000,485.000000,490.000000,495.000000,500.000000),
    y = c(0.0001575,0.0001699,0.0002350,0.0003102,0.0004917,0.0008710,0.0017418,0.0046400,0.0065895,0.0097302,0.0149002,0.0237310,0.0401683,0.0712559,0.1264458,0.2073413,0.2902366,0.3445623,0.3698049,0.3668534,0.3106727,0.2078154,0.1164354,0.0616764,0.0337200,0.0194023,0.0117831,0.0074357,0.0022732,0.0008800,0.0004579,0.0002345,0.0001586,0.0001143,0.0000710),
    nota = "NIST certified model: y = (b1/b2)*exp(-0.5*((x-b3)/b2)^2) (transmittance vs. wavelength, circular interference study). Certified values: b1=1.5543827178, b2=4.0888321754, b3=451.54121844 (RSS=0.0014635887487, 32 d.f.)."
  ),
  "nist_bennett5" = list(
    nome = "NIST StRD \u2014 Bennett5",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Bennett5 (source: Bennett, L., L. Swartzendruber, and H. Brown, NIST (1994), Superconductivity Magnetization Modeling). Higher Level of Difficulty, 154 observations. https://www.itl.nist.gov/div898/strd/nls/data/bennett5.shtml",
    x = c(7.447168, 8.102586, 8.452547, 8.711278, 8.916774, 9.087155, 9.23259, 9.359535, 9.472166, 9.573384, 9.665293, 9.749461, 9.827092, 9.899128, 9.966321, 10.02928, 10.08851, 10.14443, 10.19738, 10.24767, 10.29556, 10.34125, 10.38495, 10.42682, 10.467, 10.50564, 10.54283, 10.57869, 10.61331, 10.64678, 10.67915, 10.71052, 10.74092, 10.77044, 10.7991, 10.82697, 10.85408, 10.88047, 10.90619, 10.93126, 10.95572, 10.97959, 11.00291, 11.0257, 11.04798, 11.06977, 11.0911, 11.11198, 11.13244, 11.15248, 11.17213, 11.19141, 11.21031, 11.22887, 11.24709, 11.26498, 11.28256, 11.29984, 11.31682, 11.33352, 11.34994, 11.3661, 11.382, 11.39766, 11.41307, 11.42824, 11.4432, 11.45793, 11.47244, 11.48675, 11.50086, 11.51477, 11.52849, 11.54202, 11.55538, 11.56855, 11.58156, 11.59442, 11.607121, 11.61964, 11.632, 11.64421, 11.65628, 11.6682, 11.67998, 11.69162, 11.70313, 11.71451, 11.72576, 11.73688, 11.74789, 11.75878, 11.76955, 11.7802, 11.79073, 11.80116, 11.81148, 11.8217, 11.83181, 11.84182, 11.85173, 11.86155, 11.87127, 11.88089, 11.89042, 11.89987, 11.90922, 11.91849, 11.92768, 11.93678, 11.94579, 11.95473, 11.96359, 11.97237, 11.98107, 11.9897, 11.99826, 12.00674, 12.01515, 12.02349, 12.03176, 12.03997, 12.0481, 12.05617, 12.06418, 12.07212, 12.08001, 12.08782, 12.09558, 12.10328, 12.11092, 12.1185, 12.12603, 12.1335, 12.14091, 12.14827, 12.15557, 12.16283, 12.17003, 12.17717, 12.18427, 12.19132, 12.19832, 12.20527, 12.21217, 12.21903, 12.22584, 12.2326, 12.23932, 12.24599, 12.25262, 12.2592, 12.26575, 12.27224),
    y = c(-34.834702, -34.3932, -34.152901, -33.979099, -33.845901, -33.732899, -33.640301, -33.5592, -33.486801, -33.4231, -33.365101, -33.313, -33.260899, -33.2174, -33.176899, -33.139198, -33.101601, -33.066799, -33.035, -33.003101, -32.971298, -32.942299, -32.916302, -32.890202, -32.864101, -32.841, -32.817799, -32.797501, -32.7743, -32.757, -32.733799, -32.7164, -32.6991, -32.678799, -32.6614, -32.644001, -32.626701, -32.612202, -32.597698, -32.583199, -32.568699, -32.554298, -32.539799, -32.525299, -32.510799, -32.499199, -32.487598, -32.473202, -32.461601, -32.435501, -32.435501, -32.4268, -32.4123, -32.400799, -32.392101, -32.380501, -32.366001, -32.3573, -32.348598, -32.339901, -32.3284, -32.319698, -32.311001, -32.2994, -32.290699, -32.282001, -32.2733, -32.264599, -32.256001, -32.247299, -32.238602, -32.2299, -32.224098, -32.215401, -32.2038, -32.198002, -32.1894, -32.183601, -32.1749, -32.169102, -32.1633, -32.154598, -32.145901, -32.140099, -32.131401, -32.125599, -32.119801, -32.111198, -32.1054, -32.096699, -32.0909, -32.088001, -32.0793, -32.073502, -32.067699, -32.061901, -32.056099, -32.050301, -32.044498, -32.038799, -32.033001, -32.027199, -32.0243, -32.018501, -32.012699, -32.004002, -32.001099, -31.9953, -31.9895, -31.9837, -31.9779, -31.972099, -31.969299, -31.963501, -31.957701, -31.9519, -31.9461, -31.9403, -31.937401, -31.931601, -31.9258, -31.922899, -31.917101, -31.911301, -31.9084, -31.902599, -31.8969, -31.893999, -31.888201, -31.8853, -31.882401, -31.8766, -31.873699, -31.867901, -31.862101, -31.8592, -31.8563, -31.8505, -31.8447, -31.841801, -31.8389, -31.833099, -31.8302, -31.827299, -31.8216, -31.818701, -31.812901, -31.809999, -31.8071, -31.8013, -31.798401, -31.7955, -31.7897, -31.7868),
    nota = "NIST certified model: y = b1*(b2+x)^(-1/b3) (magnetism vs. log[time in minutes], superconductivity study). Certified values: b1=-2523.5058043, b2=46.736564644, b3=0.93218483193 (RSS=0.00052404744073, 151 d.f.)."
  ),
  "nist_thurber" = list(
    nome = "NIST StRD \u2014 Thurber",
    fonte = "NIST Statistical Reference Datasets, Nonlinear Regression, dataset Thurber (source: Thurber, R., NIST, Semiconductor Electron Mobility Modeling). Higher Level of Difficulty, 37 observations. https://www.itl.nist.gov/div898/strd/nls/data/thurber.shtml",
    x = c(-3.067,-2.981,-2.921,-2.912,-2.840,-2.797,-2.702,-2.699,-2.633,-2.481,-2.363,-2.322,-1.501,-1.460,-1.274,-1.212,-1.100,-1.046,-0.915,-0.714,-0.566,-0.545,-0.400,-0.309,-0.109,-0.103,0.010,0.119,0.377,0.790,0.963,1.006,1.115,1.572,1.841,2.047,2.200),
    y = c(80.574,84.248,87.264,87.195,89.076,89.608,89.868,90.101,92.405,95.854,100.696,101.060,401.672,390.724,567.534,635.316,733.054,759.087,894.206,990.785,1090.109,1080.914,1122.643,1178.351,1260.531,1273.514,1288.339,1327.543,1353.863,1414.509,1425.208,1421.384,1442.962,1464.350,1468.705,1447.894,1457.628),
    nota = "NIST certified model: y = (b1 + b2*x + b3*x^2 + b4*x^3) / (1 + b5*x + b6*x^2 + b7*x^3) (semiconductor electron mobility vs. natural log of density). Certified values: b1=1288.1396800, b2=1491.0792535, b3=583.23836877, b4=75.416644291, b5=0.96629502864, b6=0.39797285797, b7=0.049727297349 (RSS=5642.7082397, 30 d.f.)."
  ),
  # --- Bates & Watts (1988), Nonlinear Regression Analysis and Its Applications ---
  # Data from the R package NRAIA (D.M. Bates; in the package the year is written 1998 by
  # mistake) and, for Puromycin and BOD, from R's base datasets. No values copied
  # from the book: where given, the reference values are an independent fit with nls().
  "bw_puromycin_treated" = list(
    nome = "Bates & Watts \u2014 Puromycin, treated cells",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.3. Originally from Treloar (1974), M.Sc. thesis, University of Toronto. Data from the R package NRAIA and R's base datasets. 12 observations; x = substrate concentration (ppm), y = initial reaction velocity (counts/min^2).",
    x = c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56, 1.10, 1.10),
    y = c(76, 47, 97, 107, 123, 139, 159, 152, 191, 201, 207, 200),
    nota = "Model used in the book (Michaelis-Menten): y = theta1*x/(theta2+x). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=212.68363 (SE 6.9471), theta2=0.064121105 (SE 0.0082809); RSS=1195.4488, 10 d.f."
  ),
  "bw_puromycin_untreated" = list(
    nome = "Bates & Watts \u2014 Puromycin, untreated cells",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.3. Originally from Treloar (1974), M.Sc. thesis, University of Toronto. Data from the R package NRAIA and R's base datasets. 11 observations; x = substrate concentration (ppm), y = initial reaction velocity (counts/min^2).",
    x = c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56, 1.10),
    y = c(67, 51, 84, 86, 98, 115, 131, 124, 144, 158, 160),
    nota = "Model used in the book (Michaelis-Menten): y = theta1*x/(theta2+x). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=160.27998 (SE 6.4802), theta2=0.047708081 (SE 0.0077819); RSS=859.60429, 9 d.f."
  ),
  "bw_bod" = list(
    nome = "Bates & Watts \u2014 BOD (biochemical oxygen demand)",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.4. Originally from Marske (1967), M.Sc. thesis, University of Wisconsin-Madison. Data from the R package NRAIA and R's base datasets. 6 observations; x = time (days), y = biochemical oxygen demand (mg/l).",
    x = c(1, 2, 3, 4, 5, 7),
    y = c(8.3, 10.3, 19.0, 16.0, 15.6, 19.8),
    nota = "Model used in the book: y = theta1*(1-exp(-theta2*x)). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=19.142582 (SE 2.4959), theta2=0.53109077 (SE 0.20308); RSS=25.990267, 4 d.f."
  ),
  "bw_bod2" = list(
    nome = "Bates & Watts \u2014 BOD2 (biochemical oxygen demand)",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A4.1. Originally from Marske (1967), M.Sc. thesis, University of Wisconsin-Madison. Data from the R package NRAIA. 8 observations; x = time (days), y = biochemical oxygen demand.",
    x = c(1, 2, 3, 4, 5, 7, 9, 11),
    y = c(0.47, 0.74, 1.17, 1.42, 1.60, 1.84, 2.19, 2.17),
    nota = "Model used in the book: y = theta1*(1-exp(-theta2*x)). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=2.4979207 (SE 0.10757), theta2=0.20245629 (SE 0.017984); RSS=0.026243673, 6 d.f."
  ),
  "bw_chloride" = list(
    nome = "Bates & Watts \u2014 Chloride ion transport",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A4.1. Originally from Sredni (1970), Ph.D. thesis, University of Wisconsin-Madison. Data from the R package NRAIA. 54 observations; x = time (min), y = chloride ion concentration in blood cells (%).",
    x = c(2.45, 2.55, 2.65, 2.75, 2.85, 2.95, 3.05, 3.15, 3.25, 3.35, 3.45, 3.55, 3.65, 3.75, 3.85, 3.95, 4.05, 4.15, 4.25, 4.35, 4.45, 4.55, 4.65, 4.75, 4.85, 4.95, 5.05, 5.15, 5.25, 5.35, 5.45, 5.55, 5.65, 5.75, 5.85, 5.95, 6.05, 6.15, 6.25, 6.35, 6.45, 6.55, 6.65, 6.75, 6.85, 6.95, 7.05, 7.15, 7.25, 7.35, 7.45, 7.55, 7.65, 7.75),
    y = c(17.3, 17.6, 17.9, 18.3, 18.5, 18.9, 19.0, 19.3, 19.8, 19.9, 20.2, 20.5, 20.6, 21.1, 21.5, 21.9, 22.0, 22.3, 22.6, 22.8, 23.0, 23.2, 23.4, 23.7, 24.0, 24.2, 24.5, 25.0, 25.4, 25.5, 25.9, 25.9, 26.3, 26.2, 26.5, 26.5, 26.6, 27.0, 27.0, 27.0, 27.0, 27.3, 27.8, 28.1, 28.1, 28.1, 28.4, 28.6, 29.0, 29.2, 29.3, 29.4, 29.4, 29.4),
    nota = "Model used in the book: y = theta1*(1-theta2*exp(-theta3*x)). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=39.094847 (SE 0.97442), theta2=0.82843696 (SE 0.0083012), theta3=0.15852293 (SE 0.010444); RSS=1.8793238, 51 d.f. The residuals show patterns (serial correlation), as discussed in the book."
  ),
  "bw_leaves" = list(
    nome = "Bates & Watts \u2014 Growth of leaves",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A4.5. Originally from Heyes and Brown (1956), in Milthorpe (ed.), The Growth of Leaves, Butterworth. Data from the R package NRAIA. 15 observations; x = time from initial emergence (days), y = leaf length (cm).",
    x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5),
    y = c(1.3, 1.3, 1.9, 3.4, 5.3, 7.1, 10.6, 16.0, 16.4, 18.3, 20.9, 20.5, 21.3, 21.2, 20.9),
    nota = "Model used in the book (Richards): y = theta1/(1+theta2*exp(-theta3*x))^(1/theta4). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=21.204035 (SE 0.4436), theta2=296.45623 (SE 542.68), theta3=0.77722057 (SE 0.17838), theta4=1.6189168 (SE 0.66545); RSS=5.5228835, 11 d.f. Note the very poorly determined theta2."
  ),
  "bw_lipo" = list(
    nome = "Bates & Watts \u2014 Lipoprotein concentrations",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A4.1. Originally from Anderson, D.H. (1983), Compartmental Modeling and Tracer Kinetics, Springer-Verlag. Data from the R package NRAIA. 12 observations; x = time since drug administration (hr), y = lipoprotein concentration.",
    x = c(0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0),
    y = c(46.10, 25.90, 17.00, 12.10, 7.22, 4.51, 3.19, 2.40, 1.82, 1.41, 1.00, 0.94),
    nota = "Data only. In the book these data are analysed with a three-compartment model (Anderson, 1983); see the text for the model and its solution. No reference fit is given here."
  ),
  "bw_rumford" = list(
    nome = "Bates & Watts \u2014 Count Rumford's cooling experiment",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.5. Original experiment by Count Rumford (1798), reported in Roller, D. (1950), Harvard University Press. Data from the R package NRAIA. 13 observations; x = time since the beginning of the experiment, y = temperature of the cannon (degrees F).",
    x = c(4.0, 5.0, 7.0, 12.0, 14.0, 16.0, 20.0, 24.0, 28.0, 31.0, 34.0, 37.5, 41.0),
    y = c(126, 125, 123, 120, 119, 118, 116, 115, 114, 113, 112, 111, 110),
    nota = "Model used in the book (Newton's law of cooling, with the initial and ambient temperatures fixed): y = 60 + 70*exp(-theta*x). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta=0.0094154966 (SE 0.00042009); RSS=44.155795, 12 d.f."
  ),
  "bw_sulfi" = list(
    nome = "Bates & Watts \u2014 Pharmacokinetics of sulfisoxazole",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.5. Originally from Kaplan, S.A., Weinfeld, R.E., Abruzzo, C.W. and Lewis, M. (1972), Pharmacokinetic profile of sulfisoxazole following intravenous, intramuscular, and oral administration to man, Journal of Pharmaceutical Sciences, 61(5), 773-778. Data from the R package NRAIA. 12 observations; x = time since drug administration, y = sulfisoxazole concentration (micrograms/ml).",
    x = c(0.25, 0.50, 0.75, 1.00, 1.50, 2.00, 3.00, 4.00, 6.00, 12.00, 24.00, 48.00),
    y = c(215.6, 189.2, 176.0, 162.8, 138.6, 121.0, 101.2, 88.0, 61.6, 22.0, 4.4, 0.1),
    nota = "Model used in the book (biexponential): y = theta1*exp(-theta2*x) + theta3*exp(-theta4*x). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=81.244056 (SE 6.1073), theta2=1.3059877 (SE 0.19754), theta3=162.59437 (SE 6.943), theta4=0.1617856 (SE 0.0087798); RSS=34.376471, 8 d.f."
  ),
  "bw_tetra" = list(
    nome = "Bates & Watts \u2014 Pharmacokinetics of tetracycline",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.5. Originally from Wagner, J.G. (1967), Use of computers in pharmacokinetics, Clinical Pharmacology and Therapeutics, 8(1, part 2), 201-218. Data from the R package NRAIA. 9 observations; x = time since drug administration (hr), y = tetracycline concentration.",
    x = c(1, 2, 3, 4, 6, 8, 10, 12, 16),
    y = c(0.7, 1.2, 1.4, 1.4, 1.1, 0.8, 0.6, 0.5, 0.3),
    nota = "Model used in the book (two-compartment model with lag time theta4): y = theta3*(exp(-theta1*(x-theta4)) - exp(-theta2*(x-theta4))). Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=0.148801 (SE 0.014409), theta2=0.71574983 (SE 0.12605), theta3=2.6496498 (SE 0.36447), theta4=0.41223554 (SE 0.094946); RSS=0.010045314, 5 d.f."
  ),
  "bw_saccharin" = list(
    nome = "Bates & Watts \u2014 Pharmacokinetics of saccharin",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.5. Originally from Renwick, A.G. (1982), Pharmacokinetics in toxicology, in Hayes, A.W. (Ed.), Principles and Methods of Toxicology, Raven Press. Data from the R package NRAIA (data set Saccharin). 9 observations; x = start of the collection interval (min), y = saccharin collected in the interval (micrograms).",
    x = c(0, 5, 15, 30, 45, 60, 75, 90, 105),
    y = c(7518, 6275, 4989, 2580, 1485, 861, 561, 363, 300),
    nota = "Model used in the book (empirical compartment model for amounts collected over intervals): y = (theta3/theta1)*exp(-theta1*x)*(1-exp(-theta1*dx)) + (theta4/theta2)*exp(-theta2*x)*(1-exp(-theta2*dx)), where dx is the length of the collection interval (5, 10, then 15 min). Since dx is determined by x for this collection schedule, it can be written inside the formula as dx = (x + 20 - abs(x - 10))/2, which gives exactly 5, 10, 15, ..., 15; the resulting single-predictor formula can be used as a custom model. Reference least-squares fit computed with R nls() in the book's parameterization (not values copied from the book): theta1=0.038707898 (SE 0.0017705), theta2=0.30506498 (SE 0.0355), theta3=762.9385 (SE 45.113), theta4=1578.2648 (SE 82.959); RSS=66189.127, 5 d.f."
  ),
  "bw_pcb" = list(
    nome = "Bates & Watts \u2014 PCB in Lake Cayuga trout",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.1. Originally from Bache, C.A., Serum, J.W., Youngs, W.D. and Lisk, D.J. (1972), Polychlorinated biphenyl residues: accumulation in Cayuga Lake trout with age, Science, 177(4055), 1191-1192. Data from the R package NRAIA. 28 observations; x = age of fish (years), y = PCB concentration (ppm).",
    x = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 11, 12, 12, 12),
    y = c(0.6, 1.6, 0.5, 1.2, 2.0, 1.3, 2.5, 2.2, 2.4, 1.2, 3.5, 4.1, 5.1, 5.7, 3.4, 9.7, 8.6, 4.0, 5.5, 10.5, 17.5, 13.4, 4.5, 30.4, 12.4, 13.4, 26.2, 7.4),
    nota = "Model used in the book: Bates and Watts transform both variables (x to its cube root, y to its natural logarithm) and fit the linear model ln(y) = beta1 + beta2*x^(1/3), which corresponds to y = exp(beta1 + beta2*x^(1/3)). Estimates reported in the book: beta1 = -2.391, beta2 = 2.300 (R lm(): beta1=-2.39073, SE 0.396; beta2=2.30047, SE 0.2295; RSS on the ln scale 6.39776, 26 d.f.). To reproduce them in this app, apply the Y pre-transformation ln(y) and fit beta1 + beta2*x^(1/3). Fitting y = exp(beta1 + beta2*x^(1/3)) directly on the original scale gives different estimates (R nls(): beta1=-1.40512, beta2=1.8719), because it assumes an additive rather than a multiplicative error."
  ),
  "bw_nitrite_day1" = list(
    nome = "Bates & Watts \u2014 Nitrite utilization by bean leaves, day 1",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.10. Originally from Elliot, J.R. and Pierson, D.R. (1986), private communication. Data from the R package NRAIA. 24 observations; x = light intensity, y = nitrite utilization.",
    x = c(2.2, 2.2, 2.2, 5.5, 5.5, 5.5, 9.6, 9.6, 9.6, 17.5, 17.5, 17.5, 27.0, 27.0, 27.0, 46.0, 46.0, 46.0, 94.0, 94.0, 94.0, 170.0, 170.0, 170.0),
    y = c(256, 685, 1537, 2148, 2583, 3376, 3634, 4960, 3814, 6986, 6903, 7636, 9884, 11597, 10221, 17319, 16539, 15047, 19250, 20282, 18357, 19638, 19043, 17475),
    nota = "Two models are suggested in the book: (1) y = theta1*x/(theta2+x+theta3*x^2); (2) y = theta1*(exp(-theta3*x)-exp(-theta2*x)). Reference least-squares fits computed with R nls() (not values copied from the book): model 1: theta1=89798.214 (SE 37281), theta2=186.61936 (SE 89.362), theta3=0.016252491 (SE 0.0091436), RSS=15789509, 21 d.f.; model 2: theta1=41964.04 (SE 21755), theta2=0.016506479 (SE 0.0048811), theta3=0.004034091 (SE 0.0021693), RSS=17701320, 21 d.f."
  ),
  "bw_nitrite_day2" = list(
    nome = "Bates & Watts \u2014 Nitrite utilization by bean leaves, day 2",
    fonte = "Bates, D.M. and Watts, D.G. (1988), Nonlinear Regression Analysis and Its Applications, Wiley, Appendix A1.10. Originally from Elliot, J.R. and Pierson, D.R. (1986), private communication. Data from the R package NRAIA. 24 observations; x = light intensity, y = nitrite utilization.",
    x = c(2.2, 2.2, 2.2, 5.5, 5.5, 5.5, 9.6, 9.6, 9.6, 17.5, 17.5, 17.5, 27.0, 27.0, 27.0, 46.0, 46.0, 46.0, 94.0, 94.0, 94.0, 170.0, 170.0, 170.0),
    y = c(549, 1550, 1882, 1888, 3372, 2362, 4561, 4939, 4356, 7548, 7471, 7642, 9684, 8988, 8385, 13505, 15324, 15430, 17842, 18185, 17331, 18202, 18315, 15605),
    nota = "Two models are suggested in the book: (1) y = theta1*x/(theta2+x+theta3*x^2); (2) y = theta1*(exp(-theta3*x)-exp(-theta2*x)). Reference least-squares fits computed with R nls() (not values copied from the book): model 1: theta1=50900.031 (SE 13855), theta2=103.52759 (SE 35.727), theta3=0.0078027356 (SE 0.0037074), RSS=16233083, 21 d.f.; model 2: theta1=27681.429 (SE 7466), theta2=0.020743397 (SE 0.0044216), theta3=0.0024821218 (SE 0.0013991), RSS=16272928, 21 d.f."
  ),
  "cr_esterase" = list(
    nome = "Carroll & Ruppert \u2014 Esterase assay (weighted regression)",
    fonte = "Carroll, R.J. and Ruppert, D. (1988), Transformation and Weighting in Regression, Chapman and Hall, Table 2.3 (pp. 46\u201347), Example 2.2 (pp. 48\u201351). Counts in a binding assay against the concentration of the enzyme esterase (proprietary data). Values transcribed from the page images and checked against the text layer of the PDF. The 5 observations marked by an asterisk in the book are excluded, as in the book's analysis: (6.1, 52), (5.6, 166), (3.1, 28), (11.0, 423), (11.2, 373); 108 observations remain. Reproduced from Carroll & Ruppert (1988) for educational and validation purposes; it will be removed on request of the rights holders.",
    x = c(6.4, 6.5, 6.7, 7.8, 8.0, 8.0, 8.1, 8.2, 8.4, 8.6, 8.6, 8.8, 9.0, 9.2, 9.5, 10.3, 10.5, 10.6, 10.8, 10.9, 11.1, 11.6, 11.6, 11.8, 12.1, 12.3, 12.6, 12.8, 12.8, 13.1, 13.1, 13.3, 13.3, 13.7, 13.8, 13.8, 13.9, 14.0, 14.1, 14.4, 14.6, 14.6, 14.6, 14.8, 15.0, 15.2, 15.2, 15.2, 15.8, 15.9, 16.0, 16.0, 16.1, 16.4, 16.9, 17.0, 17.1, 17.5, 17.7, 18.1, 18.8, 19.0, 19.2, 20.5, 20.5, 20.8, 20.8, 20.8, 20.9, 21.2, 21.4, 21.8, 21.8, 22.1, 23.0, 23.2, 23.5, 23.7, 23.8, 24.2, 24.4, 24.6, 25.2, 25.2, 25.5, 26.9, 27.2, 27.4, 27.7, 29.0, 29.0, 30.3, 30.8, 33.6, 35.2, 38.2, 38.6, 39.1, 40.5, 40.9, 41.2, 41.7, 44.5, 45.0, 46.6, 52.0, 52.1, 52.4),
    y = c(84, 85, 86, 127, 104, 107, 96, 130, 124, 105, 79, 153, 79, 100, 203, 159, 191, 93, 167, 100, 116, 97, 170, 131, 233, 256, 115, 219, 201, 215, 144, 268, 139, 249, 154, 226, 97, 329, 288, 255, 239, 317, 216, 266, 301, 193, 389, 278, 271, 250, 148, 103, 315, 256, 126, 137, 340, 136, 276, 342, 262, 486, 336, 354, 393, 459, 270, 260, 343, 474, 270, 416, 296, 317, 409, 376, 381, 466, 529, 412, 566, 369, 418, 531, 435, 472, 208, 646, 412, 595, 474, 527, 438, 635, 416, 695, 717, 1042, 695, 239, 597, 1006, 718, 778, 599, 789, 921, 697),
    nota = "Model used in the book: y = b0 + b1*x (linear), with severe heterogeneity of variance; the book uses the power-of-the-mean variance model with constant coefficient of variation (theta = 1), i.e. weighting w = 1/y^2 (zeta = 2 in section 5 of the sidebar). Reference values computed with R lm(), unweighted and with iteratively reweighted weights 1/fitted^2 (not values copied from the book): unweighted b0 = -17.41, b1 = 17.066; weighted (zeta = 2) b0 = -37.98 (SE 12.23), b1 = 18.180 (SE 0.947), sigma = 0.2772. Published values: unweighted b0 = -17.02, b1 = 17.05; weighted b0 = -37.36 (SE 12.21), b1 = 18.16 (SE 0.95), sigma = 0.27. The small differences are already present in the unweighted fit, which involves no weighting choice: the data file used by the authors probably differed slightly from the printed table."
  )

)

costruisci_scelte_dataset_riferimento <- function() {
  setNames(names(DATASET_RIFERIMENTO), sapply(DATASET_RIFERIMENTO, function(d) d$nome))
}

# Expandable box with the point-by-point explanation of the Distribution
# Density Transformation - same "click to expand" style already used for
# other informative notes of the app (e.g. Inverse Prediction vs Shelf-Life).
# Shared between Single Fit and Batch, which both call it here instead
# of duplicating its content in two places to be kept aligned.
# Expandable box with the point-by-point explanation of Warm Start and
# Auto-retry (Batch), same "click to expand" style as
# costruisci_box_aiuto_densita above.
costruisci_box_aiuto_retry <- function() {
  tags$details(
    style = "margin-bottom: 10px;",
    tags$summary(style = "cursor: pointer; color: #21618c; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                 "\u2139\ufe0f Warm Start & Auto-retry \u2014 how does this work? (click to expand)"),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; padding: 10px 15px; border-radius: 6px; margin-top: 6px; font-size: 12px; color: #21618c;",
        tags$strong("Warm Start"), br(),
        "Uses each sample's fitted result as the starting guess for the next sample, instead of the same initial guess every time. Can speed up convergence when samples are similar replicates of each other. Leave unchecked if samples are expected to differ substantially \u2014 starting from an unrelated optimum could occasionally hurt convergence instead of helping it.", br(), br(),
        
        tags$strong("Auto-retry with randomized guesses"), br(),
        "For any sample where the optimizer doesn't reach true convergence, or where it does but the resulting parameter standard errors come out as NA (a sign of an ill-conditioned fit \u2014 e.g. poorly identifiable or correlated parameters), or where R\u00b2/R\u00b2 Adjusted come out negative (meaning the fitted curve does worse than just predicting the mean of Y \u2014 a clear sign of a bad optimum even when the optimizer reports convergence), retries with new starting guesses \u2014 each parameter randomly rescaled by a power of 10 up to 1.5 orders of magnitude smaller or larger (narrower near a boundary if Adaptive retry range below is checked), keeping its sign and starting from whatever guess was used for that sample (its own initial guess, or the previous sample's result if Warm Start is also checked). Stops at the first attempt that passes all checks, up to the number of tries set. The search itself always uses the fast Linear (Hessian) method, ", tags$strong("even if Monte Carlo is selected above"), " \u2014 running the full bootstrap on every discarded attempt would waste most of its cost on results that get thrown away. If Monte Carlo is indeed selected, it runs once more at the end, on the accepted result only, to get its proper bootstrap uncertainty.", br(), br(),
        
        tags$strong("Adaptive retry range"), br(),
        "When a parameter's initial guess sits close to one of its boundaries (lower or upper, from \u201cApply built-in parameter boundaries\u201d above or a custom bound you've set), a plain \u00b11.5-order-of-magnitude perturbation wastes a growing share of retries \u2014 up to nearly half \u2014 on values that all get capped to the very same boundary instead of exploring new ground. When checked, the perturbation range narrows on the side where the boundary is close and shifts the freed room to the opposite side, keeping the same total search width whenever possible. Recommended on; only turn it off to reproduce the plain fixed \u00b11.5 behavior.", br(), br(),
        
        tags$strong("Max Acceptable Relative Standard Error"), br(),
        "An extra, optional acceptance criterion on top of the ones described above: for every parameter, its standard error must not exceed the chosen percentage of the absolute value of its own estimate \u2014 100% (the default) is the classic rough sign that a parameter isn't reliably identified (the estimate wouldn't even be one standard error away from zero); lower it for a stricter check, raise it for a looser one. Off by default: it's a stricter check that can end up discarding (and retrying) fits that are otherwise genuinely valid on noisy data, so enable it only if you specifically want to weed those out."
    )
  )
}

costruisci_box_aiuto_densita <- function() {
  tags$details(
    style = "margin-bottom: 10px;",
    tags$summary(style = "cursor: pointer; color: #21618c; font-weight: 600; font-size: 12px; padding: 3px 0; list-style: none;",
                 "\u2139\ufe0f Distribution Density Transformation \u2014 how does this work? (click to expand)"),
    div(style = "background-color: #eaf2f8; border: 1px solid #aed6f1; padding: 10px 15px; border-radius: 6px; margin-top: 6px; font-size: 12px; color: #21618c;",
        tags$strong("1. What this does"), br(),
        "For particle-size/sieve-analysis data: converts a discrete series (X = sieve aperture/class threshold; Y = quantity retained on that threshold, e.g. mass) into a proper distribution density (Y divided by its class width, then normalized so the area under the curve is 1) \u2014 the standard input needed to fit distributions like log-normal, Rosin-Rammler, or Gates-Gaudin-Schuhmann. Applied ", tags$strong("after"), " the Y Pre-transformation above it, so you can still take ln/sqrt/etc. of the resulting density afterwards if needed.", br(), br(),
        tags$strong("Sieve-stacking convention: "), "the quantity at a given X is treated as lying strictly between that X and the ", tags$strong("next larger"), " X in your data (never the smaller neighbor) \u2014 e.g. a sieve aperture of 125 paired with the next one up, 250, gives the true class [125, 250]. This works identically whether your classes follow a geometric progression (standard sieve series) or are irregularly spaced \u2014 it always uses the real spacing between your own data points, never an assumed fixed step.", br(), br(),
        tags$strong("Arithmetic vs. Logarithmic: "), "which scale the class width (and so the density's units) is computed on. Logarithmic is the usual choice for sieve series, but ", tags$strong("requires every class edge to be strictly positive"), " \u2014 a \u201cpan\u201d entered as X=0 (or any assumed boundary \u2264 0) is automatically dropped from the curve in that mode, since a class starting at zero has no finite width on a log scale; use Arithmetic instead if you need to keep it.", br(), br(),
        tags$strong("2. Bin Center"), br(),
        "Each class's X is replaced by the center of its true class \u2014 e.g. [125, 250] becomes 187.5 (arithmetic mean) or ~176.8 (geometric mean, often preferred for log-spaced/particle-size data) \u2014 never the raw sieve value itself, which is only an edge of the class, not its center.", br(), br(),
        tags$strong("3. Lower / Upper Boundary"), br(),
        "These two are ", tags$strong("not symmetric"), ". The largest X in your data has no larger sieve above it (the oversize/coarsest class), so its class can't be closed the same way as the others \u2014 you must either provide an assumed maximum particle size, or discard that point from the density curve entirely. The smallest X, by contrast, already works as its own lower edge with no assumption needed by default (e.g. a \u201cpan\u201d entered as 0 correctly gives a class starting at 0) \u2014 only override it if your smallest reading does ", tags$strong("not"), " represent a true physical floor and you want to signal that some material below it went unaccounted for.", br(), br(),
        tags$strong("4. Anchor points"), br(),
        "Adds one or two ", tags$strong("artificial"), " points (density = 0, not shown as measured data anywhere else) to pull the fitted curve toward zero beyond your data \u2014 useful with only a handful of real classes, where the measured points alone don't constrain the tails well. Not the same thing as Lower/Upper Boundary above: those only affect how the ", tags$strong("density of your real boundary points"), " gets computed, while anchor points add ", tags$strong("brand new points"), " further out.", br(), br(),
        tags$strong("Use sparingly: "), "with only a handful of real data points, one or two invented ones can carry real weight in an unweighted fit. They are also counted as observations in every statistic (number of points, degrees of freedom, standard errors, confidence intervals, R\u00b2, AIC and Monte Carlo), so the reported uncertainty is smaller than the real data alone would justify. (0, 0) is usually safe (no particle has exactly zero size). For the (X, 0) point, using the same X as an assumed Upper (or Lower) Boundary is a coherent choice, not a contradiction \u2014 your last real point is plotted at the ", tags$strong("center"), " of its class, not at the boundary edge itself (e.g. class [400, 500] is plotted at X=450), so it stays a real distance away from the anchor at X=500; and if the boundary states the material goes up to 500, an anchor saying density is zero exactly there is consistent with that statement, not in tension with it. For convenience, the anchor's X field auto-fills with the assumed boundary value the first time both are set (only if you haven't typed something else there yourself)."
    )
  )
}


# Choices of the Y pre-treatment menu, shared between single fit and Batch -
# in global scope (not inside server()) because it is also used by ui.R, which
# is evaluated before server() exists.
costruisci_scelte_trasformazione_y <- function() {
  c("None (raw values)" = "nessuna",
    "Natural Log \u2014 ln(y)" = "ln",
    "Natural Log +1 \u2014 ln(y+1)" = "ln1p",
    "Log10(y)" = "log10",
    "Log10(y+1)" = "log101p",
    "Square Root \u2014 sqrt(y)" = "sqrt",
    "Exponential \u2014 exp(y)" = "exp",
    "10^y" = "pow10",
    "Convert: already ln(y) \u2192 log10(y)" = "ln_a_log10",
    "Convert: already log10(y) \u2192 ln(y)" = "log10_a_ln")
}

# ---- Example datasets for Batch Elaboration (real data, no reference parameters) ----
# Real datasets for trying Batch, selected from the "Example Datasets" input method.
# Each dataset: samples (list of data frames x, y, named) and, if available, the
# temperatures of the samples (same structure as the Long format with a
# temperature column, used by Shelf-Life and Non-Isothermal). No automatic selection
# of the model and no reference parameters.
DATASET_BATCH_ESEMPIO <- list(
  "vb_riboflavin" = list(
    nome = "Riboflavin in spinach, heating at 50-120 \u00b0C",
    descrizione = "Isothermal and nonisothermal heating: riboflavin content of spinach (Spinacia oleracea L.) during heating at seven constant temperatures.",
    fonte = "Nisha, P., Singhal, R.S. and Pandit, A.B. (2005), A study on degradation kinetics of riboflavin in spinach (Spinacea oleracea L.), Journal of Food Engineering, 67(4), 407-412. Data as reported in van Boekel, M.A.J.S. (2008), Kinetic Modeling of Reactions in Foods, CRC Press.",
    unita = "x = heating time (min), y = riboflavin content (mg/100 g); one sample per temperature (\u00b0C).",
    campioni = list(
      "50 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.213, 0.212, 0.21, 0.209, 0.207, 0.205)),
      "60 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.213, 0.21, 0.208, 0.207, 0.204, 0.201)),
      "70 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.215, 0.213, 0.21, 0.207, 0.203, 0.198)),
      "80 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.214, 0.213, 0.209, 0.204, 0.201, 0.196)),
      "90 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.214, 0.209, 0.203, 0.197, 0.194, 0.189)),
      "100 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.206, 0.201, 0.194, 0.186, 0.182, 0.177)),
      "120 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(0.215, 0.202, 0.195, 0.187, 0.178, 0.172, 0.166))
    ),
    temperature = c("50 \u00b0C" = 50, "60 \u00b0C" = 60, "70 \u00b0C" = 70, "80 \u00b0C" = 80, "90 \u00b0C" = 90, "100 \u00b0C" = 100, "120 \u00b0C" = 120)
  ),
  "vb_ascorbic_acid" = list(
    nome = "Ascorbic acid in amla, heating at 50-120 \u00b0C",
    descrizione = "Isothermal and nonisothermal heating: ascorbic acid content of the fruit amla (Phyllanthus emblica L.) during cooking at seven constant temperatures.",
    fonte = "Nisha, P., Singhal, R.S. and Pandit, A.B. (2004), A study on degradation kinetics of ascorbic acid in amla (Phyllanthus emblica L.) during cooking, International Journal of Food Sciences and Nutrition, 55(5), 415-422. Data as reported in van Boekel, M.A.J.S. (2008), Kinetic Modeling of Reactions in Foods, CRC Press.",
    unita = "x = heating time (min), y = ascorbic acid content (mg/100 g); one sample per temperature (\u00b0C).",
    campioni = list(
      "50 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 660, 647, 632, 629, 623, 619)),
      "60 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 654, 632, 622, 616, 609, 604)),
      "70 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 647, 625, 613, 602, 595, 592)),
      "80 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 643, 616, 600, 585, 578, 573)),
      "90 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 610, 582, 573, 562, 556, 528)),
      "100 \u00b0C" = data.frame(x = c(0, 10, 20, 30, 40, 50, 60), y = c(686, 595, 560, 546, 539, 532, 504)),
      "120 \u00b0C" = data.frame(x = c(0, 5, 10, 15, 20), y = c(686, 629, 589, 569, 551))
    ),
    temperature = c("50 \u00b0C" = 50, "60 \u00b0C" = 60, "70 \u00b0C" = 70, "80 \u00b0C" = 80, "90 \u00b0C" = 90, "100 \u00b0C" = 100, "120 \u00b0C" = 120)
  )
)

costruisci_scelte_dataset_batch <- function() {
  setNames(names(DATASET_BATCH_ESEMPIO), sapply(DATASET_BATCH_ESEMPIO, function(d) d$nome))
}