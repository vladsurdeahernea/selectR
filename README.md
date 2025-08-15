# caseselectR <img src="man/figures/logo.png" align="right" width="120" />

[![R-CMD-check](https://github.com/vladsurdeahernea/selectR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vladsurdeahernea/selectR/actions/workflows/R-CMD-check.yaml)
[![r-universe](https://vladsurdeahernea.r-universe.dev/badges/caseselectR)](https://vladsurdeahernea.r-universe.dev)
[![CRAN status](https://www.r-pkg.org/badges/version/caseselectR)](https://cran.r-project.org/package=caseselectR)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

---

## Overview

`caseselectR` provides **algorithmic case selection** tools for intensive within-case analysis in comparative politics, sociology, public policy, and related fields.

It implements:
- The **goal/strategy typology** from Gerring & Cojocaru (2016), and  
- The **quantitative selection procedures** from Seawright & Gerring (2008).

With a formula-first API (`Y ~ X | Z1 + Z2 + ...`), the package:
- Selects cases for **descriptive**, **causal-exploratory**, **causal-estimating**, and **causal-diagnostic** goals.
- Covers classic strategies: **typical**, **diverse**, **extreme**, **deviant**, **influential**, **most-similar**, **most-different**, **outcome**, **index**, **longitudinal**, **pathway**.
- Returns **ranked candidates**, **scores**, **plain-English rationales**, and a **7-step audit log** for transparency.

---

## Installation

**CRAN (after acceptance):**
```r
install.packages("caseselectR")
```

**Latest development build (no Rtools/Xcode needed via r-universe):**
```r
options(repos = c(
  vlad = "https://vladsurdeahernea.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
install.packages("caseselectR")
```

**From GitHub (requires build tools):**
```r
remotes::install_github("vladsurdeahernea/selectR")
```

## Core idea

The package separates what you're trying to learn from how you pick cases:
- **Goal** — descriptive / exploratory / estimating / diagnostic
- **Strategy** — typical, diverse, deviant, influential, most-similar, etc.
- **Algorithm** — concrete, reproducible scoring & ranking rules

Design safeguards:
- Estimating designs do not select on Y (outcome).
- Ties are handled with documented randomization (recorded seed).
- Every run generates an audit log of the 7 steps (question → sampling frame → measurement → model → algorithm → tie-break).

## Methodology map (what you may select on)

D = descriptive features; X = main independent variable; Z = covariates; Y = outcome.

| Goal | Strategy | Allowed variables | Function | What it does |
|------|----------|------------------|----------|--------------|
| **Descriptive** | Typical | D | `typical_case()` | Pick cases near the center (via smallest \|residual\| from Y ~ X (+ Z) or directly on D). |
| | Diverse | D | `diverse_cases()` | Cover range of D (quantiles or categories). |
| **Causal – Exploratory** | Outcome | Y | `outcome_cases()` | Choose tails/extremes of Y. |
| | Index | Y | `index_cases()` | Earliest first change in Y within units (panel). |
| | Deviant | X, Z, Y | `deviant_cases()` | Largest absolute model residuals. |
| | Most-Similar | Z, Y | `ms_cases_expl()` | Minimize Z-distance, maximize ΔY. |
| | Most-Different | Z, Y | `md_cases()` | Maximize Z-distance, minimize ΔY. |
| | Diverse | Z | `diverse_cases()` | Cover range/configurations of Z. |
| **Causal – Estimating** | Longitudinal | X, Z | `longitudinal_cases()` | Max ΔX, min ΔZ over time (panel). |
| | Most-Similar | Z, X | `ms_cases_est()` | Minimize Z-distance, maximize ΔX. |
| **Causal – Diagnostic** | Influential | X, Z, Y | `influential_cases()` | Highest Cook's distance (optionally add DFBETAs later). |
| | Pathway | X, Z, Y | `pathway_cases()` | Strong X→Y where Z is stable/"conservative" (proxy). |

**Tip:** If you're testing causal effects (estimating), avoid strategies that condition on Y (like `outcome_cases()` or `deviant_cases()`).

## Strategy explanations

| Function | Concept | Algorithmic rule |
|----------|---------|-----------------|
| `typical_case()` | Represents central tendency | Fit Y ~ X (+ Z); rank by smallest \|residual\|. |
| `deviant_cases()` | Least well-explained by model | Fit model; rank by largest \|residual\| (flag over/under). |
| `extreme_cases()` | Extremes on a variable | Rank by absolute Z-score of chosen axis (defaults to Y, else X). |
| `diverse_cases()` | Span the space | Pick across quantiles (continuous) or categories (categorical). |
| `influential_cases()` | Move the estimates a lot | Rank by Cook's distance; DFBETAs for X planned. |
| `ms_cases_expl()` | Similar controls, different outcomes | Mahalanobis distance on standardized Z; score = ΔY/(1+D<sub>Z</sub>). |
| `ms_cases_est()` | Similar controls, different treatment | Same, but ΔX/(1+D<sub>Z</sub>). |
| `md_cases()` | Different controls, similar outcomes | Score = D<sub>Z</sub>/(1+ΔY). |
| `outcome_cases()` | Outcome variation | Pick tails of Y (or balanced extremes). |
| `index_cases()` | First change event | Find the earliest ΔY within each unit; pick earliest across units. |
| `longitudinal_cases()` | Change with stability | By unit, score = ΔX/(1+ΔZ). |
| `pathway_cases()` | Mechanism clarity | Combine small residual under Z-only with small residual under X+Z; higher = clearer pathway. |

## Quick API (signatures & return)

| Function | Minimal signature | Returns |
|----------|------------------|---------|
| `select_cases()` | `data, formula, type, ...` | S3 `csel` with `$results`, `$spec`, `$model`, `$distances`, `$audit_log` |
| `typical_case()` | `(data, Y ~ X \| Z...)` | Top n cases (data columns + `.resid`, `score`, `why`) |
| `deviant_cases()` | `(data, Y ~ X \| Z...)` | Top n deviant cases |
| `extreme_cases()` | `(data, Y ~ X \| Z..., n=2)` | Extreme observations on chosen axis |
| `diverse_cases()` | `(data, Y ~ X \| Z..., n=3)` | Quantile/category picks |
| `influential_cases()` | `(data, Y ~ X \| Z..., n=1)` | Cases with highest influence (Cook's D) |
| `ms_cases_expl()` | `(data, Y ~ X \| Z..., n=1)` | Top matched pairs on Z with large ΔY |
| `ms_cases_est()` | `(data, Y ~ X \| Z..., n=1)` | Top matched pairs on Z with large ΔX |
| `md_cases()` | `(data, Y ~ X \| Z..., n=1)` | Top pairs: max D<sub>Z</sub>, min ΔY |
| `outcome_cases()` | `(data, Y ~ X \| Z..., n=2)` | Y tails |
| `index_cases()` | `(data, Y ~ X \| Z..., id, time)` | Earliest ΔY per panel, then earliest overall |
| `longitudinal_cases()` | `(data, Y ~ X \| Z..., id, time)` | Units: large ΔX, stable Z |
| `pathway_cases()` | `(data, Y ~ X \| Z...)` | Cases with strong X→Y & conservative Z proxy |

## Examples

```r
library(caseselectR)
data(toy_cross, package = "caseselectR")
data(toy_panel, package = "caseselectR")
```

### Typical vs. Deviant

```r
typ <- typical_case(toy_cross, Y ~ X | Z1 + Z2)
dev <- deviant_cases(toy_cross, Y ~ X | Z1 + Z2, n = 2)
typ$results
dev$results
```

Example (illustrative):

| unit | X | Z1 | Z2 | Y | .resid | score | why |
|------|---|----|----|---|--------|-------|-----|
| u17 | 0.12 | 0.04 | -0.80 | 0.14 | 0.010 | -0.010 | Small \|residual\|=0.010 (typical) |

### Most-Similar (Estimating)

```r
ms <- ms_cases_est(toy_cross, Y ~ X | Z1 + Z2, n = 3)
ms$results
```

Example:

| i | j | D<sub>Z</sub> | Delta | score | why |
|---|---|-------|--------|-------|-----|
| 4 | 15 | 0.305 | 1.20 | 0.92 | Most-similar on Z (D<sub>Z</sub>=0.305), maximize ΔX=1.20 |

### Longitudinal

```r
long <- longitudinal_cases(toy_panel, Y ~ X | Z1, id = "unit", time = "t", n = 2)
long$results
```

Example:

| unit | deltaX | deltaZ | score | why |
|------|---------|---------|-------|-----|
| g5 | 7.45 | 1.02 | 3.68 | Large ΔX with stable Z (longitudinal) |

## Built-in datasets

| Dataset | Type | Rows | Vars | Description |
|---------|------|------|------|-------------|
| `toy_cross` | cross-section | 40 | 5 | Simulated Y, X, Z1, Z2 for typical/deviant/matching demos |
| `toy_panel` | panel | 60 | 4 | Simulated Y, X, Z1 for longitudinal/index demos (10 units × 6 periods) |

Usage:
```r
data(toy_cross, package = "caseselectR")
data(toy_panel, package = "caseselectR")
```

## Transparency & audit logs

Every selector returns a 7-step audit log:

```r
res <- typical_case(toy_cross, Y ~ X | Z1 + Z2, seed = 123)
res$audit_log
```

Example (abbrev.):

| step | info | time |
|------|------|------|
| 1 | Defined question and population (user-provided). | 2025-08-15 10:00:00 |
| 2 | Constructed sampling frame from provided data. | … |
| 3 | Measured D/X/Z/Y from formula. | … |
| 5 | Fitted model for residuals. | … |
| 6 | Applied algorithm & tie-break. | … |

## FAQ

**Q: Can the package "choose the one true case" for me?**  
A: No. It proposes well-justified candidates; you remain in charge of theory and context.

**Q: Why can't I use `deviant_cases()` for estimating causal effects?**  
A: Estimating designs should not select on Y; use `ms_cases_est()` or `longitudinal_cases()` instead.

**Q: What if Z includes non-numeric variables?**  
A: Current Mahalanobis matching expects numeric Z; discretize or encode factors, or pair down to numeric covariates.

## Citing

```r
citation("caseselectR")
```

Please also cite:
- Gerring, J. & Cojocaru, L. (2016). Selecting Cases for Intensive Analysis: A Diversity of Goals and Methods. *Sociological Methods & Research*.
- Seawright, J. & Gerring, J. (2008). Case Selection Techniques in Case Study Research. *Political Research Quarterly*.

## Contributing

Issues and pull requests welcome! See [CONTRIBUTING.md](CONTRIBUTING.md).

Code of Conduct: [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## License

MIT © 2025 Vlad Surdea-Hernea
