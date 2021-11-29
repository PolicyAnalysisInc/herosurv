# herosurv
  
<!-- badges: start -->
[![CircleCI build status](https://circleci.com/gh/PolicyAnalysisInc/herosurv.svg?style=svg)](https://circleci.com/gh/PolicyAnalysisInc/herosurv)
[![Codecov test coverage](https://codecov.io/gh/PolicyAnalysisInc/herosurv/branch/master/graph/badge.svg)](https://codecov.io/gh/PolicyAnalysisInc/herosurv?branch=master)
<!-- badges: end -->

## Overview

herosurv is a grammar of survival modeling for use in health economic modeling. It provides a series of verbs that help you define, combine, and modify survival distributions.

## Installation

``` r
# Install latest version from github
install_github("PolicyAnalysisInc/herosurv")
```

## Defining Survival Distributions

- Parametric distributions with specified parameter values: `define_surv_param()`
- Parametric mixture & non-mixture cure models with specified parameter values: `define_surv_cure()`
- Royston & Parmar spline models with specified parameter values: `define_surv_spline()`
- Life-tables: `define_surv_lifetable()`
- Kaplan-Meiers based on product-limit table: `define_surv_km()`
- Full support for models estimated using [flexsurv](https://cran.r-project.org/web/packages/flexsurv/index.html) package
- Partial support for KMs & Cox Models estimated using [survival](https://cran.r-project.org/web/packages/survival/index.html) package

## Modifying Survival Distributions

- Apply hazard ratio: `apply_hr()`
- Apply odds ratio: `apply_or()`
- Apply acceleration factor: `apply_af()`
- Apply shift in time: `apply_shift()`
- Set covariate levels of models with covariates: `set_covaraiates()`

## Combining Survival Distributions

- Join survival distributions together at specified times: `join()`
- Mix together survival distributions with specified weights: `mix()`
- Combine distributions as independent risks: `add_hazards()`

## Generating Predicted Values

- Generate survival probabilities: `surv_prob()`
- Generate event probabilities: `event_prob()`