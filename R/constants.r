# Error/Warning messages
messages <- list(
    missing_parameters = 'Error defining {dist} distribution, parameters missing from function call: {params}.',
    invalid_theta = 'Error defining cure model, cure fraction (theta) must be in range (0-1).',
    truncated_vector = 'Parameter {param} was length > 1 and only the first element will be used.',
    n_spline_params = 'Error defining restricted cubic spline distribution, must provide at least two parameter values followed by a matching number of knot times.',
    spline_param_type = 'Error defining restricted cubic spline distribution, parameter was of type "{class}" instead of "numeric".',
    spline_param_names = 'Error defining restricted cubic spline distribution, incorrect argument names were provided.'
)

# Possible values for distribution argument to flexsurvreg
flexsurv_dists <- c("exp", "weibull", "weibullPH", "lnorm", "llogis", "gamma", "gompertz", "gengamma", "gengamma.orig", "genf", "genf.orig")

# Display names for flexsurv distributions
flexsurv_dist_aliases <- list(
    exp = 'exponential',
    weibull = 'Weibull (AFT)',
    weibullPH = 'Weibull (PH)',
    llogis = 'log-logistic',
    lnorm = 'lognormal',
    gamma = 'gamma',
    gengamma = 'generalized gamma (stable)',
    gengamma.org = 'generalized gamma (original)',
    genf = 'generalized F (stable)',
    genf.orif = 'generalized F (original)',
    gompertz = 'Gompertz'
)

# Possible values for scale argument to flexsurvspline
flexsurv_spline_scales <- c("hazard", "odds", "normal")

flexsurv_spline_scale_aliases <- list(
    hazard = 'log cumulative hazard',
    odds = 'log cumulative odds',
    normal = 'inverse normal CDF'
)

# Default values for options
default_options <- list(
    herosurv.show_call_signature_in_errors = FALSE,
    herosurv.show_call_signature_in_warnings = FALSE
)

# Use 'an' instead of 'a' when a word starts with one of
# these letters
word_start_vowels <- c('a','e','i','o','u')