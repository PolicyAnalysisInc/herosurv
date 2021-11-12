# Error/Warning messages
messages <- list(
    missing_parameters = 'Error defining {dist} distribution, parameters missing from function call: {params}.',
    invalid_theta = 'Error defining cure model, cure fraction (theta) must be in range [0-1].',
    truncated_vector = 'Parameter {param} was length > 1 and only the first element will be used.',
    n_spline_params = 'Error defining restricted cubic spline distribution, must provide at least two parameter values followed by a matching number of knot times.',
    spline_param_type = 'Error defining restricted cubic spline distribution, parameter was of type "{class}" instead of "numeric".',
    spline_param_names = 'Error defining restricted cubic spline distribution, incorrect argument names were provided.',
    model_no_covariates = 'Generating prediction from model with covariates but no covariates were provided. Predictions will reflect weighted average of predictions for subjects used to fit model.',
    life_table_missing_gender_mix = 'Error defining life-table, must provide either "percent_male", or "percent_female", but not both.',
    life_table_missing_columns = 'Error defining life-table, the following columns were expected but not found: {missing_cols}.',
    life_table_dupe_age = 'Error defining life-table, column "{age_col}" contained duplicate values.',
    life_table_varying_bands = 'Error defining life-table, life-table must use constant age bands.',
    km_missing_columns = 'Error defining KM, the following columns were expected but not found: {missing_cols}.',
    km_dupe_time = 'Error defining KM, column "{time_col}" contained duplicate values.',
    km_invalid_start = 'Error defining KM, column "{time_col}" must start with a value 0 and "{surv_col}" must start with a value 1.',
    km_increasing_surv = 'Error defining KM, column "{surv_col}" may not be increasing with respect to "{time_col}".',
    km_invalid_prob = 'Error defining KM, values in column "{surv_col}" must be within the interval [0-1].',
    km_invalid_types = 'Error defining KM, the following columns were of invalid type: {invalid_type_cols}.',
    km_missing_values = 'Error defining KM, the following columns contained missing values: {missing_value_cols}.',
    surv_prob_wrong_type = 'Error calculating survival probabilities, invalid survival distribution provided.',
    event_prob_wrong_type = 'Error calculating event probabilities, invalid survival distribution provided.',
    apply_hr_wrong_type_dist = 'Error applying hazard ratio, invalid survival distribution provided.',
    apply_hr_wrong_type_hr = 'Error applying hazard ratio, "hr" must be numeric.',
    apply_hr_missing_hr = 'Error applying hazard ratio, "hr" cannot be NA.',
    apply_hr_invalid_hr = 'Error applying hazard ratio, "hr" cannot be negative.',
    apply_af_wrong_type_dist = 'Error applying acceleration factor, invalid survival distribution provided.',
    apply_af_wrong_type_af = 'Error applying acceleration factor, "af" must be numeric.',
    apply_af_missing_af = 'Error applying acceleration factor, "af" cannot be NA.',
    apply_af_invalid_af = 'Error applying acceleration factor, "af" cannot be negative.',
    apply_or_wrong_type_dist = 'Error applying odds ratio, invalid survival distribution provided.',
    apply_or_wrong_type_or = 'Error applying odds ratio, "or" must be numeric.',
    apply_or_missing_or = 'Error applying odds ratio, "or" cannot be NA.',
    apply_or_invalid_or = 'Error applying odds ratio, "or" cannot be negative.',
    apply_shift_wrong_type_dist = 'Error applying shift, invalid survival distribution provided.',
    apply_shift_wrong_type_shift = 'Error applying shift, "shift" must be numeric.',
    apply_shift_missing_shift = 'Error applying shift, "shift" cannot be NA.',
    join_wrong_type_cut = 'Error joining distributions, cuts times must be numeric.',
    join_missing_cut = 'Error joining distributions, cuts times cannot be NA.',
    join_invalid_cut = 'Error joining distributions, cut times cannot be negative.',
    join_wrong_n_args = 'Error joining distributions, must provide an odd number of arguments corresponding to n distributions and n - 1 cut points.',
    join_wrong_type_dist = 'Error joining distributions, invalid survival distribution provided.',
    join_cuts_order = 'Error joining distributions, distributions and cutpoints must be provided in order.',
    set_covariates_wrong_type_dist = 'Error setting covariates, only survfit and flexsurv models are supported.',
    set_covariates_wrong_type_data = 'Error setting covariates, "data" must be provided as a data frame.',
    set_covariates_missing_data = 'Error setting covariates, must provide either "data" or named arguments for covariate values.',
    mix_wrong_type_weight = 'Error mixing distributions, weights must be numeric.',
    mix_missing_weight = 'Error mixing distributions, weights cannot be NA.',
    mix_invalid_weight = 'Error mixing distributions, weights cannot be negative.',
    mix_wrong_n_args = 'Error mixing distributions, must provide an even number of arguments corresponding to n distributions and weights.',
    mix_wrong_type_dist = 'Error mixing distributions, invalid survival distribution provided.',
    mix_weights_wrong_sum = 'Error mixing distributions, weights must sum to 1.',
    add_hazards_wrong_type_dist = 'Error adding hazards, invalid survival distribution provided.',
    check_time_wrong_class = 'Error {context}, "{time_name}" must be numeric.',
    check_time_negative = 'Error {context}, "{time_name}" cannot be negative.',
    check_time_missing = 'Error {context}, "{time_name}" cannot be NA.'
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