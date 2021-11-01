#' @tests
#' expect_error(
#'  check_param_names(list(shape=1,foo=2), 'weibullPH'), 
#'  'Error defining Weibull (PH) distribution, parameters missing from function call: "scale".',
#'  fixed = T
#' )
#' 
check_param_names <- function(params, dist) {
    surv_func_params <- get_flexsurv_dist_params(dist)
    missing_params <- surv_func_params[!surv_func_params %in% names(params)]
    if (length(missing_params) > 0) {
        dist_str <- get_dist_display_name(dist)
        param_str <- quoted_list_string(missing_params)
        err <- get_and_populate_message(
            'missing_parameters',
            dist = dist_str,
            params = param_str
        )
        stop(err, call. = show_call_error())
    }
}

#' @tests
#' expect_error(check_theta(1), NA)
#' expect_error(check_theta(0.5), NA)
#' expect_error(check_theta(0), NA)
#' expect_error(
#'  check_theta(-0.01),
#'  'Error defining cure model, cure fraction (theta) must be in range (0-1).',
#'  fixed = T
#' )
#' expect_error(
#'  check_theta(1.01),
#'  'Error defining cure model, cure fraction (theta) must be in range (0-1).',
#'  fixed = T
#' )
check_theta <- function(theta) {
    if (any(theta > 1 | theta < 0)) {
        err <- get_and_populate_message('invalid_theta')
        stop(err, call. = show_call_error())
    }
}