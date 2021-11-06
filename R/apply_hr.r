#' Apply a Hazard Ratio
#' 
#' Proportionally reduce or increase the hazard rate of a
#' distribution.
#' 
#' @param dist A survival distribution.
#' @param hr A hazard ratio to be applied.
#' @param log_hr If `TRUE`, the hazard ratio is exponentiated
#'   before being applied.
#'   
#' @return A `surv_ph` object.
#' @export
#' 
#' @examples
#' 
#' ph_dist <- apply_hr(
#'  define_surv_param("exp", rate = 0.25),
#'  0.5
#' )
#' 
#' @tests
#' dist1 <- define_surv_param("exp", rate = 0.25)
#' expect_equal(
#'  apply_hr(dist1, 0.5),
#'  create_list_object(c('surv_ph', 'surv_dist'), dist = dist1, hr = 0.5)
#' )
#' expect_equal(
#'  apply_hr(dist1, 0.5),
#'  apply_hr(apply_hr(dist1, 0.5), 1)
#' )
#' expect_equal(
#'  apply_hr(dist1, 0.25),
#'  apply_hr(apply_hr(dist1, 0.5), 0.5)
#' )
#' expect_equal(
#'  apply_hr(dist1, 0.5),
#'  apply_hr(dist1, log(0.5), TRUE)
#' )
#' expect_error(
#'  apply_hr('foo', 0.5),
#'  'Error applying hazard ratio, invalid survival distribution provided.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_hr(dist1, 'foo'),
#'  'Error applying hazard ratio, "hr" must be numeric.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_hr(dist1, NA_real_),
#'  'Error applying hazard ratio, "hr" cannot be NA.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_hr(dist1, -2),
#'  'Error applying hazard ratio, "hr" must be greater than 0.',
#'  fixed = TRUE
#' )
apply_hr <- function(dist, hr, log_hr = FALSE) {

    # Check that dist is a valid type
    is_surv_dist <- is_surv_dist(dist)
    if (!is_surv_dist) {
        err <- get_and_populate_message('apply_hr_wrong_type_dist')
        stop(err, call. = show_call_error())
    }

    # Check that hr is numeric
    is_numeric <- any(c('integer', 'numeric') %in% class(hr))
    if (!is_numeric) {
        err <- get_and_populate_message('apply_hr_wrong_type_hr')
        stop(err, call. = show_call_error())
    }

    # If log_hr is specified then exponentiate it
    if (log_hr) {
        hr <- exp(hr)
    }

    # Check that hr isn't missing
    missing_hr <- any(is.na(hr))
    if (missing_hr) {
        err <- get_and_populate_message('apply_hr_missing_hr')
        stop(err, call. = show_call_error())
    }

    hr <- truncate_param('hr', hr)

    # If hr equals one then noop
    if (hr == 1) {
        return(dist)
    }

    # Check that hr >= 0
    invalid_hr <- hr < 0
    if (invalid_hr) {
        err <- get_and_populate_message('apply_hr_invalid_hr')
        stop(err, call. = show_call_error())
    }

    # If the baseline distribution is of type surv_ph
    # then we can just multiply the hazard ratios.
    if (inherits(dist, 'surv_ph')) {
        dist$hr <- dist$hr * hr
        return(dist)
    }

    create_list_object(
        c('surv_ph', 'surv_dist'),
        dist = dist,
        hr = hr
    )
  
}

#' @export
#' 
#' @tests
#' 
#' dist1 <- define_surv_param("exp", rate = 0.50)
#' dist2 <- define_surv_param("exp", rate = 0.25)
#' dist3 <- apply_hr(dist1, 0.5)
#' dist4 <- apply_hr(dist1, log(0.5), TRUE)
#' expect_equal(
#'  surv_prob(dist2, seq_len(100)),
#'  surv_prob(dist3, seq_len(100))
#' )
#' expect_equal(
#'  surv_prob(dist2, seq_len(100)),
#'  surv_prob(dist4, seq_len(100))
#' )
surv_prob.surv_ph <- function(x, time, ...) {
    surv_prob(x$dist, time) ^ x$hr
}