#' Apply Acceleration Factor
#' 
#' Apply an acceleration factor to proportionally increase or reduce
#' the time-to-event of a survival distribution.
#' 
#' @name apply_af
#' @rdname apply_af
#' @export
#' 
#' @param dist a survival distribution
#' @param af an acceleration factor to be applied to survival distribution
#' @param log_af optional argument (defaults to `FALSE`) to indicate that
#' provided acceleration factor is on log scale
#' @return A `surv_aft` object.
#' 
#' @examples
#' 
#' dist1 <- define_surv_param("exp", rate = 0.25)
#' aft_dist <- apply_af(dist1, 1.5)
#' @tests
#' dist1 <- define_surv_param("exp", rate = 0.25)
#' expect_equal(
#'  apply_af(dist1, 0.5),
#'  create_list_object(c('surv_aft', 'surv_dist'), dist = dist1, af = 0.5)
#' )
#' expect_equal(
#'  apply_af(dist1, 0.5),
#'  apply_af(apply_af(dist1, 0.5), 1)
#' )
#' expect_equal(
#'  apply_af(dist1, 0.25),
#'  apply_af(apply_af(dist1, 0.5), 0.5)
#' )
#' expect_equal(
#'  apply_af(dist1, 0.5),
#'  apply_af(dist1, log(0.5), TRUE)
#' )
#' expect_error(
#'  apply_af('foo', 0.5),
#'  'Error applying acceleration factor, invalid survival distribution provided.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_af(dist1, 'foo'),
#'  'Error applying acceleration factor, "af" must be numeric.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_af(dist1, NA_real_),
#'  'Error applying acceleration factor, "af" cannot be NA.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_af(dist1, -2),
#'  'Error applying acceleration factor, "af" must be greater than 0.',
#'  fixed = TRUE
#' )
apply_af <- function(dist, af, log_af = FALSE) {

    # Check that dist is a valid type
    is_surv_dist <- is_surv_dist(dist)
    if (!is_surv_dist) {
        err <- get_and_populate_message('apply_af_wrong_type_dist')
        stop(err, call. = show_call_error())
    }

    # Check that af is numeric
    is_numeric <- any(c('integer', 'numeric') %in% class(af))
    if (!is_numeric) {
        err <- get_and_populate_message('apply_af_wrong_type_af')
        stop(err, call. = show_call_error())
    }

    # If log_af is specified then exponentiate it
    if (log_af) {
        af <- exp(af)
    }

    # Check that af isn't missing
    missing_af <- any(is.na(af))
    if (missing_af) {
        err <- get_and_populate_message('apply_af_missing_af')
        stop(err, call. = show_call_error())
    }

    af <- truncate_param('af', af)

    # If af equals one then noop
    if (af == 1) {
        return(dist)
    }

    # Check that af >= 0
    invalid_af <- af < 0
    if (invalid_af) {
        err <- get_and_populate_message('apply_af_invalid_af')
        stop(err, call. = show_call_error())
    }

    # If the baseline distribution is of type surv_aft
    # then we can just multiply the acceleration factors.
    if (inherits(dist, 'surv_aft')) {
        dist$af <- dist$af * af
        return(dist)
    }

    create_list_object(
        c('surv_aft', 'surv_dist'),
        dist = dist,
        af = af
    )
}

#' @export
#' 
#' @tests
#' 
#' dist1 <- define_surv_param("exp", rate = 0.50)
#' dist2 <- define_surv_param("exp", rate = 0.25)
#' dist3 <- apply_af(dist1, 2)
#' dist4 <- apply_af(dist1, log(2), TRUE)
#' expect_equal(
#'  surv_prob(dist1, seq_len(100)),
#'  surv_prob(dist3, 2 * seq_len(100))
#' )
#' expect_equal(
#'  surv_prob(dist2, seq_len(100)),
#'  surv_prob(dist3, seq_len(100))
#' )
#' expect_equal(
#'  surv_prob(dist2, seq_len(100)),
#'  surv_prob(dist4, seq_len(100))
#' )
surv_prob.surv_aft <- function(x, time, ...) {
    surv_prob(x$dist, time / x$af)
}

#' @export
#' 
#' @tests
#' dist1 <- apply_af(define_surv_param('exp', rate = 0.025), 0.5)
#' expect_output(
#'  print(dist1),
#'  'An accelerated failure time distribution:
#'   * Acceleration Factor: 0.5
#'   * Baseline Distribution: An exponential distribution (rate = 0.025).',
#'  fixed = T
#' )
#' 
print.surv_aft <- function(x, ...) {
    bl_dist_output <- to_list_item_output(x$dist)
    output <- paste0(
        c(
            'An accelerated failure time distribution:',
            glue('    * Acceleration Factor: {x$af}'),
            glue('    * Baseline Distribution: {bl_dist_output}')
        ),
        collapse = '\n'
    )
    cat(output)
}