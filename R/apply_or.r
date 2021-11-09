#' Apply Odds Ratio
#' 
#' Apply an odds ratio to proportionally increase or reduce
#' the odds of survival
#' 
#' @name apply_or
#' @rdname apply_or
#' @export
#' 
#' @param dist a survival distribution
#' @param or an odds ratio to be applied to survival distribution
#' @param log_or optional argument (defaults to `FALSE`) to indicate that
#' provided odds ratio is on log scale
#' @return A `surv_po` object.
#' 
#' @examples
#' 
#' dist1 <- define_surv_param("exp", rate = 0.25)
#' po_dist <- apply_or(dist1, 1.12)
#' @tests
#' dist1 <- define_surv_param("exp", rate = 0.25)
#' expect_equal(
#'  apply_or(dist1, 0.5),
#'  create_list_object(c('surv_po', 'surv_dist'), dist = dist1, or = 0.5)
#' )
#' expect_equal(
#'  apply_or(dist1, 0.5),
#'  apply_or(apply_or(dist1, 0.5), 1)
#' )
#' expect_equal(
#'  apply_or(dist1, 0.25),
#'  apply_or(apply_or(dist1, 0.5), 0.5)
#' )
#' expect_equal(
#'  apply_or(dist1, 0.5),
#'  apply_or(dist1, log(0.5), TRUE)
#' )
#' expect_error(
#'  apply_or('foo', 0.5),
#'  'Error applying odds ratio, invalid survival distribution provided.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_or(dist1, 'foo'),
#'  'Error applying odds ratio, "or" must be numeric.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_or(dist1, NA_real_),
#'  'Error applying odds ratio, "or" cannot be NA.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  apply_or(dist1, -2),
#'  'Error applying odds ratio, "or" must be greater than 0.',
#'  fixed = TRUE
#' )
apply_or <- function(dist, or, log_or = FALSE) {

    # Check that dist is a valid type
    is_surv_dist <- is_surv_dist(dist)
    if (!is_surv_dist) {
        err <- get_and_populate_message('apply_or_wrong_type_dist')
        stop(err, call. = show_call_error())
    }

    # Check that hr is numeric
    is_numeric <- any(c('integer', 'numeric') %in% class(or))
    if (!is_numeric) {
        err <- get_and_populate_message('apply_or_wrong_type_or')
        stop(err, call. = show_call_error())
    }

    # If log_or is specified then exponentiate it
    if (log_or) {
        or <- exp(or)
    }

    # Check that or isn't missing
    missing_or <- any(is.na(or))
    if (missing_or) {
        err <- get_and_populate_message('apply_or_missing_or')
        stop(err, call. = show_call_error())
    }

    or <- truncate_param('or', or)

    # If or equals one then noop
    if (or == 1) {
        return(dist)
    }

    # Check that or >= 0
    invalid_or <- or < 0
    if (invalid_or) {
        err <- get_and_populate_message('apply_or_invalid_or')
        stop(err, call. = show_call_error())
    }

    # If the baseline distribution is of type surv_po
    # then we can just multiply the acceleration factors.
    if (inherits(dist, 'surv_po')) {
        dist$or <- dist$or * or
        return(dist)
    }

    create_list_object(
        c('surv_po', 'surv_dist'),
        dist = dist,
        or = or
    )
}

#' @export
#' 
#' @tests
#' 
#' dist1 <- define_surv_param("exp", rate = 0.50)
#' dist2 <- apply_or(dist1, 0.5)
#' expect_equal(
#'  odds_to_prob(prob_to_odds(surv_prob(dist1, seq_len(100))) * 0.5),
#'  surv_prob(dist2, seq_len(100))
#' )
surv_prob.surv_po <- function(x, time, ...) {
    bl_prob <- surv_prob(x$dist, time)
    bl_odds <- prob_to_odds(bl_prob)
    odds <- bl_odds * x$or
    prob <- odds_to_prob(odds)
    prob
}

#' @export
#' 
#' @tests
#' dist1 <- apply_or(define_surv_param('exp', rate = 0.025), 0.5)
#' expect_output(
#'  print(dist1),
#'  'A proportional odds survival distribution:
#'   * Odds Ratio: 0.5
#'   * Baseline Distribution: An exponential distribution (rate = 0.025).',
#'  fixed = T
#' )
#' 
print.surv_po <- function(x, ...) {
    bl_dist_output <- to_list_item_output(x$dist)
    output <- paste0(
        c(
            'A proportional odds survival distribution:',
            glue('    * Odds Ratio: {x$or}'),
            glue('    * Baseline Distribution: {bl_dist_output}')
        ),
        collapse = '\n'
    )
    cat(output)
}