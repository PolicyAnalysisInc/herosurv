#' Mix Two or More Survival Distributions
#' 
#' Mix a set of survival distributions using the specified
#' weights.
#' 
#' @name mix
#' @rdname mix
#' @export
#' 
#' @param dist1 first survival distribution to mix
#' @param weight1 probability weight for first distribution
#' @param dist2 second survival distribution to mix
#' @param weight2 probability weight for second distribution
#' @param ... additional distributions and weights
#'   
#' @return A `surv_mix` object.
#' 
#' @examples
#' 
#' dist1 <- define_surv_param("exp", rate = .5)
#' dist2 <- define_surv_param("gompertz", rate = .5, shape = 1)
#' pooled_dist <- mix(dist1, 0.25, dist2, 0.75)
#' 
#' @tests
#' 
#' dist1 <- define_surv_param("exp", rate = .5)
#' dist2 <- define_surv_param("gompertz", rate = .5, shape = 1)
#' dist3 <- define_surv_param("weibull", shape = 1.2, scale = 20)
#' expect_equal(
#'  mix(dist1, 0.2, dist2, 0.3, dist3, 0.5),
#'  create_list_object(
#'      c('surv_mix', 'surv_combined', 'surv_dist'),
#'      dists = list(dist1, dist2, dist3),
#'      weights = c(0.2, 0.3, 0.5)
#'  )
#' )
#' expect_error(
#'  mix(dist1, 0.2, dist2, "foo"),
#'  'Error mixing distributions, weights must be numeric.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  mix(dist1, 0.2, "foo", 0.8),
#'  'Error mixing distributions, invalid survival distribution provided.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  mix(dist1, 0.2, dist2, NA_real_),
#'  'Error mixing distributions, weights cannot be NA.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  mix(dist1, 1.2, dist2, -0.2),
#'  'Error mixing distributions, weights must be in range [0-1].',
#'  fixed = TRUE
#' )
#' expect_error(
#'  mix(dist1, 0.5, dist2, 0.5, dist3),
#'  'Error mixing distributions, must provide an even number of arguments corresponding to n distributions and weights.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  mix(dist1, 0.4, dist2, 0.5),
#'  'Error mixing distributions, weights must sum to 1.',
#'  fixed = TRUE
#' )
mix <- function(dist1, weight1, dist2, weight2, ...) {
    
    dots <- list(...)
    n_args <- length(dots)
    extra_dists <- NULL
    extra_weights <- NULL

    # Check for right number of arguments
    if (n_args %% 2 != 0) {
        err <- get_and_populate_message('mix_wrong_n_args')
        stop(err, call. = show_call_error())
    }
    
    # Get extra arguments if provided
    if (n_args > 0) {
        extra_dists <- dots[seq(from = 1, to = n_args, by = 2)]
        extra_weights <- dots[seq(from = 2, to = n_args, by = 2)]
    }

    # Compile and check distributions
    dists <- append(list(dist1, dist2), extra_dists)
    walk(dists, function(x) {
        valid <- is_surv_dist(x)
        if (!valid) {
            err <- get_and_populate_message('mix_wrong_type_dist')
            stop(err, call. = show_call_error())
        }
    })

    # Compile and check weights
    weights <- imap_dbl(
        append(list(weight1, weight2), extra_weights),
        function(x, i) {

            # Check that cut is numeric
            is_numeric <- any(c('integer', 'numeric') %in% class(x))
            if (!is_numeric) {
                err <- get_and_populate_message('mix_wrong_type_weight')
                stop(err, call. = show_call_error())
            }

            # Check that weight isn't missing
            missing_weight <- any(is.na(x))
            if (missing_weight) {
                err <- get_and_populate_message('mix_missing_weight')
                stop(err, call. = show_call_error())
            }

            # Check that cut >= 0
            invalid_weight <- any(x < 0 | x > 1)
            if (invalid_weight) {
                err <- get_and_populate_message('mix_invalid_weight')
                stop(err, call. = show_call_error())
            }

            as.numeric(truncate_param(paste0('weight', i), x))
        }
    )

    if (all.equal(sum(weights), 1) != TRUE) {
        err <- get_and_populate_message('mix_weights_wrong_sum')
        stop(err, call. = show_call_error())
    }

    create_list_object(
        c('surv_mix', 'surv_combined', 'surv_dist'),
        dists = dists,
        weights = weights
    )

}

#' @export 
#' 
#' @tests
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' dist2 <- define_surv_param('exp', rate = 0.18)
#' expect_output(
#'  print(mix(dist1, 0.25, dist2, 0.75)),
#'  'A mixed survival distribution:
#'   * Distribution 1 (25%): An exponential distribution (rate = 0.12).
#'   * Distribution 2 (75%): An exponential distribution (rate = 0.18).',
#'  fixed = TRUE
#' )
print.surv_mix <- function(x, ...) {
    args <- list(...)
    if (is.null(args$digits)) {
        digits <- 3
    } else {
        digits <- args$digits
    }
    formatted_weights <- paste0(format(x$weights * 100, digits = digits), '%')
    items_lines <- map_chr(seq_along(x$dists), function(i) {
        dist_output <- to_list_item_output(x$dists[[i]])
        weight_str <- formatted_weights[i]
        glue('    * Distribution {i} ({weight_str}): {dist_output}')
    })
    output <- paste0(c('A mixed survival distribution:', items_lines), collapse = '\n')

    cat(output)
}

#' @export 
#' 
#' @tests
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' dist2 <- define_surv_param('exp', rate = 0.18)
#' expect_equal(
#'  surv_prob(mix(dist1, 0.25, dist2, 0.75), seq_len(100)),
#'  pexp(seq_len(100), rate = 0.12, lower.tail = FALSE) * 0.25 +
#'      pexp(seq_len(100), rate = 0.18, lower.tail = FALSE) * 0.75
#' )
surv_prob.surv_mix <- function(x, time, ...) {
    Reduce(`+`, map2(x$dists, x$weights, function(x, w) surv_prob(x, time, ...) * w))
}