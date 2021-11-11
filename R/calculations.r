#' Evaluate Survival Probabilities
#' 
#' Generate survival probabilities for a survival distribution
#' at the specified times.
#' 
#' @name surv_prob
#' @rdname surv_prob
#' @aliases eval_surv
#' @export
#' 
#' @param x A `surv_dist` object
#' @param time A numeric vector of times
#' @param ... additional arguments passed to methods
#' 
#' @examples
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' surv_prob(dist1, c(0, 1, 2, 3))
#'   
#' @return A numeric vector of survival probabilities
surv_prob <- function(x, time, ...) {
    UseMethod("surv_prob", x)
}

#' @export
surv_prob.default <- function(x, time, ...) {
    err <- get_and_populate_message('surv_prob_wrong_type')
    stop(err, call. = show_call_error())
}


# Alias functions for backwards compatability with heRomod/heemod

#' @rdname surv_prob
#' @export
#' 
#' @tests
#' dist1 <- define_surv_param(distribution = "exp", rate = 0.05)
#' expect_equal(
#'  surv_prob(dist1, c(1,2,3,4)),
#'  eval_surv(dist1, c(1,2,3,4))
#' )
eval_surv <- function(x, time, ...) {
    surv_prob(x, time, ...)
}

#' Evaluate Event Probabilities
#' 
#' Generate the conditional probability of an even during an
#' interval of time.
#' 
#' @name event_prob
#' @rdname event_prob
#' @export
#' 
#' @param x A `surv_dist` object
#' @param start A numeric vector of interval start times
#' @param end A numeric vector of interval end times
#' @param ... additional arguments passed to methods
#' 
#' @examples
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' surv_prob(dist1, c(0, 1, 2, 3))
#'   
#' @tests
#' 
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' expect_equal(
#'  event_prob(dist1, c(0,1,2), c(1,2,3)),
#'  rep(1-pexp(1, rate = 0.12, lower.tail = FALSE), 3)
#' )
event_prob <- function(x, start, end, ...) {
    UseMethod("event_prob", x)
}

event_prob.default <- function(x, start, end, ...) {

    # Check survival distribution
    valid_dist <- is_surv_dist(x)
    if (!valid_dist) {
        err <- get_and_populate_message('event_prob_wrong_type')
        stop(err, call. = show_call_error())
    }

    # Check times
    check_times(start, 'calculating event probabilities', 'start')
    check_times(end, 'calculating event probabilities', 'end')

    # Calculate event probabilities
    surv_start <- surv_prob(x, start, ...)
    surv_end <- surv_prob(x, end, ...)
    e_prob <- (surv_start - surv_end) / surv_start
    e_prob[surv_start == 0] <- 1

    e_prob
}