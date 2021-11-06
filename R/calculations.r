#' Evaluate Survival Probabilities
#' 
#' Generate survival probabilities for a survival distribution
#' at the specified times.
#' 
#' @rdname surv_prob
#' @aliases eval_surv
#' 
#' @param x A surv_dist object
#' @param time A numeric vector of times
#' @param ... additional arguments passed to methods.
#' 
#' @examples
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' surv_prob(dist1, c(0, 1, 2, 3))
#'   
#' @return A numeric vector of survival probabilities
#' @export
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