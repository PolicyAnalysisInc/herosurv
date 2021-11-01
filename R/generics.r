#' Evaluate Survival Probabilities
#' 
#' Generate survival probabilities for a survival distribution
#' at the specified times.
#' 
#' @aliases eval_surv
#' 
#' @param x A surv_dist object
#' @param time A numeric vector of times
#' @param ... additional arguments passed to methods.
#'   
#' @return A numeric vector of survival probabilities
#' @export
surv_prob <- function(x, time, ...) {
    UseMethod("surv_prob", x)
}