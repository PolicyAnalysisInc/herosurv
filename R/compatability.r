# Alias functions for backwards compatability with heRomod/heemod

#' @rdname surv_prob
eval_surv <- function(x, time, ...) {
    surv_prob(x, time, ...)
}

#' @export
#' 
#' @rdname define_parametric_surv
define_survival <- function(distribution, ...) {
    define_parametric_surv(distribution, ...)
}

#' @export
#' 
#' @rdname define_cure_surv
define_survival_cure <- function(distribution, theta, ..., mixture = TRUE) {
    define_cure_surv(distribution, theta, ..., mixture = mixture)
}