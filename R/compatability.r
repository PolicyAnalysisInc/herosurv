# Alias functions for backwards compatability with heRomod/heemod

#' @rdname surv_prob
#' @tests
#' 
#' dist1 <- define_parametric_surv(distribution = "exp", rate = 0.05)
#' expect_equal(
#'  surv_prob(dist1, c(1,2,3,4)),
#'  eval_surv(dist1, c(1,2,3,4))
#' )
eval_surv <- function(x, time, ...) {
    surv_prob(x, time, ...)
}

#' @export
#' 
#' @rdname define_parametric_surv
#' 
#' @tests
#' expect_equal(
#'  define_parametric_surv('lnorm', meanlog = 2.1, sdlog = 0.3),
#'  define_survival('lnorm', meanlog = 2.1, sdlog = 0.3)  
#' )
define_survival <- function(distribution, ...) {
    define_parametric_surv(distribution, ...)
}

#' @export
#' 
#' @rdname define_cure_surv
#' 
#' @tests
#' expect_equal(
#'  define_cure_surv('weibull', theta = 0.41, shape = 1.04, scale = 10.2),
#'  define_survival_cure('weibull', theta = 0.41, shape = 1.04, scale = 10.2)
#' )
define_survival_cure <- function(distribution, theta, ..., mixture = TRUE) {
    define_cure_surv(distribution, theta, ..., mixture = mixture)
}