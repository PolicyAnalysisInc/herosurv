#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution with given
#' parameter values.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A `surv_dist` object.
#' @export
#' 
#' @examples
#' 
#' define_survival(distribution = "exp", rate = .5)
#' define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' 
define_survival <- function(distribution, ...) {
    dist_string <- match.arg(distribution, choices = flexsurv_dists)
    err <- check_param_names(list(...), dist_string)
    if (!is.null(err)) {
        stop(err)
    }
    create_list_object(c("surv_object", "surv_dist"), distribution = dist_string, ...)
}

#' Define a Parametric Mixture or Non-Mixture Cure Distribution
#' 
#' Define a parametric cure survival model.
#' 
#' @param distribution A parametric survival distribution.
#' @param theta The model cure fraction.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#' @param mixture a logical determining whether a mixture
#'   or non-mixture model is being defined.
#'   
#' @return A `surv_dist_cure` object.
#' @export
#' 
#' @examples
#' 
#' define_survival_cure(distribution = "exp", theta = 0.34, rate = .5)
#' define_survival_cure(distribution = "weibull", theta = 0.5, shape = 1.5, scale = 34.43, mixture = TRUE)
#' 
define_survival_cure <- function(distribution, theta, ..., mixture = TRUE) {
    dist_string <- match.arg(distribution, choices = flexsurv_dists)
    errors <- list(
        check_param_names(list(...), dist_string),
        check_theta(theta)
    )
    for (err in errors) {
        if (!is.null(err)) {
            stop(err)
        }
    }
    create_list_object(
        c("surv_object", "surv_dist_cure"),
        distribution = dist_string,
        theta = theta,
        mixture = mixture,
        ...
    )
}

#' Define a Restricted Cubic Spline Survival Distribution
#' 
#' Define a restricted cubic spline parametric survival
#' distribution.
#' 
#' @param scale "hazard", "odds", or "normal", as described
#'   in flexsurvspline. With the default of no knots in
#'   addition to the boundaries, these models reduce to the
#'   Weibull, log-logistic and log-normal respectively. The
#'   scale must be common to all times.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A \code{surv_dist} object.
#'   
#' @examples
#' 
#' define_spline_survival(
#'   scale = "hazard", 
#'   gamma1 = -18.3122,
#'   gamma2 = 2.7511,
#'   gamma3 = 0.2292,
#'   knots1 = 4.276666,
#'   knots2 = 6.470800,
#'   knots3 = 7.806289
#'  )
# define_spline_survival(
#   scale = "odds",
#   gamma1 = -18.5809,
#   gamma2 = 2.7973,
#   gamma3 = 0.2035,
#   knots1 = 4.276666,
#   knots2 = 6.470800,
#   knots3 = 7.806289
# )
#' 
#' @export
# define_spline_survival <- function(scale, ...) {
  
#   scale <- match.arg(scale, choices = spline_scales)
#   list_arg <- lapply(list(...), unique)
#   n_param <- length(list_arg)
  
#   stopifnot(
#     all(unlist(lapply(list_arg, length)) == 1),
#     n_param >= 4,
#     n_param %% 2 == 0
#   )
  
  
#   if (! requireNamespace("flexsurv")) {
#     stop("'flexsurv' package required.")
#   }
  
#   pf <- flexsurv::unroll.function(
#     flexsurv::psurvspline,
#     gamma = seq_len(n_param/2),
#     knots = seq_len(n_param/2)
#   )
  
#   names_fun <- setdiff(names(list_arg), "scale")
#   names_par <- setdiff(names(formals(pf)), "q")
  
#   correct_names <- names_fun %in% names_par
  
#   if (! all(correct_names)) {
#     stop(sprintf(
#       "Incorrect argument%s: %s.",
#       plur(sum(! correct_names)),
#       paste(names_fun[! correct_names], collapse = ", ")))
#   }
  
#   structure(
#     list(
#       distribution = "survspline",
#       scale = scale,
#       ...
#     ),
#     class = c("surv_object", "surv_dist_spline")
#   )
# }