#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution.
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