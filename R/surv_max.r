#' Get the Maximum of Two or More Survival Distributions
#' 
#' Finds the maximum of n provided survival distributions
#' 
#' @param dist1 first survival distribution 
#' @param dist2 second survival distribution 
#' @param ... additional distributions
#' 
#' @return A `surv_max` object.
#' 
#' 
#' @name surv_max
#' @rdname surv_max
#' @export 
#' @tests
#'
#' dist1 <- define_surv_param(dist = 'weibull', shape = 1.2, scale = 1.3)
#' dist2 <- define_surv_cure(dist = 'weibull', theta = 0.2, shape = 1.2, scale = 1.3)
#'
#' expectedResult <- list(dists = list(dist1, dist2))
#' class(expectedResult) <- c('surv_max', 'surv_combined', 'surv_dist')
#'
#' expect_error(surv_max(), "Please define at least one survival distribution", fixed = TRUE)
#' expect_error(surv_max(5,5), "Survival distributions of index(s) 1, 2 are invalid", fixed = TRUE)
#' expect_equal(surv_max(dist1,dist2), expectedResult)
surv_max <- function(dist1, dist2, ...) {

    if (missing(dist1)) {
      stop("Please define at least one survival distribution")
    }

    if (missing(dist2)) {
      default_dists <- list(dist1)
    } else {
      default_dists <- list(dist1, dist2)
    }

    dots <- list(...)
    dists <- append(default_dists, dots)

    dist_validity <- map_lgl(dists, is_surv_dist)

    if (!all(dist_validity)) {
      errant_indicies <- which(!dist_validity)
      index_string <- paste(errant_indicies, collapse=', ')
      stop(glue("Survival distributions of index(s) {index_string} are invalid"))
    }

    create_list_object(
        c('surv_max', 'surv_combined', 'surv_dist'),
        dists = dists
    )
}