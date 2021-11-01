#' Define a Parametric Survival Distribution
#' 
#' Define a parametric survival distribution with given
#' parameter values.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters
#' (see details section below)
#'   
#' @return A `surv_parametric` object.
#' @export
#' @rdname define_parametric_surv
#' @aliases define_survival
#' @details
#' Supported distributions are listed in the table below.
#' 
#' | **Distribution** | **Description** | **Parameters** | **Notes** |
#' | --- | --- |  --- | --- |
#' | "exp" | Exponential | rate | |
#' | "lnorm" | Lognormal | meanlog, sdlog | |
#' | "llogis" | Log-Logistic | shape, scale | |
#' | "weibull" | Weibull (AFT) | shape, scale | |
#' | "weibullPH" | Weibull (PH) | shape, scale | |
#' | "gompertz" | Gompertz | shape, rate | |
#' | "gamma" | Gamma | shape, scale | |
#' | "gengamma" | Generalized Gamma (stable) | mu, sigma, Q | Described in Prentice (1974) |
#' | "gengamma.orig" | Generalized Gamma (original) | shape, scale, k | Described in Stacy (1962) |
#' | "genf" | Generalized F (stable) | mu, sigma, Q, P | Described in Prentice (1975) |
#' | "genf.org" | Generalized F (original) | mu, sigma, s1, s2 | Described in Prentice (1975) |
#' 
#' @references Stacy, E. W. (1962). A generalization of the gamma
#' distribution.  Annals of Mathematical Statistics 33:1187-92.
#' 
#' Prentice, R. L. (1974). A log gamma model and its maximum likelihood
#' estimation. Biometrika 61(3):539-544.
#' 
#' R. L. Prentice (1975). Discrimination among some parametric
#' models. Biometrika 62(3):607-614.
#' 
#' @tests
#' 
#' dist1 <- define_survival(distribution = "exp", rate = 0.05)
#' expect_equal(class(dist1), c('surv_parametric', 'surv_dist'))
#' expect_equal(dist1$distribution, 'exp')
#' expect_equal(dist1$parameters, list(rate = 0.05))
#' 
#' expect_error(
#'  define_survival(distribution = "weibull", shape = 1.2),
#'  'Error defining Weibull (AFT) distribution, parameters missing from function call: "scale".',
#'  fixed = TRUE
#' )
#' 
#' 
#' @examples
#' 
#' define_survival(distribution = "exp", rate = 0.05)
#' define_parametric_surv(distribution = "exp", rate = .5)
#' define_parametric_surv(distribution = "gompertz", rate = .5, shape = 1)
#' 
define_parametric_surv <- function(distribution, ...) {

  args <- list(...)
  
  # Match distribution against list
  dist_string <- match.arg(distribution, choices = flexsurv_dists)

  # Run checks
  check_param_names(args, dist_string)

  # Extract params from arguments
  params <- get_dist_params_from_args(dist_string, args)

  # Return object
  create_list_object(
    c("surv_parametric", "surv_dist"),
    distribution = dist_string,
    parameters = args
  )
}

#' @export
#' @tests
#' 
#' surv_dist1 <- define_parametric_surv('weibull', shape = 1.2438, scale = 20.3984)
#' expect_output(
#'  print(surv_dist1),
#'  "A Weibull \\(AFT\\) distribution \\(shape = 1\\.24, scale = 20\\.40\\)\\."
#' )
#' 
#' surv_dist2 <- define_parametric_surv('exp', rate = 0.34)
#' expect_output(
#'  print(surv_dist2),
#'  "An exponential distribution \\(rate = 0\\.34)\\."
#' )
print.surv_parametric <- function(x, ...) {
    formatter <- create_param_formatter(...)
    dist_name <- get_dist_display_name(x$dist)
    indef_article <- str_to_title(get_indefinite_article(dist_name))
    param_string <- paste(
        paste(names(x$parameters), '=', formatter(as.numeric(x$parameters))),
        collapse = ', '
    )
    output <- glue('{indef_article} {dist_name} distribution ({param_string}).')

    cat(output)
}

#' @rdname surv_prob
#' @export
#' 
#' @tests
#' dist1 <- define_parametric_surv('exp', rate = 0.12)
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, 3)),
#'  c(1.0000000, 0.8869204, 0.7866279, 0.6976763),
#'  tolerance = 0.00001
#' )
#' 
#' dist1 <- define_parametric_surv('gengamma', mu = 2.321, sigma = 0.434, Q = -0.034)
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, 3)),
#'  c(1.0000000, 1.0000000, 0.9999393, 0.9979701),
#'  tolerance = 0.00001
#' )
surv_prob.surv_parametric <- function(x, time, ...) {

    # Collect extra arguments
    args <- list(...)

    # Get survival distribution function
    surv_dist <- get_flexsurv_dist(x$distribution)

    # Put together arguments for call to survival distribution
    args_for_surv_dist <- append(
        list(q = time, lower.tail = FALSE),
        x$parameters
    )

    # Call survival distribution function with arguments
    ret <- do.call(surv_dist, args_for_surv_dist)

    ret
}