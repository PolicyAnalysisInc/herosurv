#' Define Parametric Mixture or Non-Mixture Cure Distribution
#' 
#' Define parametric mixture or not mixture cure distribution
#' with given parameter values.
#' 
#' @name define_surv_cure
#' @rdname define_surv_cure
#' @export
#' 
#' @param distribution A parametric survival distribution. See 
#' details for a listing of valid distributions.
#' @param theta A numeric value representing cure fraction
#' @param ... Additional distribution parameters
#' (see details section below)
#' @param mixture a logical determining whether a mixture
#'   or non-mixture model is being defined.
#'   
#' @return a `surv_dist_cure` object.
#' 
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
#' | "gengamma" | Generalized Gamma (stable) | mu, sigma, Q | Parameterization from Prentice (1974) |
#' | "gengamma.orig" | Generalized Gamma (original) | shape, scale, k | Original parameterization from Stacy (1962) |
#' | "genf" | Generalized F (stable) | mu, sigma, Q, P | Stable reparameterization from Prentice (1975) |
#' | "genf.org" | Generalized F (original) | mu, sigma, s1, s2 | Origninal parameterization described by Prentice (1975) |
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
#' @examples
#' 
#' define_surv_cure(distribution = "exp", theta = 0.34, rate = .5)
#' define_surv_cure(
#'  distribution = "weibull",
#'  theta = 0.5, shape = 1.5,
#'  scale = 34.43,
#'  mixture = TRUE
#' )
#' # Deprecated alias included for backwards compatability with heRomod
#' define_survival_cure(distribution = "exp", theta = 0.24, rate = 0.023)
#' 
define_surv_cure <- function(distribution, theta, ..., mixture = TRUE) {

  args <- list(...)

  # Match distribution against list
  dist_string <- match.arg(distribution, choices = flexsurv_dists)

  # Run checks
  check_param_names(args, dist_string)
  check_theta(theta)

  # Extract params from arguments
  params <- append(
    list(theta = truncate_param('theta', theta)),
    get_dist_params_from_args(dist_string, args)
  )

  # Return object
  create_list_object(
      c("surv_cure", "surv_dist"),
      distribution = dist_string,
      mixture = mixture,
      parameters = params
  )
}

#' @export
#' @tests
#' 
#' surv_dist1 <- define_surv_cure('weibull', theta = 0.21434, shape = 1.2438, scale = 20.3984, mixture = FALSE)
#' expect_output(
#'  print(surv_dist1),
#'  "A Weibull (AFT) non-mixture cure distribution (theta = 0.214, shape = 1.244, scale = 20.398).",
#'  fixed = T
#' )
#' 
#' surv_dist2 <- define_surv_cure('llogis', theta = 0.21434, shape = 1.2438, scale = 20.3984, mixture = TRUE)
#' expect_output(
#'  print(surv_dist2),
#'  "A log-logistic mixture cure distribution (theta = 0.214, shape = 1.244, scale = 20.398).",
#'  fixed = T
#' )
print.surv_cure <- function(x, ...) {
    formatter <- create_param_formatter(...)
    dist_name <- get_dist_display_name(x$dist)
    mix_str <- ifelse(x$mixture, 'mixture', 'non-mixture')
    indef_article <- str_to_title(get_indefinite_article(dist_name))
    param_string <- paste(
        paste(names(x$parameters), '=', formatter(as.numeric(x$parameters))),
        collapse = ', '
    )
    output <- glue('{indef_article} {dist_name} {mix_str} cure distribution ({param_string}).')

    cat(output)
}

#' @export
#' 
#' @tests
#' dist1 <- define_surv_cure('exp', theta = 0.2, rate = 0.05, mixture = TRUE)
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, Inf)),
#'  c(1.0000000, 0.9609835, 0.9238699, 0.2000000),
#'  tolerance = 0.00001
#' )
#' dist2 <- define_surv_cure('weibull', theta = 0.2, shape = 1.2, scale = 13.4, mixture = FALSE)
#' expect_equal(
#'  surv_prob(dist2, c(0, 1, 2, Inf)),
#'  c(1.0000000, 0.9324775, 0.8554689, 0.2000000),
#'  tolerance = 0.00001
#' )
surv_prob.surv_cure <- function(x, time, ...) {
    
    check_times(time, 'calculating survival probabilities', 'time')

    # Collect extra arguments
    args <- list(...)

    # Get baseline survival distribution function
    dist_func <- get_flexsurv_dist(x$distribution)

    # Get generic cure model survival function
    if (x$mixture) {
        generic_cure_func <- pmixsurv
    } else {
        generic_cure_func <- pnmixsurv
    }

    # Assemble arguments to call to generic cure survial function
    arg_list <- append(
        list(dist_func, time, lower.tail = FALSE),
        x$parameters
    )

    # Call generic cure survival function with arguments
    ret <- do.call(generic_cure_func, arg_list)

    ret
}

#' @rdname define_surv_cure
#' @export
#' 
#' @tests
#' expect_equal(
#'  define_surv_cure('weibull', theta = 0.41, shape = 1.04, scale = 10.2),
#'  define_survival_cure('weibull', theta = 0.41, shape = 1.04, scale = 10.2)
#' )
define_survival_cure <- function(distribution, theta, ..., mixture = TRUE) {
    define_surv_cure(distribution, theta, ..., mixture = mixture)
}

#' @tests
#' expect_error(check_theta(1), NA)
#' expect_error(check_theta(0.5), NA)
#' expect_error(check_theta(0), NA)
#' expect_error(
#'  check_theta(-0.01),
#'  'Error defining cure model, cure fraction (theta) must be in range [0-1].',
#'  fixed = T
#' )
#' expect_error(
#'  check_theta(1.01),
#'  'Error defining cure model, cure fraction (theta) must be in range [0-1].',
#'  fixed = T
#' )
check_theta <- function(theta) {
    if (any(theta > 1 | theta < 0)) {
        err <- get_and_populate_message('invalid_theta')
        stop(err, call. = show_call_error())
    }
}