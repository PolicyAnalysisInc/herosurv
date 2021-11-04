#' Define Parametric Survival Distribution
#' 
#' Define parametric survival distribution with given
#' parameter values. A complete listing of supported
#' distributions is provided in the details section.
#' 
#' @name define_surv_param
#' @export
#' @aliases define_survival
#' @rdname define_surv_param
#' 
#' @param distribution a parametric survival distribution.
#' @param ... additional distribution parameters
#' (see details section below)
#'   
#' @return a `surv_parametric` object.
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
#' dist1 <- define_surv_param(distribution = "exp", rate = 0.05)
#' expect_equal(class(dist1), c('surv_parametric', 'surv_dist'))
#' expect_equal(dist1$distribution, 'exp')
#' expect_equal(dist1$parameters, list(rate = 0.05))
#' 
#' expect_error(
#'  define_surv_param(distribution = "weibull", shape = 1.2),
#'  'Error defining Weibull (AFT) distribution, parameters missing from function call: "scale".',
#'  fixed = TRUE
#' )
#' 
#' 
#' @examples
#' 
#' define_surv_param(distribution = "exp", rate = .5)
#' define_surv_param(distribution = "gompertz", rate = .5, shape = 1)
#' 
#' # Deprecated alias included for backwards compatability with heRomod
#' define_survival(distribution = "exp", rate = 0.05)
#' 
define_surv_param <- function(distribution, ...) {

  args <- list(...)
  
  # Match distribution against list
  dist_string <- match.arg(distribution, choices = flexsurv_dists)

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
#' surv_dist1 <- define_surv_param('weibull', shape = 1.2438, scale = 20.3984)
#' expect_output(
#'  print(surv_dist1),
#'  "A Weibull (AFT) distribution (shape = 1.24, scale = 20.40).",
#'  fixed = TRUE
#' )
#' 
#' surv_dist2 <- define_surv_param('exp', rate = 0.34)
#' expect_output(
#'  print(surv_dist2),
#'  "An exponential distribution (rate = 0.34).",
#'  fixed = TRUE
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

#' @export
#' 
#' @tests
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, 3)),
#'  c(1.0000000, 0.8869204, 0.7866279, 0.6976763),
#'  tolerance = 0.00001
#' )
#' 
#' dist1 <- define_surv_param('gengamma', mu = 2.321, sigma = 0.434, Q = -0.034)
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

#' @tests
#' expect_equal(get_flexsurv_dist('weibull'), pweibull)
#' expect_equal(get_flexsurv_dist('genf'), pgenf)
#' expect_equal(get_flexsurv_dist('llogis'), pllogis)
get_flexsurv_dist <- function(dist_name) {
    get(paste0("p", dist_name))
}

#' @tests
#' expect_equal(
#'  get_flexsurv_dist_params('weibull'), c('shape', 'scale')
#' )
#' expect_equal(
#'  get_flexsurv_dist_params('gengamma'),
#'  c('mu', 'sigma', 'Q')
#' )
#' expect_equal(
#'  get_flexsurv_dist_params('genf'),
#'  c('mu', 'sigma', 'Q', 'P')
#' )
get_flexsurv_dist_params <- function(dist_name) {
    dist <- get_flexsurv_dist(dist_name)
    all_param_names <- names(formals(dist))
    dist_param_names <- setdiff(all_param_names, c('q', 'lower.tail', 'log.p'))

    dist_param_names
}

#' @tests
#' expect_equal(
#'  get_dist_params_from_args(
#'      'weibull',
#'      list(foo=1,shape=2,scale=c(3,3,3),bar=4)
#' ),
#'  list(shape=2,scale=3)
#' )
#' 
get_dist_params_from_args <- function(distribution, args) {

  # Run checks
  check_param_names(args, distribution)

  # Extract parameter names
  param_names <- get_flexsurv_dist_params(distribution)

  # Create named list with parameters
  params <- map(param_names, function(name) get_dist_param_from_args(name, args))
  names(params) <- param_names
  
  params
}

#' @tests
#' expect_equal(
#'  get_dist_param_from_args(
#'      'scale',
#'      list(foo=1,shape=2,scale=c(3,3,3),bar=4)
#' ),
#'  3
#' )
get_dist_param_from_args <- function(name, args) {

    values <- args[[name]]
    truncate_param(name, values)

}

#' @tests
#' 
#' expect_equal(
#'  get_dist_display_name('foo'),
#'  'foo'
#' )
#' 
#' expect_equal(
#'  get_dist_display_name('exp'),
#'  'exponential'
#' )
get_dist_display_name <- function(name) {
    if (!name %in% names(flexsurv_dist_aliases)) {
        return(name)
    }

    flexsurv_dist_aliases[[name]]
}

#' @export
#' @rdname define_surv_param
#' @tests
#' expect_equal(
#'  define_surv_param('lnorm', meanlog = 2.1, sdlog = 0.3),
#'  define_survival('lnorm', meanlog = 2.1, sdlog = 0.3)  
#' )
define_survival <- function(distribution, ...) {
    define_surv_param(distribution, ...)
}

#' @tests
#' expect_error(
#'  check_param_names(list(shape=1,foo=2), 'weibullPH'), 
#'  'Error defining Weibull (PH) distribution, parameters missing from function call: "scale".',
#'  fixed = T
#' )
#' 
check_param_names <- function(params, dist) {
    surv_func_params <- get_flexsurv_dist_params(dist)
    missing_params <- surv_func_params[!surv_func_params %in% names(params)]
    if (length(missing_params) > 0) {
        dist_str <- get_dist_display_name(dist)
        param_str <- quoted_list_string(missing_params)
        err <- get_and_populate_message(
            'missing_parameters',
            dist = dist_str,
            params = param_str
        )
        stop(err, call. = show_call_error())
    }
}