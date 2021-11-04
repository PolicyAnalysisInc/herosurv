

#' Define Royston & Parmar Spline Survival Distribution
#' 
#' Define Royston & Parmar restricted cubic spline parametric
#' survival distribution.
#' 
#' @name define_surv_spline
#' @rdname define_surv_spline
#' @export
#' 
#' @param scale "hazard", "odds", or "normal", as described
#' in flexsurvspline. With the default of no knots in
#' addition to the boundaries, these models reduce to the
#' Weibull, log-logistic and log-normal respectively. The
#' scale must be common to all times.
#' @param ... parameters and knot log times of spline distribution,
#' which can be provided either in order starting with spline
#' parameters followed by knot log times, or by names (e.g 
#' gamma1, gamma2, ... gammaN, knots1, knots2, ... knotsN). See
#' examples below for named and unnamed calls.
#'   
#' @return a \code{surv_spline} object.
#' 
#' @examples
#' 
#' define_surv_spline(
#'  scale = 'hazard',
#'  -2.08, 2.75, 0.23, # parameters
#'  -1.62, 0.57, 1.191 # knot times
#' )
#' 
#' @references Royston, P. and Parmar, M. (2002).  Flexible parametric
#' proportional-hazards and proportional-odds models for censored survival
#' data, with application to prognostic modelling and estimation of treatment
#' effects. Statistics in Medicine 21(1):2175-2197.
define_surv_spline <- function(scale, ...) {

  args <- list(...)
  
  # Match distribution against list
  scale_string <- match.arg(scale, choices = flexsurv_spline_scales)

  # Extract params from arguments
  params <- get_spline_params_from_args(args)

  # Return object
  create_list_object(
    c("surv_spline", "surv_dist"),
    scale = scale_string,
    parameters = params,
    k = length(params) / 2
  )

}

#' @tests
#' 
#' expect_equal(
#'  get_spline_params_from_args(list(1,2,3,4,5,c(6, 6))),
#'  list(
#'      gamma1 = 1, gamma2 = 2,
#'      gamma3 = 3, knots1 = 4,
#'      knots2 = 5, knots3 = 6
#'  )
#' )
#' expect_equal(
#'  get_spline_params_from_args(
#'      list(
#'          knots3 = 6, knots1 = c(4, 4),
#'          knots2 = 5, gamma1 = c(1,1,1),
#'          gamma3 = 3, gamma2 = 2
#'      )    
#'  ),
#'  list(
#'      gamma1 = 1, gamma2 = 2,
#'      gamma3 = 3, knots1 = 4,
#'      knots2 = 5, knots3 = 6
#'  )
#' )
#' expect_error(
#'  get_spline_params_from_args(
#'      list(
#'          knots3 = 6, knots1 = c(4, 4),
#'          knots2 = 5, gamma1 = c(1,1,1),
#'          gamma3 = 3, foo = 1
#'      )    
#'  ),
#'  'incorrect argument names were provided',
#'  fixed = T
#' )
get_spline_params_from_args <- function(args) {

    # Check parameters
    check_spline_params(args)

    # Determine what correct parameter names are
    len <- length(args)
    n_knots <- len / 2
    counting_seq <- seq_len(n_knots)
    param_names <- c(
        paste0('gamma', counting_seq),
        paste0('knots', counting_seq)
    )
    arg_names <- names(args)
    
    # Check if argument names were provided for parameters
    if (is.null(arg_names)) {

        # If not, populate them based on the order given assuming
        # that gamma1 - gammaN are provided first, followed by
        # knots1 - knotsN.
        ret <- set_names(
            map2(param_names, args, truncate_param),
            param_names
        )

    } else {
        
        # Otherwise populate them based on the provided names
        if (all(param_names %in% arg_names) && length(arg_names) == len) {
            ret <- set_names(
                map2(param_names, args[param_names], truncate_param),
                param_names
            )
        } else {
            # Throw error if the argument names don't match the expected
            # names.
            err <- get_and_populate_message('spline_param_names')
            stop(err, call. = show_call_error())
        }
    }

    ret
}

#' @tests
#' 
#' expect_error(
#'  check_spline_params(list(1,2,3,4,5)),
#'  'must provide at least two parameter values',
#'  fixed =T
#' )
#' 
#' expect_error(
#'  check_spline_params(list(1,2)),
#'  'must provide at least two parameter values',
#'  fixed =T
#' )
#' 
#' expect_error(
#'  check_spline_params(list(1,2,3,4,"foo", 6)),
#'  'parameter was of type "character" instead of "numeric"',
#'  fixed =T
#' )
check_spline_params <- function(args) {
    param_classes <- map_chr(args, class)
    wrong_class_index <- !param_classes %in% c('numeric', 'integer')
    # Check parameters are numeric
    if (any(wrong_class_index)) {
        bad_class <- param_classes[wrong_class_index][1]
        err <- get_and_populate_message('spline_param_type', class = bad_class)
        stop(err, call. = show_call_error())
    }

    # Check correct number of parameters
    len <- length(args)
    if (len < 4 || len %% 2 != 0) {
        err <- get_and_populate_message('n_spline_params')
        stop(err, call. = show_call_error())
    }

}


#' @export
#' @tests
#' dist1 <- define_surv_spline(
#'  scale = 'hazard',
#'  gamma1 = -2.08, gamma2 = 2.75, gamma3 = 0.23,
#'  knots1 = -1.62, knots2 = 0.57, knots3 = 1.191
#' )
#' expect_output(
#'  print(dist1),
#'  "A Royston & Parmar spline model of log cumulative hazard with 3 knots (gamma = [-2.08, 2.75, 0.23], knots = [-1.62, 0.57, 1.19]).",
#'  fixed = TRUE
#' )
print.surv_spline <- function(x, ...) {
    gamma <- as.numeric(x$parameters[seq_len(x$k)])
    knots <- as.numeric(x$parameters[x$k + seq_len(x$k)])
    formatter <- create_param_formatter(...)
    scale_name <- get_spline_scale_display_name(x$scale)
    gamma_str <- paste0(formatter(gamma), collapse = ', ')
    knot_str <- paste0(formatter(knots), collapse = ', ')
    output <- glue('A Royston & Parmar spline model of {scale_name} with {x$k} knots (gamma = [{gamma_str}], knots = [{knot_str}]).')

    cat(output)
}

#' @export
#' 
#' @tests
#' dist1 <- define_surv_spline(
#'  scale = 'hazard',
#'  gamma1 = -2.08, gamma2 = 2.75, gamma3 = 0.23,
#'  knots1 = -1.62, knots2 = 0.57, knots3 = 1.191
#' )
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, 3)),
#'  c(1.0000000, 0.9042421, 0.6387142, 0.3847163),
#'  tolerance = 0.00001
#' )
surv_prob.surv_spline <- function(x, time, ...) {

    # Collect extra arguments
    args <- list(...)

    # Get survival distribution function that will take arguments
    # in this format. This is done through "unrolling" psurvspline
    # from flexsurv package.
    param_seq <- seq_len(x$k)
    dist_func <- unroll.function(
        psurvspline,
        gamma = param_seq,
        knots = param_seq
    )

    # Assemble arguments to call to generic cure survial function
    arg_list <- append(
        list(time, scale = x$scale, lower.tail = FALSE),
        x$parameters
    )

    # Call generic cure survival function with arguments
    ret <- do.call(dist_func, arg_list)

    ret

}

#' @tests
#' expect_equal(
#'  get_spline_scale_display_name('hazard'),
#'  'log cumulative hazard'
#' )
#' expect_equal(
#'  get_spline_scale_display_name('odds'),
#'  'log cumulative odds'
#' )
#' expect_equal(
#'  get_spline_scale_display_name('blah'),
#'  'blah'
#' )
get_spline_scale_display_name <- function(name) {

    if (!name %in% names(flexsurv_spline_scale_aliases)) {
        return(name)
    }

    flexsurv_spline_scale_aliases[[name]]
}

#' @rdname define_surv_spline
#' @export
#'  
#' @tests
#' expect_equal(
#'  define_surv_spline(
#'      scale = 'hazard',
#'      gamma1 = -2.08, gamma2 = 2.75, gamma3 = 0.23,
#'      knots1 = -1.62, knots2 = 0.57, knots3 = 1.191
#'  ),
#'  define_spline_survival(
#'      scale = 'hazard',
#'      gamma1 = -2.08, gamma2 = 2.75, gamma3 = 0.23,
#'      knots1 = -1.62, knots2 = 0.57, knots3 = 1.191
#'  )
#' )
define_spline_survival <- function(scale, ...) {
    define_surv_spline(scale, ...)
}