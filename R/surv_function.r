#' Define a survival distribution based on a function
#'
#' @param f a function that takes a vector of times and returns a vector
#' of corresponding survival probabilities
#' @param ... additional arguments to be passed to f
#'
#' @return a `surv_function` object.
#' 
#' @export
define_surv_function <- function(f, ...) {
  
  create_list_object(
    c("surv_function", "surv_dist"),
    func = f,
    args = list(...)
  )
  
}

#' @rdname surv_prob
#' @export
#' 
#' @tests
#' dist1 <- define_surv_function(
#'  pweibull,
#'  lower.tail = FALSE,
#'  shape = 1.2,
#'  scale = 20.1
#' )
#' expect_equal(
#'  surv_prob(dist1, c(0, 1, 2, 3)),
#'  c(1.0000000, 0.9730691, 0.9392071, 0.9030062),
#'  tolerance = 0.00001
#' )
surv_prob.surv_function <- function(x, time, ...) {

    args <- append(list(time), x$args)
    ret <- do.call(x$func, args)

    ret

}

#' @export
#' @tests
#' 
#' surv_dist1 <- define_surv_function(function(x) x+1, shape=1.2,scale=30.1,lower.tail = FALSE)
#' expect_output(
#'  print(surv_dist1),
#'  "A survival distribution based on a custom function.\n  Arguments: shape = 1.2, scale = 30.1, lower.tail = FALSE\n  Function: function(x) x+1",
#'  fixed = T
#' )
#' surv_dist2 <- define_surv_function(pweibull, shape=1.2,scale=30.1,lower.tail = FALSE)
#' fun <- function(x) x+1
#' expect_output(
#'  print(surv_dist2),
#'  "A survival distribution based on a custom function.\n  Arguments: shape = 1.2, scale = 30.1, lower.tail = FALSE\n  Function:\n    function (q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)",
#'  fixed = T
#' )
#' surv_dist3 <- define_surv_function(function(x) x+1, shape=1.2,scale=c(30.1, 30.2),lower.tail = FALSE)
#' expect_output(
#'  print(surv_dist3),
#'  "A survival distribution based on a custom function.\n  Arguments:\n    $shape\n    [1] 1.2",
#'  fixed = T
#' )
#' 
#' surv_dist4 <- define_surv_function(function(x) x+1)
#' expect_output(
#'  print(surv_dist4),
#'  "A survival distribution based on a custom function: function(x) x+1",
#'  fixed = T
#' )
print.surv_function <- function(x, ...) {
    args_str <- get_args_display_string(x$args)
    func_str <- get_function_display_string(x$func, x$args)
    output <- glue('A survival distribution based on a custom function{args_str}{func_str}', .trim = FALSE)
    cat(output)
}

get_args_display_string <- function(args) {
    all_args_scalar <- all(map_lgl(args, function(x) is.vector(x) && length(x) == 1))
    no_args <- length(args) == 0
    if (no_args) {
        args_str <- ''
    } else if (all_args_scalar) {
        args_str <- paste0(
            '.\n  Arguments: ',
            paste(
                paste0(
                    names(args),
                    ' = ',
                    as.character(args)
                ),
                collapse = ', '
            )
        )
    } else {
        list_output <- capture.output(print(args))
        args_indented_output <- paste0(
            paste0('    ', list_output),
            collapse = '\n'
        )
        args_str <- glue('.\n  Arguments:\n{args_indented_output}', .trim = FALSE)
    }

    args_str
}

get_function_display_string <- function(func, args) {

    # Print the function and capture the output. We'll include this
    # output to provide context on what the function is.
    func_output <- capture.output(print(func))

    # If the function comes from a package environment, then we'll
    # keep that in the output. Otherwise, we'll suppress that by
    # changing the environment to globalenv (which won't be printed)
    # and then print/capture the function again.
    func_from_package_env <- any(grepl('<environment: namespace:', func_output, fixed = T))
    if (!func_from_package_env) {
        environment(func) <- globalenv()
        func_output <- capture.output(print(func))
    }

    # Put together the string representing the lines that print out the
    # function
    no_args <- length(args) == 0
    func_output_lines <- length(func_output)
    if (func_output_lines == 1 && no_args) {
        # If no extra arguments are provided and the function fits on
        # a single line, then don't add a line break.
        func_str <- glue(': {func_output}', .trim = FALSE)
    } else if(func_output_lines == 1 ) {
        # If extra arguments are provided but the function takes multiple
        # lines, then use a line break, add a function header, and put 
        # the function inline with that header.
        func_str <- glue('\n  Function: {func_output}', .trim = FALSE)
    } else {
        # Otherwise, use a line break, add a function header, then put the function
        # indented on the lines below that header.
        func_indented_output <- paste0(paste0('    ', func_output), collapse = '\n')
        func_str <- glue('\n  Function:\n{func_indented_output}', .trim = FALSE)
    }

    func_str
}