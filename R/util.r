#' @tests
#' expect_equal(
#'  get_and_populate_message('missing_parameters', dist = 'a', params = 'b'),
#'  'Error defining a distribution, parameters missing from function call: b.'
#' )
get_and_populate_message <- function(type, ...) {
    error_msg <- str_glue_data(list(...), messages[[type]])
    as.character(error_msg)
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
#'  class(create_list_object(c('a','b'),
#'  list())), c('a','b')
#' )
#' 
create_list_object <- function(class, ...) {
    structure(list(...), class = class)
}

#' @tests
#' expect_equal(
#'  quoted_list_string(c('a','b','c')),
#'  '"a", "b", "c"'
#' )
#' 
quoted_list_string <- function(x) {
    paste0(paste0('"', x, '"'), collapse = ', ')
}

get_dist_params_from_args <- function(distribution, args) {

    param_names <- get_flexsurv_dist_params(distribution)
    params <- map(param_names, function(name) get_dist_param_from_args(name, args))
    names(params) <- param_names
    
    params
}

get_dist_param_from_args <- function(name, args) {

    values <- args[[name]]
    truncate_param(name, values)

}

truncate_param <- function(name, values) {

    unique_values <- unique(values)

    # Warn user if meaningful truncation will occur
    if (length(unique_values) > 1) {
        msg <- get_and_populate_message('truncated_vector', param = name)
        warning()
    }

    values[1]

}

show_call_error <- function() {
    nm <- 'herosurv.show_call_signature_in_errors'
    getOption(nm, default = default_options[[nm]]) 
}

show_call_warn <- function() {
    nm <- 'herosurv.show_call_signature_in_errors'
    getOption(nm, default = default_options[[nm]]) 
}

get_dist_display_name <- function(name) {
    if (!name %in% names(flexsurv_dist_aliases)) {
        return(name)
    }

    flexsurv_dist_aliases[[name]]
}

get_indefinite_article <- function(word) {
    if(substr(word, 0, 1) %in% word_start_vowels) {
        return('an')
    }

    'a'
}

create_param_formatter <- function(...) {
    args <- list(...)
    args$trim <- TRUE
    if (is.null(args$digits)) {
        args$digits <- 3
    }
    function(x) {
        format_args <- append(list(x), args)
        do.call(format, format_args)
    }
}