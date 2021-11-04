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

#' @tests
#' 
#' expect_equal(
#'  truncate_param('foo', c(1)),
#'  1
#' )
#' 
#' expect_equal(
#'  truncate_param('foo', c(1,1,1)),
#'  1
#' )
#' 
#' expect_warning(
#'  truncate_param('foo', c(1,2,3)),
#'  'Parameter foo was length > 1 and only the first element will be used.',
#'  fixed = T
#' )
#' 
#' expect_equal(
#'  suppressWarnings(truncate_param('foo', c(1,2,3))),
#'  1
#' )
truncate_param <- function(name, values) {

    unique_values <- unique(values)

    # Warn user if meaningful truncation will occur
    if (length(unique_values) > 1) {
        msg <- get_and_populate_message('truncated_vector', param = name)
        warning(msg, call. = show_call_warn())
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


#' @tests
#' 
#' expect_equal(
#'  get_indefinite_article('fruit'),
#'  'a'
#' )
#' 
#' expect_equal(
#'  get_indefinite_article('apple'),
#'  'an'
#' )
get_indefinite_article <- function(word) {
    if(substr(word, 0, 1) %in% word_start_vowels) {
        return('an')
    }

    'a'
}

#' @tests
#' 
#' expect_equal(
#'  create_param_formatter(digits = 4)(0.1234567),
#'  "0.1235"
#' )
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

#' Convert Data Frame Factor Variables to Character
#' 
#' @param x A data frame.
#'   
#' @return A data frame.
#'   
#' @keywords internal
clean_factors <- function(x) {
  if (any(unlist(lapply(x, is.factor)))){
    for (i in seq_along(x)) {
      if (is.factor(x[[i]])) {
        x[[i]] <- as.character(x[[i]])
      }
    }
  }
  x
}