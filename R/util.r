#' @tests
#' 
#' expect_equal(is_surv_dist(create_list_object('flexsurvreg')), TRUE)
#' expect_equal(is_surv_dist(create_list_object('tibble')), FALSE)
#' expect_equal(is_surv_dist(create_list_object(c('foo', 'surv_km'))), TRUE)
is_surv_dist <- function(x) {
  res <- try(suppressWarnings(surv_prob(x, 1)), silent = TRUE)
  is_error <- inherits(res, 'try-error')
  if (is_error) {
    no_generic <- grepl(
      'invalid survival distribution provided',
      res[1],
      fixed = TRUE
    )
    if (no_generic) {
      return(FALSE)
    }
  }

  TRUE
}


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
#' expect_error(
#'  check_times(c(0,1,2,3), '', ''),
#'  NA
#' )
#' expect_error(
#'  check_times("blah", 'foo', 'bar'),
#'  'Error foo, "bar" must be numeric.'
#' )
#' expect_error(
#'  check_times(c(0,1,-2,3), 'foo', 'bar'),
#'  'Error foo, "bar" cannot be negative.'
#' )
#' expect_error(
#'  check_times(c(0,1,NA_real_,3), 'foo', 'bar'),
#'  'Error foo, "bar" cannot be NA.'
#' )
check_times <- function(time, context, time_name) {

    # Check that time is correct type
    if (!inherits(time, c('numeric', 'integer'))) {
        err <- get_and_populate_message(
            'check_time_wrong_class',
            context = context,
            time_name = time_name
        )
        stop(err, call. = show_call_error())
    }

    # Check that times aren't missing
    if (any(is.na(time))) {
        err <- get_and_populate_message(
            'check_time_missing',
            context = context,
            time_name = time_name
        )
        stop(err, call. = show_call_error())
    }

    # Check that times aren't negative
    if (any(time < 0)) {
        err <- get_and_populate_message(
            'check_time_negative',
            context = context,
            time_name = time_name
        )
        stop(err, call. = show_call_error())
    }
    
}


#' @tests
#' expect_equal(
#'  parse_percent_string_to_number(c('45%', '1%', '0.3434%')),
#'  c(0.45, 0.01, 0.003434)
#' )
#' expect_equal(
#'  parse_percent_string_to_number(c('45%', '1%', '0.3434%', 'dffdf')),
#'  c('45%', '1%', '0.3434%', 'dffdf')
#' )
#' expect_equal(
#'  parse_percent_string_to_number(x(1, 2, 3)),
#'  c(1, 2, 3)
#' )
parse_percent_string_to_number <- function(x) {
  if (is.character(x)) {
    end_with_pct <- all(str_sub(x,-1,-1) == '%')
    if (end_with_pct) {
      num <- as.numeric(str_replace(x, '[%]$', ''))
      if (!any(is.na(num))) {
        return(num/100)
      }
    }
  }
  
  x
}