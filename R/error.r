errors <- list(
    missing_parameters = 'Error defining {dist} distribution, parameters missing from function call: {params}.',
    invalid_theta = 'Error defining cure model, cure fraction (theta) must be in range (0-1).'
)

#' @tests
#' expect_equal(get_error_msg('missing_parameters', dist = 'a', params = 'b'), 'Error defining a distribution, parameters missing from function call: b.')
get_error_msg <- function(type, ...) {
    error_msg <- str_glue_data(list(...), errors[[type]])
    as.character(error_msg)
}