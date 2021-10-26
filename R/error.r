errors <- list(
    missing_parameters = 'Error defining {dist} distribution, parameters missing from function call: {params}.'
)

get_error_msg <- function(type, ...) {
    print(list(...))
    error_msg <- str_glue_data(list(...), errors[[type]])
}