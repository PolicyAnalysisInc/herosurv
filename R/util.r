#' @tests
#' expect_equal(get_surv_dist('weibull'), pweibull)
#' expect_equal(get_surv_dist('genf'), pgenf)
#' expect_equal(get_surv_dist('llogis'), pllogis)
get_surv_dist <- function(name) {
    get(paste0("p", name))
}

get_surv_dist_params <- function(name) {
    dist <- get_surv_dist(name)
    all_param_names <- names(formals(dist))
    dist_param_names <- setdiff(all_param_names, c('q', 'lower.tail', 'log.p'))

    dist_param_names
}

#' @tests
#' expect_error(check_param_names(list(shape=1,foo=2), 'weibullPH'), 'Error defining "weibull" distribution, parameters missing from function call: "scale".'
#' 
check_param_names <- function(params, dist) {
    surv_func_params <- get_surv_dist_params(dist)
    missing_params <- surv_func_params[!surv_func_params %in% names(params)]
    if (length(missing_params) > 0) {
        dist_str <- quoted_list_string(dist)
        param_str <- quoted_list_string(missing_params)
        res <- get_error_msg(
            'missing_parameters',
            dist = dist_str,
            params = param_str
        )
    } else {
        res <- NULL
    }

    res
}

#' @tests
#' expect_equal(class(create_list_object(c('a','b'), list())), c('a','b'))
#' 
create_list_object <- function(class, ...) {
    structure(list(...), class = class)
}

#' @tests
#' expect_equal(quoted_list_string(c('a','b','c')), '"a", "b", "c"')
#' 
quoted_list_string <- function(x) {
    paste0(paste0('"', x, '"'), collapse = ', ')
}