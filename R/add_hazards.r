#' Add Hazards
#' 
#' Combine two or more survival distributions as independent risks
#' by adding their hazards.
#' 
#' @name add_hazards
#' @rdname add_hazards
#' @export
#' 
#' @param dist1 survival distribution to add
#' @param dist2 second survival distribution to add
#' @param ... additional survival distributions to add
#'   
#' @return a `surv_add_haz` object
#' 
#' @examples
#' 
#' dist1 <- define_surv_param("exp", rate = .125)
#' dist2 <- define_surv_param("weibull", shape = 1.2, scale = 50)
#' combined_dist <- add_hazards(dist1, dist2)
#' 
#' @tests
#' 
#' dist1 <- define_surv_param("exp", rate = .125)
#' dist2 <- define_surv_param("weibull", shape = 1.2, scale = 50)
#' dist3 <- define_surv_param("weibull", shape = 1.1, scale = 30)
#' expect_equal(
#'  add_hazards(dist1, dist2, dist3),
#'  create_list_object(
#'      c('surv_add_haz', 'surv_combined', 'surv_dist'),
#'      dists = list(dist1, dist2, dist3)
#'  )
#' )
#' expect_error(
#'  add_hazards(dist1, dist2, 'foo'),
#'  'Error adding hazards, invalid survival distribution provided.',
#'  fixed = TRUE
#' )
#' 
add_hazards <- function(dist1, dist2, ...) {
  
    # Compile and check distributions
    dists <- append(list(dist1, dist2), list(...))
    walk(dists, function(x) {
        valid <- is_surv_dist(x)
        if (!valid) {
            err <- get_and_populate_message('add_hazards_wrong_type_dist')
            stop(err, call. = show_call_error())
        }
    })

    create_list_object(
        c('surv_add_haz', 'surv_combined', 'surv_dist'),
        dists = dists
    )
}

#' @export
#' 
#' @tests
#' dist1 <- define_surv_param('exp', rate = 0.025)
#' dist2 <- define_surv_param('exp', rate = 0.010)
#' dist3 <- define_surv_param('exp', rate = 0.002)
#' dist4 <- define_surv_param('exp', rate = 0.035)
#' dist5 <- define_surv_param('exp', rate = 0.037)
#' expect_equal(
#'  surv_prob(dist4, seq_len(100)),
#'  surv_prob(add_hazards(dist1, dist2), seq_len(100))
#' )
#' expect_equal(
#'  surv_prob(dist5, seq_len(100)),
#'  surv_prob(add_hazards(dist1, dist2, dist3), seq_len(100))
#' )
surv_prob.surv_add_haz <- function(x, time, ...) {
    check_times(time, 'calculating survival probabilities', 'time')
    Reduce(`*`, map(x$dists, function(dist) surv_prob(dist, time, ...)))
}

#' @export 
#' 
#' @tests
#' dist1 <- define_surv_param('exp', rate = 0.12)
#' dist2 <- define_surv_param('exp', rate = 0.18)
#' expect_output(
#'  print(add_hazards(dist1, dist2)),
#'  'A survival distribution combining the hazards of:
#'   * Distribution 1: An exponential distribution (rate = 0.12).
#'   * Distribution 2: An exponential distribution (rate = 0.18).',
#'  fixed = TRUE
#' )
print.surv_add_haz <- function(x, ...) {
    args <- list(...)
    items_lines <- map_chr(seq_along(x$dists), function(i) {
        dist_output <- to_list_item_output(x$dists[[i]])
        glue('    * Distribution {i}: {dist_output}')
    })
    output <- paste0(c('A survival distribution combining the hazards of:', items_lines), collapse = '\n')

    cat(output)
}