#' Define Kaplan-Meier Distribution
#' 
#' Define a survival distribution based on a table of Kaplan-Meier
#' output containing times and survival probabilities.
#' 
#' @name define_surv_km
#' @rdname define_surv_km
#' @export
#'
#' @param x a data frame with columns for time and survival probability. By
#' default, these columns are assumed to be named `time` and `survival`, but
#' these can be configured by via the optional parameters.
#' @param time_col the name of the time column (defaults to `time`)
#' @param surv_col the name of the time column (defaults to `survival`)
#'
#' @return a `surv_km` object.
#'
#' @examples
#' df <- data.frame(
#'      time = c(0, 1, 5, 10),
#'      survival = c(1, 0.9, 0.7, 0.5)
#' )
#' define_surv_km(df)
#'  
#' @tests
#' df <- data.frame(
#'      month = c(0, 1, 5, 10),
#'      p_surv = c(1, 0.9, 0.7, 0.5)
#' )
#' df2 <- data.frame(
#'      month = c(0, 1, 5, 10),
#'      p_surv = c('100%', '90%', '70%', '50%')
#' )
#' dist1 <- define_surv_km(df, 'month', 'p_surv')
#' dist2 <- define_surv_km(df[c(4,1,3,2), ], 'month', 'p_surv')
#' dist3 <- define_surv_km(mutate(df, time = month, survival = p_surv))
#' dist4 <- define_surv_km(df2, 'month', 'p_surv')
#' expect_equal(dist1, dist2)
#' expect_equal(dist1, dist3)
#' expect_equal(dist1, dist4)
#' 
#' expect_error(
#'  define_surv_km(data.frame()),
#'  'Error defining KM, the following columns were expected but not found: "time", "survival".',
#'   fixed = TRUE
#' )
#' expect_error(
#'  define_surv_km(
#'      mutate(df, p_surv = c(1, 0.9, 0.7, 0.9)), 'month', 'p_surv'
#'  ),
#'  'Error defining KM, column "p_surv" may not be increasing with respect to "month".',
#'   fixed = TRUE
#' )
#' expect_error(
#'  define_surv_km(
#'      mutate(df, month = c(0, 1, 1, 10)), 'month', 'p_surv'
#'  ),
#'  'Error defining KM, column "month" contained duplicate values.',
#'  fixed = TRUE
#' )
#' expect_error(
#'  define_surv_km(df[-1, ], 'month', 'p_surv'),
#'  fixed = TRUE,
#'  'Error defining KM, column "month" must start with a value 0 and "p_surv" must start with a value 1.'
#' )
#' expect_error(
#'  define_surv_km(mutate(df, p_surv = NA), 'month', 'p_surv'),
#'  fixed = TRUE,
#'  'Error defining KM, the following columns contained missing values: "p_surv".'
#' )
#' expect_error(
#'  define_surv_km(mutate(df, p_surv = "foo"), 'month', 'p_surv'),
#'  fixed = TRUE,
#'  'Error defining KM, the following columns were of invalid type: "p_surv".'
#' )
#' expect_error(
#'  define_surv_km(mutate(df, p_surv = 1.2), 'month', 'p_surv'),
#'  fixed = TRUE,
#'  'Error defining KM, values in column "p_surv" must be within the interval [0-1].'
#' )
define_surv_km <- function(x, time_col = 'time', surv_col = 'survival') {

    # Check for required column names
    required_names <- c(time_col, surv_col)
    names_present <- required_names %in% names(x)
    if(any(!names_present)){
        missing_cols <- required_names[!names_present]
        err <- get_and_populate_message(
            'km_missing_columns',
            missing_cols = quoted_list_string(missing_cols)
        )
        stop(err, call. = show_call_error())
    }

    # Check that columns contain no missing values
    missing_data_selector <- map_lgl(
        x[ ,required_names, drop = FALSE],
        function(x) any(is.na(x))
    )
    if (any(missing_data_selector)) {
        missing_value_cols <- required_names[missing_data_selector]
        err <- get_and_populate_message(
            'km_missing_values',
            missing_value_cols = quoted_list_string(missing_value_cols)
        )
        stop(err, call. = show_call_error())
    }
    
    # Parse survival column to percent, if possible
    x[[surv_col]] <- parse_percent_string_to_number(x[[surv_col]])

    # Check that columns are of correct type
    valid_classes <- c('integer', 'numeric')
    invalid_col_selector <- map_lgl(
        x[ ,required_names, drop = FALSE],
        function(x) !class(x) %in% valid_classes
    )
    if (any(invalid_col_selector)) {
        invalid_type_cols <- required_names[invalid_col_selector]
        err <- get_and_populate_message(
            'km_invalid_types',
            invalid_type_cols = quoted_list_string(invalid_type_cols)
        )
        stop(err, call. = show_call_error())
    }

    # Check that probabilities are valid
    invalid_probs <- any(x[[surv_col]] > 1 | x[[surv_col]] < 0)
    if (invalid_probs) {
        err <- get_and_populate_message(
            'km_invalid_prob',
            surv_col = surv_col
        )
        stop(err, call. = show_call_error())
    }

    # Sort times and check for duplicates
    x[[time_col]] <- as.numeric(x[[time_col]])
    x <- x[order(x[[time_col]]),]
    dup_time <- duplicated(x[[time_col]])
    if(any(dup_time)) {
        err <- get_and_populate_message(
            'km_dupe_time',
            time_col = time_col
        )
        stop(err, call. = show_call_error())
    }

    # Check that time starts at zero and survival at 1
    if(x[[time_col]][1] != 0 | x[[surv_col]][1] != 1) {
        err <- get_and_populate_message(
            'km_invalid_start',
            time_col = time_col,
            surv_col = surv_col
        )
        stop(err, call. = show_call_error())
    }

    # Check that survival doesn't increase
    increasing_survival <- diff(x[[surv_col]]) > 0
    if(any(increasing_survival)){
        err <- get_and_populate_message(
            'km_increasing_surv',
            time_col = time_col,
            surv_col = surv_col
        )
        stop(err, call. = show_call_error())
    }

    create_list_object(
        c('surv_km', 'surv_dist'),
        table = tibble(
            time = x[[time_col]],
            survival = x[[surv_col]]
        )
    )
}


#' @export
#' 
#' @tests
#' 
#' df <- data.frame(
#'      month = c(0, 1, 5, 10),
#'      p_surv = c(1, 0.9, 0.7, 0.5)
#' )
#' dist1 <- define_surv_km(df, 'month', 'p_surv')
#' expect_output(
#'      print(dist1),
#'      "A Kaplan-Meier distribution:",
#'      fixed = T
#' )
print.surv_km <- function(x, ...) {
    tibble_output <- paste0(capture.output(print(x$table))[-1], collapse = '\n')
    cat('A Kaplan-Meier distribution:', tibble_output, sep = '\n')
}

#' @export
#' @rdname define_surv_km
#' 
#' @tests
#' df <- data.frame(
#'      time = c(0, 1, 5, 10),
#'      survival = c(1, 0.9, 0.7, 0.5)
#' )
#' dist1 <- define_surv_table(df)
#' dist2 <- define_surv_km(df)
#' expect_equal(dist1, dist2)
define_surv_table <- function(x, time_col = 'time', surv_col = 'survival') {
    define_surv_km(x, time_col, surv_col)
}

#' @export
#' 
#' @tests
#' 
#' df <- data.frame(
#'      time = c(0, 1, 5, 10),
#'      survival = c(1, 0.9, 0.7, 0.5)
#' )
#' dist1 <- define_surv_table(df)
#' expect_equal(
#'  surv_prob(dist1, c(0, 0.99, 1, 1.01, 4.99, 5, 5.01, 11)),
#'  c(1, 1, 0.9, 0.9, 0.9, 0.7, 0.7, NA)
#' )
#' df2 <- data.frame(
#'      time = c(0, 1, 5, 10),
#'      survival = c(1, 0.9, 0.7, 0)
#' )
#' dist2 <- define_surv_table(df2)
#' expect_equal(
#'  surv_prob(dist2, c(0, 0.99, 1, 1.01, 4.99, 5, 5.01, 11)),
#'  c(1, 1, 0.9, 0.9, 0.9, 0.7, 0.7, 0)
#' )
surv_prob.surv_km <- function(x, time, ...) {

    check_times(time, 'calculating survival probabilities', 'time')

    # Create a numeric vector to store results
    len <- length(time)
    res <- vector(mode = 'numeric', length = len)
    
    # Determine the maximum time contained in KM table
    max_time <- max(x$table$time)

    goes_to_zero <- tail(x$table$survival, 1) == 0
    over_max_time <- time > max_time

    if (goes_to_zero) {
        # Set survival beyond max time to zero if KM goes to zero
        res[over_max_time] <- 0
    } else {
        # Otherwise set survival beyond max time to NA
        res[over_max_time] <- NA
    }

    # For other times, look it up using stepfun
    lookup_fun <- stepfun(x$table$time[-1], x$table$survival)
    res[!over_max_time] <- lookup_fun(time[!over_max_time])

    res
}

#' @export
plot.surv_km <- function(x, max_time, steps = 1000, ...) {
    df <- x$table
    ggplot(aes(x = time, y = survival), data = df) +
        geom_step() +
        coord_cartesian(xlim = c(0, max_time), ylim = c(0, 1)) +
        labs(x = 'Time', y = 'Survival')
}