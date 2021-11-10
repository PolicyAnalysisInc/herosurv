#' Define Life-Table Distribution
#' 
#' Define a survival distribution based on a life-table containing mortality rates by age
#' and gender.
#' 
#' @name define_surv_lifetable
#' @rdname define_surv_lifetable
#' @export
#'
#' @param x a data frame with columns for the starting age in each age band, the
#' conditional probability of death at each age for men, and the conditional probability
#' of death at each for women. Default column names are "age", "male", and "female", but can
#' be set via optional arguments.
#' @param start_age starting age of the population.
#' @param percent_male percent of population that is male. Calculated as 1 - percent_female if not provided.
#' @param percent_female percent of population that is female. Calculated as 1 - percent_male if not provided.
#' @param output_unit optional arguemnt for time unit resulting survival distribution will be defined
#' in terms of. Valid options are `c("days", "weeks", "months", "years")`. Defaults to `"years"`.
#' @param age_col optional argument to change name of the `age` column accepted by the
#' first argument.
#' @param male_col optional argument to change name of the `male` column accepted by the
#' first argument.
#' @param female_col optional argument to change name of the `female` column accepted by the
#' first argument.
#' @param dpy optional argument specifying the number of days per year used in time unit
#' conversion. This argument will be populated automatically in a heromod model.
#'
#' @return a `surv_lifetable` object.
#'
#' @examples
#'  x <- data.frame(
#'      age = c(0, 1, 2, 3),
#'      male = c(0.011, 0.005, 0.003, 0.002),
#'      female = c(0.010, 0.005, 0.004, 0.002)
#'  )
#'  define_surv_lifetable(x, 1, 0.45)
#'  
#' @tests
#'  x <- data.frame(
#'      age = c(0, 1, 2, 3),
#'      male = c(0.011, 0.005, 0.003, 0.002),
#'      female = c(0.010, 0.005, 0.004, 0.002)
#'  )
#'  expect_equal(
#'      define_surv_lifetable(x, 1, 0.45)[-4],
#'      define_surv_lifetable(x[c(4,3,2,1), ], 1, 0.45)[-4]
#'  )
#'  expect_error(
#'      define_surv_lifetable(x, 1, 0.45, age_col = "foo", male_col = "bar"),
#'      'Error defining life-table, the following columns were expected but not found: "foo", "bar".'
#'  )
#'  expect_error(
#'      define_surv_lifetable(x, 1, 0.45, percent_female = 0.1),
#'      'Error defining life-table, must provide either "percent_male", or "percent_female", but not both.'
#'  )
#'  expect_error(
#'      define_surv_lifetable(x[c(1,1,2,3,4), ], 1, 0.45),
#'      'Error defining life-table, column "age" contained duplicate values.'
#'  )
#'  expect_error(
#'      define_surv_lifetable(x[c(1,3,4), ], 1, 0.45),
#'      'Error defining life-table, life-table must use constant age bands.'
#'  )
define_surv_lifetable <- function(x, start_age, percent_male, output_unit = "years", age_col = "age", male_col = "male", female_col = "female", dpy = get_dpy(), percent_female = NULL) {

    # Throw error if user doesn't provide exactly one of percent_male and percent_female
    gender_mix_properly_specified <- xor(!missing(percent_male), !is.null(percent_female))
    if (!gender_mix_properly_specified) {
            err <- get_and_populate_message('life_table_missing_gender_mix')
            stop(err, call. = show_call_error())
    }

    # Truncate vector arguments to handle unintended recycling in heRomod
    if (missing(percent_male)) {
        percent_male <- 1 - truncate_param('percent_female', percent_female)
    } else {
        percent_male <- truncate_param('percent_male', percent_male)
    }
    start_age <- truncate_param('start_age', start_age)
    output_unit <- truncate_param('start_age', output_unit)
    male_col <- truncate_param('male_col', male_col)
    age_col <- truncate_param('age_col', age_col)
    female_col <- truncate_param('female_col', female_col)

    # Check that lifetable dataframe has correct columns 
    required_names <- c(age_col, male_col, female_col)
    names_present <- required_names %in% names(x)
    if(any(!names_present)){
        missing_cols <- required_names[!names_present]
        err <- get_and_populate_message(
            'life_table_missing_columns',
            missing_cols = quoted_list_string(missing_cols)
        )
        stop(err, call. = show_call_error())
    }

    # Get the age column, sort it, and check for duplicates
    x[[age_col]] <- as.numeric(x[[age_col]])
    x <- as_tibble(x[order(x[[age_col]]),])
    dup_time <- duplicated(x[[age_col]])
    rownames(x) <- NULL
    if(any(dup_time)) {
        err <- get_and_populate_message(
            'life_table_dupe_age',
            age_col = age_col
        )
        stop(err, call. = show_call_error())
    }
    
    # Calculate the age bands
    agediffs <- diff(x[[age_col]])
    agediff <- agediffs[1]
    if(!all(agediffs == agediff)) {
        err <- get_and_populate_message('life_table_varying_bands')
        stop(err, call. = show_call_error())
    }
    
    # Calculate mortality rates by gender for each age band
    first_index <- tail(which(x[[age_col]] <= start_age), 1)
    first_used_age <- x[[age_col]][first_index]
    indices_to_use <- seq_len(nrow(x)) >= first_index
    cut_points <- x[indices_to_use, ][[age_col]] - start_age
    cut_points[1] <- 0
    lambdas_male <- (-log(1 - x[[male_col]]) / agediff)[indices_to_use]
    lambdas_female <- (-log(1 - x[[female_col]]) / agediff)[indices_to_use]
    
    # Create piecewise exponential distributions for male & female
    func_male <- function(time) ppexp(time,  rate = lambdas_male, t = cut_points, lower.tail = F)
    func_female <- function(time) ppexp(time,  rate = lambdas_female, t = cut_points, lower.tail = F)

    # Create a survival distribution representing mixture of male & female survival
    life_table_surv_func <- function(time) {
        converted_time <- time * time_in_days(output_unit, dpy) / dpy
        func_male(converted_time) * percent_male[1] + func_female(converted_time) * (1 - percent_male[1])
    }

    create_list_object(
        c('surv_lifetable', 'surv_dist'),
        start_age = start_age,
        percent_male = percent_male,
        output_unit = output_unit,
        surv_func = life_table_surv_func,
        days_per_year = dpy,
        first_used_row = first_index,
        table = transmute(
            x,
            age = .data[[age_col]],
            male = .data[[male_col]],
            female = .data[[female_col]]
        )
    )

}

#' @export
#' 
#' @tests
#' dist1 <- define_surv_lifetable(
#'  data.frame(age=c(1,2,3),male=c(0.01,0.01,0.01),female=c(0.009,0.009,0.009)),
#'  percent_male = 0.49,
#'  start_age=1
#' )
#' expect_output(
#'  print(dist1),
#'  'A life-table survival distribution (49.0% male, 51.0% female):',
#'  fixed = T
#' )
print.surv_lifetable <- function(x, ...) {
    start_age <- formatC(x$start_age, digits = 0, format = 'f')
    pct_male <- formatC(x$percent_male * 100, digits = 1, format = 'f')
    pct_female <- formatC((1 - x$percent_male) * 100, digits = 1, format = 'f')
    first_line <- glue(
        'A life-table survival distribution ({pct_male}% male, {pct_female}% female):'
    )
    rows_used <- x$table[seq_len(nrow(x$table)) >= x$first_used_row, ]
    tibble_output <- paste0(capture.output(print(rows_used, n = 5))[-1], collapse = '\n')

    cat(first_line, tibble_output, sep = '\n')
}

#' @export 
#' 
#' @tests
#' surv_lifetable_df <- data.frame(
#'  age = c(0, 1, 2, 3),
#'  male = c(0.011, 0.004, 0.003, 0.002),
#'  female = c(0.010, 0.005, 0.004, 0.002)
#' )
#' reg <- define_surv_lifetable(surv_lifetable_df, c(1,1,1), 0.5)     
#' expect_equal(
#'  surv_prob(reg, time = c(0, 0.5, 1, 1.5, 2, 3, 10)),
#'  c(1, 0.9977474, 0.9955000, 0.9937564, 0.9920160, 0.9900320, 0.9762544),
#'  tolerance = 1e-7
#' )
surv_prob.surv_lifetable <- function(x, time, ...) {
    x$surv_func(time)
}