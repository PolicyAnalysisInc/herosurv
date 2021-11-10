#' Set Covariates of a Survival Model
#' 
#' Generate a survival distribution representing model predictions
#' for a specified cohort. The cohort can be defined by providing a
#' data frame of covariate values (for multiple subjects) or by providing
#' covariate values as named arguments (for a single subject).
#' 
#' @name set_covariates
#' @rdname set_covariates
#' @export
#' 
#' @param dist a survfit or flexsurvreg object
#' @param data a data.frame representing subjets for which predictions
#' will be generated
#' @param ... optional argument representing covariate values to generate
#' predictions for, can be used instead of data argument
#'   
#' @return a `surv_model` object
#' 
#' @examples
#' 
#' fs1 <- flexsurvreg(
#'   Surv(rectime, censrec)~group,
#'   data=flexsurv::bc,
#'   dist = "llogis"
#' )
#' good_model <- set_covariates(fs1, group = "Good")
#' cohort <- data.frame(group=c("Good", "Good", "Medium", "Poor"))
#' mixed_model <- set_covariates(fs1, data = cohort)
set_covariates <- function(dist, data, ...) {

    args <- list(...)
    n_args <- length(args)

    # Must provide data or covariate values as individual arguments but not both
    if ((!missing(data) && n_args > 0) || (missing(data) && n_args == 0)) {
        err <- get_and_populate_message('set_covariates_missing_data')
        stop(err, call. = show_call_error())
    }

    if (missing(data) && n_args > 0) {
        # Covariates provided as named arguments
        # Assemble and truncate covariate values
        covar_names <- names(args)
        covar_list <- set_names(
            map2(
                args,
                covar_names,
                function(x, nm) truncate_param(nm, x)
            ),
            covar_names
        )
        covar <- do.call(data.frame, covar_list)
    } else {
        # Covariates provided through data argument
        # Check that data is supported type
        if (!inherits(data, 'data.frame')) {
            err <- get_and_populate_message('set_covariates_wrong_type_data')
            stop(err, call. = show_call_error())
        }
        covar <- data
    }

    # Check that dist is a supported type
    supported_class <- inherits(dist, c('flexsurvreg', 'survfit'))
    if (!supported_class) {
        err <- get_and_populate_message('set_covariates_wrong_type_dist')
        stop(err, call. = show_call_error())
    }

    create_list_object(
        c('surv_model', 'surv_dist'),
        model = dist,
        covar = covar
    )
}

#' @export
#' 
#' @tests
#' 
#' fs <- flexsurvreg(Surv(rectime, censrec)~group, data = flexsurv::bc, dist = 'weibull')
#' model1 <- set_covariates(fs, group = 'Good')
#' expect_equal(
#'  surv_prob(model1, seq_len(100)),
#'  summary(fs, type = 'survival', t = seq_len(100), tidy = TRUE, newdata = data.frame(group = 'Good'))$est
#' )
#' fs <- flexsurvreg(Surv(rectime, censrec)~group, data = flexsurv::bc, dist = 'weibull')
#' model2 <- set_covariates(fs, data.frame(group = c('Good', 'Medium', 'Poor')))
#' expect_equal(
#'  surv_prob(model2, seq_len(100)),
#'  summary(fs, type = 'survival', t = seq_len(100), tidy = TRUE) %>%
#'      group_by(time) %>%
#'      summarize(est = mean(est)) %>% .$est
#' )
surv_prob.surv_model <- function(x, time, ...) {
    surv_prob(x$model, time, covar = x$covar, ...)
}

#' @export
#' 
#' @tests
#' 
#' fs <- flexsurvreg(Surv(rectime, censrec)~group, data = flexsurv::bc, dist = 'weibull')
#' model <- set_covariates(fs, group = 'Good')
#' expect_output(
#'  print(model),
#'  'Predicted survival from fitted model for specified cohort'
#' )
#' expect_output(
#'  print(model),
#'  'flexsurvreg'
#' )
print.surv_model <- function(x, ...) {
    model_output <- to_list_item_output(x$model)
    covar_output <- to_list_item_output(as_tibble(x$covar), skip = 1)

    output <- paste0(
        c(
            'Predicted survival from fitted model for specified cohort:',
            glue('    * Cohort: {covar_output}'),
            glue('    * Model: {model_output}')
        ),
        collapse = '\n'
    )
    cat(output)
}