#' @export
#' 
#' @tests
#' surv_dist1 <- flexsurvreg(Surv(rectime, censrec)~1, data = flexsurv::bc, dist = 'weibull')
#' surv_dist2 <- define_surv_param('weibull', shape = 1.271519, scale = 2259.852523)
#' expect_equal(
#'  surv_prob(surv_dist1, seq_len(100)),
#'  surv_prob(surv_dist2, seq_len(100))
#' )
#' 
#' surv_dist3 <- flexsurvreg(Surv(rectime, censrec)~group, data = flexsurv::bc, dist = 'weibull')
#' surv_dist4 <- define_surv_param('weibull', shape = 1.3796518, scale = 4169.3445656)
#' surv_dist5 <- define_surv_param('weibull', shape = 1.3796518, scale = 2257.301)
#' surv_dist6 <- define_surv_param('weibull', shape = 1.3796518, scale = 1240.538)
#' expect_equal(
#'  surv_prob(surv_dist3, seq_len(100), covar = data.frame(group = 'Good')),
#'  surv_prob(surv_dist4, seq_len(100)),
#'  tolerance = 0.00001
#' )
#' expect_equal(
#'  surv_prob(surv_dist3, seq_len(100), covar = data.frame(group = 'Medium')),
#'  surv_prob(surv_dist5, seq_len(100)),
#'  tolerance = 0.00001
#' )
#' expect_equal(
#'  suppressWarnings(surv_prob(surv_dist3, seq_len(100))),
#'  surv_prob(surv_dist4, seq_len(100)) * 0.334 + 
#'      surv_prob(surv_dist5, seq_len(100)) * 0.334 + 
#'      surv_prob(surv_dist6, seq_len(100)) * 0.332,
#'  tolerance = 0.00001
#' )
#' 
#' expect_warning(
#'  surv_prob(surv_dist3, seq_len(100)),
#'  'Predictions will reflect weighted average of predictions for subjects used to fit model.',
#'  fixed = T
#' )
surv_prob.flexsurvreg <- function(x, time,  ...) {
  
  dots <- list(...)
  
  time_surv <- time
  # Extract parameter estimates
  coef_obj <- x$coefficients
  
  n_coef <- length(coef_obj)
  n_time <- length(time_surv)
  
  if(x$ncovs > 0 && is.null(dots$covar)) {
    msg <- get_and_populate_message('model_no_covariates')
    warning(msg, show_call_warn())
  }
  
  # For efficiency, survival probabilities are only calculated
  # for each distinct set of covariates, then merged back onto
  # the full dataset (data_full).
  if (is.null(dots$covar)) {
    # if covar is not provided, usfe the
    # original model.frame
    data_full <- x$data$m %>%
      select(-1, -ncol(x$data$m))
    data <- distinct(data_full)
  } else {
    # Use covar if provided
    data_full <- dots$covar
    data <- distinct(dots$covar)
  }
  
  # If there is no data, make an empty df
  if (ncol(data) == 0) {
    data <- data.frame(value = numeric(n_time))
  }
  
  # Get a data frame of parameter values for each observation
  param_df <- extract_flexsurv_params(x, data = data)
  n_obs <- nrow(param_df)
  
  # Repeat rows of parameter df to match number of time points
  param_df <- slice(param_df, rep(seq_len(n_obs), each = n_time))
  
  # Assumble arguments to p<dist> function
  fncall <- list(rep(time_surv, n_obs), lower.tail = FALSE) %>%
    append(x$aux) %>%
    append(param_df)
  
  # Calculate survival probabilities for each distinct level/time,
  surv_df <- data %>%
    slice(rep(seq_len(n_obs), each = n_time))
  surv_df$t <- rep(time_surv, n_obs)
  surv_df$value <- do.call(x$dfns$p, fncall)
  
  # Join to the full data, then summarize over times.
  if(x$ncovs > 0) {
    surv_df <- surv_df %>%
      left_join(data_full, by = colnames(data)) %>%
      group_by(t) %>%
      summarize(value = mean(.data$value))
  }
  
  
  # Just get the results column
  ret <- surv_df$value
  
  return(ret)
}

#' Extract Evaluated Parameters
#' 
#' Extracts the covariate-adjusted parameters from a
#' [flexsurv::flexsurvreg()] object.
#' 
#' @param obj A [flexsurv::flexsurvreg()] object.
#' @param data An optional dataset of covariate values to
#'   generate parameters for. Defaults to the original data
#'   to which the model was fit.
#'   
#' @return A tidy data frame of curve parameters for each
#'   covariate level.
#'   
#' @keywords internal
#'   
extract_flexsurv_params <- function(obj, data = NULL) {
  
  # Use data from object if not given
  if (is.null(data)) {
    data <- select(obj$data$m, -1, - ncol(obj$data$m))
  } else {
    # Apply factor levels of original data
    for(i in colnames(data)) {
      if (is.character(data[[i]]) | is.factor(data[[i]])) {
        data[[i]] <- factor(data[[i]], levels = unique(levels(as.factor(obj$data$m[[i]]))))
      }
    }
  }
  
  # Grab parameter estimates
  coef_obj <- obj$coefficients
  n_coef <- length(coef_obj)
  
  if (obj$ncovs == 0) {
    # Null model, extract parameter estimates
    out_params <- head(as.data.frame(t(as.data.frame(obj$res))), 1)
    rownames(out_params) <- NULL
    
  } else {
    # Get parameters of distribution
    par_names <- obj$dlist$pars
    names(par_names) <- par_names
    n_pars <- length(par_names)
    # Replicate matrix of coefficents, row = obs, col = param
    n_obs <- nrow(data)
    coef_mat <- matrix(
        rep(coef_obj, n_obs),
        ncol = n_coef,
        nrow = n_obs,
        byrow = TRUE
    )
    names(coef_mat) <- par_names
    # Preallocate a matrix to hold calculated parameters
    par_mat <- matrix(ncol = n_pars, nrow = n_obs)
    
    # Loop to compute covariate-adjusted parmaeters
    for (i in seq_len(n_pars)) {
      # Extract inverse transformation
      inv_trans <- obj$dlist$inv.transforms[[i]]
      # Subset coefficients relevant to parameter
      coef_selector <- c(i, obj$mx[[par_names[i]]] + n_pars)
      n_par_coefs <- length(coef_selector)
      par_coef_mat <- coef_mat[ , coef_selector]
      if (n_par_coefs > 1) {
        mm <- get_flexsurv_model_matrix(obj$all.formulae[[par_names[i]]], data)
        par_mat[ , i] <- inv_trans(rowSums(mm * par_coef_mat))
      } else {
        par_mat[ , i] <- inv_trans(par_coef_mat)
      }
    }
    
    out_params <- as.data.frame(par_mat)
    colnames(out_params) <- par_names
  }
  
  return(out_params)
}

get_flexsurv_model_matrix <- function(formula, data) {

    # Get formula representing only the predictors
    rhs_of_formula <- rhs(formula)
    rhs_only_formula_str <- paste0('~', deparse(rhs_of_formula))
    rhs_ony_formula <- as.formula(rhs_only_formula_str)

    # Generate the model matrix for predictors
    mm <- model.matrix(rhs_ony_formula, data = data)

    mm
}