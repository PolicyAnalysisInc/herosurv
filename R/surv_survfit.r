#' @export
#' 
#' @tests
#' sf1 <- survfit(Surv(rectime, censrec)~1, data = flexsurv::bc)
#' expect_equal(
#'  surv_prob(sf1, c(0, 1000, 2000, 3000)),
#'  c(1.0000000, 0.6577504, 0.4624312, NA)
#' )
#' sf2 <- survfit(Surv(rectime, censrec)~group, data = flexsurv::bc)
#' expect_equal(
#'  surv_prob(sf2, c(0, 1000, 2000, 3000), covar = data.frame(group = "Poor")),
#'  c(1.0000000, 0.4073690, 0.2151746, NA),
#'  tolerance = 0.00001
#' )
#' sf3 <- survfit(Surv(rectime, censrec)~group, data = flexsurv::bc)
#' expect_equal(
#'  suppressWarnings(surv_prob(sf2, c(0, 1000, 2000, 3000))),
#'  c(1.0000000, 0.6563702, 0.4584150, NA),
#'  tolerance = 0.00001
#' )
#' expect_warning(
#'  surv_prob(sf2, c(0, 1000, 2000, 3000)),
#'  'Predictions will reflect weighted average of predictions for subjects used to fit model.',
#'  fixed = TRUE
#' )
surv_prob.survfit <- function(x, time,  ...) {
  
  dots <- list(...)
  
  pl_table <- extract_strata(x)
  
  # Identify the terms which separate groups (if any)
  terms <- setdiff(
    colnames(pl_table),
    c("time", "n", "nrisk", "ncensor", "nevent", "surv", "lower", "upper")
  )
  
  # Generate predicted survival for each group
  surv_df <- ddply(
    pl_table,
    terms,
    function(d) {
      maxtime <- max(d$time)
      selector <- (time > maxtime)
      # Use stepfun to look up survival probabilities
      value <- stepfun(d$time[-1], d$surv)(time)
      # Use NA when time > max time
      value[selector] <- as.numeric(NA)
      tibble(
        t = time, 
        value = value,
        n = d$n[1])
    }
  )
  
  if (is.null(dots$covar)) {
    if (length(terms) > 0) {
      msg <- get_and_populate_message('model_no_covariates')
      warning(msg, show_call_warn())
    }
    # If covariates are not provided, do weighted average for each time.
    agg_df <- surv_df %>%
      as_tibble() %>% 
      group_by(t) %>%
      summarize(value = sum(.data$value * n) / sum(n))
  } else {
    
    # If covariates are provided, join the predictions to them and then
    # do simple average for each time.
    
    agg_df <- clean_factors(dots$covar) %>% 
      left_join(surv_df, by = terms) %>%
      group_by(t) %>%
      summarize(value = mean(.data$value))
  }
  
  # Get the vector of predictions
  ret <- agg_df$value
  return(ret)
}

#' Extract Product-Limit Table for Stratum
#' 
#' Extracts the product-limit table from a survfit object 
#' for a given stratum. Only [survival::survfit()] and
#' unstratified [survival::survfit.coxph()] objects are
#' supported.
#' 
#' @param sf A survit object.
#' @param index The index number of the strata to extract.
#'   
#' @return A data frame of the product-limit table for the 
#'   given stratum.
#'   
#' @keywords internal
extract_stratum <- function(sf, index) {
  if(is.null(sf$strata)) {
    # If there is no stratification, get the full table
    selector <- seq_len(length(sf$time))
    values <- list()
  } else {
    
    # If there are strata, create a selector which selects only the rows
    # corresponding to the given index
    end_index <- sum(sf$strata[seq_len(index)])
    start_index <- 1 + end_index - sf$strata[index]
    selector <- seq(from = start_index, to = end_index)
    
    # Extract the variable names and values corresponding to the stratum
    split_strata <- strsplit(names(sf$strata[index]),"(=|, )")[[1]]
    len <- length(split_strata) / 2
    keys <- split_strata[seq_len(len) * 2 - 1]
    values <- split_strata[seq_len(len) * 2]
    names(values) <- keys
  }
  
  # Return the stratum's product-limit table
  arg_list <- as.list(values) %>% append(
    list(
      time = c(0, sf$time[selector]),
      n = sum(sf$n.censor[selector] +
                sf$n.event[selector]),
      nrisk = c(sum(sf$n.censor[selector] +
                      sf$n.event[selector]), sf$n.risk[selector]),
      ncensor = c(0, sf$n.censor[selector]),
      nevent = c(0, sf$n.event[selector]),
      surv = c(1, sf$surv[selector]),
      lower = c(1, sf$lower[selector]),
      upper = c(1, sf$upper[selector])
    )
  )

  pl_tibble <- do.call(tibble, arg_list)
  
  return(pl_tibble)
}

#' Extract Product-Limit Tables
#' 
#' Extracts the product-limit table from a survfit object
#' for all strata. Only `survfit` and unstratified
#' `survfit.coxph` objects are supported.
#' 
#' @param sf A survit object.
#'   
#' @return A tidy data.frame of the product-limit tables for
#'   all strata.
#'   
#' @keywords internal
#'   
extract_strata <- function(sf) {
  if (is.null(sf$strata)) {
    extract_stratum(sf, 1)
  } else {
    ldply(
      seq_len(length(sf$strata)),
      function(i) extract_stratum(sf, i)
    )
  }
}