# Generated by roxytest: Do not edit by hand!

# File R/apply_or.r: @tests

test_that("Function apply_or() @ L58", {
  dist1 <- define_surv_param("exp", rate = 0.25)
  expect_equal(
   apply_or(dist1, 0.5),
   create_list_object(c('surv_po', 'surv_dist'), dist = dist1, or = 0.5)
  )
  expect_equal(
   apply_or(dist1, 0.5),
   apply_or(apply_or(dist1, 0.5), 1)
  )
  expect_equal(
   apply_or(dist1, 0.25),
   apply_or(apply_or(dist1, 0.5), 0.5)
  )
  expect_equal(
   apply_or(dist1, 0.5),
   apply_or(dist1, log(0.5), TRUE)
  )
  expect_error(
   apply_or('foo', 0.5),
   'Error applying odds ratio, invalid survival distribution provided.',
   fixed = TRUE
  )
  expect_error(
   apply_or(dist1, 'foo'),
   'Error applying odds ratio, "or" must be numeric.',
   fixed = TRUE
  )
  expect_error(
   apply_or(dist1, NA_real_),
   'Error applying odds ratio, "or" cannot be NA.',
   fixed = TRUE
  )
  expect_error(
   apply_or(dist1, -2),
   'Error applying odds ratio, "or" cannot be negative.',
   fixed = TRUE
  )
})


test_that("Function surv_prob.surv_po() @ L124", {
  dist1 <- define_surv_param("exp", rate = 0.50)
  dist2 <- apply_or(dist1, 0.5)
  expect_equal(
   odds_to_prob(prob_to_odds(surv_prob(dist1, seq_len(100))) / 0.5),
   surv_prob(dist2, seq_len(100))
  )
})


test_that("Function print.surv_po() @ L143", {
  dist1 <- apply_or(define_surv_param('exp', rate = 0.025), 0.5)
  expect_output(
   print(dist1),
   'A proportional odds survival distribution:
    * Odds Ratio: 0.5
    * Baseline Distribution: An exponential distribution (rate = 0.025).',
   fixed = T
  )
})

