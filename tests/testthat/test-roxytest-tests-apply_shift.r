# Generated by roxytest: Do not edit by hand!

# File R/apply_shift.r: @tests

test_that("Function apply_shift() @ L48", {
  dist1 <- define_surv_param("exp", rate = 0.25)
  expect_equal(
   apply_shift(dist1, 2),
   create_list_object(c('surv_shift', 'surv_dist'), dist = dist1, shift = 2)
  )
  expect_equal(
   apply_shift(dist1, 4),
   apply_shift(apply_shift(dist1, 2), 2)
  )
  expect_error(
   apply_shift('foo', 0.5),
   'Error applying shift, invalid survival distribution provided.',
   fixed = TRUE
  )
  expect_error(
   apply_shift(dist1, 'foo'),
   'Error applying shift, "shift" must be numeric.',
   fixed = TRUE
  )
  expect_error(
   apply_shift(dist1, NA_real_),
   'Error applying shift, "shift" cannot be NA.',
   fixed = TRUE
  )
})


test_that("Function surv_prob.surv_shift() @ L103", {
  dist1 <- define_surv_param("exp", rate = 0.50)
  dist2 <- apply_shift(dist1, 2)
  expect_equal(
   surv_prob(dist1, seq_len(100)),
   surv_prob(dist2, seq_len(100) + 2)
  )
})


test_that("Function print.surv_shift() @ L136", {
  dist1 <- apply_shift(define_surv_param('exp', rate = 0.025), 2.5)
  expect_output(
   print(dist1),
   'A shifted survival distribution:
    * Shift: 2.5
    * Baseline Distribution: An exponential distribution (rate = 0.025).',
   fixed = T
  )
})
