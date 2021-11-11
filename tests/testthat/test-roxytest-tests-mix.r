# Generated by roxytest: Do not edit by hand!

# File R/mix.r: @tests

test_that("Function mix() @ L67", {
  dist1 <- define_surv_param("exp", rate = .5)
  dist2 <- define_surv_param("gompertz", rate = .5, shape = 1)
  dist3 <- define_surv_param("weibull", shape = 1.2, scale = 20)
  expect_equal(
   mix(dist1, 0.2, dist2, 0.3, dist3, 0.5),
   create_list_object(
       c('surv_mix', 'surv_combined', 'surv_dist'),
       dists = list(dist1, dist2, dist3),
       weights = c(0.2, 0.3, 0.5)
   )
  )
  expect_error(
   mix(dist1, 0.2, dist2, "foo"),
   'Error mixing distributions, weights must be numeric.',
   fixed = TRUE
  )
  expect_error(
   mix(dist1, 0.2, "foo", 0.8),
   'Error mixing distributions, invalid survival distribution provided.',
   fixed = TRUE
  )
  expect_error(
   mix(dist1, 0.2, dist2, NA_real_),
   'Error mixing distributions, weights cannot be NA.',
   fixed = TRUE
  )
  expect_error(
   mix(dist1, 1.2, dist2, -0.2),
   'Error mixing distributions, weights must be in range [0-1].',
   fixed = TRUE
  )
  expect_error(
   mix(dist1, 0.5, dist2, 0.5, dist3),
   'Error mixing distributions, must provide an even number of arguments corresponding to n distributions and weights.',
   fixed = TRUE
  )
  expect_error(
   mix(dist1, 0.4, dist2, 0.5),
   'Error mixing distributions, weights must sum to 1.',
   fixed = TRUE
  )
})


test_that("Function print.surv_mix() @ L151", {
  dist1 <- define_surv_param('exp', rate = 0.12)
  dist2 <- define_surv_param('exp', rate = 0.18)
  expect_output(
   print(mix(dist1, 0.25, dist2, 0.75)),
   'A mixed survival distribution:
    * Distribution 1 (25%): An exponential distribution (rate = 0.12).
    * Distribution 2 (75%): An exponential distribution (rate = 0.18).',
   fixed = TRUE
  )
})


test_that("Function surv_prob.surv_mix() @ L179", {
  dist1 <- define_surv_param('exp', rate = 0.12)
  dist2 <- define_surv_param('exp', rate = 0.18)
  expect_equal(
   surv_prob(mix(dist1, 0.25, dist2, 0.75), seq_len(100)),
   pexp(seq_len(100), rate = 0.12, lower.tail = FALSE) * 0.25 +
       pexp(seq_len(100), rate = 0.18, lower.tail = FALSE) * 0.75
  )
})
