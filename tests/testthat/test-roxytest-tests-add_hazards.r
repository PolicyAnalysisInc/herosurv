# Generated by roxytest: Do not edit by hand!

# File R/add_hazards.r: @tests

test_that("Function add_hazards() @ L40", {
  dist1 <- define_surv_param("exp", rate = .125)
  dist2 <- define_surv_param("weibull", shape = 1.2, scale = 50)
  dist3 <- define_surv_param("weibull", shape = 1.1, scale = 30)
  expect_equal(
   add_hazards(dist1, dist2, dist3),
   create_list_object(
       c('surv_add_haz', 'surv_combined', 'surv_dist'),
       dists = list(dist1, dist2, dist3)
   )
  )
  expect_error(
   add_hazards(dist1, dist2, 'foo'),
   'Error adding hazards, invalid survival distribution provided.',
   fixed = TRUE
  )
})


test_that("Function surv_prob.surv_add_haz() @ L74", {
  dist1 <- define_surv_param('exp', rate = 0.025)
  dist2 <- define_surv_param('exp', rate = 0.010)
  dist3 <- define_surv_param('exp', rate = 0.002)
  dist4 <- define_surv_param('exp', rate = 0.035)
  dist5 <- define_surv_param('exp', rate = 0.037)
  expect_equal(
   surv_prob(dist4, seq_len(100)),
   surv_prob(add_hazards(dist1, dist2), seq_len(100))
  )
  expect_equal(
   surv_prob(dist5, seq_len(100)),
   surv_prob(add_hazards(dist1, dist2, dist3), seq_len(100))
  )
})


test_that("Function print.surv_add_haz() @ L91", {
  dist1 <- define_surv_param('exp', rate = 0.12)
  dist2 <- define_surv_param('exp', rate = 0.18)
  expect_output(
   print(add_hazards(dist1, dist2)),
   'A survival distribution combining the hazards of:
    * Distribution 1: An exponential distribution (rate = 0.12).
    * Distribution 2: An exponential distribution (rate = 0.18).',
   fixed = TRUE
  )
})
