# Generated by roxytest: Do not edit by hand!

# File R/surv_cure.r: @tests

test_that("Function print.surv_cure() @ L99", {
  surv_dist1 <- define_cure_surv('weibull', theta = 0.21434, shape = 1.2438, scale = 20.3984, mixture = FALSE)
  expect_output(
   print(surv_dist1),
   "A Weibull (AFT) non-mixture cure distribution (theta = 0.214, shape = 1.244, scale = 20.398).",
   fixed = T
  )
  
  surv_dist2 <- define_cure_surv('llogis', theta = 0.21434, shape = 1.2438, scale = 20.3984, mixture = TRUE)
  expect_output(
   print(surv_dist2),
   "A log-logistic mixture cure distribution (theta = 0.214, shape = 1.244, scale = 20.398).",
   fixed = T
  )
})


test_that("Function surv_prob.surv_cure() @ L129", {
  dist1 <- define_cure_surv('exp', theta = 0.2, rate = 0.05, mixture = TRUE)
  expect_equal(
   surv_prob(dist1, c(0, 1, 2, Inf)),
   c(1.0000000, 0.9609835, 0.9238699, 0.2000000),
   tolerance = 0.00001
  )
  dist2 <- define_cure_surv('weibull', theta = 0.2, shape = 1.2, scale = 13.4, mixture = FALSE)
  expect_equal(
   surv_prob(dist2, c(0, 1, 2, Inf)),
   c(1.0000000, 0.9324775, 0.8554689, 0.2000000),
   tolerance = 0.00001
  )
})

