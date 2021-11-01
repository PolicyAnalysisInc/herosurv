# Generated by roxytest: Do not edit by hand!

# File R/compatability.r: @tests

test_that("Function eval_surv() @ L11", {
  dist1 <- define_parametric_surv(distribution = "exp", rate = 0.05)
  expect_equal(
   surv_prob(dist1, c(1,2,3,4)),
   eval_surv(dist1, c(1,2,3,4))
  )
})


test_that("Function define_survival() @ L24", {
  expect_equal(
   define_parametric_surv('lnorm', meanlog = 2.1, sdlog = 0.3),
   define_survival('lnorm', meanlog = 2.1, sdlog = 0.3)  
  )
})


test_that("Function define_survival_cure() @ L37", {
  expect_equal(
   define_cure_surv('weibull', theta = 0.41, shape = 1.04, scale = 10.2),
   define_survival_cure('weibull', theta = 0.41, shape = 1.04, scale = 10.2)
  )
})
