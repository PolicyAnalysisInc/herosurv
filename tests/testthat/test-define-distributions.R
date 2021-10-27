test_that("Defining a simple parametric distribution", {

  dist1 <- define_survival('exp', rate = 0.12)
  expect_equal(
    dist1,
    structure(
      list(
        distribution = 'exp',
        rate = 0.12
      ),
      class = c("surv_object", "surv_dist")
    )
  )

  expect_error(
    define_survival('exp'),
    'Error defining "exp" distribution, parameters missing from function call: "rate".'
  )

  expect_error(
    define_survival('gengamma', mu = 2),
    'Error defining "gengamma" distribution, parameters missing from function call: "sigma", "Q".'
  )

})

test_that("Defining a parametric cure distribution", {

  dist1 <- define_survival_cure('exp', theta = 0.21, rate = 0.12)
  expect_equal(
    dist1,
    structure(
      list(
        distribution = 'exp',
        theta = 0.21,
        mixture = T,
        rate = 0.12
      ),
      class = c("surv_object", "surv_dist_cure")
    )
  )

  expect_error(
    define_survival_cure('exp', theta = 1.1, rate = 0.2),
    'Error defining cure model, cure fraction (theta) must be in range (0-1).',
    fixed = TRUE
  )

  expect_error(
    define_survival_cure('exp', theta = 0.2),
    'Error defining "exp" distribution, parameters missing from function call: "rate".',
    fixed = TRUE
  )

  expect_error(
    define_survival_cure('gengamma', theta = 0.2, mu = 2),
    'Error defining "gengamma" distribution, parameters missing from function call: "sigma", "Q".',
    fixed = TRUE
  )

})

