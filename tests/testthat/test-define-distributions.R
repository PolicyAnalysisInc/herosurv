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
})
