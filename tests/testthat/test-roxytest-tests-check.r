# Generated by roxytest: Do not edit by hand!

# File R/check.r: @tests

test_that("Function check_param_names() @ L8", {
  expect_error(
   check_param_names(list(shape=1,foo=2), 'weibullPH'), 
   'Error defining Weibull (PH) distribution, parameters missing from function call: "scale".',
   fixed = T
  )
})


test_that("Function check_theta() @ L37", {
  expect_error(check_theta(1), NA)
  expect_error(check_theta(0.5), NA)
  expect_error(check_theta(0), NA)
  expect_error(
   check_theta(-0.01),
   'Error defining cure model, cure fraction (theta) must be in range (0-1).',
   fixed = T
  )
  expect_error(
   check_theta(1.01),
   'Error defining cure model, cure fraction (theta) must be in range (0-1).',
   fixed = T
  )
})

