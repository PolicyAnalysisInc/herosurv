# Generated by roxytest: Do not edit by hand!

# File R/surv_spline.r: @tests

test_that("Function get_spline_params_from_args() @ L99", {
  expect_equal(
   get_spline_params_from_args(list(1,2,3,4,5,c(6, 6))),
   list(
       gamma1 = 1, gamma2 = 2,
       gamma3 = 3, knots1 = 4,
       knots2 = 5, knots3 = 6
   )
  )
  expect_equal(
   get_spline_params_from_args(
       list(
           knots3 = 6, knots1 = c(4, 4),
           knots2 = 5, gamma1 = c(1,1,1),
           gamma3 = 3, gamma2 = 2
       )    
   ),
   list(
       gamma1 = 1, gamma2 = 2,
       gamma3 = 3, knots1 = 4,
       knots2 = 5, knots3 = 6
   )
  )
  expect_error(
   get_spline_params_from_args(
       list(
           knots3 = 6, knots1 = c(4, 4),
           knots2 = 5, gamma1 = c(1,1,1),
           gamma3 = 3, foo = 1
       )    
   ),
   'incorrect argument names were provided',
   fixed = T
  )
})


test_that("Function check_spline_params() @ L163", {
  expect_error(
   check_spline_params(list(1,2,3,4,5)),
   'must provide at least two parameter values',
   fixed =T
  )
  
  expect_error(
   check_spline_params(list(1,2)),
   'must provide at least two parameter values',
   fixed =T
  )
  
  expect_error(
   check_spline_params(list(1,2,3,4,"foo", 6)),
   'parameter was of type "character" instead of "numeric"',
   fixed =T
  )
})
