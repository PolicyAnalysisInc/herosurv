# Generated by roxytest: Do not edit by hand!

# File R/herosurv.r: @tests

test_that("Function .onLoad() @ L20", {
  .onLoad()
  expect_equal(
   c(
       'herosurv.show_call_signature_in_errors',
       'herosurv.show_call_signature_in_warnings'
   ) %in% names(options()),
   c(TRUE, TRUE)
  )
})

