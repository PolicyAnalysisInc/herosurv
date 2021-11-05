# Generated by roxytest: Do not edit by hand!

# File R/util.r: @tests

test_that("Function get_and_populate_message() @ L6", {
  expect_equal(
   get_and_populate_message('missing_parameters', dist = 'a', params = 'b'),
   'Error defining a distribution, parameters missing from function call: b.'
  )
})


test_that("Function create_list_object() @ L17", {
  expect_equal(
   class(create_list_object(c('a','b'),
   list())), c('a','b')
  )
})


test_that("Function quoted_list_string() @ L27", {
  expect_equal(
   quoted_list_string(c('a','b','c')),
   '"a", "b", "c"'
  )
})


test_that("Function truncate_param() @ L53", {
  expect_equal(
   truncate_param('foo', c(1)),
   1
  )
  
  expect_equal(
   truncate_param('foo', c(1,1,1)),
   1
  )
  
  expect_warning(
   truncate_param('foo', c(1,2,3)),
   'Parameter foo was length > 1 and only the first element will be used.',
   fixed = T
  )
  
  expect_equal(
   suppressWarnings(truncate_param('foo', c(1,2,3))),
   1
  )
})


test_that("Function get_indefinite_article() @ L89", {
  expect_equal(
   get_indefinite_article('fruit'),
   'a'
  )
  
  expect_equal(
   get_indefinite_article('apple'),
   'an'
  )
})


test_that("Function create_param_formatter() @ L103", {
  expect_equal(
   create_param_formatter(digits = 4)(0.1234567),
   "0.1235"
  )
})


test_that("Function clean_factors() @ L120", {
  expect_equal(
   clean_factors(data.frame(a = 'foo', b = 1, stringsAsFactors = TRUE)),
   data.frame(a = 'foo', b = 1, stringsAsFactors = FALSE)
  )
})

