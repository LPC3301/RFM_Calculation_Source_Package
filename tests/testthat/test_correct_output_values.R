context("Correct Value Output")

test_that("numeric vector of length 3",{
  expect_is(addFirstTwo(c(1,5,-7)),"numeric")
})
