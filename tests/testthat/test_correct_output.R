context("Correct Output")

test_that("numeric vector of length 3",{
  expect_equal(addFirstTwo(c(1,5,-7)),6)
  expect_equal(addFirstTwo(c(-5,-6, 7)),-11 )
})
