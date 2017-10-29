library(batwintor)
library(testthat)
context("Time checks")

test_that("returned time is numeic",{
  expect_equal(class(hour.to.month(60)),"numeric")
  expect_equal(class(hour.to.day(60)),"numeric")
  expect_equal(class(day.to.month(60)),"numeric")
  expect_equal(class(prop.to.months(.60)),"numeric")
})
