## Test that output is of correct type -----------------------------------------

testthat::test_that("output is of correct type", {
  expect_type(detect_inflammation_agp(2), "character")
  expect_type(detect_inflammation_agp(2, label = FALSE), "double")
})

testthat::test_that("output is of correct type", {
  expect_type(detect_inflammation_crp(2), "character")
  expect_type(detect_inflammation_crp(mnData$crp), "character")
})

testthat::test_that("output is of correct type", {
  expect_type(detect_inflammation_crp(2, label = FALSE), "double")
  expect_type(detect_inflammation_crp(mnData$crp, label = FALSE), "double")
})

testthat::test_that("output is of correct type", {
  expect_type(detect_inflammation(crp = 2, agp = 2), "character")
  expect_type(detect_inflammation(agp = 2), "character")
  expect_type(detect_inflammation(crp = 2), "character")
  expect_type(detect_inflammation(crp = 2, agp = 2, label = FALSE), "double")
  expect_type(detect_inflammation(agp = 2, label = FALSE), "double")
  expect_type(detect_inflammation(crp = 2, label = FALSE), "double")
})


## Test that error message shows

testthat::test_that("error message shows", {
  expect_error(detect_inflammation_crp())
  expect_error(detect_inflammation_agp())
  expect_error(detect_inflammation())
})





