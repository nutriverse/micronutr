


testthat::test_that("output is character", {
  expect_type(detect_inflammation_agp(2), "character")
})

testthat::test_that("output is character", {
  expect_type(detect_inflammation_agp(2, label = FALSE), "double")
})


testthat::test_that("output is character", {
  expect_type(detect_inflammation_crp(2), "character")
  expect_type(detect_inflammation_crp(mnData$crp), "character")
})

testthat::test_that("output is character", {
  expect_type(detect_inflammation_crp(2, label = FALSE), "double")
  expect_type(detect_inflammation_crp(mnData$crp, label = FALSE), "double")
})

#'
#'
#' ## Detect inflammation by CRP
#' detect_inflammation_crp(2)
#' detect_inflammation(crp = mnData$crp)
#' detect_inflammation(crp = mnData$crp, label = FALSE)
#'
#' ## Detect inflammation by AGP and CRP
#' detect_inflammation(crp = 2, agp = 2)
#' detect_inflammation(crp = 2, agp = 2, label = FALSE)







