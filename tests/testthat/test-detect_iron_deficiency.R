
test_that("Ferritin correction output is correct", {
  expect_type(
    correct_ferritin(crp = mnData$crp[1], agp = 2, ferritin = mnData$ferritin[1]),
    "double"
  )
})

test_that("Ferritin correction errors show", {
  expect_error(correct_ferritin())
  expect_error(correct_ferritin(crp = "a", agp = "b", ferritin = "c"))
})
