
test_that("Ferritin correction output is correct", {
  expect_type(
    correct_ferritin(crp = mnData$crp[1], agp = 2, ferritin = mnData$ferritin[1]),
    "double"
  )
  expect_type(
    correct_ferritin(crp = mnData$crp[1], ferritin = mnData$ferritin[1]),
    "double"
  )
  expect_type(
    correct_ferritin(agp = 2, ferritin = mnData$ferritin[1]),
    "double"
  )
})

test_that("Ferritin correction errors show", {
  expect_error(correct_ferritin())
  expect_error(correct_ferritin(crp = "a", agp = "b", ferritin = "c"))
})


test_that("detect iron deficiency in under 5 output is correct", {
  expect_type(
    detect_iron_deficiency_u5(mnData$ferritin), "character"
  )
  expect_type(
    detect_iron_deficiency_u5(mnData$ferritin, label = FALSE), "double"
  )
})

test_that("detect iron deficiency in under 5 errors", {
  expect_error(detect_iron_deficiency_u5())
  expect_error(detect_iron_deficiency_u5(ferritin = "a"))
})


test_that("detect iron deficiency in 5 over output is correct", {
  expect_type(
    detect_iron_deficiency_5over(mnData$ferritin), "character"
  )
  expect_type(
    detect_iron_deficiency_5over(mnData$ferritin, label = FALSE), "double"
  )
})

test_that("detect iron deficiency in under 5 errors", {
  expect_error(detect_iron_deficiency_5over())
  expect_error(detect_iron_deficiency_5over(ferritin = "a"))
})


test_that("detect iron deficiency by group output is correct", {
  expect_type(
    detect_iron_deficiency(mnData$ferritin), "character"
  )
  expect_type(
    detect_iron_deficiency(mnData$ferritin, group = "5over"), "character"
  )
  expect_type(
    detect_iron_deficiency(mnData$ferritin, label = FALSE), "double"
  )
  expect_type(
    detect_iron_deficiency(mnData$ferritin, group = "5over", label = FALSE), "double"
  )
})

test_that("detect iron deficiency by group errors", {
  expect_error(detect_iron_deficiency())
  expect_error(detect_iron_deficiency(ferritin = "a"))
})

test_that("detect iron deficiency qualitatively output is correct", {
  expect_type(
    detect_iron_deficiency_qualitative(
      mnData$ferritin[1], inflammation = TRUE),
    "character"
  )
  expect_type(
    detect_iron_deficiency_qualitative(
      mnData$ferritin[1], inflammation = TRUE, label = FALSE),
    "double"
  )
  expect_equal(
    detect_iron_deficiency_qualitative_(ferritin = mnData$ferritin[1]),
    NA_character_
  )
  expect_equal(
    detect_iron_deficiency_qualitative_(
      ferritin = mnData$ferritin[1], label = FALSE
    ),
    NA_integer_
  )
  expect_equal(
    detect_iron_deficiency_qualitative_(
      ferritin = mnData$ferritin[1], inflammation = NA
    ),
    NA_character_
  )
  expect_equal(
    detect_iron_deficiency_qualitative_(
      ferritin = mnData$ferritin[1], inflammation = NA, label = FALSE
    ),
    NA_integer_
  )
  expect_type(
    detect_iron_deficiency_qualitative_(
      ferritin = mnData$ferritin[1], inflammation = FALSE
    ),
    "character"
  )
})

test_that("detect iron deficiency qualitative errors", {
  expect_error(
    detect_iron_deficiency_qualitative_()
  )
  expect_error(
    detect_iron_deficiency_qualitative_(ferritin = "a")
  )
  expect_error(
    detect_iron_deficiency_qualitative_(
      ferritin = mnData$ferritin[1], inflammation = 1
    )
  )
})
