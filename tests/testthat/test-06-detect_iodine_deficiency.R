

test_that("detect iodine output is correct", {
  expect_type(
    detect_iodine(iodine = 10), "character"
  )
  expect_type(
    detect_iodine(iodine = 10, label = FALSE), "integer"
  )
  expect_type(
    detect_iodine(iodine = 10, group = "pregnant"), "character"
  )
  expect_type(
    detect_iodine(iodine = 10, group = "pregnant", label = FALSE), "integer"
  )
  expect_type(
    detect_iodine(iodine = 10, group = "lactating"), "character"
  )
  expect_type(
    detect_iodine(iodine = 10, group = "lactating", label = FALSE), "integer"
  )
})

test_that("detect iodine errors", {
  expect_error(detect_iodine())
  expect_error(detect_iodine(iodine = "a"))
  expect_error(detect_iodine_general())
  expect_error(detect_iodine_general(iodine = "a"))
  expect_error(detect_iodine_pregnant())
  expect_error(detect_iodine_pregnant(iodine = "a"))
  expect_error(detect_iodine_lactating())
  expect_error(detect_iodine_lactating(iodine = "a"))
})

