

test_that("correction factor for altitude output is correct", {
  expect_type(get_altitude_correction(alt = 1500), "double")
  expect_type(get_altitude_correction(alt = mnData$altitude), "double")
})

test_that("correction factor for altitude output value is correct", {
  expect_equal(get_altitude_correction(alt = 900), 0)
  expect_equal(get_altitude_correction(alt = 1005), -2)
  expect_equal(get_altitude_correction(alt = 1545), -5)
  expect_equal(get_altitude_correction(alt = 2100), -8)
  expect_equal(get_altitude_correction(alt = 2550), -13)
  expect_equal(get_altitude_correction(alt = 3400), -19)
  expect_equal(get_altitude_correction(alt = 3600), -27)
  expect_equal(get_altitude_correction(alt = 4001), -35)
  expect_equal(get_altitude_correction(alt = 4600), -45)
})

test_that("correction factor for altitude errors", {
  expect_error(get_altitude_correction())
  expect_error(get_altitude_correction(alt = "a"))
})


test_that("correction factor for smoking status output is correct", {
  expect_type(get_smoking_correction(smoke = 1), "double")
})

test_that("correction factor for smoking output value is correct", {
  expect_equal(get_smoking_correction(smoke = 0), 0)
  expect_equal(get_smoking_correction(smoke = 1), -0.3)
  expect_equal(get_smoking_correction(smoke = 1.5), -0.5)
  expect_equal(get_smoking_correction(smoke = 2.5), -0.7)
})

test_that("correction factor for altitude errors", {
  expect_error(get_smoking_correction())
  expect_error(get_smoking_correction(smoke = "a"))
})


test_that("correct haemoglobin output is correct", {
  expect_type(
    correct_hb(hb = mnData$hb[1]), "double"
  )
  expect_type(
    correct_hb(hb = mnData$hb[1], alt = mnData$altitude[1]), "double"
  )
  expect_type(
    correct_hb(hb = mnData$hb[1], alt = mnData$altitude[1], smoke = 0),
    "double"
  )
  expect_type(
    correct_hb(hb = mnData$hb, alt = mnData$altitude, smoke = 1),
    "double"
  )
})

test_that("correct haemoglobin errors", {
  expect_error(correct_hb())
  expect_error(correct_hb(hb = "a"))
})

test_that("correct haemoglobin warns", {
  expect_warning(correct_hb(hb = mnData$hb[1], smoke = 1))
})
