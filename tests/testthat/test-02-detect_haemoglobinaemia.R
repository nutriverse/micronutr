
x <- subset(mnData, age < 5)

test_that("detect anaemia under 5 output is correct", {
  expect_type(
    detect_anaemia_u5(hb = x$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_u5(hb = x$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia under 5 errors", {
  expect_error(
    detect_anaemia_u5()
  )
  expect_error(
    detect_anaemia_u5(hb = "a")
  )
})


x <- subset(mnData, age >= 5 & age < 11)

test_that("detect anaemia 5 to 11 output is correct", {
  expect_type(
    detect_anaemia_5to11(hb = x$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_5to11(hb = x$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia 5 to 11 errors", {
  expect_error(
    detect_anaemia_5to11()
  )
  expect_error(
    detect_anaemia_5to11(hb = "a")
  )
})


x <- subset(mnData, age >= 12 & age < 14)

test_that("detect anaemia 12 to 14 output is correct", {
  expect_type(
    detect_anaemia_12to14(hb = x$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_12to14(hb = x$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia 12 to 14 errors", {
  expect_error(
    detect_anaemia_12to14()
  )
  expect_error(
    detect_anaemia_12to14(hb = "a")
  )
})


x <- subset(mnData, group == "non-pregnant non-lactating")

test_that("detect anaemia non-pregnant women output is correct", {
  expect_type(
    detect_anaemia_np_women(hb = x$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_np_women(hb = x$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia non-pregnant women errors", {
  expect_error(
    detect_anaemia_np_women()
  )
  expect_error(
    detect_anaemia_np_women(hb = "a")
  )
})


x <- subset(mnData, group %in% c("pregnant", "pregnant and lactating"))

test_that("detect anaemia pregnant women output is correct", {
  expect_type(
    detect_anaemia_pregnant(hb = x$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_pregnant(hb = x$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia pregnant women errors", {
  expect_error(
    detect_anaemia_pregnant()
  )
  expect_error(
    detect_anaemia_pregnant(hb = "a")
  )
})


test_that("detect anaemia men output is correct", {
  expect_type(
    detect_anaemia_men(hb = mnData$hb * 10), "character"
  )
  expect_type(
    detect_anaemia_men(hb = mnData$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia men errors", {
  expect_error(
    detect_anaemia_men()
  )
  expect_error(
    detect_anaemia_men(hb = "a")
  )
})


test_that("detect anaemia output is correct", {
  expect_type(
    detect_anaemia(hb = mnData$hb * 10), "character"
  )
  expect_type(
    detect_anaemia(hb = mnData$hb * 10, label = FALSE), "integer"
  )
})

test_that("detect anaemia errors", {
  expect_error(
    detect_anaemia()
  )
  expect_error(
    detect_anaemia(hb = "a")
  )
})

