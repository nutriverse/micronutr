library(nutricheckr)

# individual vector
u5 <- c(69, 69.6, 70, 99, 99.1, 100, 109.1, 110)
c5to11 <- c(79, 79.6, 80, 109, 109.1, 110, 114.1, 115)
c12to14 <- c(79, 79.6, 80, 109, 109.1, 110, 119.1, 120)
nonpreg_women <- c(79, 79.6, 80, 109, 109.1, 110, 119.1, 120)
pregnant <- c(69, 69.6, 70, 99, 99.1, 100, 109.1, 110)
men <- c(79, 79.6, 80, 109, 109.1, 110, 129.1, 130)

# sample data.frame
hb <- runif(50, min = 60, max = 130)
gender <- rep(c("male", "female"), each = 25)
df <- data.frame(gender, hb)

anaemia_u5       <- find_anaemia_u5(u5)
anaemia_5to11    <- find_anaemia_5to11(c5to11)
anaemia_12to14   <- find_anaemia_12to14(c12to14)
anaemia_np_women <- find_anaemia_np_women(nonpreg_women)
anaemia_pregnant <- find_anaemia_pregnant(pregnant)
anaemia_men      <- find_anaemia_men(men)

test_that("Individual `name_anaemia` function define anaemia category correctly", {

  results <- c("severe anaemia", "severe anaemia", "moderate anaemia",
               "moderate anaemia", "moderate anaemia", "mild anaemia",
               "mild anaemia", "no anaemia")

  expect_equal(as.character(anaemia_u5), results)
  expect_equal(as.character(anaemia_5to11), results)
  expect_equal(as.character(anaemia_12to14), results)
  expect_equal(as.character(anaemia_np_women), results)
  expect_equal(as.character(anaemia_pregnant), results)
  expect_equal(as.character(anaemia_men), results)

})


test_that("The output vector has same non - `NA` value", {
  expect_equal(sum(!is.na(anaemia_u5)), sum(!is.na(u5)))
  expect_equal(sum(!is.na(anaemia_5to11)), sum(!is.na(c5to11)))
  expect_equal(sum(!is.na(anaemia_12to14)), sum(!is.na(c12to14)))
  expect_equal(sum(!is.na(anaemia_np_women)), sum(!is.na(nonpreg_women)))
  expect_equal(sum(!is.na(anaemia_pregnant)), sum(!is.na(pregnant)))
  expect_equal(sum(!is.na(anaemia_men)), sum(!is.na(men)))
})


# data.frame
anaemia <-  find_anaemia(df = df,
                         hb = "hb",
                         add = TRUE)


test_that("The output result is a `data.frame`", {
  expect_s3_class(anaemia, "data.frame")
})


test_that("The output data.frame has correct column's names", {
  expect_named(anaemia, c("gender", "hb", "anaemia_status"))
})


test_that("Additional columns has correct variable type", {
  expect_s3_class(anaemia$anaemia_status, "factor")
})


test_that("The output `data.frame` varaibles has same `NA` value", {
  expect_equal(sum(!is.na(anaemia$anaemia_status)), sum(!is.na(df$hb)))
})

## Test errors and warnings

# sample data.frame
hb <- runif(50, min = 60, max = 130)
gender <- rep(c("male", "female"), each = 25)

df <- data.frame(gender, hb)

test_that("Errors and warnings are working", {
  expect_error(find_anaemia(df = df, hb = "Hb", add = TRUE))
})

df <- data.frame(gender, Hb = hb, hb = hb)

test_that("Errors and warnings are working", {
  expect_warning(find_anaemia(df = df, add = TRUE))
})

df <- data.frame(gender, v1 = hb)

test_that("Errors and warnings are working", {
  expect_error(find_anaemia(df = df, add = TRUE))
})


## Use haemoglobin full dataset to test overall function

