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

anaemia_u5 <- name_anaemia_u5(u5)
anaemia_c5to11 <- name_anaemia_c5to11(c5to11)
anaemia_c12to14 <- name_anaemia_c12to14(c12to14)
anaemia_nonpreg_women <- name_anaemia_nonpreg_women(nonpreg_women)
anaemia_pregnant <- name_anaemia_pregnant(pregnant)
anaemia_men <- name_anaemia_men(men)

test_that("Individual `name_anaemia` function define anaema category correctly", {

  results <- c("severe anaemia", "severe anaemia", "moderate anaemia",
               "moderate anaemia", "moderate anaemia", "mild anaemia",
               "mild anaemia", "no anaemia")

  expect_equal(as.character(anaemia_u5), results)
  expect_equal(as.character(anaemia_c5to11), results)
  expect_equal(as.character(anaemia_c12to14), results)
  expect_equal(as.character(anaemia_nonpreg_women), results)
  expect_equal(as.character(anaemia_pregnant), results)
  expect_equal(as.character(anaemia_men), results)

})


test_that("The output vector has same non - `NA` value", {

  expect_equal(sum(!is.na(anaemia_u5)), sum(!is.na(u5)))
  expect_equal(sum(!is.na(anaemia_c5to11)), sum(!is.na(c5to11)))
  expect_equal(sum(!is.na(anaemia_c12to14)), sum(!is.na(c12to14)))
  expect_equal(sum(!is.na(anaemia_nonpreg_women)), sum(!is.na(nonpreg_women)))
  expect_equal(sum(!is.na(anaemia_pregnant)), sum(!is.na(pregnant)))
  expect_equal(sum(!is.na(anaemia_men)), sum(!is.na(men)))
})


# data.frame
anaemia <-  name_anaemia(df = df,
                         hb = hb,
                         group = c("u5", "c5to11", "c12to14", "nonpreg_women",
                                   "pregnant", "men"),
                         add = TRUE)


test_that("The output result is a `data.frame`", {
  expect_s3_class(anaemia, "data.frame")
})


test_that("The output data.frame has correct column's names", {
  expect_named(anaemia, c("gender",
                          "hb",
                          "anaemia_u5",
                          "anaemia_c5to11",
                          "anaemia_c12to14",
                          "anaemia_nonpreg_women",
                          "anaemia_pregnant",
                          "anaemia_men"))
})


test_that("The output data.frame additional columns has correct variable type", {

  expect_s3_class(anaemia$anaemia_u5, "factor")
  expect_s3_class(anaemia$anaemia_c5to11, "factor")
  expect_s3_class(anaemia$anaemia_c12to14, "factor")
  expect_s3_class(anaemia$anaemia_nonpreg_women, "factor")
  expect_s3_class(anaemia$anaemia_pregnant, "factor")
  expect_s3_class(anaemia$anaemia_men, "factor")

})


test_that("The output `data.frame` varaibles has same `NA` value", {

  expect_equal(sum(!is.na(anaemia$anaemia_u5)), sum(!is.na(df$hb)))
  expect_equal(sum(!is.na(anaemia$anaemia_c5to11)), sum(!is.na(df$hb)))
  expect_equal(sum(!is.na(anaemia$anaemia_c12to14)), sum(!is.na(df$hb)))
  expect_equal(sum(!is.na(anaemia$anaemia_nonpreg_women)), sum(!is.na(df$hb)))
  expect_equal(sum(!is.na(anaemia$anaemia_pregnant)), sum(!is.na(df$hb)))
  expect_equal(sum(!is.na(anaemia$anaemia_men)), sum(!is.na(df$hb)))
})


