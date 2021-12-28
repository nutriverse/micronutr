library(nutricheckr)

# Create testing data
dta_child <- subset(iodine, group == "Child")
dta_preg <- subset(iodine, group == "Pregnant Principal carer")
dta_lact <- subset(iodine, group == "Lactating Principal carer")


# For individual target group function;
iodine_child <- detect_iodine_general(dta_child$iodine) # school-age-children (6 years or above)
iodine_preg <- detect_iodine_pregnant(dta_preg$iodine) # pregnant
iodine_lact <- detect_iodine_lactating(dta_lact$iodine) # lactating or U2 children

test_that("Individual `detect_iodine` function determine population iodine intake category correctly", {

  result_child <- "adequate"
  result_preg <- "insufficient"
  result_lact <- "insufficient"

  expect_equal(iodine_child, result_child)
  expect_equal(iodine_preg, result_preg)
  expect_equal(iodine_lact, result_lact)


})


test_that("The output objects of individual `detect_iodine` function has correct variable types", {

  expect_type(iodine_child, "character")
  expect_type(iodine_preg, "character")
  expect_type(iodine_lact, "character")

})

# For overall population function;
iodine_child <- detect_iodine(dta_child$iodine, group = "general") # school-age-children (6 years or above)
iodine_preg <- detect_iodine(dta_preg$iodine, group = "pregnant") # pregnant
iodine_lact <- detect_iodine(dta_lact$iodine, group = "lactating") # lactating or U2 children


test_that("Overall `detect_iodine` function determine population iodine intake category correctly", {

  result_child <- "adequate"
  result_preg <- "insufficient"
  result_lact <- "insufficient"

  expect_equal(iodine_child, result_child)
  expect_equal(iodine_preg, result_preg)
  expect_equal(iodine_lact, result_lact)


})

test_that("The output objects of overall `detect_iodine` function has correct variable types", {

  expect_type(iodine_child, "character")
  expect_type(iodine_preg, "character")
  expect_type(iodine_lact, "character")

})


#########################################################################################################
#########################################################################################################

# Check for single value input
# For individual target group function;
iodine_child <- detect_iodine_general(30) # school-age-children (6 years or above)
iodine_preg <- detect_iodine_pregnant(255) # pregnant
iodine_lact <- detect_iodine_lactating(90) # lactating or U2 children

test_that("Individual `detect_iodine` function determine population iodine intake category correctly", {

  result_child <- "insufficient (moderate)"
  result_preg <- "above requirement"
  result_lact <- "insufficient"

  expect_equal(iodine_child, result_child)
  expect_equal(iodine_preg, result_preg)
  expect_equal(iodine_lact, result_lact)


})


# For overall population function;
iodine_child <- detect_iodine(30, group = "general") # school-age-children (6 years or above)
iodine_preg <- detect_iodine(255, group = "pregnant") # pregnant
iodine_lact <- detect_iodine(90, group = "lactating") # lactating or U2 children


test_that("Overall `detect_iodine` function determine population iodine intake category correctly", {

  result_child <- "insufficient (moderate)"
  result_preg <- "above requirement"
  result_lact <- "insufficient"

  expect_equal(iodine_child, result_child)
  expect_equal(iodine_preg, result_preg)
  expect_equal(iodine_lact, result_lact)


})

