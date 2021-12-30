# library(nutricheckr)

#  Based on inflammation status defined by acute-phase protein
## Create testing data

### identify the inflammation status and perform ferritin value correction
inflam_crp_yes <- detect_inflammation_crp(6)
inflam_crp_no  <- detect_inflammation_crp(5)
inflam_crp_na  <- detect_inflammation_crp(NA)

### identify the inflammation status and perform ferritin value correction
inflam_agp_yes <- detect_inflammation_agp(1.5)
inflam_agp_no  <- detect_inflammation_agp(1)
inflam_agp_na  <- detect_inflammation_agp(NA)

### identify the inflammation status and perform ferritin value correction
inflam_all_incu <- detect_inflammation(6, 1)
inflam_all_late <- detect_inflammation(5, 1.5)
inflam_all_early <- detect_inflammation(6, 1.5)
inflam_all_no <- detect_inflammation(4, 1)
inflam_all_na1 <- detect_inflammation(NA, 2)
inflam_all_na2 <- detect_inflammation(1, NA)
inflam_all_na3 <- detect_inflammation(NA, NA)


## check for the output class
test_that("The output objects of individual inflammation detection function have correct variable types", {

  expect_type(inflam_crp_yes, "character")
  expect_type(inflam_crp_no, "character")

  expect_type(inflam_agp_yes, "character")
  expect_type(inflam_agp_no, "character")

  expect_type(inflam_all_incu, "character")
  expect_type(inflam_all_late, "character")
  expect_type(inflam_all_early, "character")
  expect_type(inflam_all_no, "character")

})

# check for the output result
test_that("Individual inflammation detection functions define anaema category correctly", {

  expect_equal(inflam_crp_yes, "inflammation")
  expect_equal(inflam_crp_no, "no inflammation")
  expect_equal(is.na(inflam_crp_na), TRUE)

  expect_equal(inflam_agp_yes, "inflammation")
  expect_equal(inflam_agp_no, "no inflammation")
  expect_equal(is.na(inflam_agp_na), TRUE)

  expect_equal(inflam_all_incu, "incubation")
  expect_equal(inflam_all_late, "late convalescence")
  expect_equal(inflam_all_early, "early convalescence")
  expect_equal(inflam_all_no, "no inflammation")

  expect_equal(is.na(inflam_all_na1), TRUE)
  expect_equal(is.na(inflam_all_na2), TRUE)
  expect_equal(is.na(inflam_all_na3), TRUE)

})


#  Ferritin Correction
## check for the output class

test_that("Ferritin correction function output has correct variable types", {

  # correction with either CRP or AGP only
  expect_type(correct_ferritin(crp = 6, ferritin = 30), "double")
  expect_type(correct_ferritin(crp = 5, ferritin = 30), "double")
  expect_type(correct_ferritin(agp = 1.5, ferritin = 30), "double")
  expect_type(correct_ferritin(agp = 1, ferritin = 30), "double")

  # correction with both CRP and AGP
  expect_type(correct_ferritin(crp = 4, agp = 1, ferritin = 30), "double")
  expect_type(correct_ferritin(crp = 6, agp = 1, ferritin = 30), "double")
  expect_type(correct_ferritin(crp = 5, agp = 1.5, ferritin = 30), "double")
  expect_type(correct_ferritin(crp = 6, agp = 1.5, ferritin = 30), "double")

})

# check for the output result
test_that("iron deficiency function defines anaemia category correctly", {

  # correction with either CRP or AGP only
  expect_equal(correct_ferritin(crp = 6, ferritin = 30), 30 * 0.65)
  expect_equal(correct_ferritin(crp = 4, ferritin = 30), 30)
  expect_equal(correct_ferritin(agp = 1.5, ferritin = 30), 30 * 0.72)
  expect_equal(correct_ferritin(agp = 1, ferritin = 30), 30)

  # expect_equal(is.na(correct_ferritin(30, NA)), TRUE)
  # expect_equal(is.na(correct_ferritin_crp(NA, "inflammation")), TRUE)
  # expect_equal(is.na(correct_ferritin_crp(NA, "No Inflammation")), TRUE)
  # expect_equal(is.na(correct_ferritin_agp(30, NA)), TRUE)
  # expect_equal(is.na(correct_ferritin_agp(NA, "inflammation")), TRUE)
  # expect_equal(is.na(correct_ferritin_agp(NA, "No Inflammation")), TRUE)

  # correction with both CRP and AGP
  expect_equal(correct_ferritin(crp = 4, agp = 1, ferritin = 30), 30)
  expect_equal(correct_ferritin(crp = 6, agp = 1, ferritin = 30), 30 * 0.77)
  expect_equal(correct_ferritin(crp = 5, agp = 1.5, ferritin = 30), 30 * 0.53)
  expect_equal(correct_ferritin(crp = 6, agp = 1.5, ferritin = 30), 30 * 0.75)

  # expect_equal(is.na(correct_ferritin(30, NA)), TRUE)
  # expect_equal(is.na(correct_ferritin(NA, "No Inflammation")), TRUE)
  # expect_equal(is.na(correct_ferritin(NA, "Incubation")), TRUE)
  # expect_equal(is.na(correct_ferritin(NA, "Late Convalescence")), TRUE)
  # expect_equal(is.na(correct_ferritin(NA, "Early Convalescence")), TRUE)

})



#  Iron storage status identification
## check for the output class
test_that("The iron deficiency function output has correct variable types", {
  expect_type(detect_iron_deficiency(ferritin = 12), "character")
  expect_type(
    detect_iron_deficiency(ferritin = 12, group = "5over"), "character"
  )
  expect_type(detect_iron_deficiency(ferritin = 11), "character")
  expect_type(detect_iron_deficiency(
    ferritin = 11, group = "5over"), "character"
  )
  expect_type(detect_iron_deficiency(ferritin = 15), "character")
  expect_type(
    detect_iron_deficiency(ferritin = 15, group = "5over"), "character"
  )
  expect_type(detect_iron_deficiency(ferritin = 14), "character")
  expect_type(
    detect_iron_deficiency(ferritin = 14, group = "5over"), "character"
  )
})


# check for the output result
test_that("iron deficiency function defines anaemia category correctly", {
  expect_equal(detect_iron_deficiency(ferritin = 12), "no iron deficiency")
  expect_equal(
    detect_iron_deficiency(ferritin = 12, group = "5over"), "iron deficiency"
  )
  expect_equal(
    detect_iron_deficiency(ferritin = 11), "iron deficiency"
  )
  expect_equal(
    detect_iron_deficiency(ferritin = 11, group = "5over"), "iron deficiency"
  )
  # expect_equal(is.na(detect_iron(12, NA)), TRUE)
  # expect_equal(is.na(detect_iron(11, NA)), TRUE)
  # expect_equal(is.na(detect_iron(NA, "under 5 years")), TRUE)
  # expect_equal(is.na(detect_iron(NA, "5 years and older")), TRUE)
  expect_equal(detect_iron_deficiency(ferritin = 15), "no iron deficiency")
  expect_equal(
    detect_iron_deficiency(ferritin = 15, group = "5over"), "no iron deficiency"
  )
  expect_equal(detect_iron_deficiency(ferritin = 14), "no iron deficiency")
  expect_equal(
    detect_iron_deficiency(ferritin = 14, group = "5over"), "iron deficiency"
  )
  # expect_equal(is.na(detect_iron(15, NA)), TRUE)
  # expect_equal(is.na(detect_iron(14, NA)), TRUE)
  # expect_equal(is.na(detect_iron(NA, "under 5 years")), TRUE)
  # expect_equal(is.na(detect_iron(NA, "5 years and older")), TRUE)
})


## Based on the qualitative information of infection or inflammation
## check for the output class
test_that("iron deficiency qualitative function has correct variable types", {
  expect_type(
    detect_iron_deficiency_qualitative(ferritin = 30, inflammation = TRUE),
    "character"
  )
  expect_type(
    detect_iron_deficiency_qualitative(ferritin = 30, inflammation = FALSE),
    "character"
  )
  expect_type(
    detect_iron_deficiency_qualitative(ferritin = 29, inflammation = TRUE),
    "character"
  )
  expect_type(
    detect_iron_deficiency_qualitative(ferritin = 29, inflammation = FALSE),
    "character"
  )
})

# check for the output result
test_that(
  "iron deficiency qualitative function defines anaemia category correctly", {
  expect_equal(
    detect_iron_deficiency_qualitative(ferritin = 30, inflammation = TRUE),
    "no iron deficiency"
  )
  expect_equal(
    detect_iron_deficiency_qualitative(ferritin = 30, inflammation = FALSE),
    NA_character_
  )
  expect_equal(
    detect_iron_deficiency_qualitative(ferritin = 29, inflammation = TRUE),
    "iron deficiency"
  )
  expect_equal(
    detect_iron_deficiency_qualitative(ferritin = 29, inflammation = FALSE),
    NA_character_
  )
  # expect_equal(is.na(detect_iron_quali(29, NA)), TRUE)
  # expect_equal(is.na(detect_iron_quali(30, NA)), TRUE)
  # expect_equal(is.na(detect_iron_quali(NA, 1)), TRUE)
  # expect_equal(is.na(detect_iron_quali(NA, 0)), TRUE)
})
