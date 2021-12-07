library(nutricheckr)

# Create testing data

# For individual target group function;
## U5 children
## no ferritin correction
u5_1 <- detect_iron_u5(df = ferritin_sample, ferritin = ferritin, add = TRUE)

## ferritin correction (CRP only)
u5_2 <- detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
               app = "crp", crp = crp, add = TRUE)

## ferritin correction (AGP only)
u5_3 <- detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
               app = "agp", crp = agp, add = TRUE)

## ferritin correction (AGP and CRP combined)
u5_4 <- detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
               app = c("crp", "agp"), crp = crp,
               agp = agp, add = TRUE)

# check for the output class
test_that("The output objects of individual `detect_iron` for U5 function has correct variable types", {

  expect_type(u5_1$iron_storage, "character")
  expect_type(u5_2$iron_storage, "character")
  expect_type(u5_3$iron_storage, "character")
  expect_type(u5_4$iron_storage, "character")
})

# check for the output result
test_that("Individual `detect_iron` for U5 function define anaema category correctly", {

  u5_1_result <- c("no deficiency", "no deficiency", "no deficiency", "deficiency",
                     "deficiency", "no deficiency", "deficiency", "deficiency",
                     "deficiency", NA)

  u5_2_3_result <- c("deficiency", "no deficiency", "no deficiency", "deficiency",
                     "deficiency", "no deficiency", "deficiency", "deficiency",
                     "deficiency", NA)

  u5_4_result <- c("deficiency", "no deficiency", "no deficiency", "deficiency",
                   "deficiency", "no deficiency", "deficiency", "deficiency",
                   "deficiency", NA )

  expect_equal(head(u5_1$iron_storage, 10), u5_1_result)
  expect_equal(head(u5_2$iron_storage, 10), u5_2_3_result)
  expect_equal(head(u5_3$iron_storage, 10), u5_2_3_result)
  expect_equal(head(u5_4$iron_storage, 10), u5_4_result)
})

## 5 years old and older
## no ferritin correction
over5_1 <- detect_iron_over5(df = ferritin_sample, ferritin = ferritin,
                     sex = sex, add = TRUE)

## ferritin correction (CRP only)
over5_2 <- detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
                     app = "crp", crp = crp, add = TRUE)

## ferritin correction (AGP only)
over5_3 <- detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
                     app = "agp", crp = agp, add = TRUE)

## ferritin correction (AGP and CRP combined)
over5_4 <- detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
                     app = c("crp", "agp"), crp = crp,
                     agp = agp, add = TRUE)

# check for the output class
test_that("The output objects of individual `detect_iodine` for oer 5 function has correct variable types", {

   expect_type(over5_1$iron_storage, "character")
   expect_type(over5_2$iron_storage, "character")
   expect_type(over5_3$iron_storage, "character")
   expect_type(over5_4$iron_storage, "character")
})

# check for the output result
test_that("Individual `detect_iron` for over 5 function define anaema category correctly", {

  over5_1_3_result <- c("deficiency", "no deficiency", "no deficiency", "deficiency",
                        "deficiency", "no deficiency", "deficiency", "deficiency",
                        "deficiency", NA)

  over5_4_result <- c("deficiency", "no deficiency", "deficiency", "deficiency",
                      "deficiency", "deficiency", "deficiency", "deficiency",
                      "deficiency", NA)

  expect_equal(head(over5_1$iron_storage, 10), over5_1_3_result)
  expect_equal(head(over5_2$iron_storage, 10), over5_1_3_result)
  expect_equal(head(over5_3$iron_storage, 10), over5_1_3_result)
  expect_equal(head(over5_4$iron_storage, 10), over5_4_result)
})

# For overall population function;
## no ferritin correction
overall_1 <- detect_iron(df = ferritin_sample, ferritin = ferritin,
                    sex = sex, age_group = age_group,
                    add = TRUE)

## ferritin correction (CRP only)
overall_2 <- detect_iron(df = ferritin_sample, ferritin = ferritin,
                    sex = sex, age_group = age_group,
                    app = "crp", crp = crp, add = TRUE)

## ferritin correction (AGP only)
overall_3 <- detect_iron(df = ferritin_sample, ferritin = ferritin,
                    sex = sex, age_group = age_group,
                    app = "agp", agp = agp, add = TRUE)

## ferritin correction (AGP and CRP combined)
overall_4 <- detect_iron(df = ferritin_sample, ferritin = ferritin,
                    sex = sex, age_group = age_group,
                    app = c("crp", "agp"), crp = crp, agp = agp,
                    add = TRUE)

# check for the output class
test_that("The output objects of individual `detect_iodine` function has correct variable types", {

 expect_type(overall_1$iron_storage, "character")
 expect_type(overall_2$iron_storage, "character")
 expect_type(overall_3$iron_storage, "character")
 expect_type(overall_4$iron_storage, "character")
})

# check for the output result <<<< WRONG CODE NEED TO CHECK AT .R FILE
test_that("Individual `detect_iron` for overall function define anaema category correctly", {

  over5_result <- c("no deficiency", "no deficiency", "no deficiency", "no deficiency",
                    "no deficiency", "no deficiency", "no deficiency", "no deficiency",
                    "no deficiency", NA)

  expect_equal(head(overall_1$iron_storage, 10), over5_result)
  expect_equal(head(overall_2$iron_storage, 10), over5_result)
  expect_equal(head(overall_3$iron_storage, 10), over5_result)
  expect_equal(head(overall_4$iron_storage, 10), over5_result)
})

## with qualitative information on infection
quali <- detect_iron_infu5(df = ferritin_sample, ferritin = ferritin,
                    infection = infection)

# check for the output class
test_that("The output objects of individual `detect_iodine` function has correct variable types", {

  expect_type(quali$iron_storage, "character")
})

# check for the output result
test_that("Individual `detect_iron` for qualitative function define anaema category correctly", {

  quali_result <- c("deficiency", "no deficiency", "deficiency", "deficiency", "deficiency",
                    "no deficiency", "deficiency", "deficiency", "deficiency", NA)

  expect_equal(head(quali$iron_storage, 10), quali_result)

})
