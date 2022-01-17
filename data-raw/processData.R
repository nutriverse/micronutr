## download hb data if not available
if (grep(pattern = "hbData.csv", x = list.files("data-raw"), value = TRUE) != "hbData.csv") {
  download.file(
    url = "https://raw.githubusercontent.com/ernestguevarra/writing_r_functions/main/data/hbData.csv",
    destfile = "data-raw/hbData.csv"
  )
}

################################################################################

haemoglobin <- read.csv("data-raw/hbData.csv")
haemoglobin$ch.age <- as.integer(floor(haemoglobin$ch.age))
haemoglobin$hb <- round(as.numeric(haemoglobin$hb), digits = 1)
usethis::use_data(haemoglobin, overwrite = TRUE, compress = "xz")

################################################################################

iodine <- read.csv("data-raw/iodine_sample.csv")
names(iodine)[1] <- "psu"
usethis::use_data(iodine, overwrite = TRUE, compress = "xz")

################################################################################

ferritin <- read.csv("data-raw/ferritin_sample.csv")
names(ferritin)[1] <- "psu"

ferritin$agp <- round(
  runif(
    nrow(ferritin),
    min = 0.2,
    max = 10
  ), 1
)

ferritin$agp[ferritin$agp > 9 & ferritin$agp < 11] <- NA
ferritin$age_group <- factor(
  ifelse(
    !is.na(ferritin$m.age),
    "under 5 years",
    "5 years and older"
  )
)

ferritin$infection <- sample(c(TRUE, FALSE), nrow(ferritin),
                             replace = TRUE, prob=c(.7, .3))

ferritin[1:3, 12] <- NA
ferritin[4:6, 11] <- NA


usethis::use_data(ferritin, overwrite = TRUE, compress = "xz")

################################################################################
