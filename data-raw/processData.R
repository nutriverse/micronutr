
## Doesn't work for now; need to figure out how to get download URL of GitHub content
download.file(
  url = "https://raw.githubusercontent.com/nutriverse/writing-r-functions-nicholustintzaw/main/data/hbData.csv",
  destfile = "data-raw/hbData.csv"
)


################################################################################

hbData <- read.csv("data-raw/hbData.csv")
usethis::use_data(hbData, overwrite = TRUE, compress = "xz")

################################################################################

iodine_sample <- read.csv("data-raw/iodine_sample.csv")
names(iodine_sample)[1] <- "psu"
usethis::use_data(iodine_sample, overwrite = TRUE, compress = "xz")

################################################################################

ferritin_sample <- read.csv("data-raw/ferritin_sample.csv")
names(ferritin_sample)[1] <- "psu"

ferritin_sample$agp <- round(runif(nrow(ferritin_sample), min = 0.2, max = 10), 1)
ferritin_sample$agp[ferritin_sample$agp >9 & ferritin_sample$agp < 11] <- "NA"
ferritin_sample$age_group <- factor(ifelse(!is.na(ferritin_sample$m.age), "under 5 years", "5 years and older"))

ferritin_sample$infection <- round(runif(nrow(ferritin_sample), min = 0, max = 1))

usethis::use_data(ferritin_sample, overwrite = TRUE, compress = "xz")

################################################################################
