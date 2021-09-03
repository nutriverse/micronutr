
## Doesn't work for now; need to figure out how to get download URL of GitHub content
download.file(
  url = "https://raw.githubusercontent.com/nutriverse/writing-r-functions-nicholustintzaw/main/data/hbData.csv",
  destfile = "data-raw/hbData.csv"
)

hbData <- read.csv("data-raw/hbData.csv")
usethis::use_data(hbData, overwrite = TRUE, compress = "xz")


iodine_sample <- read.csv("data-raw/iodine_sample.csv")
usethis::use_data(iodine_sample, overwrite = TRUE, compress = "xz")
