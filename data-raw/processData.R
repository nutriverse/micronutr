################################################################################
#
#'
#' Read and process micronutrient data
#'
#'
#
################################################################################

## Load libraries
library(readxl)


## Read micronutrient data XLSX
mnData <- readxl::read_xlsx(
  path = "data-raw/mnData_master_Sudan_V12.xlsx", sheet = 1
)

## Tidy up grouping
mnData$group <- with(
  mnData,
  {
    ifelse(
      group == "Lactating Principal carer", "lactating",
        ifelse(
          group == "Pregnant and lactating Principal Carer", "pregnant and lactating",
            ifelse(
              group == "Pregnant Not Principal Carer" | group == "Pregnant Principal carer", "pregnant",
                ifelse(
                  group == "Principal carer nighther pregnant nor lactating" | group == "Woman in child bearing age (pregnancy/lactation status not known)", "non-pregnant non-lactating", "child"
                )
            )
        )
    )
  }
)


## Tidy up age
mnData$age <- with(
  mnData,
  {
    ifelse(is.na(m.age), floor(ch.age / 12), m.age)
  }
)


## Create dataset
#mnData <- mnData[ , c("state", "locality", "psu", "sex", "age", "group", "hb", "calcium", "crp", "ferritin", "iodine")]
mnData <- mnData[ , c("psu", "sex", "age", "group", "hb", "calcium", "crp", "ferritin", "iodine")]

## download hb data if not available
if (length(list.files("data-raw", pattern = "hbData.csv")) == 0) {
  download.file(
    url = "https://raw.githubusercontent.com/ernestguevarra/writing_r_functions/main/data/hbData.csv",
    destfile = "data-raw/hbData.csv"
  )
}

x <- read.csv("data-raw/hbData.csv")
x <- x[!duplicated(x$psu), ]
x <- x[order(x$psu), c("psu", "altitude")]

mnData <- merge(mnData, x, by = "psu", all.x = TRUE)

mnData <- tibble::tibble(mnData)

## Export data
usethis::use_data(mnData, overwrite = TRUE, compress = "xz")

