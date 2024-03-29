% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-detect_inflammation.R
\name{detect_inflammation}
\alias{detect_inflammation}
\alias{detect_inflammation_crp}
\alias{detect_inflammation_agp}
\title{Determine inflammation status}
\usage{
detect_inflammation(crp = NULL, agp = NULL, label = TRUE)

detect_inflammation_crp(crp = NULL, label = TRUE)

detect_inflammation_agp(agp = NULL, label = TRUE)
}
\arguments{
\item{crp}{A numeric value or numeric vector of c-reactive protein (crp)
values in micrograms per litre (microgram/l).}

\item{agp}{A numeric value or numeric vector of alpha(1)-acid-glycoprotein
(agp) values in micrograms per litre (microgram/l).}

\item{label}{Logical. Should labels be used to classify inflammation status?
If TRUE (default), status is classified as "no inflammation" or
"inflammation" based on either CRP or AGP or status is classified as
"no inflammation", "incubation", "early convalescence", or
"late convalescence" based on both CRP and AGP. If FALSE, simple integer
codes are returned: 0 for no inflammation and 1 for inflammation based on
either CRP or AGP; 0 for no inflammation, 1 for incubation, 2 for early
convalescence, or 3 for late convalescence.}
}
\value{
If \code{label} is TRUE, a character value or character vector of
inflammation classification based on c-reactive protein (CRP) and/or
alpha(1)-acid-glycoprotein (AGP) values. If \code{label} is FALSE, an integer
value or vector of inflammation classification.
}
\description{
Given laboratory values for serum c-reactive protein (CRP) and/or serum
alpha(1)-acid-glycoprotein (AGP), the inflammation status of a subject can
be determined based on cut-off values described in Namaste, S. M.,
Rohner, F., Huang, J., Bhushan, N. L., Flores-Ayala, R., Kupka, R., Mei, Z.,
Rawat, R., Williams, A. M., Raiten, D. J., Northrop-Clewes, C. A., &
Suchdev, P. S. (2017). Adjusting ferritin concentrations for inflammation:
Biomarkers Reflecting Inflammation and Nutritional Determinants of Anemia
(BRINDA) project. The American journal of clinical nutrition, 106(Suppl 1),
359S–371S. https://doi.org/10.3945/ajcn.116.141762
}
\examples{
## Detect inflammation by AGP
detect_inflammation_agp(2)
detect_inflammation_agp(2, label = FALSE)

## Detect inflammation by CRP
detect_inflammation_crp(2)
detect_inflammation(crp = mnData$crp)
detect_inflammation(crp = mnData$crp, label = FALSE)

## Detect inflammation by AGP and CRP
detect_inflammation(crp = 2, agp = 2)
detect_inflammation(crp = 2, agp = 2, label = FALSE)

}
\author{
Nicholus Tint Zaw and Ernest Guevarra
}
