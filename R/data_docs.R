#' Toy cross-section for caseselectR
#'
#' A tiny dataset with 40 units and variables Y, X, Z1, Z2.
#' @format A data frame with 40 rows and 5 variables:
#' \describe{
#'   \item{unit}{unit id}
#'   \item{X}{main independent variable}
#'   \item{Z1}{covariate}
#'   \item{Z2}{covariate}
#'   \item{Y}{numeric outcome}
#' }
"toy_cross"

#' Toy panel for caseselectR
#'
#' A tiny panel dataset with 10 units and 6 periods, variables Y, X, Z1.
#' @format A data frame with 60 rows and 4 variables:
#' \describe{
#'   \item{unit}{unit id}
#'   \item{t}{time index}
#'   \item{X}{main independent variable}
#'   \item{Z1}{covariate}
#'   \item{Y}{numeric outcome}
#' }
"toy_panel"
