#' Toy cross-section for caseselectR
#'
#' A tiny dataset with 40 units and variables Y, X, Z1, Z2.
#' Designed for quick examples and tests.
#'
#' @format A data frame with 40 rows and 5 variables:
#' \describe{
#'   \item{unit}{unit id}
#'   \item{Y}{numeric outcome}
#'   \item{X}{main independent variable}
#'   \item{Z1}{covariate}
#'   \item{Z2}{covariate}
#' }
#' @examples
#' data(toy_cross, package = "caseselectR")
#' str(toy_cross)
"toy_cross"

#' Toy panel for caseselectR
#'
#' A tiny panel with 10 units and 6 periods, variables Y, X, Z1.
#' Good for longitudinal and index strategies.
#'
#' @format A data frame with 60 rows and 4 variables:
#' \describe{
#'   \item{unit}{unit id (factor or character)}
#'   \item{t}{time index (integer)}
#'   \item{Y}{numeric outcome}
#'   \item{X}{main independent variable}
#'   \item{Z1}{covariate}
#' }
#' @examples
#' data(toy_panel, package = "caseselectR")
#' head(toy_panel)
"toy_panel"
