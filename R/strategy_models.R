#' @keywords internal
fit_model <- function(data, y, x, z, model, family) {
  rhs <- c(x, z)
  f <- stats::as.formula(paste(y, "~", if (length(rhs)) paste(rhs, collapse = " + ") else "1"))
  if (model == "lm") {
    m <- stats::lm(f, data = data)
  } else {
    if (is.null(family)) stop("For glm, please provide a `family`.")
    m <- stats::glm(f, data = data, family = family)
  }
  m
}

#' @keywords internal
augment_residuals <- function(m) {
  res <- stats::residuals(m, type = if (inherits(m, "glm")) "deviance" else "response")
  hat <- try(stats::hatvalues(m), silent = TRUE); if (inherits(hat, "try-error")) hat <- rep(NA_real_, length(res))
  cook <- try(stats::cooks.distance(m), silent = TRUE); if (inherits(cook, "try-error")) cook <- rep(NA_real_, length(res))
  data.frame(.resid = res, .hat = hat, .cook = cook, stringsAsFactors = FALSE)
}
