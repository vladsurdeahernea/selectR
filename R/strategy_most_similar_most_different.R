# Most-Similar (exploratory vs estimating) and Most-Different ------------------

#' @keywords internal
strategy_most_similar <- function(data, y, x, z, n, distance, mode, audit, spec) {
  if (!length(z)) stop("Most-similar requires Z covariates for matching.")
  # Build standardized Z numeric matrix
  std <- std_numeric(data, z)
  if (is.null(std$X)) stop("All Z must be numeric for now (stub supports numeric Mahalanobis).")
  M <- mahal_matrix(std$X)
  pairs <- pair_index(nrow(data))
  # For exploratory: minimize DZ and maximize |ΔY|
  # For estimating:  minimize DZ and maximize |ΔX| (first X)
  if (mode == "exploratory") {
    if (!length(y)) stop("Exploratory most-similar requires Y.")
    delta <- abs(data[[y]][pairs$i] - data[[y]][pairs$j])
  } else if (mode == "estimating") {
    if (!length(x)) stop("Estimating most-similar requires X.")
    delta <- abs(data[[x[1]]][pairs$i] - data[[x[1]]][pairs$j])
  } else stop("Unknown mode.")
  DZ <- M[cbind(pairs$i, pairs$j)]
  # Score: larger delta and smaller DZ is better; combine as delta / (1 + DZ)
  score <- delta / (1 + DZ)
  out <- data.frame(
    i = pairs$i, j = pairs$j, DZ = DZ, Delta = delta, score = score,
    why = sprintf("Most-similar on Z (DZ=%.3f), maximize Δ%s=%.3f",
                  DZ, if (mode=="exploratory") y else x[1], delta),
    stringsAsFactors = FALSE
  )
  ord <- out[order(-out$score), ]
  top <- utils::head(ord, n)
  # attach row data for convenience
  top$i_row <- top$i; top$j_row <- top$j
  audit <- c(audit, mk_audit(6, paste0("Computed Mahalanobis DZ on Z; ranked by Δ/(1+DZ) (most-similar, ", mode, ").")))
  spec$pairwise <- TRUE
  new_csel(top, spec, model = NULL, distances = M, audit = audit)
}

#' @keywords internal
strategy_most_different <- function(data, y, x, z, n, distance, audit, spec) {
  if (!length(z)) stop("Most-different requires Z covariates.")
  if (!length(y)) stop("Most-different requires Y.")
  std <- std_numeric(data, z)
  if (is.null(std$X)) stop("All Z must be numeric for now (stub supports numeric Mahalanobis).")
  M <- mahal_matrix(std$X)
  pairs <- pair_index(nrow(data))
  DZ <- M[cbind(pairs$i, pairs$j)]
  DY <- abs(data[[y]][pairs$i] - data[[y]][pairs$j])
  # Score: larger DZ and smaller DY is better; combine as DZ / (1 + DY)
  score <- DZ / (1 + DY)
  out <- data.frame(
    i = pairs$i, j = pairs$j, DZ = DZ, DY = DY, score = score,
    why = sprintf("Most-different on Z (DZ=%.3f), minimize ΔY=%.3f", DZ, DY),
    stringsAsFactors = FALSE
  )
  ord <- out[order(-out$score), ]
  top <- utils::head(ord, n)
  top$i_row <- top$i; top$j_row <- top$j
  audit <- c(audit, mk_audit(6, "Computed Mahalanobis DZ; ranked by DZ/(1+ΔY) (most-different)."))
  spec$pairwise <- TRUE
  new_csel(top, spec, model = NULL, distances = M, audit = audit)
}
