# Outcome -----------------------------------------------------------------

#' @keywords internal
strategy_outcome <- function(data, y, n, audit, spec) {
  if (!length(y)) stop("`outcome` strategy needs Y.")
  v <- data[[y]]
  if (!is.numeric(v)) stop("Outcome strategy expects numeric Y for now.")
  # maximize variation: pick tails
  ord <- data[order(v), , drop = FALSE]
  picks <- rbind(utils::head(ord, ceiling(n/2)), utils::tail(ord, floor(n/2)))
  picks$score <- NA_real_
  picks$why <- "Tail-selection on Y (outcome)"
  audit <- c(audit, mk_audit(6, "Selected tail cases on Y to maximize variation (outcome)."))
  new_csel(picks, spec, model = NULL, distances = NULL, audit = audit)
}

# Index -------------------------------------------------------------------

#' @keywords internal
strategy_index <- function(data, y, id, time, n, audit, spec) {
  if (missing(id) || missing(time)) stop("`index` requires `id` and `time` for panel data.")
  idc <- deparse(substitute(id)); tc <- deparse(substitute(time))
  idv <- data[[idc]]; tv <- data[[tc]]; yv <- data[[y]]
  ord <- order(idv, tv)
  idv <- idv[ord]; tv <- tv[ord]; yv <- yv[ord]
  d <- data[ord, , drop = FALSE]
  # Detect first change in Y per unit
  idx <- tapply(seq_along(yv), idv, function(ix) {
    yy <- yv[ix]
    ch <- which(diff(yy) != 0)
    if (length(ch)) ix[min(ch) + 1] else NA_integer_
  })
  idx <- idx[!is.na(idx)]
  chosen <- d[as.integer(idx), , drop = FALSE]
  chosen <- head(chosen[order(chosen[[tc]]), , drop = FALSE], n)
  chosen$score <- NA_real_
  chosen$why <- "Earliest first ΔY by unit (index)"
  audit <- c(audit, mk_audit(6, "Identified first instances of ΔY across units (index)."))
  new_csel(chosen, spec, model = NULL, distances = NULL, audit = audit)
}

# Longitudinal ------------------------------------------------------------

#' @keywords internal
strategy_longitudinal <- function(data, y, x, z, id, time, n, audit, spec) {
  if (missing(id) || missing(time)) stop("`longitudinal` requires `id` and `time`.")
  if (!length(x)) stop("`longitudinal` requires X.")
  idc <- deparse(substitute(id)); tc <- deparse(substitute(time))
  d <- data[order(data[[idc]], data[[tc]]), , drop = FALSE]
  # compute ΔX and ΔZ norms by unit
  unit_summ <- by(d, d[[idc]], function(dd) {
    dx <- sum(abs(diff(dd[[x[1]]])), na.rm = TRUE)
    if (length(z)) {
      dz <- 0
      for (zz in z) if (is.numeric(dd[[zz]])) dz <- dz + sum(abs(diff(dd[[zz]])), na.rm = TRUE)
    } else dz <- 0
    data.frame(unit = dd[[idc]][1], deltaX = dx, deltaZ = dz, stringsAsFactors = FALSE)
  })
  S <- do.call(rbind, unit_summ)
  S$score <- S$deltaX / (1 + S$deltaZ)  # big ΔX, small ΔZ
  ord <- S[order(-S$score), ]
  top_units <- utils::head(ord$unit, n)
  picked <- lapply(top_units, function(u) d[d[[idc]] == u, , drop = FALSE])
  # Return a one-row summary per unit
  res <- ord[ord$unit %in% top_units, , drop = FALSE]
  res$why <- "Large ΔX with stable Z (longitudinal)"
  audit <- c(audit, mk_audit(6, "Computed unit-level ΔX and ΔZ; ranked by ΔX/(1+ΔZ) (longitudinal)."))
  new_csel(res, spec, model = NULL, distances = NULL, audit = audit)
}

# Pathway -----------------------------------------------------------------

#' @keywords internal
strategy_pathway <- function(data, y, x, z, model, family, n, audit, spec) {
  # Simple proxy: strong alignment of X with Y controlling for Z, and low |residual| from Z-only model.
  if (!length(x)) stop("`pathway` requires X and Y.")
  # Fit Z-only model and full model
  mZ <- if (length(z)) fit_model(data, y, character(0), z, model, family) else NULL
  mXZ <- fit_model(data, y, x, z, model, family)
  augXZ <- augment_residuals(mXZ)
  # Path strength proxy: |cor(X1, Y)| / (1 + |residual_Z_only|)
  corxy <- suppressWarnings(abs(stats::cor(data[[x[1]]], data[[y]], use = "pairwise.complete.obs")))
  prox <- abs(augXZ$.resid)
  if (!is.null(mZ)) {
    resZ <- augment_residuals(mZ)$`.resid`
    prox <- abs(resZ)
  }
  score <- (1 + corxy) / (1 + prox)
  out <- data
  out$score <- as.numeric(score)
  out$why <- sprintf("Strong X↦Y with Z 'conservative' (proxy), residual=%.3f", prox)
  ord <- out[order(-out$score), , drop = FALSE]
  audit <- c(audit, mk_audit(5, "Fitted Z-only and X+Z model."),
             mk_audit(6, "Ranked by pathway proxy: strong X→Y & low residual under Z-only (pathway)."))
  new_csel(utils::head(ord[, c(names(data), "score", "why")], n), spec, model = mXZ, distances = NULL, audit = audit)
}
