# Outcome -----------------------------------------------------------------

#' @keywords internal
strategy_outcome <- function(data, y, n, audit, spec) {
  if (!length(y)) stop("`outcome` strategy needs Y.")
  v <- data[[y]]
  if (!is.numeric(v)) stop("Outcome strategy expects numeric Y for now.")
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
  if (is.null(id) || is.null(time)) stop("`index` requires `id` and `time` for panel data.")
  idc <- id; tc <- time
  idv <- data[[idc]]; tv <- data[[tc]]; yv <- data[[y]]
  ord <- order(idv, tv)
  idv <- idv[ord]; tv <- tv[ord]; yv <- yv[ord]
  d <- data[ord, , drop = FALSE]
  idx <- tapply(seq_along(yv), idv, function(ix) {
    yy <- yv[ix]
    ch <- which(diff(yy) != 0)
    if (length(ch)) ix[min(ch) + 1] else NA_integer_
  })
  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    chosen <- d[0, , drop = FALSE]
  } else {
    chosen <- d[as.integer(idx), , drop = FALSE]
  }
  chosen <- head(chosen[order(chosen[[tc]]), , drop = FALSE], n)
  chosen$score <- NA_real_
  chosen$why <- "Earliest first ΔY by unit (index)"
  audit <- c(audit, mk_audit(6, "Identified first instances of ΔY across units (index)."))
  new_csel(chosen, spec, model = NULL, distances = NULL, audit = audit)
}

# Longitudinal ------------------------------------------------------------

#' @keywords internal
strategy_longitudinal <- function(data, y, x, z, id, time, n, audit, spec) {
  if (is.null(id) || is.null(time)) stop("`longitudinal` requires `id` and `time`.")
  if (!length(x)) stop("`longitudinal` requires X.")
  idc <- id; tc <- time
  d <- data[order(data[[idc]], data[[tc]]), , drop = FALSE]
  unit_summ <- by(d, d[[idc]], function(dd) {
    dx <- sum(abs(diff(dd[[x[1]]])), na.rm = TRUE)
    if (length(z)) {
      dz <- 0
      for (zz in z) if (is.numeric(dd[[zz]])) dz <- dz + sum(abs(diff(dd[[zz]])), na.rm = TRUE)
    } else dz <- 0
    data.frame(unit = dd[[idc]][1], deltaX = dx, deltaZ = dz, stringsAsFactors = FALSE)
  })
  S <- if (is.list(unit_summ)) do.call(rbind, unit_summ) else unit_summ
  if (is.null(S) || nrow(S) == 0) {
    res <- data.frame(unit = character(0), deltaX = numeric(0), deltaZ = numeric(0), score = numeric(0))
  } else {
    S$score <- S$deltaX / (1 + S$deltaZ)
    res <- S[order(-S$score), ]
    res <- utils::head(res, n)
  }
  res$why <- "Large ΔX with stable Z (longitudinal)"
  audit <- c(audit, mk_audit(6, "Computed unit-level ΔX and ΔZ; ranked by ΔX/(1+ΔZ) (longitudinal)."))
  new_csel(res, spec, model = NULL, distances = NULL, audit = audit)
}

# Pathway -----------------------------------------------------------------

#' @keywords internal
strategy_pathway <- function(data, y, x, z, model, family, n, audit, spec) {
  if (!length(x)) stop("`pathway` requires X and Y.")
  mZ  <- if (length(z)) fit_model(data, y, character(0), z, model, family) else NULL
  mXZ <- fit_model(data, y, x, z, model, family)
  augXZ <- augment_residuals(mXZ)

  # proxy for "conservative Z": small residual under Z-only model (if available)
  prox <- if (!is.null(mZ)) abs(augment_residuals(mZ)$`.resid`) else rep(0, nrow(data))

  out <- data
  out$score <- 1 / (1 + abs(augXZ$.resid) + prox)
  out$why <- sprintf("Strong X→Y & Z conservative proxy; resid=%.3f", abs(augXZ$.resid))
  ord <- out[order(-out$score), , drop = FALSE]
  audit <- c(audit,
             mk_audit(5, "Fitted Z-only and X+Z model."),
             mk_audit(6, "Ranked by pathway proxy (strong X→Y & conservative Z)."))
  new_csel(utils::head(ord[, c(names(data), "score", "why")], n), spec, model = mXZ, distances = NULL, audit = audit)
}
