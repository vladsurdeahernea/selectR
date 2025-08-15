# Typical -----------------------------------------------------------------

#' @keywords internal
strategy_typical <- function(data, y, x, z, model, family, n, audit, spec) {
  m <- fit_model(data, y, x, z, model, family)
  aug <- augment_residuals(m)
  df <- cbind(data, aug)
  # "Typical": smallest absolute residual (Seawright & Gerring 2008). 
  df$score <- -abs(df$.resid)
  df$why <- sprintf("Small |residual|=%.4f (typical)", abs(df$.resid))
  ord <- df[order(-df$score), , drop = FALSE]
  res <- head(ord[, c(rownames(ord), setdiff(names(ord), names(data)))], 0) # quiet R CMD note
  out <- ord[, c(setdiff(names(ord), c(".hat",".cook",".resid")), ".resid", ".hat", ".cook", "score", "why"), drop = FALSE]
  audit <- c(audit, mk_audit(5, "Fitted model for residuals."), mk_audit(6, "Ranked by smallest |residual| (typical)."))
  new_csel(utils::head(out, n), spec, model = m, distances = NULL, audit = audit)
}

# Deviant -----------------------------------------------------------------

#' @keywords internal
strategy_deviant <- function(data, y, x, z, model, family, n, audit, spec) {
  m <- fit_model(data, y, x, z, model, family)
  aug <- augment_residuals(m)
  df <- cbind(data, aug)
  df$score <- abs(df$.resid) # largest absolute residual
  df$why <- sprintf("Large |residual|=%.4f (deviant)", abs(df$.resid))
  ord <- df[order(-df$score), , drop = FALSE]
  audit <- c(audit, mk_audit(5, "Fitted model for residuals."), mk_audit(6, "Ranked by largest |residual| (deviant)."))
  new_csel(utils::head(ord[, c(setdiff(names(ord), c(".hat",".cook")), "score", "why"), drop = FALSE], n),
           spec, model = m, distances = NULL, audit = audit)
}

# Extreme -----------------------------------------------------------------

#' @keywords internal
strategy_extreme <- function(data, y, x, z, n, quantiles, audit, spec) {
  # "Extreme" on X or Y; here we privilege Y if present (exploratory), otherwise X[1].
  vname <- if (length(y)) y else if (length(x)) x[1] else stop("Need Y or X for `extreme` strategy.")
  v <- data[[vname]]
  if (!is.numeric(v)) stop("`extreme` requires numeric variable for scoring: ", vname)
  zscore <- (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)
  score <- abs(zscore)
  ord <- data[order(-score), , drop = FALSE]
  ord$score <- sort(score, decreasing = TRUE)
  ord$why <- sprintf("Extreme |z(%s)|=%.3f", vname, ord$score)
  audit <- c(audit, mk_audit(6, paste0("Ranked by |z| on ", vname, " (extreme).")))
  new_csel(utils::head(ord[, c(names(data), "score", "why")], n), spec, model = NULL, distances = NULL, audit = audit)
}

# Diverse -----------------------------------------------------------------

#' @keywords internal
strategy_diverse <- function(data, y, x, z, n, quantiles, audit, spec) {
  # Simple: choose cases across quantiles of the *descriptive axis* D.
  # If a D axis is not specified, use Y if goal is descriptive/exploratory, else first X.
  axis <- if (length(y)) y else if (length(x)) x[1] else stop("Need Y or X to define diversity axis.")
  v <- data[[axis]]
  if (!is.numeric(v)) stop("`diverse` requires a numeric axis; provide Y or numeric X.")
  qs <- stats::quantile(v, probs = quantiles, na.rm = TRUE, names = FALSE)
  chosen <- lapply(qs, function(qv) {
    idx <- which.min(abs(v - qv))
    data[idx, , drop = FALSE]
  })
  res <- do.call(rbind, chosen)
  res$score <- NA_real_
  res$why <- sprintf("Diverse on %s (quantile picks)", axis)
  audit <- c(audit, mk_audit(6, paste0("Selected across quantiles of ", axis, " (diverse).")))
  new_csel(utils::head(res, n), spec, model = NULL, distances = NULL, audit = audit)
}
