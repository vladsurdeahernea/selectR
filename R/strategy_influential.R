# Influential --------------------------------------------------------------

#' @keywords internal
strategy_influential <- function(data, y, x, z, model, family, n, audit, spec) {
  m <- fit_model(data, y, x, z, model, family)
  aug <- augment_residuals(m)
  df <- cbind(data, aug)
  # Rank by Cook's distance (influence); tie-break by |DFBETA| for first X if available
  df$score <- df$.cook
  df$why <- sprintf("High influence: Cook's D=%.4f", df$.cook)
  ord <- df[order(-df$score), , drop = FALSE]
  audit <- c(audit, mk_audit(5, "Fitted model; computed hat and Cook's distance."),
             mk_audit(6, "Ranked by Cook's distance (influential)."))
  new_csel(utils::head(ord[, c(setdiff(names(ord), c(".hat",".cook")), "score", "why"), drop = FALSE], n),
           spec, model = m, distances = NULL, audit = audit)
}
