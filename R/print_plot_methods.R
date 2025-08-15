#' @export
print.csel <- function(x, ...) {
  cat("Case selection results (type:", x$spec$type, ")\n")
  if (!is.null(x$spec$goal)) cat("Goal:", x$spec$goal, "\n")
  if (!is.null(x$spec$seed)) cat("Seed:", x$spec$seed, "\n")
  cat("---- Top candidates ----\n")
  print(utils::head(x$results, 10))
  if (!is.null(x$model)) {
    cat("\nModel family:", x$spec$model, if (!is.null(x$spec$family)) paste0("(", x$spec$family, ")"), "\n")
  }
  cat("\nAudit log steps recorded:", length(x$audit_log), "\n")
  invisible(x)
}
