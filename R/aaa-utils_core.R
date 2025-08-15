#' @keywords internal
is_binary <- function(v) {
  u <- unique(stats::na.omit(v))
  length(u) == 2 && (all(u %in% c(0, 1)) || all(u %in% c(FALSE, TRUE)))
}

#' @keywords internal
coerce_df <- function(data) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  data
}

#' @keywords internal
std_numeric <- function(df, vars) {
  if (!length(vars)) return(list(X = NULL, cols = character(0)))
  cols <- vars
  X <- df[, cols, drop = FALSE]
  # keep only numeric; drop others quietly
  num_cols <- cols[sapply(X, is.numeric)]
  if (!length(num_cols)) return(list(X = NULL, cols = character(0)))
  Xn <- scale(as.matrix(df[, num_cols, drop = FALSE]))
  list(X = Xn, cols = num_cols)
}

#' @keywords internal
safe_cov <- function(X) {
  S <- try(stats::cov(X, use = "pairwise.complete.obs"), silent = TRUE)
  if (inherits(S, "try-error")) {
    S <- diag(apply(X, 2, stats::var, na.rm = TRUE))
  }
  if (det(as.matrix(S)) <= .Machine$double.eps) {
    S <- S + diag(1e-6, ncol(S))
  }
  S
}

#' @keywords internal
mahal_matrix <- function(X) {
  if (is.null(X) || ncol(X) == 0 || nrow(X) == 0) return(matrix(0, 0, 0))
  S <- safe_cov(X)
  S_inv <- solve(S)
  n <- nrow(X)
  M <- matrix(0, n, n)
  for (i in seq_len(n)) {
    diffs <- sweep(X, 2, X[i, ], "-")
    M[i, ] <- sqrt(rowSums((diffs %*% S_inv) * diffs))
  }
  M
}

#' @keywords internal
pair_index <- function(n) {
  if (n < 2) return(data.frame(i = integer(0), j = integer(0)))
  comb <- utils::combn(n, 2)
  data.frame(i = comb[1, ], j = comb[2, ], stringsAsFactors = FALSE)
}

#' @keywords internal
mk_audit <- function(step, info) {
  list(step = step, info = info, time = Sys.time())
}

#' @keywords internal
new_csel <- function(results, spec, model = NULL, distances = NULL, audit = list()) {
  structure(
    list(results = results, spec = spec, model = model, distances = distances, audit_log = audit),
    class = "csel"
  )
}
