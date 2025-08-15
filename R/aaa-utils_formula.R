#' @title Parse case-selection formula
#' @description Split a formula of the form `Y ~ X | Z1 + Z2 + ...`
#' into Y, X, and Z names.
#' @param formula an object coercible to formula; use the pipe '|' to separate X and Z
#' @return list(y, x, z) with character vectors
#' @keywords internal
parse_csel_formula <- function(formula) {
  f <- stats::as.formula(formula)
  # Convert to string to find a '|'
  fs <- deparse(f)
  fs <- paste(fs, collapse = " ")
  parts <- strsplit(fs, "\\~")[[1]]
  if (length(parts) != 2) stop("Formula must be of the form Y ~ X [| Z1 + Z2 + ...]")
  y <- trimws(parts[1])
  rhs <- trimws(parts[2])

  # Split RHS at '|'
  rhs_parts <- strsplit(rhs, "\\|")[[1]]
  x_str <- trimws(rhs_parts[1])
  z_str <- if (length(rhs_parts) > 1) trimws(rhs_parts[2]) else ""

  # Extract names via model.frame parsing
  get_terms <- function(s) {
    if (nchar(s) == 0) return(character(0))
    # Use a dummy LHS to parse terms
    tf <- stats::as.formula(paste0("~", s))
    all.vars(tf)
  }

  x <- get_terms(x_str)
  z <- get_terms(z_str)

  # y could be a name or expression; we only allow single outcome name here
  yv <- all.vars(stats::as.formula(paste0(y, "~ 1")))
  if (length(yv) != 1) stop("Left-hand side must be a single outcome variable name.")
  list(y = yv, x = x, z = z)
}
