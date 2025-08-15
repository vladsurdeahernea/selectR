#' @title Select cases for intensive analysis
#' @description Unified API for algorithmic case selection strategies.
#' @param data data.frame
#' @param formula Y ~ X | Z1 + Z2 + ...
#' @param id optional unit id for panels (unquoted name or character string)
#' @param time optional time var for panels (unquoted name or character string)
#' @param type strategy key (e.g., "typical","deviant","influential","most_similar_expl",
#'   "most_similar_est","most_different","diverse","extreme","outcome","index",
#'   "longitudinal","pathway")
#' @param n number of cases or pairs to return
#' @param model "lm" or "glm"
#' @param family optional glm family
#' @param distance "mahalanobis" (default)
#' @param match_method "nearest" (default)
#' @param quantiles numeric vector for diverse/extreme
#' @param seed optional RNG seed (recorded in audit)
#' @param ... reserved
#' @return csel object
#' @export
select_cases <- function(
  data,
  formula,
  id = NULL,
  time = NULL,
  type,
  n = 1,
  model = c("lm","glm"),
  family = NULL,
  distance = c("mahalanobis"),
  match_method = c("nearest"),
  quantiles = c(0.1, 0.5, 0.9),
  seed = NULL,
  ...
) {
  data <- coerce_df(data)
  model <- match.arg(model)
  distance <- match.arg(distance)
  match_method <- match.arg(match_method)
  if (!is.null(seed)) set.seed(seed)

  # Normalize id/time to character names if provided as symbols
  id_name <- NULL
  if (!missing(id) && !is.null(id)) id_name <- if (is.character(id)) id else deparse(substitute(id))
  time_name <- NULL
  if (!missing(time) && !is.null(time)) time_name <- if (is.character(time)) time else deparse(substitute(time))

  f <- parse_csel_formula(formula)
  y <- f$y; x <- f$x; z <- f$z
  all_vars <- unique(c(y, x, z, id_name, time_name))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars)) stop("Missing variables in `data`: ", paste(missing_vars, collapse = ", "))

  spec <- list(
    y = y, x = x, z = z, id = id_name, time = time_name,
    type = type, goal = NA_character_, model = model, family = family,
    distance = distance, match_method = match_method, quantiles = quantiles, seed = seed
  )

  audit <- list(
    mk_audit(1, "Defined question and population (user-provided)."),
    mk_audit(2, "Constructed sampling frame from provided `data`."),
    mk_audit(3, sprintf("Measured D/X/Z/Y via formula: Y=%s; X=%s; Z=%s", y, paste(x, collapse=", "), paste(z, collapse=", ")))
  )

  out <- switch(
    tolower(type),
    typical        = strategy_typical(data, y, x, z, model, family, n, audit, spec),
    deviant        = strategy_deviant(data, y, x, z, model, family, n, audit, spec),
    extreme        = strategy_extreme(data, y, x, z, n, quantiles, audit, spec),
    diverse        = strategy_diverse(data, y, x, z, n, quantiles, audit, spec),
    influential    = strategy_influential(data, y, x, z, model, family, n, audit, spec),
    most_similar_expl = strategy_most_similar(data, y, x, z, n, distance, mode = "exploratory", audit
