# Run this once locally, then save the .rda files to data/
set.seed(123)

# Cross-section: 40 units
n <- 40
toy_cross <- data.frame(
  unit = paste0("u", seq_len(n)),
  X  = rnorm(n, 0, 1),
  Z1 = rnorm(n, 0, 1),
  Z2 = rnorm(n, 0, 1)
)
# Outcome with signal from X and Z1
toy_cross$Y <- 1.0 * toy_cross$X + 0.5 * toy_cross$Z1 + rnorm(n, 0, 0.7)

# Panel: 10 units x 6 periods
U <- 10; Tt <- 6
toy_panel <- expand.grid(unit = paste0("g", seq_len(U)), t = seq_len(Tt))
toy_panel <- toy_panel[order(toy_panel$unit, toy_panel$t), ]
# Random walks
toy_panel$X  <- ave(rnorm(nrow(toy_panel), 0, 1), toy_panel$unit, FUN = cumsum)
toy_panel$Z1 <- ave(rnorm(nrow(toy_panel), 0, 0.5), toy_panel$unit, FUN = cumsum)
# Outcome depends on lagged X and Z1, plus noise
toy_panel$Y <- 0
for (u in unique(toy_panel$unit)) {
  ix <- which(toy_panel$unit == u)
  x  <- toy_panel$X[ix]
  z1 <- toy_panel$Z1[ix]
  y  <- numeric(length(ix))
  for (k in seq_along(ix)) {
    y[k] <- 0.8 * ifelse(k>1, x[k-1], x[k]) + 0.2 * z1[k] + rnorm(1, 0, 0.8)
  }
  toy_panel$Y[ix] <- y
}

# Save
usethis::use_data(toy_cross, overwrite = TRUE)
usethis::use_data(toy_panel, overwrite = TRUE)
