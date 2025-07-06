# install.packages("rugarch")           # run once
library(rugarch)

seed <- 42L
S0 <- 6000
n_days <- 252L # 1 trading year
n_paths <- 10000L # Monte-Carlo replications
mu_annual <- 0.06
mu_daily <- log(1 + mu_annual) / 252

# 1. Specify a GARCH(1,1) with Student-t errors ----
# ChatGPT o3 found some specs here:
# https://vlab.stern.nyu.edu/volatility/VOL.SPX%3AIND-R.GARCH
# I tweaked them till the summary stats for the last day and plots looked
# decent.
spx_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std", # Student-t
  fixed.pars = list(
    mu     = mu_daily, # daily drift
    omega  = 6e-6, # ⇒  σ² = ω / (1-α-β) = 0.0001  →  σ ≈ 1 %
    alpha1 = 0.03, # short-run shock
    beta1  = 0.935, # persistence
    shape  = 10 # slightly lighter tails than df = 7
  )
)


simulate_prices <- function(n_days, n_path, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  sim_paths <- ugarchpath(spx_spec, n.sim = n_days, m.sim = n_paths)

  # matrix of simulated returns (n_days × n_paths)
  r_mc <- fitted(sim_paths)

  S0 * exp(apply(r_mc, 2, cumsum))
}

plot_random_paths <- function(price_mc, n = 10, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  pick <- sample(ncol(price_mc), n)

  matplot(
    x = price_mc[, pick],
    type = "l", lty = 1, lwd = 1.2,
    xlab = "Day", ylab = "Simulated S&P-500 level",
    main = paste(n, "Random Monte-Carlo Price Paths")
  )

  legend(
    "topright",
    legend = paste("Path", seq_along(pick)),
    col = 1:10, lty = 1, bty = "n", cex = 0.7
  )
}

price_mc <- simulate_prices(n_days, n_paths, seed = seed)
print(summary(price_mc[252, ]))
boxplot(price_mc[252, ])
plot_random_paths(price_mc, n = 20)
