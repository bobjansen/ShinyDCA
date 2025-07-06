# install.packages("rugarch")           # run once
library(rugarch)

# MC parameters ----
seed <- 42L

n_days <- 252L # 1 trading year
n_paths <- 10000L # Monte-Carlo replications

# SP500 parameters ----
S0 <- 6000
mu_annual <- 0.04
mu_daily <- log(1 + mu_annual) / 252

# Jump parameters ----
jump_lambda      <- 0.005   # expected jumps per day
jump_mean        <- -0.02  # average jump size
jump_sd          <-  0.25  # jump size st.dev.
compensate_drift <- TRUE   # keep the original drift after adding jumps?
negative_only <- TRUE

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

# create a GARCH specification with arbitrary daily drift and target volatility
create_spec <- function(mu_daily, target_vol = 0.0098, alpha1 = 0.03, beta1 = 0.935, shape = 10) {
  # target_vol is daily volatility (e.g., 0.01 for 1% daily)
  omega <- (target_vol^2) * (1 - alpha1 - beta1)
  ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std",
    fixed.pars = list(
      mu     = mu_daily,
      omega  = omega,
      alpha1 = alpha1,
      beta1  = beta1,
      shape  = shape
    )
  )
}

reflected_normal_mean <- function(mu, sigma) {
  # E[-|X|] for X ~ N(mu, sigma^2)
  - ( sigma * sqrt(2 / pi) * exp(- (mu^2) / (2 * sigma^2)) +
        mu * (2 * pnorm(mu / sigma) - 1) )
}

simulate_prices <- function(
    n_days, n_paths,
    S0,
    spec = spx_spec,
    lambda = jump_lambda,
    jmean  = jump_mean,
    jsd    = jump_sd,
    drift_fix   = compensate_drift,
    negative_only = TRUE,
    seed   = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # ---- 1. GARCH-t -------------------------------------------------
  sim_garch <- ugarchpath(spec, n.sim = n_days, m.sim = n_paths)
  r_mc      <- fitted(sim_garch)

  # ---- 2. Jump matrix --------------------------------------------
  jump_occ <- matrix(
    rbinom(n_days * n_paths, size = 1, prob = lambda),
    nrow = n_days
  )

  rdraw <- if (negative_only) {
    function(n) {
      x <- rnorm(n, mean = jmean, sd = jsd)
      ifelse(x > 0, -x, x)
    }
  } else {
    function(n) rnorm(n, mean = jmean, sd = jsd)
  }

  jump_size <- matrix(rdraw(n_days * n_paths), nrow = n_days)
  J         <- jump_occ * jump_size

  # ---- 3. Drift compensation -------------------------------------
  r_mc <- if (drift_fix) {
    if (negative_only) {
      mu_jump <- reflected_normal_mean(jmean, jsd)  # ≈ -0.0270
    } else {
      mu_jump <- jmean
    }
    r_mc - lambda * mu_jump + J
  } else {
    r_mc + J
  }

  # ---- 4. Prices --------------------------------------------------
  returns <- S0 * exp(apply(r_mc, 2, cumsum))

  # ---- 5. Normalize mean return ---------------------------------
  mean_target <- spec@model$fixed.pars$mu
  returns * exp(n_days * mean_target) / (mean(returns[n_days, ]) / S0)
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

portfolio_metrics <- function(price_mc, S0, rf_daily = 0) {
  # price_mc: matrix of prices (rows: days, cols: paths)
  # rf_daily: daily risk-free rate (default 0)

  returns <- diff(price_mc) / price_mc[-nrow(price_mc), ]
  mean_ret <- colMeans(returns)
  sd_ret <- apply(returns, 2, sd)
  sharpe <- (mean_ret - rf_daily) / sd_ret

  # Drawdown calculation
  max_drawdown <- function(prices) {
    cummaxp <- cummax(prices)
    drawdowns <- (prices - cummaxp) / cummaxp
    min(drawdowns)
  }
  drawdowns <- apply(price_mc, 2, max_drawdown)

  # Annualized metrics
  ann_factor <- sqrt(252)
  ann_ret <- price_mc[nrow(price_mc), ] / S0
  ann_vol <- sd_ret * ann_factor

  list(
    avg_sharpe = mean(sharpe),
    avg_drawdown = mean(drawdowns),
    avg_ann_return = mean(ann_ret) - 1,
    median_ann_return = median(ann_ret) - 1,
    avg_ann_vol = mean(ann_vol),
    sharpe = sharpe,
    drawdowns = drawdowns,
    ann_return = ann_ret,
    ann_vol = ann_vol
  )
}

if (sys.nframe() == 0) {
  price_mc <- simulate_prices(n_days, n_paths, S0, seed = seed)
  metrics <- portfolio_metrics(price_mc, S0 = S0)
  print(summary(price_mc[252, ]))
  print(metrics)
  boxplot(price_mc[252, ])
  plot_random_paths(price_mc, n = 20)
}
