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
jump_lambda      <- 0.02   # expected jumps per day
jump_mean        <- -0.02  # average jump size
jump_sd          <-  0.05  # jump size st.dev.
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

# create a GARCH specification with arbitrary daily drift
create_spec <- function(mu_daily) {
  ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std",
    fixed.pars = list(
      mu     = mu_daily,
      omega  = 6e-6,
      alpha1 = 0.03,
      beta1  = 0.935,
      shape  = 10
    )
  )
}

reflected_normal_mean <- function(mu, sigma) {
  # E[-|X|] for X ~ N(mu, sigma^2)
  - ( sigma * sqrt(2 / pi) * exp(- (mu^2) / (2 * sigma^2)) +
        mu * (2 * pnorm(mu / sigma) - 1) )
}

simulate_prices <- function(
    n_days, n_paths, spec = spx_spec,
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

if (sys.nframe() == 0) {
  price_mc <- simulate_prices(n_days, n_paths, seed = seed)
  print(summary(price_mc[252, ]))
  boxplot(price_mc[252, ])
  plot_random_paths(price_mc, n = 20)
}
