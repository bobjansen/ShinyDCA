library(ggplot2)

# source("sim.R")

gammas <- 1:4

trades_per_month_option <- 5
trades_per_month_text <- paste0("DCA ", trades_per_month_option, " -trades")

# Helper: generic DCA simulator under log-utility
simulate_dca <- function(
    P, # price matrix (n_days × n_paths)
    total = 1e6, # dollars to invest
    months = 1, # spread over 1…12 months
    trades_per_month = 1, # 1 or 3,
    gamma = 1 # CRRA lambda, with gamma = 1: log utility
    ) {
  n_days <- nrow(P)
  n_paths <- ncol(P)

  # pick (approximate) first trading day of each calendar month ----
  avg_month_len <- floor(n_days / 12) # ≈ 21
  month_starts <- 1 + avg_month_len * (0:11) # length-12 vector
  invest_months <- month_starts[seq_len(months)]

  # build the full set of investment dates ----
  offsets <- 0:(trades_per_month - 1)
  invest_dates <- sort(rep(invest_months, each = trades_per_month) +
    rep(offsets, times = months))
  invest_dates <- invest_dates[invest_dates <= n_days] # just in case

  # dollar amount per trade ----
  allot <- total / length(invest_dates)

  # shares bought on each path & date ----
  shares_mat <- allot / P[invest_dates, ] # matrix (n_trades × n_paths)
  # Calculate the number of shares obtained by investing over the period
  shares_total <- if (is.null(dim(shares_mat))) {
    shares_mat
  } else {
    colSums(shares_mat)
  }

  # final wealth & log-utility ----
  W_final <- shares_total * P[n_days, ]
  relative_wealth <- W_final / total

  # CRRA utility ----
  if (abs(gamma - 1) < 1e-10) {
    util <- log(relative_wealth)
    CE_ratio <- exp(mean(util))
  } else {
    util <- (relative_wealth^(1 - gamma) - 1) / (1 - gamma)
    CE_ratio <- (mean(relative_wealth^(1 - gamma)))^(1 / (1 - gamma))
  }

  list(
    wealth = W_final,
    EU = mean(util),
    CE_ratio = CE_ratio,
    CE_absolute = CE_ratio * total
  )
}

if (sys.nframe() == 0) {
  res_all <- list()
  for (gamma in gammas) {
    #  Run all strategies ----
    strategies <- data.frame(
      strategy = character(),
      months = integer(),
      EU = numeric(),
      CE = numeric()
    )

    ## 0) Lump-sum benchmark
    lsum <- simulate_dca(price_mc, months = 1, trades_per_month = 1, gamma = gamma) # all on day-1
    strategies <- rbind(
      strategies,
      data.frame(
        strategy = "Lump sum",
        months = 0,
        EU = lsum$EU,
        CE = lsum$CE_absolute
      )
    )

    ## 1) DCA: 1 trade / month
    for (m in 1:12) {
      res <- simulate_dca(price_mc, months = m, trades_per_month = 1, gamma = gamma)
      strategies <- rbind(
        strategies,
        data.frame(
          strategy = "DCA 1-trade",
          months = m,
          EU = res$EU,
          CE = res$CE_absolute
        )
      )
    }

    ## 2) DCA: 3 trades / month
    for (m in 1:12) {
      res <- simulate_dca(
        price_mc,
        months = m,
        trades_per_month = trades_per_month_option,
        gamma = gamma
      )
      strategies <- rbind(
        strategies,
        data.frame(
          strategy = trades_per_month_text,
          months = m,
          EU = res$EU,
          CE = res$CE_absolute
        )
      )
    }

    print(strategies, row.names = FALSE, digits = 4)

    res_all[[as.character(gamma)]] <- strategies
  }

  ##  Visualise ----
  df <- do.call(rbind, Map(cbind,
    gamma = gammas,
    res_all
  ))

  ## prettier labels for legend
  df$strategy <- factor(df$strategy,
    levels = c("Lump sum", "DCA 1-trade", trades_per_month_text)
  )

  ggplot(
    df[df$strategy != "Lump sum", ], # plot DCA lines …
    aes(months, EU,
      colour = strategy,
      linetype = strategy, shape = strategy
    )
  ) +
    geom_line() +
    geom_point(size = 2) +
    geom_hline(
      data = df[df$strategy == "Lump sum", ],
      aes(yintercept = EU),
      linetype = 3, colour = "black"
    ) +
    facet_wrap(~gamma,
      ncol = 2, scales = "fixed",
      labeller = label_bquote(gamma == .(gamma))
    ) +
    labs(
      x = "Number of months invested",
      y = "Expected CRRA utility",
      colour = "", linetype = "", shape = "",
      title = paste(
        "Dollar-Cost Averaging vs. Lump-Sum for γ = ",
        paste(gammas, collapse = ", ")
      )
    ) +
    scale_x_continuous(breaks = 0:12) +
    theme_bw() +
    theme(legend.position = "bottom")
}
