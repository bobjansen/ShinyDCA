library(shiny)
library(ggplot2)
library(rugarch)

# ---- Simulation functions from sim.R ----

# Parameters for Monte Carlo simulation
seed <- 42L
n_days <- 252L
n_paths <- 1000L  # reduce paths to keep the app responsive

S0 <- 6000

jump_lambda      <- 0.02
jump_mean        <- -0.02
jump_sd          <- 0.05
compensate_drift <- TRUE
negative_only    <- TRUE

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
  - ( sigma * sqrt(2 / pi) * exp(- (mu^2) / (2 * sigma^2)) +
        mu * (2 * pnorm(mu / sigma) - 1) )
}

simulate_prices <- function(
    n_days, n_paths, spec,
    lambda = jump_lambda,
    jmean  = jump_mean,
    jsd    = jump_sd,
    drift_fix   = compensate_drift,
    negative_only = TRUE,
    seed   = NULL) {

  if (!is.null(seed)) set.seed(seed)

  sim_garch <- ugarchpath(spec, n.sim = n_days, m.sim = n_paths)
  r_mc      <- fitted(sim_garch)

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

  r_mc <- if (drift_fix) {
    if (negative_only) {
      mu_jump <- reflected_normal_mean(jmean, jsd)
    } else {
      mu_jump <- jmean
    }
    r_mc - lambda * mu_jump + J
  } else {
    r_mc + J
  }

  S0 * exp(apply(r_mc, 2, cumsum))
}

# Prices will be simulated each time the user runs the app

# ---- DCA function ----
simulate_dca <- function(P, total = 1e6, months = 1,
                         trades_per_month = 1, gamma = 1) {
  n_days <- nrow(P)
  avg_month_len <- floor(n_days / 12)
  month_starts <- 1 + avg_month_len * (0:11)
  invest_months <- month_starts[seq_len(months)]
  offsets <- 0:(trades_per_month - 1)
  invest_dates <- sort(rep(invest_months, each = trades_per_month) +
                         rep(offsets, times = months))
  invest_dates <- invest_dates[invest_dates <= n_days]

  allot <- total / length(invest_dates)
  shares_mat <- allot / P[invest_dates, ]
  shares_total <- if (is.null(dim(shares_mat))) shares_mat else colSums(shares_mat)

  W_final <- shares_total * P[n_days, ]
  relative_wealth <- W_final / total

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

# ---- Shiny app ----
ui <- fluidPage(
  titlePanel("Dollar-Cost Averaging Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("months", "Months to Invest", min = 1, max = 12, value = 6),
      numericInput("trades", "Trades per Month", value = 1, min = 1, max = 12),
      numericInput("mu", "Annual Return Assumption", value = 0.04,
                   min = -0.5, max = 0.5, step = 0.01),
      numericInput("gamma", "Risk Aversion (gamma)", value = 1, min = 0.1, max = 5,
                   step = 0.1),
      actionButton("run", "Run Simulation")
    ),
    mainPanel(
      plotOutput("dcaPlot")
    )
  )
)

server <- function(input, output) {
  results <- eventReactive(input$run, {
    months_seq <- 1:input$months

    mu_daily <- log(1 + input$mu) / 252
    spec <- create_spec(mu_daily)
    price_mc <- simulate_prices(n_days, n_paths, spec = spec, seed = seed)

    lsum <- simulate_dca(price_mc, months = 1, trades_per_month = 1,
                         gamma = input$gamma)
    dca_eu <- sapply(months_seq, function(m) {
      simulate_dca(price_mc, months = m,
                   trades_per_month = input$trades,
                   gamma = input$gamma)$EU
    })

    data.frame(month = months_seq, EU = dca_eu, lump_sum = lsum$EU)
  })

  output$dcaPlot <- renderPlot({
    df <- results()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(month, EU)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      geom_hline(aes(yintercept = lump_sum), linetype = 2, color = "red") +
      labs(x = "Number of Months", y = "Expected Utility",
           title = "DCA vs. Lump Sum") +
      theme_minimal()
  })
}

shinyApp(ui, server)

