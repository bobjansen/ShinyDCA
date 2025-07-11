library(shiny)
library(ggplot2)
library(rugarch)

source("sim.R")
source("dca.R")

n_paths <- 5000L
n_months <- 12L
S0 <- 6000

# Shiny app ----
ui <- fluidPage(
  titlePanel("Dollar-Cost Averaging Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("trades", "Trades per Month", value = 1, min = 1, max = 12),
      numericInput("mu", "Annual Return Assumption",
        value = 0.04,
        min = -0.5, max = 0.5, step = 0.01
      ),
      numericInput("gamma", "Risk Aversion (gamma)",
        value = 1, min = 0.1, max = 5,
        step = 0.1
      ),
      numericInput("rf", "Risk-Free Rate (annual, decimal)",
        value = 0, min = -0.1, max = 0.2, step = 0.01
      ),
      numericInput("seed", "Random Seed",
        value = 42, min = 1, max = 1e6, step = 1
      ),
      tags$hr(),
      tags$details(
        tags$summary(HTML('Advanced Settings (GARCH Model) <span style="font-size:0.9em;">&#9654;</span>')),
        numericInput("alpha", "GARCH: alpha",
          value = 0.05, min = 0, max = 1, step = 0.01
        ),
        numericInput("beta", "GARCH: beta",
          value = 0.9, min = 0, max = 1, step = 0.01
        ),
        numericInput("shape", "GARCH: shape (Student-t df)",
          value = 10, min = 2.1, max = 50, step = 0.1
        ),
        tags$hr(),
        numericInput("jump_lambda", "Jump: lambda (expected jumps/day)",
          value = 0.005, min = 0, max = 0.1, step = 0.001
        ),
        numericInput("jump_mean", "Jump: mean (average jump size)",
          value = -0.02, min = -1, max = 1, step = 0.01
        ),
        numericInput("jump_sd", "Jump: sd (jump size st.dev.)",
          value = 0.25, min = 0, max = 2, step = 0.01
        ),
        checkboxInput("compensate_drift", "Compensate drift for jumps?", value = TRUE),
        checkboxInput("negative_only", "Negative jumps only?", value = TRUE)
      )
    ),
    mainPanel(
      plotOutput("dcaPlot"),
      plotOutput("pathPlot"),
      tableOutput("metricsTable")
    )
  )
)

server <- function(input, output) {
  results <- reactive({
    months_seq <- 1:n_months

    mu_daily <- log(1 + input$mu) / 252
    spec <- create_spec(
      mu_daily,
      alpha1 = input$alpha,
      beta1 = input$beta,
      shape = input$shape
    )
    price_mc <- simulate_prices(
      n_days, n_paths, S0, spec = spec, seed = input$seed,
      lambda = input$jump_lambda,
      jmean = input$jump_mean,
      jsd = input$jump_sd,
      drift_fix = input$compensate_drift,
      negative_only = input$negative_only
    )

    lsum <- simulate_dca(price_mc,
      months = 1, trades_per_month = 1,
      gamma = input$gamma,
      rf = input$rf
    )
    dca_ce <- sapply(months_seq, function(m) {
      simulate_dca(price_mc,
        months = m,
        trades_per_month = input$trades,
        gamma = input$gamma,
        rf = input$rf
      )$CE_ratio
    })

    list(
      df = data.frame(
        month = months_seq,
        CE = dca_ce,
        lump_sum = lsum$CE_ratio
      ),
      prices = price_mc,
      metrics = portfolio_metrics(price_mc, S0)
    )
  })

  output$dcaPlot <- renderPlot({
    df <- results()$df
    if (nrow(df) == 0) {
      return()
    }
    ggplot(df, aes(month, CE)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      geom_hline(aes(yintercept = lump_sum), linetype = 2, color = "red") +
      labs(
        x = "Number of Months", y = "Certainty Equivalent",
        title = "DCA vs. Lump Sum"
      ) +
      scale_x_continuous(breaks = 0:12) +
      theme_minimal()
  })

  output$pathPlot <- renderPlot({
    price_mc <- results()$prices
    if (is.null(price_mc)) {
      return()
    }
    pick <- sample(ncol(price_mc), 20)
    df <- data.frame(
      Day = rep(seq_len(nrow(price_mc)), times = 20),
      Price = as.vector(price_mc[, pick]),
      Path = factor(rep(seq_len(20), each = nrow(price_mc)))
    )
    ggplot(df, aes(Day, Price, group = Path, color = Path)) +
      geom_line(alpha = 0.6) +
      labs(title = "20 Random Price Paths", y = "Price") +
      theme_minimal() +
      guides(color = "none")
  })

  output$metricsTable <- renderTable(
    {
      m <- results()$metrics
      if (is.null(m)) {
        return()
      }
      data.frame(
        Metric = c(
          "Avg. Ann. Return %",
          "Median Ann. Return %",
          "Avg. Ann. Volatility",
          "Avg. Sharpe Ratio",
          "Avg. Max Drawdown"
        ),
        Value = c(
          round(m$avg_ann_return * 100, 3),
          round(m$median_ann_return * 100, 3),
          round(m$avg_ann_vol * 100, 3),
          round(m$avg_sharpe, 3),
          round(m$avg_drawdown, 3)
        )
      )
    },
    digits = 3,
    rownames = FALSE
  )
}

shinyApp(ui, server)
