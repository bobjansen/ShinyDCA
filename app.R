library(shiny)
library(ggplot2)
library(rugarch)

source("sim.R")
source("dca.R")

# reduce paths to keep the app responsive
n_paths <- 1000L
n_months <- 12L

# ---- Shiny app ----
ui <- fluidPage(
  titlePanel("Dollar-Cost Averaging Simulation"),
  sidebarLayout(
    sidebarPanel(
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
    months_seq <- 1:n_months

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

