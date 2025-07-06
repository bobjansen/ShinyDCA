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
      checkboxInput("show_paths", "Show 20 Sample Paths", value = FALSE)
    ),
    mainPanel(
      plotOutput("dcaPlot"),
      conditionalPanel(
        condition = "input.show_paths == true",
        plotOutput("pathPlot")
      )
    )
  )
)

server <- function(input, output) {
  results <- reactive({
    months_seq <- 1:n_months

    mu_daily <- log(1 + input$mu) / 252
    spec <- create_spec(mu_daily)
    price_mc <- simulate_prices(n_days, n_paths, spec = spec, seed = seed)

    lsum <- simulate_dca(price_mc, months = 1, trades_per_month = 1,
                         gamma = input$gamma)
    dca_ce <- sapply(months_seq, function(m) {
      simulate_dca(price_mc, months = m,
                   trades_per_month = input$trades,
                   gamma = input$gamma)$CE_ratio
    })

    list(
      df = data.frame(month = months_seq,
                      CE = dca_ce,
                      lump_sum = lsum$CE_ratio),
      prices = price_mc
    )
  })

  output$dcaPlot <- renderPlot({
    df <- results()$df
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(month, CE)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      geom_hline(aes(yintercept = lump_sum), linetype = 2, color = "red") +
      labs(x = "Number of Months", y = "Certainty Equivalent",
           title = "DCA vs. Lump Sum") +
      theme_minimal()
  })

  output$pathPlot <- renderPlot({
    req(input$show_paths)
    price_mc <- results()$prices
    if (is.null(price_mc)) return(NULL)
    pick <- sample(ncol(price_mc), 20)
    df <- data.frame(
      Day = rep(seq_len(nrow(price_mc)), times = 20),
      Price = as.vector(price_mc[, pick]),
      Path = rep(seq_len(20), each = nrow(price_mc))
    )
    ggplot(df, aes(Day, Price, group = Path)) +
      geom_line(alpha = 0.6) +
      labs(title = "20 Random Price Paths", y = "Price") +
      theme_minimal()
  })
}

shinyApp(ui, server)

