############################################################
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R
# Visualization Phase
#
# Student Name: Krupal Shaileshkumar Rathod
# Project: Interactive Stock Portfolio Dashboard
#
# File: Rathod_Krupal_CA_BDA400_A06.R
############################################################


# ==========================================================
# SECTION 1: LOAD REQUIRED LIBRARIES
# ==========================================================

required_packages <- c(
  "shiny", "ggplot2", "quantmod", "dplyr", "tidyr",
  "lubridate", "TTR", "plotly", "DT", "zoo"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))


# ==========================================================
# SECTION 2: PROJECT CONFIGURATION
# ==========================================================

default_stocks <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")
default_start_date <- Sys.Date() - 365
default_end_date <- Sys.Date()

cat("====================================\n")
cat("BDA400 Assignment 6\n")
cat("Technical Analysis Dashboard\n")
cat("Student: Krupal Shaileshkumar Rathod\n")
cat("====================================\n")
cat("\nDefault stock symbols:\n")
print(default_stocks)


# ==========================================================
# SECTION 3: DOWNLOAD STOCK MARKET DATA
# ==========================================================

stock_symbols <- default_stocks
start_date <- default_start_date
end_date <- default_end_date
stock_data <- list()

for (symbol in stock_symbols) {
  cat("\nDownloading:", symbol, "\n")

  tryCatch({
    downloaded_data <- quantmod::getSymbols(
      symbol,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )

    if (NROW(downloaded_data) > 0) {
      stock_data[[symbol]] <- downloaded_data
      cat("Successfully downloaded:", symbol, "\n")
    }
  }, error = function(e) {
    cat("Download failed for", symbol, ":\n")
    cat(conditionMessage(e), "\n")
  })
}

if (length(stock_data) == 0) {
  stop("No stock data was downloaded. Check your internet connection and try again.")
}

stock_symbols <- names(stock_data)
cat("\nStock data download completed!\n")
print(stock_symbols)


# ==========================================================
# SECTION 4: CHECK DOWNLOADED DATA
# ==========================================================

cat("\nDownloaded stock data summary:\n")
for (symbol in names(stock_data)) {
  cat(symbol, ":", NROW(stock_data[[symbol]]), "records\n")
}


# ==========================================================
# SECTION 5: CALCULATE TECHNICAL INDICATORS
# ==========================================================

indicator_data <- list()

for (symbol in names(stock_data)) {
  prices <- quantmod::Cl(stock_data[[symbol]])

  ma20 <- TTR::SMA(prices, n = 20)
  ma50 <- TTR::SMA(prices, n = 50)
  rsi <- TTR::RSI(prices, n = 14)

  macd_values <- TTR::MACD(
    prices,
    nFast = 12,
    nSlow = 26,
    nSig = 9,
    maType = "EMA"
  )

  indicator_data[[symbol]] <- merge(
    prices, ma20, ma50, rsi, macd_values
  )

  colnames(indicator_data[[symbol]]) <- c(
    "Close", "MA20", "MA50", "RSI", "MACD", "Signal"
  )
}

cat("\nTechnical indicators calculated successfully!\n")
print(names(indicator_data))


# ==========================================================
# SECTION 6: GENERATE BUY, SELL AND HOLD SIGNALS
# ==========================================================

signal_data <- list()

for (symbol in names(indicator_data)) {
  stock_xts <- indicator_data[[symbol]]

  data <- data.frame(
    Date = as.Date(zoo::index(stock_xts)),
    zoo::coredata(stock_xts)
  )

  short_ma <- data$MA20
  long_ma <- data$MA50

  previous_short <- c(NA, head(short_ma, -1))
  previous_long <- c(NA, head(long_ma, -1))

  trading_signal <- rep("HOLD", nrow(data))

  valid <- stats::complete.cases(
    short_ma, long_ma, previous_short, previous_long
  )

  buy_condition <- valid &
    short_ma > long_ma &
    previous_short <= previous_long

  sell_condition <- valid &
    short_ma < long_ma &
    previous_short >= previous_long

  trading_signal[buy_condition] <- "BUY"
  trading_signal[sell_condition] <- "SELL"

  data$Trading_Signal <- trading_signal
  signal_data[[symbol]] <- data

  cat("\nTrading signals generated for:", symbol, "\n")
  print(table(data$Trading_Signal))
}

cat("\nAll trading signals generated successfully!\n")


# ==========================================================
# SECTION 7: PREPARE COMPLETE DASHBOARD DATA
# ==========================================================

dashboard_data <- do.call(
  rbind,
  lapply(names(signal_data), function(symbol) {
    raw_data <- stock_data[[symbol]]
    indicators <- signal_data[[symbol]]

    price_data <- data.frame(
      Date = as.Date(zoo::index(raw_data)),
      Open = as.numeric(quantmod::Op(raw_data)),
      High = as.numeric(quantmod::Hi(raw_data)),
      Low = as.numeric(quantmod::Lo(raw_data)),
      Close = as.numeric(quantmod::Cl(raw_data)),
      Volume = as.numeric(quantmod::Vo(raw_data))
    )

    merged_data <- merge(
      price_data,
      indicators[, c(
        "Date", "MA20", "MA50", "RSI",
        "MACD", "Signal", "Trading_Signal"
      )],
      by = "Date",
      all.x = TRUE
    )

    merged_data$Symbol <- symbol
    merged_data
  })
)

rownames(dashboard_data) <- NULL
dashboard_data <- dashboard_data[
  order(dashboard_data$Symbol, dashboard_data$Date),
]

cat("\nDashboard dimensions:\n")
print(dim(dashboard_data))
cat("\nDashboard columns:\n")
print(colnames(dashboard_data))
cat("\nFirst six records:\n")
print(head(dashboard_data))


# ==========================================================
# SECTION 8: SHINY DASHBOARD USER INTERFACE
# ==========================================================

ui <- shiny::fluidPage(
  shiny::titlePanel("Stock Technical Analysis Dashboard"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(
        "stock",
        "Select Stock:",
        choices = stock_symbols,
        selected = stock_symbols[1]
      ),

      shiny::selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line Chart", "Candlestick Chart", "Area Chart"),
        selected = "Line Chart"
      ),

      shiny::checkboxInput(
        "show_ma20", "Show 20-Day Moving Average", value = TRUE
      ),

      shiny::checkboxInput(
        "show_ma50", "Show 50-Day Moving Average", value = TRUE
      ),

      shiny::checkboxInput(
        "show_rsi", "Show RSI Indicator", value = TRUE
      ),

      shiny::checkboxInput(
        "show_macd", "Show MACD Indicator", value = TRUE
      )
    ),

    shiny::mainPanel(
      shiny::h3("Stock Price Chart"),
      plotly::plotlyOutput("price_chart", height = "450px"),

      shiny::h3("RSI Indicator"),
      shiny::conditionalPanel(
        condition = "input.show_rsi == true",
        plotly::plotlyOutput("rsi_chart", height = "300px")
      ),

      shiny::h3("MACD Indicator"),
      shiny::conditionalPanel(
        condition = "input.show_macd == true",
        plotly::plotlyOutput("macd_chart", height = "300px")
      ),

      shiny::h3("Trading Signals"),
      DT::DTOutput("signal_table")
    )
  )
)


# ==========================================================
# SECTION 9: SHINY SERVER LOGIC
# ==========================================================

server <- function(input, output, session) {

  selected_data <- shiny::reactive({
    req(input$stock)

    dashboard_data %>%
      dplyr::filter(Symbol == input$stock) %>%
      dplyr::arrange(Date)
  })

  output$price_chart <- plotly::renderPlotly({
    data <- selected_data()

    shiny::validate(
      shiny::need(nrow(data) > 0, "No stock data available.")
    )

    if (input$chart_type == "Candlestick Chart") {
      p <- plotly::plot_ly(
        data,
        x = ~Date,
        type = "candlestick",
        open = ~Open,
        high = ~High,
        low = ~Low,
        close = ~Close,
        name = input$stock
      )

      if (isTRUE(input$show_ma20)) {
        p <- p %>% plotly::add_lines(
          x = data$Date,
          y = data$MA20,
          name = "MA20",
          connectgaps = FALSE
        )
      }

      if (isTRUE(input$show_ma50)) {
        p <- p %>% plotly::add_lines(
          x = data$Date,
          y = data$MA50,
          name = "MA50",
          connectgaps = FALSE
        )
      }

      p %>% plotly::layout(
        title = paste(input$stock, "Candlestick Chart"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)")
      )

    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Close))

      if (input$chart_type == "Area Chart") {
        p <- p + ggplot2::geom_area(
          fill = "skyblue", alpha = 0.45
        ) +
          ggplot2::geom_line(
            color = "blue", linewidth = 0.7
          )
      } else {
        p <- p + ggplot2::geom_line(
          color = "blue", linewidth = 0.8
        )
      }

      if (isTRUE(input$show_ma20)) {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(y = MA20, color = "MA20"),
          linewidth = 0.8, na.rm = TRUE
        )
      }

      if (isTRUE(input$show_ma50)) {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(y = MA50, color = "MA50"),
          linewidth = 0.8, na.rm = TRUE
        )
      }

      p <- p +
        ggplot2::labs(
          title = paste(input$stock, input$chart_type),
          x = "Date",
          y = "Closing Price (USD)",
          color = "Indicator"
        ) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    }
  })

  output$rsi_chart <- plotly::renderPlotly({
    shiny::req(input$show_rsi)

    data <- selected_data()

    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = Date, y = RSI)
    ) +
      ggplot2::geom_line(color = "purple", linewidth = 0.8, na.rm = TRUE) +
      ggplot2::geom_hline(
        yintercept = 70, linetype = "dashed"
      ) +
      ggplot2::geom_hline(
        yintercept = 30, linetype = "dashed"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 100)) +
      ggplot2::labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI"
      ) +
      ggplot2::theme_minimal()

    plotly::ggplotly(p)
  })

  output$macd_chart <- plotly::renderPlotly({
    shiny::req(input$show_macd)

    data <- selected_data()

    p <- ggplot2::ggplot(data, ggplot2::aes(x = Date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = MACD, color = "MACD"),
        linewidth = 0.8, na.rm = TRUE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = Signal, color = "Signal"),
        linewidth = 0.8, na.rm = TRUE
      ) +
      ggplot2::labs(
        title = "MACD Indicator",
        x = "Date",
        y = "Value",
        color = "Legend"
      ) +
      ggplot2::theme_minimal()

    plotly::ggplotly(p)
  })

  output$signal_table <- DT::renderDT({
    selected_data() %>%
      dplyr::filter(Trading_Signal != "HOLD") %>%
      dplyr::select(
        Date, Symbol, Close, MA20, MA50, Trading_Signal
      ) %>%
      dplyr::arrange(dplyr::desc(Date))
  }, options = list(pageLength = 10, scrollX = TRUE))
}


# ==========================================================
# SECTION 10: RUN SHINY DASHBOARD
# ==========================================================

shiny::shinyApp(ui = ui, server = server)
