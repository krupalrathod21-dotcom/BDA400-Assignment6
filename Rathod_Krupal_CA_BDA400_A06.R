
############################################################
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R
# Visualization Phase
#
# Student Name: Krupal Shaileshkumar Rathod
# Project: Interactive Stock Portfolio Dashboard
# File: Rathod_Krupal_CA_BDA400_A06.R
############################################################

# SECTION 1: LOAD REQUIRED PACKAGES

required_packages <- c(
  "shiny", "ggplot2", "quantmod", "dplyr",
  "lubridate", "TTR", "plotly", "DT", "zoo"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace,
          logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library,
                 character.only = TRUE))

# SECTION 2: CONFIGURATION

default_stocks <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")
start_date <- Sys.Date() - 1825
end_date <- Sys.Date()

# SECTION 3: DOWNLOAD STOCK DATA

stock_data <- list()

for (symbol in default_stocks) {
  cat("Downloading:", symbol, "\n")
  
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
      cat("Downloaded:", symbol, "\n")
    }
  }, error = function(e) {
    cat("Download failed for", symbol, ":",
        conditionMessage(e), "\n")
  })
}

if (length(stock_data) == 0) {
  stop("No stock data downloaded. Check your internet connection.")
}

stock_symbols <- names(stock_data)

# SECTION 4: PREPARE DAILY OHLC DATA

prepare_daily_data <- function(stock_xts, symbol) {
  
  data <- data.frame(
    Date = as.Date(zoo::index(stock_xts)),
    Open = as.numeric(quantmod::Op(stock_xts)),
    High = as.numeric(quantmod::Hi(stock_xts)),
    Low = as.numeric(quantmod::Lo(stock_xts)),
    Close = as.numeric(quantmod::Cl(stock_xts)),
    Volume = as.numeric(quantmod::Vo(stock_xts))
  )
  
  data$Symbol <- symbol
  data
}

daily_data <- do.call(
  rbind,
  lapply(names(stock_data), function(symbol) {
    prepare_daily_data(stock_data[[symbol]], symbol)
  })
)

rownames(daily_data) <- NULL

# SECTION 5: AGGREGATE DATA BY TIMEFRAME

aggregate_stock_data <- function(data, timeframe) {
  
  if (timeframe == "Daily") {
    return(data)
  }
  
  data <- data %>%
    dplyr::mutate(
      Period = if (timeframe == "Weekly") {
        lubridate::floor_date(Date, unit = "week",
                              week_start = 1)
      } else {
        lubridate::floor_date(Date, unit = "month")
      }
    ) %>%
    dplyr::group_by(Period, Symbol) %>%
    dplyr::summarise(
      Date = min(Date),
      Open = dplyr::first(Open),
      High = max(High, na.rm = TRUE),
      Low = min(Low, na.rm = TRUE),
      Close = dplyr::last(Close),
      Volume = sum(Volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(Date, Open, High, Low, Close,
                  Volume, Symbol) %>%
    dplyr::arrange(Date)
  
  data
}

# SECTION 6: CALCULATE INDICATORS AND TRADING SIGNALS

calculate_indicators <- function(data) {
  
  data <- data %>% dplyr::arrange(Date)
  
  data$MA20 <- as.numeric(TTR::SMA(data$Close, n = 20))
  data$MA50 <- as.numeric(TTR::SMA(data$Close, n = 50))
  data$RSI <- as.numeric(TTR::RSI(data$Close, n = 14))
  
  macd_values <- TTR::MACD(
    data$Close,
    nFast = 12,
    nSlow = 26,
    nSig = 9,
    maType = "EMA"
  )
  
  data$MACD <- as.numeric(macd_values[, 1])
  data$MACD_Signal <- as.numeric(macd_values[, 2])
  
  previous_short <- c(NA, head(data$MA20, -1))
  previous_long <- c(NA, head(data$MA50, -1))
  
  valid <- stats::complete.cases(
    data$MA20, data$MA50,
    previous_short, previous_long
  )
  
  data$Trading_Signal <- "HOLD"
  
  buy_condition <- valid &
    data$MA20 > data$MA50 &
    previous_short <= previous_long
  
  sell_condition <- valid &
    data$MA20 < data$MA50 &
    previous_short >= previous_long
  
  data$Trading_Signal[buy_condition] <- "BUY"
  data$Trading_Signal[sell_condition] <- "SELL"
  
  data
}

# SECTION 7: SHINY USER INTERFACE

ui <- shiny::fluidPage(
  
  shiny::titlePanel(
    "Interactive Stock Portfolio Technical Analysis Dashboard"
  ),
  
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
        choices = c(
          "Line Chart",
          "Candlestick Chart",
          "Area Chart"
        ),
        selected = "Line Chart"
      ),
      
      shiny::selectInput(
        "timeframe",
        "Select Timeframe:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      shiny::dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = min(daily_data$Date),
        end = max(daily_data$Date),
        min = min(daily_data$Date),
        max = max(daily_data$Date),
        format = "yyyy-mm-dd"
      ),
      
      shiny::hr(),
      
      shiny::checkboxInput(
        "show_ma20",
        "Show 20-Period Moving Average",
        value = TRUE
      ),
      
      shiny::checkboxInput(
        "show_ma50",
        "Show 50-Period Moving Average",
        value = TRUE
      ),
      
      shiny::checkboxInput(
        "show_rsi",
        "Show RSI Indicator",
        value = TRUE
      ),
      
      shiny::checkboxInput(
        "show_macd",
        "Show MACD Indicator",
        value = TRUE
      )
    ),
    
    shiny::mainPanel(
      
      shiny::h3("Stock Price Chart"),
      plotly::plotlyOutput("price_chart", height = "500px"),
      
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

# SECTION 8: SHINY SERVER

server <- function(input, output, session) {
  
  # Prepare selected stock and timeframe data
  
  timeframe_data <- shiny::reactive({
    
    shiny::req(input$stock, input$timeframe)
    
    data <- daily_data %>%
      dplyr::filter(Symbol == input$stock)
    
    data <- aggregate_stock_data(data, input$timeframe)
    
    calculate_indicators(data)
  })
  
  # Apply selected date range after calculating indicators
  
  selected_data <- shiny::reactive({
    
    shiny::req(input$date_range)
    
    data <- timeframe_data()
    
    data %>%
      dplyr::filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      ) %>%
      dplyr::arrange(Date)
  })
  
  # PRICE CHART: LINE, AREA AND CANDLESTICK
  
  output$price_chart <- plotly::renderPlotly({
    
    data <- selected_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0,
                  "No data available for the selected date range.")
    )
    
    signals <- data %>%
      dplyr::filter(Trading_Signal %in% c("BUY", "SELL"))
    
    if (input$chart_type == "Candlestick Chart") {
      
      p <- plotly::plot_ly(
        data = data,
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
      
      if (nrow(signals) > 0) {
        p <- p %>% plotly::add_markers(
          data = signals,
          x = ~Date,
          y = ~Close,
          color = ~Trading_Signal,
          colors = c("BUY" = "green", "SELL" = "red"),
          marker = list(size = 10),
          text = ~paste(Trading_Signal, Date),
          name = "Trading Signals"
        )
      }
      
      p %>% plotly::layout(
        title = paste(input$stock, "Candlestick Chart"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)")
      )
      
    } else {
      
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = Date, y = Close)
      )
      
      if (input$chart_type == "Area Chart") {
        p <- p +
          ggplot2::geom_area(
            fill = "skyblue",
            alpha = 0.4
          ) +
          ggplot2::geom_line(
            color = "blue",
            linewidth = 0.7
          )
      } else {
        p <- p +
          ggplot2::geom_line(
            color = "blue",
            linewidth = 0.8
          )
      }
      
      if (isTRUE(input$show_ma20)) {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(y = MA20, color = "MA20"),
            linewidth = 0.8,
            na.rm = TRUE
          )
      }
      
      if (isTRUE(input$show_ma50)) {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(y = MA50, color = "MA50"),
            linewidth = 0.8,
            na.rm = TRUE
          )
      }
      
      if (nrow(signals) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = signals,
            ggplot2::aes(
              x = Date,
              y = Close,
              color = Trading_Signal,
              shape = Trading_Signal
            ),
            size = 3
          )
      }
      
      p <- p +
        ggplot2::scale_color_manual(
          values = c(
            "MA20" = "orange",
            "MA50" = "purple",
            "BUY" = "green",
            "SELL" = "red"
          )
        ) +
        ggplot2::labs(
          title = paste(input$stock, input$chart_type),
          x = "Date",
          y = "Price (USD)",
          color = "Legend",
          shape = "Trading Signal"
        ) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p)
    }
  })
  
  # RSI CHART
  
  output$rsi_chart <- plotly::renderPlotly({
    
    shiny::req(input$show_rsi)
    
    data <- selected_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "No RSI data available.")
    )
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = Date, y = RSI)
    ) +
      ggplot2::geom_line(
        color = "purple",
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::geom_hline(
        yintercept = 70,
        linetype = "dashed",
        color = "red"
      ) +
      ggplot2::geom_hline(
        yintercept = 30,
        linetype = "dashed",
        color = "green"
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
  
  # MACD CHART
  
  output$macd_chart <- plotly::renderPlotly({
    
    shiny::req(input$show_macd)
    
    data <- selected_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "No MACD data available.")
    )
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = Date)
    ) +
      ggplot2::geom_line(
        ggplot2::aes(y = MACD, color = "MACD"),
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = MACD_Signal, color = "Signal"),
        linewidth = 0.8,
        na.rm = TRUE
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
  
  # TRADING SIGNAL TABLE
  
  output$signal_table <- DT::renderDT({
    
    selected_data() %>%
      dplyr::filter(Trading_Signal != "HOLD") %>%
      dplyr::select(
        Date, Symbol, Close, MA20, MA50, Trading_Signal
      ) %>%
      dplyr::arrange(dplyr::desc(Date))
    
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
}

# SECTION 9: RUN DASHBOARD

shiny::shinyApp(ui = ui, server = server)
