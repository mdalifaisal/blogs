# Interactive Time Series Graph of Turkish Lira

library(plotly)
library(quantmod)

# Get Turkish Lira to US Dollar exchange rate from Yahoo Finance
symbols <- "TRYUSD=X"
TRY.X <- getSymbols(symbols, src = "yahoo", from = "2011-01-01", auto.assign = FALSE)
df <- data.frame(Date = index(TRY.X), ExchangeRate = as.numeric(TRY.X$`TRYUSD=X.Close`))
#  in this graph we have turned off the annotation arrows but they can be enabled
#  by changing showarrow argument to TRUE
#  Create annotations for arrows at specific points
# annotations <- list(
#   list(
#     x = df$Date[464],
#     y = df$ExchangeRate[464],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[486],
#     y = df$ExchangeRate[486],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[1156],
#     y = df$ExchangeRate[1156],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[1438],
#     y = df$ExchangeRate[1438],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[1961],
#     y = df$ExchangeRate[1961],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[1985],
#     y = df$ExchangeRate[1985],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[2483],
#     y = df$ExchangeRate[2483],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[2596],
#     y = df$ExchangeRate[2596],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   ),
#   list(
#     x = df$Date[2861],
#     y = df$ExchangeRate[2861],
#     xref = "x",
#     yref = "y",
#     text="",
#     showarrow = FALSE,
#     arrowhead = 2,
#     arrowcolor = "black"
#   )
# )
# if using annotations, put annotations=annotations in layout function
# Create interactive time series graph using plotly
fig <- plot_ly(df, x = ~Date, y = ~ExchangeRate, type = "scatter", mode = "lines",
               line = list(color = "#007bff"), name=" ") %>%
  layout( title = "Turkish Lira to US Dollar Exchange Rate",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Exchange Rate"), showlegend=FALSE) %>%
  add_trace(
    x = df$Date[c(464,486,1156,1438,1961,1985,2483,2596,2863)],
    y = df$ExchangeRate[c(464,486,1156,1438,1961,1985,2483,2596,2863)],
    showlegend = FALSE,
    name = " ",
    type = "scatter",
    mode = "markers+lines",
    hovertemplate = c("Date: %{x}<br>Value: %{y}<br>European sovereign debt crisis",
                      "Date: %{x}<br>Value: %{y}<br>Turkey gets IG rating from Fitch",
                      "Date: %{x}<br>Value: %{y}<br>Chinese Stock Crash",
                      "Date: %{x}<br>Value: %{y}<br>Failed coup de'etat",
                      "Date: %{x}<br>Value: %{y}<br>Trade Wars",
                      "Date: %{x}<br>Value: %{y}<br>Two credit agencies downgraded Turkey's ratings",
                      "Date: %{x}<br>Value: %{y}<br>Y2Y CPI goes above 25%",
                      "Date: %{x}<br>Value: %{y}<br>Covid 19",
                      "Date: %{x}<br>Value: %{y}<br>Announcement of KKM scheme"),
    marker = list(
      color = "orange",
      line = list(width = 1),
      size = 8,
      symbol = "triangle"
    ), line = list(color = "rgba(0,0,0,0)")
  ) %>%
  layout(hovermode = "closest") %>%
  style(hoverlabel = list(bgcolor = "white", font = list(size = 14))) %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")

#print(fig)
#htmlwidgets::saveWidget(as_widget(fig), "lira.html")
