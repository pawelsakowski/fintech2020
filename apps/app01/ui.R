library(shiny)
library(dygraphs)
library(tidyverse)

wregular            <- readRDS("../../data/regular/wregular.rds")
wcrypto             <- readRDS("../../data/crypto/wcrypto.rds")

last.date <- wcrypto %>% select(Date) %>% pull() %>% max()

shinyUI(fluidPage(
    headerPanel("Markowitz portfolio optimization - crypto assets and equity indices"),
    
    tabsetPanel(
        # ======================================================================
        tabPanel(
            title = "Data exploration",
            sidebarPanel(
                width = 3,
                p(strong("Last observation found in the data:")),
                textOutput("lastDate"), 
                br(),
                selectInput(
                    inputId = "plotType", 
                    label = "Select type of the plot", 
                    choices = c(
                        "Prices for the single asset" = 1,
                        "Normalized prices for multiple assets" = 2,
                        "Correlations" = 3,
                        "Returns analysis" = 4
                    ), 
                    selected = 1, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL
                ),
                conditionalPanel(
                    "input.plotType == 1",
                    selectInput(
                        "asset", 
                        label = "Select asset", 
                        c(wregular %>% select(-Date) %>% colnames(),
                          wcrypto  %>% select(-Date) %>% colnames()),
                        selected = "S&P500"
                    )
                ),
                conditionalPanel(
                    "input.plotType == 2",
                    dateRangeInput("analysisRange", 
                                   label = "Analysis period", 
                                   start = "2014-06-30", 
                                   end   = as.character(last.date), 
                                   min   = "2014-06-30", 
                                   max   = as.character(last.date), 
                                   separator = " to "),
                    checkboxGroupInput(
                        inputId = "whichAssets", 
                        label = "Which assets?", 
                        choices = c(
                            wregular %>% select(-Date) %>% colnames(),
                            wcrypto  %>% select(-Date) %>% colnames()
                        ), 
                        selected = "S&P500", inline = FALSE, width = NULL),
                    hr(),
                    helpText("Parameters of the plot"),
                    radioButtons(
                        inputId = "plotScale", 
                        label = "Scale of Y asis", 
                        choices = c("Linear" = 1, "Log10" = 2), 
                        selected = 1, inline = FALSE, width = NULL)
                ),
                conditionalPanel(
                    "input.plotType == 3",
                    dateInput(inputId = "correlationDate", 
                              label   = "Last observation to include", 
                              value   = as.character(last.date), 
                              min = "2014-06-30", 
                              max = as.character(last.date),
                              format = "yyyy-mm-dd", startview = "month", 
                              weekstart = 1,
                              language = "en", width = NULL),
                    selectInput(
                        "cLB", 
                        "Lookback period", 
                        choices = c(
                            "30 days"  = 30,
                            "60 days"  = 60,
                            "90 days"  = 90,
                            "180 days" = 180
                        )
                    ),
                    checkboxGroupInput(
                        inputId = "whichAssetsForCorrelation", 
                        label = "Which assets?", 
                        choices = c(
                            wregular %>% select(-Date) %>% colnames(),
                            wcrypto  %>% select(-Date) %>% colnames()
                        ), 
                        selected = c(wregular %>% select(-Date) %>% colnames(),
                                     wcrypto  %>% select(-Date) %>% colnames()), 
                        inline = FALSE, width = NULL)
                ),
                conditionalPanel(
                    "input.plotType == 4",
                    checkboxGroupInput(
                        inputId = "returnsWhichAssets", 
                        label = "Which assets?", 
                        choices = c(
                            wregular %>% select(-Date) %>% colnames(),
                            wcrypto  %>% select(-Date) %>% colnames()
                        ), 
                        selected = c("NASDAQ", "Dash", "Ethereum", 'S&P500'), 
                        inline = FALSE, width = NULL)
                )
            ),
            mainPanel(
                conditionalPanel(
                    "input.plotType == 1",
                    helpText(h3("Prices")),
                    dygraphOutput("pricesPlot")
                ),
                conditionalPanel(
                    "input.plotType == 2",
                    helpText(h3("Normalized prices")),
                    dygraphOutput("normalizedPricesPlot")
                ),
                conditionalPanel(
                    "input.plotType == 3",
                    tabsetPanel(
                        tabPanel(
                            "heatmap",
                            helpText(h3("Correlation between daily returns")),
                            plotOutput("correlationHeatMap")
                        ),
                        tabPanel(
                            "dynamics in time",
                            helpText(h3("not ready yet..."))
                        )
                    )
                ),
                conditionalPanel(
                    "input.plotType == 4",
                    plotOutput("returnsAnalysis")
                )
            )
        ),
    
        # ======================================================================
        tabPanel(
            title = "Markowitz analysis",
            sidebarPanel(
                width = 3,
                helpText(
                    p("Parameters of the Markowitz model")
                ),
                dateInput(inputId = "markowitzDate", 
                          label   = "Last observation for the model", 
                          value   = as.character(last.date), 
                          min = "2014-06-30", 
                          max = as.character(last.date),
                          format = "yyyy-mm-dd", startview = "month", 
                          weekstart = 1,
                          language = "en", width = NULL),
                selectInput(
                    "mLB", 
                    "Lookback period", 
                    choices = c(
                        "30 days"  = 30,
                        "60 days"  = 60,
                        "90 days"  = 90,
                        "180 days" = 180
                    ), 
                    selected = 60, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                radioButtons(inputId = "withCrypto", 
                             label = "Should cryptocurrencies be included?", 
                             choices = c("Yes" = 1, "No" = 2), 
                             selected = 1, inline = FALSE, width = NULL),
                helpText(
                    p("Parameters of the plot")
                ),
                numericInput(inputId = "mNumPort", 
                             label = "Number of portfolios to draw", 
                             value = 1000, 
                             min = 100, max = 10000, step = 100, 
                             width = NULL)
            ),
            mainPanel(
                plotOutput("markowitzPlot"),
                conditionalPanel(
                    "input.withCrypto == 1",
                    helpText(h4("Tangency Portfolio (TP) structure:")),
                    htmlOutput("TP1")
                ),
                conditionalPanel(
                    "input.withCrypto == 2",
                    helpText(h4("Tangency Portfolio (TP) structure:")),
                    htmlOutput("TP0")
                )
            )
        ),
        
        
        # ======================================================================
        tabPanel(
            title = "Strategies",
            sidebarPanel(
                width = 3,
                helpText(
                    p("Parameters of the strategy")
                ),
                selectInput(
                    "sRB", 
                    "Rebalancing period", 
                    choices = c(
                        "1 week"   = "1w",
                        "2 weeks"  = "2w",
                        "1 month"  = "1m",
                        "2 months" = "2m",
                        "3 months" = "3m",
                        "6 months" = "6m"
                    ), 
                    selected = "1m", multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                selectInput(
                    "sLB", 
                    "Lookback period", 
                    choices = c(
                        "30 days"   = "30d",
                        "60 days"   = "60d",
                        "90 days"   = "90d",
                        "180 days"  = "180d"
                    ), 
                    selected = "90d", multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                dateRangeInput("datesRange", 
                               label = "Investment period", 
                               start = "2014-06-30", 
                               end   = as.character(last.date), 
                               min   = "2014-06-30", 
                               max   = as.character(last.date), 
                               separator = " to "),
                sliderInput(
                    "DFL1", 
                    "DFL for strategy with crypto:", 
                    min = 0.1, max = 5, value = 1.0, step = 0.1),
                sliderInput(
                    "DFL0", 
                    "DFL for strategy without crypto:", 
                    min = 0.1, max = 5, value = 1.0, step = 0.1),
                sliderInput(
                    "costs", 
                    "transaction costs (%) for both strategies:", 
                    min = 0.01, max = 0.5, value = 0.1, step = 0.01),
                hr(),
                helpText("Parameters of the plot"),
                radioButtons(
                    inputId = "strategyPlotScale", 
                    label = "Scale of Y asis", 
                    choices = c("Linear" = 1, "Log10" = 2), 
                    selected = 1, inline = FALSE, width = NULL)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Equity lines",
                        helpText(h2("Equity lines")),
                        dygraphOutput("strategyPlot")),
                    tabPanel(
                        "Performance measures",
                        helpText(h2("Performance measures")),
                        htmlOutput("strategyPerformance"),
                        helpText(
                            p("abbrevations:"),
                            tags$ul(
                                tags$li(strong("aRC"),     " - Annualized Rate of Return (%), compounded daily"),
                                tags$li(strong("aSD"),     " - Annualized Standard Deviation of daily returns (%)"),
                                tags$li(strong("MD"),      " - Maximum Drawdown (%)"),
                                tags$li(strong("MLD"),     " - Maximum Loss Duration (in years)"),
                                tags$li(strong("IR"),      " - Information Ratio, aRC/aSD"),
                                tags$li(strong("IRMD"),    " - IR/MD"),
                                tags$li(strong("IRaRCMD"), " - IR * aRC/MD"),
                                tags$li(strong("IR2"),     " - aRC ^ 3 * sign(aRC) / (aSD * MD * MLD)"),
                                tags$li(strong("nObs"),    " - number of observations")
                            )
                        )
                        ),
                    tabPanel(
                        "Weights dynamics - with crypto",
                        helpText(h2("Weights dynamics - strategy with crypto")),
                        plotOutput("weightsDynamics1")    
                    ),
                    tabPanel(
                        "Weights dynamics - w/o crypto",
                        helpText(h2("Weights dynamics - strategy without crypto")),
                        plotOutput("weightsDynamics0")    
                    ),
                    tabPanel(
                        "Drawdowns",
                        helpText(h2("Drawdowns")),
                        plotOutput("drawdownsPlot")
                    )
                )
            )
        ),
        
        
        # ======================================================================
        tabPanel(
            title = "About",
            sidebarPanel(
                br(), br(),
                tags$img(src = "qfrg_new.png", width = "350px",
                         style = "display: block; margin-left: auto; 
                         margin-right: auto;"), 
                br(), br(),
                tags$img(src = "wne-eng2.png", width = "250px",
                         style = "display: block; margin-left: auto; 
                         margin-right: auto;"), 
                br(), br()
            ),
            mainPanel(
                helpText(
                    h3("Application:"),
                    p("This application has been designed and implemented for 
                      WhyR 2019 conference by members of Quantitative Finance 
                      Research Group at WNE UW. The main purpose is to allow 
                      the user to analyze the impact of different assumptions 
                      on portfolio strategy performance and risk profile. 
                      The application is composed of a few modules 
                      for data exploration, portfolio analysis based 
                      on Markowitz theory and strategy backtesting. 
                      The backtesting engine lets the user test strategies 
                      with different lookback periods, levels of rebalancing
                      frequency, degree of financial leverage (DFL) on 
                      historical data from the manually specified period. 
                      One of the most important features of the application 
                      is possibility to include in the portfolio 
                      assets from a new investment class - cryptocurrencies. 
                      Core calculations and web interface have been 
                      fully implemented in R using Shiny technology."),
                    h3("Data vendors:"),
                    p("Daily OHLC prices of assets from regulated markets are 
                           provided by ", 
                           a(href = "www.stooq.com", "stooq.com"), ".",
                           "Daily OHLC prices and market capitalizations of 
                           cryptocurriencies are scraped from ",
                           a(href = "www.coinmarketcap.com", "coinmarketcap.com"),
                           "."),
                    h3("Disclaimer:"),
                    p("No offer or solicitation to buy or sell securities or 
                      cryptocurrency products of any kind, or any type of 
                      investment or trading advice or strategy, is made, 
                      given or in any manner endorsed by Quantitative Finance 
                      Research Group at WNE UW or their
                      affiliates. Testimonials regarding past performance 
                      are no guarantee of future results and may not be 
                      representative of the experience of other customers."),
                    h3("Authors:"),
                    p("Paweł Sakowski, WNE UW, QFRG, ", 
                           a(href = "mailto:pawelsakowski@gmail.com", 
                             "pawelsakowski@gmail.com"),
                           br(),
                           "Przemysław Ryś, MIMUW, QFRG, ",
                           a(href = "mailto:przrys@gmail.com",
                             "przrys@gmail.com")
                    )
                )
            )
        )
    )
    
))
    
