library(shiny)
library(dygraphs)
library(tidyverse)
library(ggrepel)
library(kableExtra)

wregular            <- readRDS("../../data/regular/wregular.rds")
crypto_topN         <- readRDS("../../data/crypto/crypto_topN.rds")
wrets               <- readRDS("../../data/wrets.rds")
wdata               <- readRDS("../../data/wdata.rds")
ldata               <- readRDS("../../data/ldata.rds")
crypto_topN_names   <- readRDS("../../data/crypto/crypto_topN_names.rds")
wcrypto             <- readRDS("../../data/crypto/wcrypto.rds")

# crypto            <- readRDS("../../data/crypto/crypto.rds")
# regular           <- readRDS("../../data/regular/regular.rds")

source("../../src/fun-optimizePortfolio.R")
source("../../src/fun-drawEfficientFrontier.R")
source("../../src/fun-drawRandomPortfolio.R")
source("../../src/fun-extractWeightsFromEF.R")
source("../../src/fun-getEquityLine2.R")
source("../../src/fun-getPerformanceStats.R")
source("../../src/fun-findFirstNonNAValue.R")


shinyServer(function(input, output) {
    
    output$lastDate <- renderText({
        wdata %>% select(Date) %>% 
            tail(1) %>% pull() %>%
            format(., "%Y-%m-%d") 
    })
    
    output$pricesPlot <- renderDygraph({
        wdata %>%
            filter(Date >= "2014-06-30") %>%
            select(input$asset) %>%
            zoo::na.locf(na.rm = F) %>%
            xts::xts(., order.by = wdata %>%
                         filter(Date >= "2014-06-30") %>%
                         select(Date) %>% 
                         pull()) %>%
            dygraph(., 
                    main = paste0(input$asset, 
                                  ", daily closing prices")) %>%
            dyRangeSelector(., height = 40)
    })
    
    output$normalizedPricesPlot <- renderDygraph({
        wdata %>%
            filter(Date >= input$analysisRange[[1]]) %>%
            filter(Date <= input$analysisRange[[2]]) %>%
            select(-Date) %>%
            select(input$whichAssets) %>%
            zoo::na.locf(na.rm = F) %>%
            mutate_all(., function(x) x / findFirstNonNAValue(x)) %>%
            xts::xts(., order.by = wdata %>%
                         filter(Date >= input$analysisRange[[1]]) %>%
                         filter(Date <= input$analysisRange[[2]]) %>%
                         select(Date) %>% 
                         pull()) %>%
            dygraph(.,
                    main = paste0(input$whichAssets %>% 
                                      paste(., collapse = ", "))) %>%
            dyLegend(width = 650) %>%
            dyAxis("y", logscale = (input$plotScale == "2")) %>%
            dyRangeSelector(., height = 40)
    })
    
    output$markowitzPlot <- renderPlot({
        pf1 <- optimizePortfolio(this.date = input$markowitzDate,
                                 topN_data = crypto_topN,
                                 wide_data = wrets,
                                 lookback_days = input$mLB %>% as.numeric,
                                 n.port = 50, 
                                 includeCrypto = T)
        pf0 <- optimizePortfolio(this.date = input$markowitzDate,
                                 topN_data = crypto_topN,
                                 wide_data = wrets,
                                 lookback_days = input$mLB %>% as.numeric,
                                 n.port = 50, 
                                 includeCrypto = F)
        
        if (input$withCrypto == "1") {
            drawEfficientFrontier(pf1 = pf1,
                                  pf0 = pf0,
                                  extractWeightsFromEF = T,
                                  numPortfolios = input$mNumPort,
                                  pType = "maxSR", 
                                  scale = 1)
        } else {
            drawEfficientFrontier(pf1 = NULL,
                                  pf0 = pf0,
                                  extractWeightsFromEF = T,
                                  numPortfolios = input$mNumPort,
                                  pType = "maxSR", 
                                  scale = 1)
        }
        
        
    })
    
    plotData <- reactive({
        # load appropriate previous optimization results
        opt_portfolio_list_C1 <- 
            paste0("../../out/optPortfolios/opt_portfolio_list",
                   "_RB_", input$sRB,
                   "_LB_", input$sLB,
                   "_C1.rds") %>% readRDS()
        
        opt_portfolio_list_C0 <-
            paste0("../../out/optPortfolios/opt_portfolio_list",
                   "_RB_", input$sRB,
                   "_LB_", input$sLB,
                   "_C0.rds") %>% readRDS()
        
        weights_list_C1 <-
            lapply(1:length(opt_portfolio_list_C1), 
                   function(x) opt_portfolio_list_C1[[x]][[2]] %>% 
                       extractWeightsFromEF(pType = "maxSR") )
        names(weights_list_C1) <- names(opt_portfolio_list_C1)
        
        eqL_C1_DFL_100 <-
            getEquityLine(quotes = ldata, 
                          weightsList = weights_list_C1, 
                          proportionalCost = as.numeric(input$costs)/100, 
                          initialEquity = 1000, 
                          DFL = input$DFL1 %>% as.numeric()) %>%
            filter(Date >= "2014-06-29")
        
        weights_list_C0 <-
            lapply(1:length(opt_portfolio_list_C0), 
                   function(x) opt_portfolio_list_C0[[x]][[2]] %>% 
                       extractWeightsFromEF(pType = "maxSR") )
        names(weights_list_C0) <- names(opt_portfolio_list_C0)
        
        eqL_C0 <-
            getEquityLine(quotes = ldata, 
                          weightsList = weights_list_C0, 
                          proportionalCost = as.numeric(input$costs)/100, 
                          initialEquity = 1000, 
                          DFL = input$DFL0 %>% as.numeric()) %>%
            filter(Date >= "2014-06-29")
        
        weights_list_EW <-
            weights_list_C0 %>%
            lapply(., function(x) {
                x[names(x)] <- 1/length(x)
                return(x)
            })
        
        eqL_EW <-
            getEquityLine(quotes = ldata, 
                          weightsList = weights_list_EW, 
                          proportionalCost = as.numeric(input$costs)/100, 
                          initialEquity = 1000, 
                          DFL = 1) %>%
            filter(Date >= "2014-06-29")
        
        
        plot.data <-
            wdata %>% select(Date, "S&P500") %>%
            left_join(eqL_C1_DFL_100) %>% rename(eqL_C1_DFL_100 = equityLine) %>%
            left_join(eqL_C0) %>% rename(eqL_C0 = equityLine) %>%
            left_join(eqL_EW) %>% rename(eqL_EW = equityLine) %>%
            filter(Date >= input$datesRange[[1]]) %>%
            filter(Date <= input$datesRange[[2]]) %>%
            mutate(eqL_C1_DFL_100 = eqL_C1_DFL_100 / eqL_C1_DFL_100[1],
                   eqL_C0 = eqL_C0 / eqL_C0[1],
                   eqL_EW = eqL_EW / eqL_EW[1],
                   `S&P500` = `S&P500`/`S&P500`[1]
            ) %>%
            rename(`Strategy with crypto`       = eqL_C1_DFL_100,
                   `Strategy without crypto`    = eqL_C0,
                   `Equally Weighted portfolio` = eqL_EW,
                   `S&P500 B&H`                 = `S&P500`)
        
        return(plot.data)
    })
    
    output$strategyPlot <- renderDygraph({
        
        # ggplot - not used so far 
        # plot.data %>%
        #     gather(key = asset, value = Price, -Date) %>%
        #     ggplot(aes(x = Date, y = Price, col = asset)) + 
        #     geom_line() + 
        #     theme_bw()
        
        # dygraph
        plotData() %>% 
            select(-Date) %>%
            xts::xts(., order.by = plotData()$Date) %>%
            dygraph(.) %>%
            dyAxis("y", logscale = (input$strategyPlotScale == "2")) %>%
            dyLegend(width = 750) %>%
            dyRangeSelector(., height = 40)
       
    })
    
    output$strategyPerformance <- renderText({
        
        results <- 
            plotData() %>% 
            select(-Date) %>% 
            lapply(., function(x) getPerformanceStats(x)) %>%
            lapply(., as.data.frame.list) %>%
            bind_rows() %>% 
            as_tibble() %>%
            mutate(IR = ifelse(IR > 0,
                               cell_spec(IR %>% round(1), bold = T),
                               cell_spec(IR %>% round(1), bold = T, color = "red")))
        
        rownames(results) <- names(plotData() %>% select(-Date))
        
        results %>%
            kableExtra::kable(
                caption = "based on daily intervals",
                row.names = T, digits = 2, escape = F
            ) %>%
            kableExtra::kable_styling(
                .,
                bootstrap_options = c("striped", 
                                      "hover", 
                                      #"condensed", 
                                      "responsive" ),
                full_width = T, font_size = 14
            )
        
    })
    
    output$correlationHeatMap <- renderPlot({
        wrets %>%
            filter(Date <= input$correlationDate ) %>%
            filter(Date >= (input$correlationDate - 
                                (input$cLB %>% as.numeric()))) %>%
            select(-Date) %>%
            select(input$whichAssetsForCorrelation) %>%
            cor(. , use = "pairwise.complete.obs") %>%
            round(2) %>%
            reshape2::melt() %>%
            ggplot(aes(x = Var1, y = Var2, fill = value)) + 
            geom_tile() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1)) +
            geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.title = element_blank()) +
            labs(title = "")
    })
    
    output$returnsAnalysis <- renderPlot({
        wrets %>%
            select(input$returnsWhichAssets) %>%
            psych::pairs.panels(method = "pearson",
                                hist.col = "#3746f8",
                                breaks = 70,
                                rug = FALSE,
                                stars = TRUE,
                                lm = TRUE,
                                density = FALSE)
    })
    
    output$weightsDynamics1 <- renderPlot({
        
        opt_portfolio_list_C1 <- 
            paste0("../../out/optPortfolios/opt_portfolio_list",
                   "_RB_", input$sRB,
                   "_LB_", input$sLB,
                   "_C1.rds") %>% readRDS()
        
        weights_list_C1 <-
            lapply(1:length(opt_portfolio_list_C1), 
                   function(x) opt_portfolio_list_C1[[x]][[2]] %>% 
                       extractWeightsFromEF(pType = "maxSR") )
        names(weights_list_C1) <- names(opt_portfolio_list_C1)
        
        weights <- weights_list_C1[[1]]
        for (i in 2:length(weights_list_C1)) {
            weights <- weights %>%
                bind_rows(weights_list_C1[[i]])
        }
        
        weights <- weights %>%
            mutate(date = names(weights_list_C1) %>% as.Date())
        
        weights[is.na(weights)] <- 0
        weights[weights < 0] <- 0
        
        for (i in 1:nrow(weights)) {
            if (sum(weights[i, -ncol(weights)]) != 0) {
                weights[i, -ncol(weights)] <- 
                    weights[i, -ncol(weights)] / sum(weights[i, -ncol(weights)])
            }
        }
        
        seq(weights %>% select(date) %>% pull() %>% min() ,
            weights %>% select(date) %>% pull() %>% max(),
            by = "days") %>% 
            as_tibble() %>%
            rename(date = value) %>%
            left_join(weights[, colnames(weights) %in%
                                  c("date", wregular %>% select(-Date) %>% colnames())]) %>%
            left_join(weights[, colnames(weights) %in%
                                  c("date", wcrypto %>% select(-Date) %>% colnames())]) %>%
            zoo::na.locf() %>%
            gather(key = "asset", value = "weight", -date) %>%
            mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
            ggplot(aes(x = date, y = weight, fill = asset)) + 
            geom_area(alpha = 0.6 , size = .5, colour = "white") +
            # viridis::scale_fill_viridis(discrete = T) +
            # hrbrthemes::theme_ipsum() + 
            theme_bw() +
            ggtitle("Portfolio composition")
    })
    
    output$weightsDynamics0 <- renderPlot({
        
        opt_portfolio_list_C0 <- 
            paste0("../../out/optPortfolios/opt_portfolio_list",
                   "_RB_", input$sRB,
                   "_LB_", input$sLB,
                   "_C0.rds") %>% readRDS()
        
        weights_list_C0 <-
            lapply(1:length(opt_portfolio_list_C0), 
                   function(x) opt_portfolio_list_C0[[x]][[2]] %>% 
                       extractWeightsFromEF(pType = "maxSR") )
        names(weights_list_C0) <- names(opt_portfolio_list_C0)
        
        weights <- weights_list_C0[[1]]
        for (i in 2:length(weights_list_C0)) {
            weights <- weights %>%
                bind_rows(weights_list_C0[[i]])
        }
        
        weights <- weights %>%
            mutate(date = names(weights_list_C0) %>% as.Date())
        
        weights[is.na(weights)] <- 0
        weights[weights < 0] <- 0
        
        for (i in 1:nrow(weights)) {
            if (sum(weights[i, -ncol(weights)]) != 0) {
                weights[i, -ncol(weights)] <- 
                    weights[i, -ncol(weights)] / sum(weights[i, -ncol(weights)])
            }
        }
        
        seq(weights %>% select(date) %>% pull() %>% min() ,
            weights %>% select(date) %>% pull() %>% max(),
            by = "days") %>% 
            as_tibble() %>%
            rename(date = value) %>%
            left_join(weights[, colnames(weights) %in%
                                  c("date", wregular %>% select(-Date) %>% colnames())]) %>%
            zoo::na.locf() %>%
            gather(key = "asset", value = "weight", -date) %>%
            mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
            ggplot(aes(x = date, y = weight, fill = asset)) + 
            geom_area(alpha = 0.6 , size = .5, colour = "white") +
            # viridis::scale_fill_viridis(discrete = T) +
            # hrbrthemes::theme_ipsum() + 
            theme_bw() +
            ggtitle("Portfolio composition")
    })
    
    output$drawdownsPlot <- renderPlot({
        tmp <-
            plotData() %>%
            select(-Date) %>%
            mutate_all(function(x) xts::diff.xts(x) / xts::lag.xts(x)) %>%
            xts::xts(., 
                     order.by =  plotData()$Date)
        
        chart.Drawdown(tmp, legend.loc = "bottomleft") 
        
    })
    
    output$TP1 <- renderText({
        pf1 <- optimizePortfolio(this.date = input$markowitzDate,
                                 topN_data = crypto_topN,
                                 wide_data = wrets,
                                 lookback_days = input$mLB %>% as.numeric,
                                 n.port = 50, 
                                 includeCrypto = T)
        w <- pf1[[2]] %>% 
            extractWeightsFromEF() %>% round(., 2)
        w <- w[w > 0] * 100
        w %>%
            kableExtra::kable(
                row.names = T, col.names = "(%)", digits = 2, escape = F
            ) %>%
            kableExtra::kable_styling(
                .,
                bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed", 
                                      "responsive" ),
                full_width = F, font_size = 14
            )
    })
    
    output$TP0 <- renderText({
        pf0 <- optimizePortfolio(this.date = input$markowitzDate,
                                 topN_data = crypto_topN,
                                 wide_data = wrets,
                                 lookback_days = input$mLB %>% as.numeric,
                                 n.port = 50, 
                                 includeCrypto = F)
        w <- pf0[[2]] %>% 
            extractWeightsFromEF() %>% round(., 2)
        w <- w[w > 0] * 100
        w %>%
            kableExtra::kable(
                row.names = T, col.names = "(%)", digits = 2, escape = F
            ) %>%
            kableExtra::kable_styling(
                .,
                bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed", 
                                      "responsive" ),
                full_width = F, font_size = 14
            )
    })
})
