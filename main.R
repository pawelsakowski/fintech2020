## 2019-07-018
## Przemysław Ryś, Paweł Sakowski
## QFRG WNE UW
## LABYRINTH HF 
## Shiny App for "Why R? 2019" Conference

rm(list = ls())
gc()

# ==============================================================================
# LOADING PACKAGES
library(tidyverse)
library(gapminder)
library(gganimate)
library(PortfolioAnalytics)
library(ggrepel)
library(transformr)
library(sf)
library(PerformanceAnalytics)

# ==============================================================================
# UPDATING DATA with crypto and regular assets prices
# directly from stooq.com and coinmarketcap.com
if (0) source("src/update_data.R")


# ==============================================================================
# READING crypto and regular data from updated rds files
# TRANSFORMING data
# SAVING to final rds files
start_date <- as.Date("2013-12-30")
stop_date  <- as.Date("2020-05-01")

# load full long data with crypto prices
crypto <- 
  read_rds("data/crypto/data_crypto.rds") %>%
  filter(!(Name %in% c("Aphroditecoin", "SpainCoin", "The DAO", "Electric"))) %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(obs = row_number()) %>%
  ungroup() %>%
  filter(Date >= start_date) %>%
  arrange(Date, MarketCap %>% desc(), Volume %>% desc(), Name) %>%
  select(Date, Name, Symbol, Price, MarketCap, obs) %>%
  filter(Date <= stop_date) 

# filter out topN crypto with largest market cap for every day
topN <- 100
crypto_topN <-
  crypto %>%
  arrange(Date, MarketCap %>% desc()) %>%
  group_by(Date) %>%
  # allowed are only crypto with at least 183 historical price observations 
  filter(obs >= 183) %>% 
  filter(row_number() <= topN) %>%
  ungroup() 

crypto 
crypto_topN

# crypto names
crypto_topN_names <-
  crypto_topN %>% 
  select(Name) %>% arrange() %>% pull() %>% unique()
crypto_topN_names

# wide crypto prices
wcrypto <-
  crypto %>%
  filter(Name %in% crypto_topN_names) %>%
  select(-Symbol, -MarketCap, -obs) %>%
  spread(key = Name, value = Price)
wcrypto

# VERIFICATION
wcrypto %>%
  select(Date) %>% pull %>% xts::diff.xts(.) %>% table(useNA = "always")
wcrypto %>% nrow()
# VERIFICATION: OK!

# reading regular assets prices
regular <- 
  read_rds("data/regular/spx.rds") %>%
  mutate(Name = "S&P500", Symbol = "SPX", Price = Close) %>%
  select(Date, Name, Symbol, Price) %>%
  bind_rows(read_rds("data/regular/ndq.rds") %>%
              mutate(Name = "NASDAQ", Symbol = "NDQ", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/dax.rds") %>% 
              mutate(Name = "DAX", Symbol = "DAX", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/nkx.rds") %>%
              mutate(Name = "NIKKEI", Symbol = "NKX", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/kospi.rds") %>%
              mutate(Name = "KOSPI", Symbol = "KSP", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/cac.rds") %>%
              mutate(Name = "CAC40", Symbol = "CAC", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/x.f.rds") %>%
              mutate(Name = "FTSE100", Symbol = "FTS", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdeur.rds") %>%
              mutate(Name = "USDEUR", Symbol = "EUR", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdgbp.rds") %>%
              mutate(Name = "USDGBP", Symbol = "GBP", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdjpy.rds") %>%
              mutate(Name = "USDJPY", Symbol = "JPY", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdkrw.rds") %>%
              mutate(Name = "USDKRW", Symbol = "KRW", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  arrange(Date) %>%
  filter(Date >= start_date,
         Date <= stop_date)

# wide regular prices
wregular <-
  regular %>% select(-Symbol) %>%
  spread(key = Name, value = Price) %>%
  mutate(CAC40   = CAC40 / USDEUR,
         DAX     = DAX / USDEUR,
         FTSE100 = FTSE100 / USDGBP,
         KOSPI   = KOSPI / USDKRW,
         NIKKEI  = NIKKEI / USDJPY) %>%
  select(-USDEUR, -USDGBP, -USDJPY, -USDKRW)

# join WIDE crypto and regular data  
wdata <-
  full_join(wcrypto, wregular) %>%
  arrange(Date) %>%
  zoo::na.locf(na.rm = F) # don't remove leading NAs
wdata

# VERIFICATION
wdata %>%
  select(Date) %>% pull %>% xts::diff.xts(.) %>% table(useNA = "always")
wdata %>% nrow()
# VERIFICATION: OK!

# LONG data with prices for crypto and regular assets
ldata <-
  wdata %>%
  gather(key = Name, value = Price, -Date) %>%
  arrange(Date, Name)

# function to get simple returs
getSimpleReturn <- function(x) {
  xts::diff.xts(x)/xts::lag.xts(x)
}

# wide data with returns for both regular and crypto
wrets <-
  wdata %>%
  mutate_at(., vars(-Date), getSimpleReturn)

# saving all tibbles with prices and returns into final rds files
crypto            %>% saveRDS("data/crypto/crypto.rds")
crypto_topN       %>% saveRDS("data/crypto/crypto_topN.rds")
crypto_topN_names %>% saveRDS("data/crypto/crypto_topN_names.rds")
wcrypto           %>% saveRDS("data/crypto/wcrypto.rds")
regular           %>% saveRDS("data/regular/regular.rds")
wregular          %>% saveRDS("data/regular/wregular.rds")
wdata             %>% saveRDS("data/wdata.rds")
ldata             %>% saveRDS("data/ldata.rds")
wrets             %>% saveRDS("data/wrets.rds")


# ==============================================================================
# LOADING all tibbles with prices and returns from final rds files
crypto            <- readRDS("data/crypto/crypto.rds")
crypto_topN       <- readRDS("data/crypto/crypto_topN.rds")
crypto_topN_names <- readRDS("data/crypto/crypto_topN_names.rds")
wcrypto           <- readRDS("data/crypto/wcrypto.rds")
regular           <- readRDS("data/regular/regular.rds")
wregular          <- readRDS("data/regular/wregular.rds")
wdata             <- readRDS("data/wdata.rds")
ldata             <- readRDS("data/ldata.rds")
wrets             <- readRDS("data/wrets.rds")



# ==============================================================================
# SINGLE OPTIMIZATIONs

## defining function for portfolio optimization at given rebalancing dates
source("src/fun-optimizePortfolio.R")
source("src/fun-optimizePortfolioForRBLB.R")

# single optimizing and saving results
optimizePortfolioForRBLB(RB = "1 month", 
                         LB = 60, 
                         topN_data  = crypto_topN, 
                         wide_data  = wrets,
                         n.port = 50, 
                         includeCrypto = T)

## loading list with optimized poftfolios from rds file
opt_portfolio_list_C1 <- 
  readRDS("out/optPortfolios/opt_portfolio_list_RB_1m_LB_60d_C1.rds")

# single optimizing and saving results
optimizePortfolioForRBLB(RB = "1 month", 
                         LB = 60, 
                         topN_data  = crypto_topN, 
                         wide_data  = wrets,
                         n.port = 50, 
                         includeCrypto = F)

## loading list with optimized poftfolios from rds file
opt_portfolio_list_C0 <- 
  readRDS("out/optPortfolios/opt_portfolio_list_RB_1m_LB_60d_C0.rds")


# ==============================================================================
# BATCH OPTIMIZATIONs of all strategies inside nested loops

RBs <- c("1 week", "2 weeks", "1 month", "2 months", "3 months", "6 months")
LBs <- c(30, 60, 90, 180)

time0 <- Sys.time() i
for (i in RBs) {
  for (j in LBs) {
    cat("RB = ", i, ", LB = ", j, " days\n", sep = "")
    optimizePortfolioForRBLB(RB = i, 
                             LB = j, 
                             topN_data  = crypto_topN, 
                             wide_data  = wrets,
                             n.port = 50, 
                             includeCrypto = T)
    optimizePortfolioForRBLB(RB = i, 
                             LB = j, 
                             topN_data  = crypto_topN, 
                             wide_data  = wrets,
                             n.port = 50, 
                             includeCrypto = F)
  }
}
time1 <- Sys.time()
time1 - time0

# ==============================================================================
# EQUITY LINES - calculating and visualizing 
source("src/fun-getEquityLine.R")
source("src/fun-extractWeightsFromEF.R")

#weights_list <-
#  lapply(1:length(opt_portfolio_list), 
#         function(x) opt_portfolio_list[[x]][[1]] %>% extractWeights)

weights_list_C1 <-
  lapply(1:length(opt_portfolio_list_C1), 
         function(x) opt_portfolio_list_C1[[x]][[2]] %>% 
           extractWeightsFromEF(pType = "maxSR") )
names(weights_list_C1) <- names(opt_portfolio_list_C1)

eqL_C1_DFL_100 <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_list_C1, 
                proportionalCost = 0.001, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-06-29")

eqL_C1_DFL_050 <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_list_C1, 
                proportionalCost = 0.001, 
                initialEquity = 1000, 
                DFL = 0.5) %>%
  filter(Date >= "2014-06-29")


weights_list_C0 <-
  lapply(1:length(opt_portfolio_list_C0), 
         function(x) opt_portfolio_list_C0[[x]][[2]] %>% 
           extractWeightsFromEF(pType = "maxSR") )
names(weights_list_C0) <- names(opt_portfolio_list_C0)

eqL_C0 <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_list_C0, 
                proportionalCost = 0.001, 
                initialEquity = 1000, 
                DFL = 1) %>%
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
                proportionalCost = 0.001, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-06-29")


plot.data <-
  wdata %>% select(Date, "S&P500") %>%
  left_join(eqL_C1_DFL_100) %>% rename(eqL_C1_DFL_100 = equityLine) %>%
  left_join(eqL_C1_DFL_050) %>% rename(eqL_C1_DFL_050 = equityLine) %>%
  left_join(eqL_C0) %>% rename(eqL_C0 = equityLine) %>%
  left_join(eqL_EW) %>% rename(eqL_EW = equityLine) %>%
  filter(Date >= "2014-06-30") %>%
  mutate(eqL_C1_DFL_100 = eqL_C1_DFL_100 / eqL_C1_DFL_100[1],
         eqL_C1_DFL_050 = eqL_C1_DFL_050 / eqL_C1_DFL_050[1],
         eqL_C0 = eqL_C0 / eqL_C0[1],
         eqL_EW = eqL_EW / eqL_EW[1],
         `S&P500` = `S&P500`/`S&P500`[1]
         ) 

plot.data %>%
  gather(key = asset, value = Price, -Date) %>%
  ggplot(aes(x = Date, y = Price, col = asset)) + 
  geom_line() + 
  theme_bw()

plot.data %>% 
  select(-Date) %>%
  xts::xts(., order.by = plot.data$Date) %>%
  dygraphs::dygraph(.) %>%
  dygraphs::dyRangeSelector(., height = 40)



# ==============================================================================
# EFFICIENT FRONTIER - plots for a given date

source("src/fun-drawEfficientFrontier.R")
source("src/fun-drawRandomPortfolio.R")

day <- 59
drawEfficientFrontier(opt_portfolio_list_C1[[day]],
                      opt_portfolio_list_C0[[day]],
                      extractWeightsFromEF = T,
                      numPortfolios = 1000,
                      pType = "maxSR", 
                      scale = 1,
                      yMin = -0.001, yMax = 0.006,
                      xMin = 0, xMax = 0.035)
drawEfficientFrontier(pf0 = opt_portfolio_list_C0[[day]],
                      extractWeightsFromEF = T,
                      numPortfolios = 1000,
                      pType = "maxSR", 
                      scale = 1, 
                      yMin = -0.001, yMax = 0.006,
                      xMin = 0, xMax = 0.035)



pf0 <- opt_portfolio_list_C0[[62]]
pf1 <- opt_portfolio_list_C1[[62]]


# ==============================================================================
# ANNIMATION of EFFICIENT FRONTIER through all avaiable points in time

ef <-
  sapply(1:63,
         function(x) opt_portfolio_list[[x]][[2]]$frontier[, c("StdDev", "mean")])
names(ef) <- names(opt_portfolio_list)

tb <- plyr::ldply(ef, data.frame) %>% 
  as_tibble() %>%
  mutate(Date = .id %>% as.Date()) %>%
  select(Date, StdDev, mean)

tb

a <-
  tb %>%
  ggplot(aes(x = StdDev,
             y = mean,
             group = Date)) + 
  geom_point() + geom_line() +
  ylim(c(-0.005, 0.065)) +
  xlim(c(-0.000, 0.225)) +
  theme_bw() + 
  transition_time(Date) +
  labs(title = "Day: {frame_time}") +
  enter_fade() +
  exit_fade()

a2 <-
  animate(a,
          duration = 30,
          fps  =  24
          # nframes = 200
  )

a2 %>% saveRDS("out/img/a2.rds")

a2 %>% show()

