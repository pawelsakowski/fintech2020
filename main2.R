## 2020-04-18
## Paweł Sakowski
## QFRG WNE UW
## Shiny App for Kosc et. al. (2019) "Momentum and contrarian effects 
##   on the cryptocurrency market", Physica A 523 (2019) 691-701

rm(list = ls())
gc()

# LOADING PACKAGES =============================================================
library(tidyverse)
library(gapminder)
library(PortfolioAnalytics)
library(ggrepel)
library(transformr)
library(sf)
library(PerformanceAnalytics)

# START/STOP DATE ==============================================================
start_date <- as.Date("2013-12-30")
stop_date  <- as.Date("2020-05-01")


# UPDATING DATA ================================================================
# with crypto and regular assets prices
# directly from stooq.com and coinmarketcap.com
if (0) source("src/update_data.R")


# READING, TRANSFORMING, SAVING ================================================
# reading crypto and regular data from updated rds files
# transforming data, saving to final rds files
if (0) {
  
  # load full long data with crypto prices
  crypto <- 
    read_rds("data/crypto/data_crypto.rds") %>%
    mutate(Name = ifelse(substr(Name, 1, 1) == " ",
                         substr(Name, 2, nchar(Name)),
                         Name)) %>%
    filter(!(Name %in% c("Aphroditecoin", 
                         "SpainCoin", 
                         "The DAO", 
                         "Electric",
                         "HempCoin",
                         "DubaiCoin",
                         "Scotcoin",
                         "Spots",
                         "EncryptoTel [..."))) %>%
    filter(!(Name == "Bytecoin" & Symbol == "BTE")) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(obs = row_number()) %>%
    ungroup() %>%
    filter(Date >= start_date) %>%
    arrange(Date, MarketCap %>% desc(), Volume %>% desc(), Name) %>%
    select(Date, Name, Symbol, Price, MarketCap, obs) %>%
    filter(Date <= stop_date) 
  
  # allowed are only crypto with at least 61 historical price observations 
  crypto <-
    crypto %>%
    filter(obs > 60)
  
  # removing coins with short history
  namesToRemove <- 
    crypto %>% 
    group_by(Name) %>% 
    summarize(n = n()) %>%
    arrange(n) %>%
    filter(n <= 60) %>%
    select(Name) %>%
    pull()
  
  crypto <- crypto %>% filter(!(Name %in% namesToRemove))
  rm(namesToRemove)
  
  # filter out topN crypto with largest market cap for every day
  topN <- 100
  crypto_topN <-
    crypto %>%
    arrange(Date, MarketCap %>% desc()) %>%
    group_by(Date) %>%
    filter(row_number() <= topN) %>%
    ungroup() 
  
  crypto 
  crypto_topN
  
  # crypto names
  crypto_topN_names <-
    crypto_topN %>% 
    select(Name) %>% arrange() %>% pull() %>% unique()
  crypto_topN_names
  
  # filtering out cryptos out of topN range
  crypto <-
    crypto %>%
    filter(Name %in% crypto_topN_names)
    
  # wide crypto prices
  wcrypto <-
    crypto %>%
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
  
  # wide data with returns for both regular and crypto
  source("src/fun-getSimpleReturn.R")
  wrets <-
    wdata %>%
    mutate_at(., vars(-Date), getSimpleReturn)
  
  # crypto - adding 1D/1W/1M returns
  crypto <-
    crypto %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(
      ret1D = diff.xts(Price, lag = 1) /  lag.xts(Price, k = 1),
      ret1W = diff.xts(Price, lag = 7) /  lag.xts(Price, k = 7),
      ret1M = diff.xts(Price, lag = 30) / lag.xts(Price, k = 30)  
    ) %>%
    ungroup()
  
  # crypto - adding 1D returns rank
  crypto <-
    crypto %>%
    arrange(Date, desc(ret1D)) %>%
    group_by(Date) %>%
    mutate(rank1D = row_number()) %>%
    ungroup()
  
  # crypto - adding 1W returns rank
  crypto <-
    crypto %>%
    arrange(Date, desc(ret1W)) %>%
    group_by(Date) %>%
    mutate(rank1W = row_number()) %>%
    ungroup()
  
  # crypto - adding 1M returns rank
  crypto <-
    crypto %>%
    arrange(Date, desc(ret1M)) %>%
    group_by(Date) %>%
    mutate(rank1M = row_number()) %>%
    ungroup()
  
  # crypto - adding MarketCap rank
  crypto <-
    crypto %>%
    arrange(Date, desc(MarketCap)) %>%
    group_by(Date) %>%
    mutate(rankMC = row_number()) %>%
    ungroup()
  
  
  
  
  
  
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
  
}


# LOADING all tibbles with prices and returns from final rds files =============
if (1) {
  crypto            <- readRDS("data/crypto/crypto.rds")
  crypto_topN       <- readRDS("data/crypto/crypto_topN.rds")
  crypto_topN_names <- readRDS("data/crypto/crypto_topN_names.rds")
  wcrypto           <- readRDS("data/crypto/wcrypto.rds")
  regular           <- readRDS("data/regular/regular.rds")
  wregular          <- readRDS("data/regular/wregular.rds")
  wdata             <- readRDS("data/wdata.rds")
  ldata             <- readRDS("data/ldata.rds")
  wrets             <- readRDS("data/wrets.rds")
}



# REBALANCING DATES ============================================================
RB <- 7
rebalancing_dates <- seq(from = as.Date("2014-05-13"), 
                         to = stop_date, 
                         by = RB) - 1
rebalancing_dates %>% length()


# CREATING WEIGHTS =============================================================
source("src/fun-getMcVec.R")
source("src/fun-createWeights.R")

weights_RE_1W_RA_1W_mom <-
  createWeights(rebDates     = rebalancing_dates,
                cdata        = crypto,
                rankType     = "1W",
                strategyType = "mom",
                includePct   = 0.25)
weights_RE_1W_RA_1W_con <-
  createWeights(rebDates     = rebalancing_dates,
                cdata        = crypto,
                rankType     = "1W",
                strategyType = "con",
                includePct   = 0.25)
weights_RE_1W_McW <-
  createWeights(rebDates     = rebalancing_dates,
                cdata        = crypto,
                rankType     = "MC",
                includePct   = 1 )
weights_RE_1W_EqW <-
  createWeights(rebDates     = rebalancing_dates,
                cdata        = crypto,
                rankType     = "1W",
                strategyType = "mom",
                includePct   = 1)


# EQUITY LINES - calculating and visualizing ===================================
source("src/fun-getEquityLine.R")

eqL_RE_1W_RA_1W_mom <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_RE_1W_RA_1W_mom, 
                proportionalCost = 0.01, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-05-01")
eqL_RE_1W_RA_1W_con <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_RE_1W_RA_1W_con, 
                proportionalCost = 0.01, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-05-01")
eqL_RE_1W_McW <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_RE_1W_McW, 
                proportionalCost = 0.01, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-05-01")
eqL_RE_1W_EqW <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_RE_1W_EqW, 
                proportionalCost = 0.01, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-05-01")


plot.data <-
  wdata %>% select(Date, "S&P500") %>%
  left_join(eqL_RE_1W_RA_1W_mom) %>% rename(eqL_RE_1W_RA_1W_mom = equityLine) %>%
  left_join(eqL_RE_1W_RA_1W_con) %>% rename(eqL_RE_1W_RA_1W_con = equityLine) %>%
  left_join(eqL_RE_1W_McW) %>% rename(eqL_RE_1W_McW = equityLine) %>%
  left_join(eqL_RE_1W_EqW) %>% rename(eqL_RE_1W_EqW = equityLine) %>%
  filter(Date >= "2014-05-01") %>%
  mutate(eqL_RE_1W_RA_1W_mom = eqL_RE_1W_RA_1W_mom / eqL_RE_1W_RA_1W_mom[1],
         eqL_RE_1W_RA_1W_con = eqL_RE_1W_RA_1W_con / eqL_RE_1W_RA_1W_con[1],
         eqL_RE_1W_McW = eqL_RE_1W_McW / eqL_RE_1W_McW[1],
         eqL_RE_1W_EqW = eqL_RE_1W_EqW / eqL_RE_1W_EqW[1],
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
  dygraphs::dyRangeSelector(., height = 40) %>%
  dygraphs::dyAxis(., name = "y", logscale = "Log10")






