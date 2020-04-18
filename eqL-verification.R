# verification: comparing single Bitcoin with 100% Bitcoin portfolios 
weights_list2 <-
  weights_list %>%
  lapply(., function(x) {
    x[names(x) == "S&P500"] <- 1
    x[!(names(x) == "S&P500")] <- 0
    return(x)
  })

weights_list2[[25]][7] <- 0

eqL <-
  getEquityLine(quotes = ldata, 
                weightsList = weights_list2, 
                proportionalCost = 0.000, 
                initialEquity = 1000, 
                DFL = 1) %>%
  filter(Date >= "2014-06-29")

plot.data <-
  wdata %>% select(Date, Bitcoin, "S&P500") %>%
  left_join(eqL) %>%
  filter(Date >= "2014-06-30") %>%
  mutate(Bitcoin = Bitcoin/Bitcoin[1],
         `S&P500` = `S&P500`/`S&P500`[1],
         equityLine = equityLine / equityLine[1])

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
