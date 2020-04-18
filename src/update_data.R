### updating data on crypto

source("src/fun-getCryptoPricesForGivenDate.R")
source("src/fun-updateCryptoPrices.R")

updateCryptoPrices(start.date = "2020-02-20",
                   stop.date  = "2020-04-17",
                   updateLongTibble = F)


### updating data on equity indices
source("src/fun-getDataFromStooq.R")
getDataFromStooq("^spx")   # S&P500
getDataFromStooq("^ndq")   # NASDAQ
getDataFromStooq("^dax")   # DAX
getDataFromStooq("^nkx")   # NIKKEI
getDataFromStooq("x.f")    # FTSE 100  
getDataFromStooq("^kospi") # KOSPI
getDataFromStooq("^cac")   # CAC40

getDataFromStooq("usdeur")   # USDEUR
getDataFromStooq("usdjpy")   # USDJPY
getDataFromStooq("usdgbp")   # USDGBP
getDataFromStooq("usdkrw")   # USDKRW

getDataFromStooq("ukousd3m")   # LIBOR USD 3M

