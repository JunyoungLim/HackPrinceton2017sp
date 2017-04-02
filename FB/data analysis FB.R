# ticker strapping - facebook, capitalone, apple, 
tickers <- c("FB", "COF", "AAPL", "MSFT", "AMZN", "IBM")
tickers <- c("FB")

for (nametag in tickers) {
  company <- subset(data, ticker == nametag)
  
  pre2015 <- subset(company, year <= 2015)
  post2015 <- subset(company, year > 2015)
}

