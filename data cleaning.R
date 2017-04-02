setwd("/Users/jaredlim/Desktop/Programming/HACKATHON/HackPrinceton2017sp/")
data <- read.csv("stock.csv")

# modification of the intial dataset
summary(data)
head(data)

# feature engineering of date
library(tidyr)
data <- separate(data, date, c("year", "monthday"), sep="-", extra="merge")
data <- separate(data, monthday, c("month", "day"), sep="-", extra="merge")
data$year <- as.numeric(data$year)
data$month <- as.numeric(data$month)
data$day <- as.numeric(data$day)
data$date <- paste(data$year, data$month, data$day, sep="-")

# resave the file
write.csv(data, "stock.csv", row.names = FALSE)

# save summary
capture.output(summary(data), file="summary.txt")

setwd("Data")
# google data
GOOGL <- subset(data, ticker == "GOOGL")
write.csv(GOOGL, "GOOGL.csv", row.names = FALSE)

# facebook data
FB <- subset(data, ticker == "FB")
write.csv(FB, "FB.csv", row.names = FALSE)

# capitalone data
COF <- subset(data, ticker == "COF")
write.csv(COF, "COF.csv", row.names = FALSE)

# apple data
AAPL <- subset(data, ticker == "AAPL")
write.csv(AAPL, "AAPL.csv", row.names = FALSE)

#microsoft data
MSFT <- subset(data, ticker == "MSFT")
write.csv(MSFT, "MSFT.csv", row.names = FALSE)

#amazon data
AMZN <- subset(data, ticker == "AMZN")
write.csv(AMZN, "AMZN.csv", row.names = FALSE)

#ibm data
IBM <- subset(data, ticker == "IBM")
write.csv(IBM, "IBM.csv", row.names = FALSE)

#honeywell data
HON <- subset(data, ticker == "HON")
write.csv(HON, "HON.csv", row.names = FALSE)

# change column
tickers <- c("GOOGL", "FB", "COF", "AAPL", "MSFT", "AMZN", "IBM", "HON")

for (nametag in tickers) {
  company_string <- paste(nametag, ".csv", sep="")
  company <- read.csv(company_string)
  company$change <- 0
  for (i in c(1:(nrow(company)-1))) {
    company$change[i+1] <- if (company$close[i+1] >= company$close[i]) 1 else 0;
  }
  write.csv(company, company_string, row.names = FALSE)
}

for (nametag in tickers) {
  company_string <- paste(nametag, ".csv", sep="")
  company <- read.csv(company_string)
  company$delta <- 0
  for (i in c(1:(nrow(company)-1))) {
    company$delta[i] <- company$close[i+1] - company$close[i];
  }
  write.csv(company, company_string, row.names = FALSE)
}
