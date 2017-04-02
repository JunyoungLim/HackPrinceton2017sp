setwd("/Users/jaredlim/Desktop/Programming/HACKATHON/HackPrinceton2017sp/Data/")
data <- read.csv("stock.csv")

ticker <- unique(data$ticker)

# for each ticker
for (i in c(1:length(ticker))) {
  name <- ticker[i]
  
  com <- subset(data, ticker == name)
  
}
