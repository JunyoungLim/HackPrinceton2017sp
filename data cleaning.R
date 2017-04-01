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

# resave the file
write.csv(data, "stock.csv", row.names = FALSE)

# save summary
capture.output(summary(data), file="summary.txt")

# google data
GOOGL <- subset(data, ticker == "GOOGL")
