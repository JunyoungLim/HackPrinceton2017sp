# ticker strapping - facebook, capitalone, apple, 
tickers <- c("FB", "COF", "AAPL", "MSFT", "AMZN", "IBM")
tickers <- c("FB")
tickers <- c("GOOGL")

for (nametag in tickers) {
  company_string <- paste(nametag, ".csv", sep="")
  company <- read.csv(company_string)
  
  pre2015 <- subset(company, year <= 2015)
  post2015 <- subset(company, year > 2015)
  
  print("pre2015")
  
  logit <- glm(change ~ date + volume + open + close,
               data = pre2015,
               family = binomial())
  
  pred <- predict(logit, newdata = post2015, type="response")
  post2015$pred <- pred
  
  # logistic linear
  cur_close <- post2015$close[1]
  post2015$predicted <- cur_close
  for (i in c(1:(nrow(post2015)-1))) {
    cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/54);
    post2015$predicted[i+1] <- cur_close;
  }
  
  sqrt(mean((log(gglpost$predicted)-log(gglpost$close))^2,na.rm=TRUE))
}

