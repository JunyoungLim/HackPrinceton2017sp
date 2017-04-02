ggl <- GOOGL

ggl$change <- 0
for (i in c(1:(nrow(ggl)-1))) {
  ggl$change[i+1] <- if (ggl$close[i+1] >= ggl$close[i]) 1 else 0;
}

gglpre <- subset(ggl, year <= 2015)
gglpost <- subset(ggl, year > 2015)

logit <- glm(change ~ date + volume + open + close,
             data = gglpre,
             family = binomial())


write.csv(submission[,c(1:3)], "future_submissions.csv", row.names = FALSE)

pred <- predict(logit, newdata = gglpost, type="response")
gglpost$pred <- pred

# logistic linear
cur_close <- gglpost$close[1]
gglpost$predicted <- cur_close
for (i in c(1:(nrow(gglpost)-1))) {
  cur_close <- cur_close * (1 + (gglpost$pred[i]-0.5)/112);
  gglpost$predicted[i+1] <- cur_close;
}

sqrt(mean((log(gglpost$predicted)-log(gglpost$close))^2,na.rm=TRUE))

predicted <- ifelse(gglpost$pred > 0.5,1,0)

misClasificError <- mean(predicted != gglpost$change)
print(paste('Accuracy',1-misClasificError))

head(gglpost[c(1,2,3,4,8,18,19,20)], 10)
