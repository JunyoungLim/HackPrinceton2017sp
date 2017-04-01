analysis <- GOOGL
pre2015 <- subset(GOOGL, year <= 2015)
post2015 <- subset(GOOGL, year > 2015)

reg <- lm(close ~ year + month + day, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))

######################################################
reg <- lm(close ~ year, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))

reg <- lm(close ~ month, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))

reg <- lm(close ~ timeline, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))
######################################################

comparison <- cbind(post2015$close, pred)
combined <- cbind(pred, post2015$year, post2015$month, post2015$day)
colnames(combined)[2:4] <- c("year", "month", "day")
combined <- as.data.frame(combined)

combined <- cbind(post2015,combined)

library(ggplot2)
ggplot(data=post2015) + geom_point(aes(timeline,close),color="red") + geom_point(aes(timeline,pred),color="blue")


pre2015$timeline <- (pre2015$year-2004)*365 + (pre2015$month-1)*31 + pre2015$day
post2015$timeline <- (post2015$year-2016)*365 + (post2015$month-1)*31 + post2015$day
