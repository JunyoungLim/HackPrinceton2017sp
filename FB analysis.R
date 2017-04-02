setwd("Data")
FB <- read.csv("FB.csv")

FB$date <- as.POSIXct(strptime(FB$date, '%Y-%m-%d',tz='GMT'))

pre2015 <- subset(FB, year <= 2015)
post2015 <- subset(FB, year > 2015)

# linear
lm <- lm(delta ~ date + volume + open + close + high + low,
         data = pre2015)
pred <- predict(lm, newdata = post2015, type="response")
post2015$pred <- pred

cur_close <- post2015$close[1]
post2015$predicted <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close + post2015$pred[i];
  post2015$predicted[i+1] <- cur_close;
}

# logit
logit <- glm(change ~ date + open + close,
             data = pre2015,
             family = binomial())

pred <- predict(logit, newdata = post2015, type="response")
post2015$pred <- pred


# 63 for best 0.04566491
# 55 for diff 0.9462311
cur_close <- post2015$close[1]
post2015$predicted <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/63);
  post2015$predicted[i+1] <- cur_close;
}

sqrt(mean((log(post2015$predicted)-log(post2015$close))^2,na.rm=TRUE))


########################################################################

# ggplot
gg_plot <- ggplot(data=post2015) + 
  geom_line(aes(date,open,color="open")) +
  geom_line(aes(date,high,color="high")) +
  geom_line(aes(date,low,color="low")) +
  geom_line(aes(date,close,color="close")) +
  geom_line(aes(date,predicted,color="prediction")) +
  scale_colour_manual(name="Line Color",
                      values=c(open="green",
                               high="darkorchid1",
                               low="darkorchid4",
                               close="red",
                               prediction="cyan3")) +
  scale_x_datetime(date_labels="%m-%d-%y", date_breaks = "3 months")

# design
gg_plot <- gg_plot + ggtitle("FB Stock Prediction 2016-17 k=54") +
  ylab("stock prices") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_line(colour = "grey50", linetype = "dotted"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "black"))
gg_plot

library(plotly)
x <- ggplotly(gg_plot)
x
htmlwidgets::saveWidget(x, "FB plotly logit temp k54.html")
