setwd("Data")
IBM <- read.csv("IBM.csv")

IBM$date <- as.POSIXct(strptime(IBM$date, '%Y-%m-%d',tz='GMT'))

pre2015 <- subset(IBM, year <= 2015)
post2015 <- subset(IBM, year > 2015)

# linear
lm <- lm(change ~ open + close + adj_close + adj_open,
         data = pre2015)
pred <- predict(lm, newdata = post2015, type="response")
post2015$pred <- pred

# 36 best
# 42 diff
cur_close <- post2015$close[1]
post2015$predicted1 <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/36);
  post2015$predicted1[i+1] <- cur_close;
}

cur_close <- post2015$close[1]
post2015$predicted2 <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/42);
  post2015$predicted2[i+1] <- cur_close;
}

sqrt(mean((log(post2015$predicted+1)-log(post2015$close+1))^2,na.rm=TRUE))

# logit
logit <- glm(change ~ open + close + adj_open + adj_close,
             data = pre2015,
             family = binomial())

pred <- predict(logit, newdata = post2015, type="response")
post2015$pred <- pred

# 33 best
# 47 diff
cur_close <- post2015$close[1]
post2015$predicted3 <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/33);
  post2015$predicted3[i+1] <- cur_close;
}

cur_close <- post2015$close[1]
post2015$predicted4 <- cur_close
for (i in c(1:(nrow(post2015)-1))) {
  cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/47);
  post2015$predicted4[i+1] <- cur_close;
}

post2015$predicted <- (post2015$predicted1 +
                         post2015$predicted2 +
                         post2015$predicted3 +
                         post2015$predicted4) / 4;

sqrt(mean((log(post2015$predicted+1)-log(post2015$close+1))^2,na.rm=TRUE))


########################################################################

#####
post2015$predicted <- post2015$predicted4
#####

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
gg_plot <- gg_plot + ggtitle("IBM Stock Prediction 2016-17 Ensemble") +
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

#################################################################
# ggplot
gg_plot <- ggplot(data=IBM) + 
  geom_line(aes(date,open,color="open")) +
  geom_line(aes(date,high,color="high")) +
  geom_line(aes(date,low,color="low")) +
  geom_line(aes(date,close,color="close")) +
  scale_colour_manual(name="Line Color",
                      values=c(open="green",
                               high="darkorchid1",
                               low="darkorchid4",
                               close="red")) +
  scale_x_datetime(date_labels="%Y", date_breaks = "1 year")

# design
gg_plot <- gg_plot + ggtitle("IBM Stock 1962-2017") +
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
htmlwidgets::saveWidget(x, "IBM historical.html")
