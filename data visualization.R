library(ggplot2)
google_plot <- ggplot(data=GOOGL,aes(date,close)) + geom_point()
google_plot

GOOGL$date <- as.POSIXct(strptime(GOOGL$date, '%Y-%m-%d',tz='GMT'))
typeof(GOOGL$date)

reg <- lm(close ~ date + volume, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))

post2015$pred <- pred

# ggplot
google_plot <- ggplot(data=post2015) + 
               geom_line(aes(date,high),color="darkorchid1") +
               geom_line(aes(date,low),color="darkorchid4") +
               geom_line(aes(date,open),color="green") +
               geom_line(aes(date,close),color="red") +
               geom_line(aes(date,pred),color="blue")

# force origin
google_plot <- ggplot(data=post2015) + geom_point(aes(date,close),color=post2015$close) + geom_point(aes(date,pred),color=post2015$pred) +
                  expand_limits(y= 0) + scale_y_continuous(expand = c(0, 0))
google_plot

# design
google_plot <- google_plot + ggtitle("GOOGL Stock Prediction") +
                            ylab("stock prices") +
                            theme(plot.title = element_text(hjust = 0.5, size = 15),
                                  plot.background = element_rect(fill = "white"),
                                  panel.background = element_rect(fill = "black"),
                                  panel.grid.major = element_line(colour = "grey40"),
                                  panel.grid.minor = element_line(colour = "grey50", linetype = "dotted"))
google_plot

library(plotly)
x <- ggplotly(google_plot)
x
htmlwidgets::saveWidget(x, "GOOGL plotly temp.html")
