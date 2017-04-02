library(ggplot2)
google_plot <- ggplot(data=GOOGL,aes(date,close)) + geom_point()
google_plot

GOOGL$date <- as.POSIXct(strptime(GOOGL$date, '%Y-%m-%d',tz='GMT'))
GOOGL$date <- as.Date(GOOGL$date)
class(GOOGL$date)

reg <- lm(close ~ date + volume, data=pre2015)
summary(reg)
pred <- predict(reg, post2015)
sqrt(mean((log(pred)-log(post2015$close))^2,na.rm=TRUE))

post2015$pred <- pred

# ggplot
google_plot <- ggplot(data=post2015) + 
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
google_plot <- google_plot + ggtitle("GOOGL Stock Prediction 2016-17 k=54") +
                            ylab("stock prices") +
                            theme(plot.title = element_text(hjust = 0.5, size = 15),
                                  plot.background = element_rect(fill = "white"),
                                  panel.background = element_rect(fill = "black"),
                                  panel.grid.major = element_line(colour = "grey40"),
                                  panel.grid.minor = element_line(colour = "grey50", linetype = "dotted"),
                                  legend.position = "right",
                                  legend.title = element_blank(),
                                  legend.key = element_rect(fill = "black"))
google_plot

library(plotly)
x <- ggplotly(google_plot)
x
htmlwidgets::saveWidget(x, "GOOGL plotly logit temp k54.html")



##########################################################################

# force origin
google_plot <- google_plot +
  expand_limits(y= 0) + scale_y_continuous(expand = c(0, 0))
google_plot
