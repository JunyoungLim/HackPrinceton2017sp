library(randomForest)
library(dplyr)
library(ggplot2)

set.seed(42)

train <- GOOGL[,-c(1,17)] # exclude row numbers and Id

rf <- randomForest(change ~ ., data=train)

set <- data_frame(feature=setdiff(colnames(train), "delta"),
                  importance=as.vector(importance(rf)))
set <- arrange(set, desc(importance))
set$feature <- factor(set$feature, levels=set$feature)

p <- ggplot(set, aes(x=feature, weight=importance, fill=feature))
p <- p + geom_bar() + ggtitle("Feature by Influence")
p <- p + xlab("GOOGL Stock Features") + ylab("Feature Importance")
p <- p + scale_fill_discrete(name="Features")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title=element_text(size=15),
          plot.title=element_text(size=20),
          legend.title=element_text(size=17),
          legend.text=element_text(size=10),
          panel.background = element_rect(fill = "grey90"))

