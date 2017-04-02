library(corrplot)
M <- cor(AMZN[c(-1,-17)])
corrplot(M, method="circle")
