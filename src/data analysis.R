# recent data

pre2011 <- data[which(data$year <= 2011),]
post2011 <- data[which(data$year > 2011),]


# linear regression

reg <- lm(close ~ ., data=pre2011)
summary(reg)
pred <- predict(reg, post2011)
sqrt(mean((log(pred)-log(post2011$close))^2,na.rm=TRUE))

# remove first index
setosa <- setosa[-1,]

# Now use predict function
model <- lm(Sepal.Width ~ Sepal.Length, data=setosa)
predict(model, post2011)

# use 30 random points to test models

set.seed(111) # set the seed for consistancy

index <- sample(150,30)
test.iris <- iris[index,]
train.iris <- iris[-index,]

# make model with train.iris, then predict all points from test.iris
model <- lm(Sepal.Width ~ Sepal.Length, data=train.iris)
pred <- predict(model, test.iris)

model2 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length, data=train.iris)
pred2 <- predict(model2, test.iris)

model3 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, data=train.iris)
pred3 <- predict(model3, test.iris)

# value models by RMSLE
sqrt(mean((log(pred)-log(test.iris$Sepal.Width))^2,na.rm=TRUE))
sqrt(mean((log(pred2)-log(test.iris$Sepal.Width))^2,na.rm=TRUE))
sqrt(mean((log(pred3)-log(test.iris$Sepal.Width))^2,na.rm=TRUE))