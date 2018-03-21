
# party 패키지를 이용한 의사결정나무 ---------------------------------------------------

data(iris)
summary(iris)

library(party)

# 시험용 데이터와 훈련용데이터로 나누어보자. -------------------------------------------------
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(.7,.3))
traindata <- iris[ind==1,]
testdata <- iris[ind==2,]




# 분석을 하자 ------------------------------------------------------------------


myformula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myformula, data=traindata)
iris_ctree
plot(iris_ctree)
plot(iris_ctree, type="simple")
# plot(predict(iris_ctree))
# plot(table(predict(iris_ctree), traindata$Species))
# predict(iris_ctree)

table(predict(iris_ctree), traindata$Species) # 개발된 모형이 얼마나 정확하게 분류하였는지 보자. 

testpred <- predict(iris_ctree, newdata=testdata)
table(testpred, testdata$Species)



# rparty 패키지를 이용한 의사결정나무  -------------------------------------------------
install.packages("mboost")
library(mboost)
install.packages("rpart")
library(rpart)

iris_rpart <- rpart(myformula, data=traindata, control=rpart.control(minsplit=10))
attributes(iris_rpart)
iris_rpart
plot(iris_rpart)
text(iris_rpart, use.n=T)


# 랜덤포레스트를 이용한 의사결정나무 ------------------------------------------------------

install.packages("randomForest")
library(randomForest)
rf <- randomForest(myformula, data=traindata, ntree=100, proximity=T)
rf
plot(rf)
importance(rf)
varImpPlot(rf)

irispred <- predict(rf, newdata=testdata)
table(irispred, testdata$Species)
plot(margin(rf, testdata$Species))
