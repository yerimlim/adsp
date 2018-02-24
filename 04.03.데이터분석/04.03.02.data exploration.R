
# 변수 중요도 ------------------------------------------------------------------

install.packages("klaR")
library(klaR)

iris2 <- iris[, c(1, 3, 5)]
head(iris2)

plineplot(Species ~ ., data = iris2, 
          method = "lda", x = iris[, 4], xlab = "petal. width") # 변수 중요도를 보여줌. 

mN <- NaiveBayes(Species~., data=iris)
par(mfrow=c(2,2));  plot(mN)




data(B3)
head(B3)
gw_obj <- greedy.wilks(PHASEN~., data=B3, niveau=0.1)
gw_obj


install.packages("party")
library(party)

data(iris)
iris$Petal.Width.c <- cut(iris$Petal.Width, 5) # convert numeric to factor according to given groups.
# levels(iris$Petal.Width.c) # 왼쪽 코드를 실행하면 다섯 개의 범주로 나뉜 것을 확인할 수 있다. 

a <- ctree(Species~. , data=iris) #의사결정나무의 함수는 CTREE
plot(a)
a


# 기초분석 및 데이터 관리 -----------------------------------------------------------


# 결측값 처리하는 방법 측using Amelia ---------------------------------------------------------------------

install.packages("Amelia")
library(Amelia)

data("freetrade")
a.out <- amelia(freetrade, m=5, ts="year", cs="country")
hist(a.out$imputations[[3]]$tariff, col="grey",border="white")
save(a.out, file="imputations.RData")
write.amelia(obj=a.out, file.stem="outdata")
missmap(a.out)

freetrade$tariff <- a.aout$imputation[[5]]$tariff # 결측값을 대치하자. 
missmap(freetrade)
