

# MDS multi dimensional scaling 다차원 척도법----------------------------------------------------------------

library(MASS)
data("eurodist")
head(eurodist)
str(eurodist)

data <- cmdscale(eurodist)
loc <- cmdscale(eurodist)
loc

x <- loc[, 1]
y <- loc[, 2]
plot(x, y, type = "n", main = "eurodist")
text(x, y, rownames(loc), cex = 0.8)
abline(v = 0, h = 0, col = "red")

## swiss 데이터를 이용한 비계량적 MDS 분석

# isoMDS 사용 사례
data(swiss)
swiss.x <- as.matrix(swiss[,-1])
swiss.dist <- dist(swiss.x) #순서척도를 거리의 속성과 같도록 변환함
swiss.mds <- isoMDS(swiss.dist)
str(swiss.mds)
plot(swiss.mds$points, type="n")
text(swiss.mds$points, labels=as.character(1:nrow(swiss.x)))
abline(v=0, h=0,lty=2,lwd=0.5)

# samone 함수 사용 사례
# swiss.x <- as.matrix(swiss[,-1])
# swiss.dist <- dist(swiss.x)
swiss.sammon <- sammon(swiss.dist)
str(swiss.sammon)
plot(swiss.sammon$points, type="n")
text(swiss.sammon$points, labels=as.character(1:nrow(swiss.x)))
abline(v=0,h=0, lty=2, lwd=0.5)

# PCA ---------------------------------------------------------------------


library(datasets)
data("USArrests")
head(USArrests)

pairs(USArrests, panel=panel.smooth, main="USArrests data")


mydata <- USArrests
fit <- princomp(mydata, cor = TRUE)
summary(fit)
str(fit)
summary(fit, loadings = T)
loadings(fit)
plot(fit, type = "lines")
fit$scores
biplot(fit) # 주성분 분석 도표로 나타내는 것




# PCA ---------------------------------------------------------------------


library(datasets)
data("USArrests")
head(USArrests)

mydata <- USArrests
fit <- princomp(mydata, cor=TRUE)
summary(fit)
summary(fit, loadings = T)
loadings(fit)
plot(fit, type="lines")
fit$scores
biplot(fit) #주성분 분석 도표로 나타내는 것


data <- matrix(c(2,1,5,2,4,5,3,2,5,1,4,3),c(4,3))
data <- as.data.frame((data))
colnames(data) <- c("면","그릇","국물")
rownames(data) <- c("쇠고기라면","해물라면","얼큰라면,","떡라면")

p1 <- princomp(data, scale=TRUE)
# str(p1)
summary(p1)
p1$loadings
round(predict(p1),2)
biplot(p1)

