
# MDS multi dimensional scaling 다차원 척도법----------------------------------------------------------------


data("eurodist")
head(eurodist)
str(eurodist)

loc <- cmdscale(eurodist)
loc

x <- loc[, 1]
y <- loc[, 2]
plot(x, y, type = "n", main = "eurodist")
text(x, y, rownames(loc), cex = 0.8)
abline(v = 0, h = 0, col = "red")


# PCA ---------------------------------------------------------------------


library(datasets)
data("USArrests")
head(USArrests)

mydata <- USArrests
fit <- princomp(mydata, cor = TRUE)
summary(fit)
summary(fit, loadings = T)
loadings(fit)
plot(fit, type = "lines")
fit$scores
biplot(fit) # 주성분 분석 도표로 나타내는 것