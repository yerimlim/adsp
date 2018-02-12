
# reshape -----------------------------------------------------------------
library(reshape2)

data("airquality")
head(airquality)
names(airquality)
names(airquality) <- tolower(names(airquality))

# wide data를 long data로 바꿔보자
aqm <- melt(airquality, c("month", "day"), na.rm = T)
head(aqm)

# cast를 이용해서 자료 요약해보자.
a <- cast(aqm, day~month~variable)
a

b <- cast(aqm, month~ variable)
b

c <- cast(aqm, month ~ . | variable, mean)
c

d <- cast(aqm, month ~ variable, margins = c("grand_row", "grand_col"))
d

e <- cast(aqm, day ~ month, mean, subset = variable == "ozone")
e

f <- cast(aqm, month ~ variable, range)
f
colnames(f)


# sqldf -------------------------------------------------------------------

library(sqldf)

data(iris)
head(iris)


sqldf("select * from iris limit 10")

sqldf("select count(*) from iris where Species like 'Se%'")


# plyr --------------------------------------------------------------------
library(plyr)

set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 6), count = round(runif(9, 0, 20)))
print(d)

ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count / mean.count
  data.frame(cv.count = cv)
})


ddply(d, "year", summarise, mean.count = mean(count))
ddply(d, "year", summarise, total.count = sum(count))
ddply(d, "year", transform, total.count = sum(count))

# 병렬처리
x <- c(1:10)
wait <- function(i) Sys.sleep(0.1)
system.time((llply(x, wait)))
system.time(sapply(x, wait))


install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 2)

system.time(llply(x, wait, .parallel = T))
system.time(llply(x, wait, .parallel = T))



# data table --------------------------------------------------------------

install.packages("data.table")
library(data.table)

dt <- data.table(x = c("b", "b", "b", "a", "a"), v = rnorm(5))
dt

data(cars)
head(cars)

CARS <- data.table(cars)
CARS
str(CARS)

tables()

sapply(CARS, class)
dt
dt[2, ]
dt[dt$x == "b"]

cat(try(dt["b", ], silent = T))
setkey(dt, x)
dt

tables()

# 키가 지정된 데이터 테이블을 이용하여 여러 조건을 추출해보자
dt["b"] # dt["b", ]
dt["b", mult = "first"]
dt["b", mult = "last"]

# 크기가 큰 데이터프레임 df를 임의로 만들어보자.
grpsize <- ceiling(1e7 / 25 ^ 2)
tt <- system.time(df <- data.frame(
  x = rep(LETTERS, each = 26 * grpsize),
  y = rep(letters, each = grpsize),
  v = runif(grpsize * 26 ^ 2),
  stringAsFactor = F
))
tt
head(df, 3)
tail(df, 3)
dim(df)

# df에서 일정한 조건을 만족하는 데이터를 추출해보자. 시간이 얼마나 걸리나 보자.
tt <- system.time(ans1 <- df[df$x == "R" & df$y == "h", ])
tt
head(ans1, 3)

dt <- data.table(df)
setkey(dt, x, y)
ss <- system.time(ans2 <- dt[J("R", "h")])
ss # 데이터 테이블로 변환했을 때, 처리속도가 0에 수렴하는 것을 확인할 수 있다.
head(ans2, 3)
dim(ans2)
identical(ans1$v, ans2$v)

# 데이터 테이블은 binary search를 이용하여 사용해야 빠르다. 데이터프레임과 동일한 방법으로 사용하면 처리 속도에는 차이가 없다.
system.time(ans3 <- df[df$x == "R" & df$y == "h", ])
mapply(identical, ans1, ans3)



dt[, sum(v)]
dt[, sum(v), by = x]

# 데이터 테이블 형식으로  summary를 해보자. 속도가 매우 빨라져서 효율적이다.
ttt <- system.time(tt <- tapply(dt$v, dt$x, sum))
ttt

sss <- system.time((ss <- dt[, sum(v), by = x]))
sss

head(tt)
head(ss)

sss <- system.time(ss <- dt[, sum(v), by = "x,y"])
sss
ss

dt <- data.table(a = LETTERS[c(1, 1:3)], b = 4:7, key = "a")
dt

dt[, c := 8]
dt 
dt[, d := 9L]
dt
dt[, c := NULL]
dt
dt[, d := 10L]
dt
dt[b > 4, b := d * 2L]
dt
dt["A", b := 0L]
dt
dt[, e := mean(d), by = a]
dt
dt["B", f := mean(d)]
dt
dt <- data.table(a = 1:5, b = 6:10)
dt[b %between% c(7, 9)]

