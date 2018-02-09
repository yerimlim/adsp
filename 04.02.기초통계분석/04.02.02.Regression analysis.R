## 회귀분석

# 단순회귀분석 ------------------------------------------------------------------
#예제를 풀어보자
# 회귀방정식은 $y= 5.8951 -0.1174x$로 추정된다.

#난수를 추출하여 임의의 데이터프레임을 만들자
set.seed(2)
x=runif(10,0,11) ; x
y=runif(10,0,11) ;y
dfrm <- data.frame(x,y); head(dfrm)

#회귀분석을 해보자.
lm(formula = y~x, data=dfrm)


library(MASS)
head(ChickWeight)

Chick <- ChickWeight[ChickWeight$Diet==1,]
Chick

lm(weight~ Time,data= Chick)

summary(lm(weight~Time, data=Chick))


head(cars)
speed2 <- cars$speed^2
speed <- cars$speed
dist <- cars$dist
# new.cars2 <- cbind(spped2, cars)

new.cars <- data.frame(dist, speed, speed2)

lmcars <- lm(dist~speed + speed2, data=new.cars)
summary(lmcars)






# 변수선택법 -------------------------------------------------------------------
?step
head(iris)
x1 <- iris$Sepal.Length
x2 <- iris$Sepal.Width
x3 <- iris$Petal.Length
y <- iris$Petal.Width

df <- data.frame(x1,x2,x3,y)

linear.m <- lm(y~x1+x2+x3, data=df)

step(lm(y~ 1, df), scope=list(lower=~1, upper=~x1+x2+x3), direction ="forward")
step(lm(y~ x1+x2+x3, df), scope=list(lower=~x1, upper=~x1+x2+x3), direction ="forward")


# 변수선택법 예제 (414p) ---------------------------------------------------------

library(MASS)
data(hills)
head(hills)

lm(time~ climb + dist , data=hills)
summary(lm(time~ climb + dist , data=hills))

step(lm(time~1, hills), scope=list(lower=~1, upper=~climb+dist), direction="forward")


