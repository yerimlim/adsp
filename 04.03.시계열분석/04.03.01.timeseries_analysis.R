
knitr::opts_chunk$set(comment = "")


library(TTR)
library(forecast)

# reading data ------------------------------------------------------------
kings <- scan("https://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
skirt <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat", skip = 5)

# basic timeseries analysis -----------------------------------------------
head(births)
head(kings)
head(souvenir)
kingstimeseries <- ts(kings)
birthstimeseries <- ts(births, frequency = 12, start = c(1946, 1))
birthstimeseries2 <- ts(births, frequency = 12, start = c(1946, 7))
birthstimeseries3 <- ts(births, frequency = 10, start = c(1946, 1))
birthstimeseries4 <- ts(births, frequency = 6, start = c(1946, 1))
head(birthstimeseries)
head(birthstimeseries2)
head(birthstimeseries3)
head(birthstimeseries4)

plot.ts(birthstimeseries)
plot.ts(birthstimeseries2)
plot.ts(birthstimeseries3)
plot.ts(kingstimeseries)


souvenirtimeseries <- ts(souvenir, frequency = 12, start = c(1987, 1))
souvenirtimeseries

logsouvenierstimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenierstimeseries)

# decompose non-seasonal data ---------------------------------------------
kingstimeseries.SMA3 <- SMA(kingstimeseries, n = 3)
plot.ts(kingstimeseries.SMA3)
kingstimeseries.SMA8 <- SMA(kingstimeseries, n = 8)
plot.ts(kingstimeseries.SMA8)


# Decompose Seasonal data------
birthstimeseries.components <- decompose(birthstimeseries)
# decompose(birthstimeseries, type="additive")
# decompose(birthstimeseries, type="multiplicative")
birthstimeseries.components$seasonal
birthstimeseries.components$trend
birthstimeseries.components$type

plot(birthstimeseries.components)
plot.ts(birthstimeseries.components$seasonal)
plot.ts(birthstimeseries.components$trend)


# Seosonal adjusting ------------------------------------------------------

birthstimeseries.seasonally.adjusted <- birthstimeseries - birthstimeseries.components$seasonal
plot(birthstimeseries.seasonally.adjusted, main = "seasonally adjusted") # 계절적인 요소가 제거된 시계열 모형

op <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))

plot(birthstimeseries, main = "timeseries")
plot(birthstimeseries.components$seasonal, main = "seasomal")
plot(birthstimeseries.seasonally.adjusted, main = "seasonally adjusted") # 계절적인 요소가 제거된 시계열 모형

par(mfrow = c(1, 1))


# exercise ----------------------------------------------------------------

is.ts(births)

is.ts(birthstimeseries)

str(birthstimeseries)

# timeseries exercises(from R documentation example) ----------------------

require(graphics)

ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print(ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
# print.ts(.)
## Using July 1954 as start date:
gnp <- ts(
  cumsum(1 + round(rnorm(100), 2)),
  start = c(1954, 7), frequency = 12
)
plot(gnp) # using 'plot.ts' for time-series plot

## Multivariate
z <- ts(
  matrix(rnorm(300), 100, 3),
  start = c(1961, 1), frequency = 12
)
class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1:3) # , col=c(1,2,3))

## A phase plot:
plot(
  nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
  main = "Lag plot of New Haven temperatures"
)



# ARIMA model -------------------------------------------------------------

# skirt data
head(skirt)

skirtseries <- ts(skirt, frequency = 1, start = 1866)
plot.ts(skirtseries)

par(mfrow = c(3, 1))

skirtseriesdiff <- diff(skirtseries, differences = 1) # 시간에 따라 평균이 다름. 비정상시계열.
plot.ts(skirtseriesdiff)
skirtseriesdiff2 <- diff(skirtseries, differences = 2)
plot.ts(skirtseriesdiff2)
skirtseriesdiff3 <- diff(skirtseries, differences = 3)
plot.ts(skirtseriesdiff3)

par(mfrow = c(1, 1))

# kings data
par(mfrow = c(3, 1))

plot.ts(kingstimeseries, main = "정상성이 만족되지 않은 그래프")
kingstimeseriesdiff1 <- diff(kingstimeseries, differences = 1)
plot.ts(kingstimeseriesdiff1, main = "1차분한 시계열. 정상성이 만족된다")
kingstimeseriesdiff2 <- diff(kingstimeseries, differences = 2)
plot.ts(kingstimeseriesdiff2, main = "2차분한 시계열. 이 또한 역시 정상정이 만족된다")
par(mfrow = c(1, 1))

# 적합한 ARIMA모델 결정하기 (ACF PACF을 이용하기)
acf(kingstimeseriesdiff1, lag.max = 20)
acf(kingstimeseriesdiff1, lag.max = 20, plot = F)

pacf(kingstimeseriesdiff1, lag.max=20)
pacf(kingstimeseriesdiff1, lag.max=20, plot= F)

library(forecast)
auto.arima(kings)

# volcano data
volcano <- scan("https://robjhyndman.com/tsdldata/annual/dvi.dat", skip = 1)
head(volcano)
volcanotimeseries <- ts(volcano,  start = c(1500))
plot.ts(volcanotimeseries)
    # tmp <- ts(volcano, frequency = 10,  start = c(1500))
    # tmp <- decompose(tmp)
    # plot.ts(tmp$random)

acf(volcanotimeseries, lag.max=20)    
acf(volcanotimeseries, lag.max=20, plot=F)    
pacf(volcanotimeseries, lag.max=20)    
pacf(volcanotimeseries, lag.max=20, plot=F)    

auto.arima(volcano)
pacf()

# king data로 예측하기
kingstimeseries.arima <- arima(kingstimeseries, order=c(0,1,1))
kingstimeseries.arima

library(forecast)
kingstimeseries.forecast <- forecast(kingstimeseries.arima)
plot(kingstimeseries.forecast)

# volcano data 예측하기
volcanotimeseries.arima <- arima(volcanotimeseries, order=c(2,0,0))
volcanotimeseries.arima
volcanotimeseries.forecast <- forecast(volcanotimeseries.arima)
plot(volcanotimeseries.arima)
plot(volcanotimeseries.forecast, n=31)
