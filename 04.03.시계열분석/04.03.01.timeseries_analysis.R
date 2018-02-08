library('TTR')
library('forecast')

# reading data ------------------------------------------------------------
kings <- scan('https://robjhyndman.com/tsdldata/misc/kings.dat', skip=3)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

# basic timeseries analysis -----------------------------------------------
head(births)
head(kings)
head(souvenir)
kingstimeseries <- ts(kings)
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries2<- ts(births, frequency=12, start=c(1946,7))
birthstimeseries3 <- ts(births, frequency=10, start=c(1946,1))
birthstimeseries4 <- ts(births, frequency=6, start=c(1946,1))
head(birthstimeseries)
head(birthstimeseries2)
head(birthstimeseries3)
head(birthstimeseries4)

plot.ts(birthstimeseries)
plot.ts(birthstimeseries2)
plot.ts(birthstimeseries3)
plot.ts(kingstimeseries)


souvenirtimeseries <- ts(souvenir, frequency= 12, start=c(1987,1))
souvenirtimeseries

logsouvenierstimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenierstimeseries)

# decompose non-seasonal data ---------------------------------------------
kingstimeseries.SMA3 <- SMA(kingstimeseries, n=3)
plot.ts(kingstimeseries.SMA3)
kingstimeseries.SMA8 <- SMA(kingstimeseries, n=8)
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
plot(birthstimeseries.seasonally.adjusted, main="seasonally adjusted") #계절적인 요소가 제거된 시계열 모형

op <- par(no.readonly = TRUE)
par(mfrow=c(1,3))

plot(birthstimeseries, main="timeseries")
plot(birthstimeseries.components$seasonal, main="seasomal") 
plot(birthstimeseries.seasonally.adjusted, main="seasonally adjusted") #계절적인 요소가 제거된 시계열 모형

par(mfrow=c(1,1))


