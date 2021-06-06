#getting data form yahoo web page
str <- sprintf("%s?s=^GSPC&d=7&e=4&f=2011&g=d&a=0&b=3&c=1950",
    "http://ichart.finance.yahoo.com/table.csv")
df <- tryCatch(read.csv(url(str)), error = function(e) NA)
names(df) <- tolower(names(df))
df$date <- as.Date(df$date)
df <- df[order(df$date), ]
start.date <- "1990-01-01"
end.date <- "2011-01-01"
close.data<-ts(subset(df,date>=start.date|date>=end.date)
               [,c("close")])
df$return <- c(diff(log(df$close)), NA)
return.data<-ts(subset(df,date>=start.date|date>=end.date)
                [,c("return")])
par(mfrow=c(1,2))
plot(subset(df,date>=start.date|date>=end.date)
     [,c("date","close")],
     type="l",main="S&P 500", xlab="", col="tomato")
plot(subset(df,date>=start.date|date>=end.date)
     [,c("date","return")],
     type="l",main="S&P 500 Return", xlab="", col="blue")
#recurrence plots to see characteristics of time series
install.packages("tseriesChaos")
recurr(close.data,m=12,d=1)
recurr(close.data,m=60,d=2)
recurr(return.data,m=12,d=1)
recurr(return.data,m=60,d=2)
#histogram and Q-Q plot
par(mfrow=c(2,1))
hist(close.data, prob=TRUE, 60)
lines(density(close.data,na.rm = TRUE))
qqnorm(close.data) 
qqline(close.data)
par(mfrow=c(2,1))
hist(return.data, prob=TRUE, 60)
lines(density(return.data,na.rm = TRUE))
qqnorm(return.data) 
qqline(return.data)
library(car)
par(mfrow=c(2,2))
qqPlot(return.data, distribution="norm",
       ylab="S&P500 quantiles", 
       envelope=FALSE)
qqPlot(return.data, distribution="lnorm",
       ylab="S&P500 quantiles", 
       envelope=FALSE)
qqPlot(return.data, distribution="unif",
       ylab="S&P500 quantiles", 
       envelope=FALSE)
qqPlot(return.data, distribution="t", df=5,
       ylab="S&P500 quantiles", 
       envelope=FALSE)
#Shapiro-Wilk normality test
shapiro.test(close.data[0:4999])
shapiro.test(return.data[0:4999])
shapiro.test(diff(close.data[0:4999]))
#Kolmogorov-Smirnov test
ks.test(close.data, "pnorm", mean(close.data), 
        sd(close.data))
ks.test(return.data, "pnorm", mean(return.data), 
        sd(return.data))
ks.test(diff(close.data), "pnorm", mean(diff(close.data)), 
        sd(diff(close.data)))
#for looking the lag-lag plot fill the missing values with 
#average values and get the lag plot
mean(close.data, na.rm=T)
#for missing values
close.data[is.na(close.data)]<-mean(close.data, 
                                    na.rm=T) 
lag1.plot(close.data, 9)
mean(return.data, na.rm=T)
return.data[is.na(return.data)]<-mean(return.data, 
                                    na.rm=T)
lag1.plot(return.data, 9)
#Abs(return) and squared return
par(mfrow=c(3,1))
plot(return.data)
plot(abs(return.data))
plot(return.data^2)
dev.new()
par(mfrow=c(1,2))
acf(abs(return.data))
acf(return.data^2)
#Acf and Pacf analysis
dev.new()
par(mfrow=c(2,2))
acf(close.data,40)
pacf(close.data,40)
acf(return.data,40)
pacf(return.data,40)
acf2(diff(return.data),40)
#Big picture of the data with frequecny 1 and frequency 260
acf2(close.data,5566)
freq.close<-ts(close.data,frequency=260,
               start=c(1990,1))
acf2(freq.close, 5566)
acf2(return.data,5566) 
freq.return<-ts(return.data,frequency=260,
                start=c(1990,1))
acf2(freq.return, 5566)
acf2(diff(return.data),5565) 
#descriptive statistics
library(moments)
close.stats <- c(mean(close.data), sd(close.data), 
        skewness(close.data), kurtosis(close.data))
names(close.stats) <- statNames
close.stats
return.stats <- c(mean(return.data), 
        sd(return.data), skewness(return.data), 
        kurtosis(return.data))
names(return.stats) <- statNames
return.stats
log.close.stats <- c(mean(log(close.data)), 
        sd(log(close.data)), skewness(log(close.data)), 
        kurtosis(log(close.data)))
names(log.close.stats) <- statNames
log.close.stats
diff.close.stats <- c(mean(diff(close.data)), 
       sd(diff(close.data)), skewness(diff(close.data)), 
       kurtosis(diff(close.data)))
names(diff.close.stats) <- statNames
diff.close.stats
diff.log.close.stats <- c(mean(diff(log(close.data))), 
      sd(diff(log(close.data))), skewness(diff(log(close.data))), 
      kurtosis(diff(log(close.data))))
names(diff.log.close.stats) <- statNames
diff.log.close.stats
diff.diff.close.stats <- c(mean(diff(diff(close.data))), 
      sd(diff(diff(close.data))), skewness(diff(diff(close.data))), 
      kurtosis(diff(diff(close.data))))
names(diff.diff.close.stats) <- statNames
diff.diff.close.stats
diff.return.stats <- c(mean(diff(return.data)), 
      sd(diff(return.data)), skewness(diff(return.data)), 
      kurtosis(diff(return.data)))
names(diff.return.stats) <- statNames
diff.return.stats
#tests
adf.test(close.data, alternative="stationary")
adf.test(return.data, alternative="stationary")
library(tseries)
kpss.test(close.data, null = "Trend")
kpss.test(return.data, null = "Trend")
#linear filtering
plot(close.data, type = "l")
tui.1 <- filter(close.data, filter = rep(1/5, 5))
tui.2 <- filter(close.data, filter = rep(1/25, 25))
tui.3 <- filter(close.data, filter = rep(1/131, 131))
lines(tui.1, col = "red")
lines(tui.2, col = "purple")
lines(tui.3, col = "blue")
#decomposition
plot(decom <- stl(log(freq.close), "per")) #Loess method
plot(decom2 <-decompose(log(freq.close))) #MA method
decomposed.data <- decompose(freq.close, 
                             type="multiplicative")
plot(freq.close - decomposed.data$trend, 
     main="signal without trend component")
#Exponential smoothing
model <- HoltWinters(freq.close)
plot(freq.close)
lines(model$fitted[,"xhat"], col="red")
pred <- predict(model, n.ahead=200)
plot(freq.close)
lines(pred, col="red", lty=2)
plot(forecast(model, h=200, level=c(75,95)))
dev.new()
fit <- stlf(freq.close)
plot(forecast(fit, level=c(75,95)))
summary(fit)
#Fourier analysis
par(mfrow=c(2,2))
plot(subset(df, date >= start.date)[ , c("date", "close")], 
     type="l",main="S&P 500", xlab="", col="tomato")
mtext(sprintf("Closing prices since %s", start.date))
spectrum(close.data,na.action=na.pass)
spectrum(close.data,kernel("daniell", c(10,20)),
         na.action=na.pass)
spectrum(close.data,method="ar",na.action=na.pass)
spec.ar(close.data, plot=TRUE, method = "mle", 
        add = TRUE,col = "forest green")
spec.ar(close.data, plot=TRUE, method = "ols", 
        add = TRUE,col = "blue")
spec.ar(close.data, plot=TRUE, method = "burg", 
        add = TRUE,col = "red")
legend("topleft",c("YuleWalker","MLE","OLS","Burg"),
       lty=c(1,1,1,1),lwd=c(2.5,2.5,2.5,2.5),
       col=c("black","forest green","blue","red"))
#smoothed periodograms of daily closing prices
par(mfrow=c(2,2))
specvalues1 <- spec.pgram(close.data,
                          kernel("daniell", c(13,13)),taper=0)
text(0.4,100000,"widths(13,13)",col="blue")
specvalues2 <- spec.pgram(close.data,
                          kernel("daniell", c(21,21)),taper=0)
text(0.4,100000,"widths(21,21)",col="blue")
specvalues3 <- spec.pgram(close.data,
                          kernel("daniell", c(13,41)),taper=0)
text(0.4,100000,"widths(13,41)",col="blue")
specvalues <- spec.pgram(close.data,taper=0, log="no")
text(0.4,40000000,"lag=no",col="blue")
#peaks at periodogram
spectrum(close.data,method="ar",na.action=na.pass)
a<-spec.ar(close.data, plot=TRUE, method = "burg", 
           add = TRUE,col = "red")
x<-a$spec
y<-a$freq
z<-ts(x,y)
install.packages("quantmod")
findPeaks(z, thresh=0) #thresh is for minimum peak/valley threshold
legend("topleft",c("YuleWalker","Burg"),lty=c(1,1),
       lwd=c(2.5,2.5), col=c("black","red"))
a$freq
abline(v=1/13.4865, lty="dotted")
abline(v=1/9.7843, lty="dotted")
abline(v=1/7.4478, lty="dotted")
abline(v=1/6.0485, lty="dotted")
abline(v=1/4.4554, lty="dotted")
abline(v=1/3.9447, lty="dotted")
abline(v=1/3.2090, lty="dotted")
abline(v=1/2.7194, lty="dotted")
abline(v=1/2.0493, lty="dotted")
#Wavelet analysis
install.packages("wavelets")
library(wavelets)
#DWT
wt <- dwt((close.data[1:5444]), boundary="reflection", 
          fast=FALSE)
plot.dwt(wt, levels = list(c(1,2,3,4,5),c()), 
         draw.boundary = TRUE,col.boundary = "green")
wt2 <- dwt((return.data[1:5444]), boundary="reflection", 
           fast=FALSE)
plot.dwt(wt2, levels=6, draw.boundary = TRUE,
         col.boundary = "green")
#MODWT
modwt.close.data<-modwt(close.data,filter="d2",
                  n.levels=8, boundary="periodic",fast=TRUE)
plot.modwt(modwt.close.data)
dev.new()
modwt.close.data2<-modwt(close.data,filter="d2",
                  n.levels=10,boundary="periodic",fast=TRUE)
plot.modwt(modwt.close.data2)
modwt.close.data3<-modwt(close.data,filter="la8",
                  n.levels=8,boundary="periodic",fast=TRUE)
plot.modwt(modwt.close.data3)
modwt.close.data4<-modwt(close.data,filter="la8",
                  n.levels=10,boundary="periodic",fast=TRUE)
plot.modwt(modwt.close.data4)
modwt.return.data<-modwt(return.data,filter="d2",
                  n.levels=8,boundary="periodic",fast=TRUE)
plot.modwt(modwt.return.data)
modwt.return.data2<-modwt(return.data,filter="la8",
                  n.levels=8,boundary="periodic",fast=TRUE)
plot.modwt(modwt.return.data2)
plot.modwt(modwt.return.data2, levels = list(c(5,6,7,8),c(8)), 
           draw.boundary = TRUE,col.boundary = "green")
modwt.return.data3<-modwt(return.data,filter="la8",
                  n.levels=9,boundary="periodic",fast=TRUE)
plot.modwt(modwt.return.data3, levels = list(c(8,9),c(9)), 
           draw.boundary = TRUE,col.boundary = "green")
library(waveslim)
SP500.volatility <- abs(return.data[1:5444])
SP500V.haar <- mra(SP500.volatility, "haar", 4, "modwt")
names(SP500V.haar) <- c("d1", "d2", "d3", "d4", "s4")
SP500V.la8 <- mra(SP500.volatility, "la8", 4, "modwt")
names(SP500V.la8) <- c("d1", "d2", "d3", "d4", "s4")
par(mfcol=c(6,1), pty="m", mar=c(5-2,4,4-2,2))
plot.ts(SP500.volatility, axes=FALSE, ylab="", main="(a)")
for(i in 1:5)
  plot.ts(SP500V.haar[[i]], axes=FALSE, ylab=names(SP500V.haar)[i])
axis(side=1, at=seq(0,368,by=23),
       + labels=c(0,"",46,"",92,"",138,"",184,"",230,"",276,"
                  ",322,"",368))
par(mfcol=c(6,1), pty="m", mar=c(5-2,4,4-2,2))
plot.ts(SP500.volatility, axes=FALSE, ylab="", main="(b)")
for(i in 1:5)
  plot.ts(SP500V.la8[[i]], axes=FALSE, ylab=names(SP500V.la8)[i])
axis(side=1, at=seq(0,368,by=23),
  labels=c(0,"",46,"",92,"",138,"",184,"",230,"",276,"
           ",322,"",368)) 
#Wavelet Power Spectrum
install.packages("WaveletComp")
library(WaveletComp)
my.data = data.frame(freq.close)
my.w = analyze.wavelet(my.data)
wt.image(my.w)
dev.new()
reconstruct(my.w, plot.waves = F, lwd = c(1,2), 
            legend.coords = "bottomleft")
my.w1 = analyze.wavelet(my.data,method="shuffle")
wt.image(my.w1)
dev.new()
reconstruct(my.w1, plot.waves = F, lwd = c(1,2), 
            legend.coords = "bottomleft")
my.w2 = analyze.wavelet(my.data,method="ARIMA")
wt.image(my.w2)
dev.new()
reconstruct(my.w2, plot.waves = F, lwd = c(1,2), 
            legend.coords = "bottomleft")
library(biwavelet)
wt1=wt(cbind(1:5444,close.data[1:5444]))
par(mfrow=c(1,2))
plot(wt1, type="power.corr.norm", 
     main="Bias-corrected wavelet power")
plot(wt1, type="power.norm", 
     main="Biased wavelet power")
#SARIMA fitting
#The Box-Jenkins model
a<-pacf(close.data)
b<-acf(return.data)
par(mfrow=c(1,2))
plot(a)
plot(b)
#SARIMA models
sarima(close.data, 1, 1, 1)
sarima(return.data, 1, 1, 1)
sarima(freq.close,1,1,1,0,1,1,42)
#Autoselected models
library(forecast)
auto.arima(close.data) #gives ARIMA(0,1,2)
par(mfrow=c(1,2))
plot(close.data)
plot(arima.sim(list(order=c(0,1,2), ma=c(.0675,.0525)), 
               n=5566))
#Data fitting
close.data.arima <- arima(close.data, 
                          order=c(0,1,2))
close.data.arima
diff.close.data.arima <- arima(diff(close.data), 
                               order=c(0,0,2))
diff.close.data.arima
#Auto selection by bic criterion
auto.arima(close.data,ic="bic") #gives ARIMA(1,1,1)
diff.close.data.arima <- arima(diff(close.data), 
                               order=c(1,0,1))
diff.close.data.arima
#Best ARIMA model by AIC
get.best.arima <- function(x.ts, maxord = c(1,1,1))
  {
    best.aic <- 1e8
    n <- length(x.ts)
    for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
      {
        fit <- arima(x.ts, order = c(p,d,q))
        fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
        if (fit.aic < best.aic)
          {
            best.aic <- fit.aic
            best.fit <- fit
            best.model <- c(p,d,q)
            }}
    list(best.aic, best.fit, best.model)
    }
get.best.arima(close.data, maxord=c(2,2,2))
close.arima<-arima(close.data, ord=c(2,1,0), 
                   xreg=1:length(close.data))
nobs=length(close.data)
close.pred <- predict(close.arima, n.ahead=100,
                      newxreg=(nobs+1):(nobs+100))
ts.plot(close.data,close.pred$pred, col=1:2,
        xlim=c(5000,5544),ylim=c(800,1400))
#forecasts
library("forecast")
par(mfrow=c(2,2))
close.data.arima <- arima(close.data, 
                          order=c(0,1,2))
forecasts <- forecast.Arima(close.data.arima , 
                            h=500,level=c(99.5))
plot.forecast(forecasts)
diff.close.data.arima <- arima(diff(close.data), 
                               order=c(0,0,2))
forecasts2 <- forecast.Arima(diff.close.data.arima , 
                             h=500,level=c(99.5))
plot.forecast(forecasts2)
close.data.arima2 <- arima(close.data, 
                           order=c(1,1,1))
forecasts3 <- forecast.Arima(close.data.arima2 , 
                             h=500,level=c(99.5))
plot.forecast(forecasts3)
diff.close.data.arima2 <- arima(diff(close.data), 
                                order=c(1,0,1))
forecasts4 <- forecast.Arima(diff.close.data.arima2 , 
                             h=500,level=c(99.5))
plot.forecast(forecasts4)
dev.new()
par(mfrow=c(2,2))
acf(forecasts$residuals, lag.max=40)
acf(forecasts2$residuals, lag.max=40)
acf(forecasts3$residuals, lag.max=40)
acf(forecasts4$residuals, lag.max=40)
dev.new()
par(mfrow=c(2,2))
plot.ts(forecasts$residuals)
plot.ts(forecasts2$residuals)
plot.ts(forecasts3$residuals)
plot.ts(forecasts4$residuals)
dev.new()
par(mfrow=c(2,2))
plotForecastErrors <- function(forecasterrors)
  {
    mybinsize <- IQR(forecasterrors)/4
    mysd   <- sd(forecasterrors)
    mymin  <- min(forecasterrors) - mysd*5
    mymax  <- max(forecasterrors) + mysd*3
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, 
         breaks=mybins)
    myhist <- hist(mynorm, plot=FALSE, 
                   breaks=mybins)
    points(myhist$mids, myhist$density, type="l", 
           col="blue", lwd=2)
      }
plotForecastErrors(forecasts$residuals)
plotForecastErrors(forecasts2$residuals)
plotForecastErrors(forecasts3$residuals)
plotForecastErrors(forecasts4$residuals)
#mean of forecast residuals
mean(forecasts$residuals)
mean(forecasts2$residuals)
mean(forecasts3$residuals)
mean(forecasts4$residuals)
#ARIMA with Drift
fit1 = arima(close.data, order=c(0,1,2))
nobs = length(close.data)
fit2 = arima(close.data, order=c(0,1,2), 
             xreg=1:nobs)
fore1 = predict(fit1, 200)
fore2 = predict(fit2, 200, newxreg=(nobs+1):(nobs+200))
par(mfrow=c(2,1))
ts.plot(close.data,fore1$pred, col=1:2,
        main="Prediction Without Constant Term")
ts.plot(close.data,fore2$pred, col=1:2,
        main="Prediction With Constant Term")
par(mfrow=c(2,1))
ts.plot(close.data,fore1$pred, col=1:2,
        xlim=c(5000,5644),ylim=c(800,1400),
        main="Prediction Without Constant Term")
ts.plot(close.data,fore2$pred, col=1:2,
        xlim=c(5000,5644),ylim=c(800,1400),
        main="Prediction With Constant Term")
#predictions for sarima models with frequency 260
par(mfrow=c(2,2))
close.s1<-arima(close.data, order=c(2,1,2), 
                seas=list(order=c(0,1,1), 260))
predict<-predict(close.s1, n.ahead=300)
ts.plot(cbind(close.data, predict$pred), lty=1:2, col=1:2,
        main="ARIMA(2,1,2)x(0,1,1) with Frequency 260")
lines(predict$pred+2*predict$se, col="red", lty=3)
lines(predict$pred-2*predict$se ,col="red", lty=3)
close.s2<-arima(close.data, order=c(0,1,2), 
                seas=list(order=c(0,1,1), 260))
predict<-predict(close.s2, n.ahead=300)
ts.plot(cbind(close.data, predict$pred), lty=1:2, col=1:2,
        main="ARIMA(0,1,2)x(0,1,1) with Frequency 260")
lines(predict$pred+2*predict$se, col="red", lty=3)
lines(predict$pred-2*predict$se ,col="red", lty=3)
close.s3<-arima(close.data, order=c(2,1,0), 
                seas=list(order=c(0,1,1), 260))
predict<-predict(close.s3, n.ahead=300)
ts.plot(cbind(close.data, predict$pred), lty=1:2, col=1:2,
        main="ARIMA(2,1,0)x(0,1,1) with Frequency 260")
lines(predict$pred+2*predict$se, col="red", lty=3)
lines(predict$pred-2*predict$se ,col="red", lty=3)
close.s4<-arima(close.data, order=c(1,1,1), 
                seas=list(order=c(0,1,1), 260))
predict<-predict(close.s4, n.ahead=300)
ts.plot(cbind(close.data, predict$pred), lty=1:2, col=1:2,
        main="ARIMA(1,1,1)x(0,1,1) with Frequency 260")
lines(predict$pred+2*predict$se, col="red", lty=3)
lines(predict$pred-2*predict$se ,col="red", lty=3)
#Accuracy
library(forecast)
a<- window(close.data,1,5344)
fit0<-arima(a, order=c(0,1,1), 
            seas=list(order=c(0,1,1), 260))
fore0<-forecast(fit0,h=101)
accuracy(fore0,window(close.data,5344,5444),d=0,D=0)
fit<-arima(a, order=c(1,1,0), 
           seas=list(order=c(0,1,1), 260))
fore<-forecast(fit,h=101)
accuracy(fore,window(close.data,5344,5444),d=0,D=0)
fit1<-arima(a, order=c(1,1,1), 
            seas=list(order=c(0,1,1), 260))
fore1<-forecast(fit1,h=101)
accuracy(fore1,window(close.data,5344,5444),d=0,D=0)
fit2<-arima(a, order=c(0,1,2), 
            seas=list(order=c(0,1,1), 260))
fore2<-forecast(fit2,h=101)
accuracy(fore2,window(close.data,5344,5444),d=0,D=0)
fit3<-arima(a, order=c(2,1,2), 
            seas=list(order=c(0,1,1), 260))
fore3<-forecast(fit3,h=101)
accuracy(fore3,window(close.data,5344,5444),d=0,D=0)
fit4<-arima(a, order=c(2,1,0), 
            seas=list(order=c(0,1,1), 260))
fore4<-forecast(fit4,h=101)
accuracy(fore4,window(close.data,5344,5444),d=0,D=0)
fit5<-arima(a, order=c(2,1,1), 
            seas=list(order=c(0,1,1), 260))
fore5<-forecast(fit5,h=101)
accuracy(fore5,window(close.data,5344,5444),d=0,D=0)
fit6<-arima(a, order=c(1,1,2), 
            seas=list(order=c(0,1,1), 260))
fore6<-forecast(fit6,h=101)
accuracy(fore6,window(close.data,5344,5444),d=0,D=0)
fit7<-arima(a, order=c(1,1,0))
fore7<-forecast(fit7,h=101)
accuracy(fore7,window(close.data,5344,5444),d=0,D=0)
fit8<-arima(a, order=c(0,1,1))
fore8<-forecast(fit8,h=101)
accuracy(fore8,window(close.data,5344,5444),d=0,D=0)
fit9<-arima(a, order=c(0,1,2))
fore9<-forecast(fit9,h=101)
accuracy(fore9,window(close.data,5344,5444),d=0,D=0)
fit10<-arima(a, order=c(2,1,0))
fore10<-forecast(fit10,h=101)
accuracy(fore10,window(close.data,5344,5444),d=0,D=0)
fit11<-arima(a, order=c(1,1,1))
fore11<-forecast(fit11,h=101)
accuracy(fore11,window(close.data,5344,5444),d=0,D=0)
fit12<-arima(a, order=c(2,1,2))
fore12<-forecast(fit12,h=101)
accuracy(fore12,window(close.data,5344,5444),d=0,D=0)
fit13<-arima(a, order=c(1,1,2))
fore13<-forecast(fit13,h=101)
accuracy(fore13,window(close.data,5344,5444),d=0,D=0)
fit14<-arima(a, order=c(2,1,1))
fore14<-forecast(fit14,h=101)
accuracy(fore14,window(close.data,5344,5444),d=0,D=0)
par(mfrow=c(2,2))
plot(fore,main="ARIMA(1,1,0)x(0,1,1) 
     with Frequency 260")
plot(fore2,main="ARIMA(0,1,2)x(0,1,1) 
     with Frequency 260")
plot(fore4,main="ARIMA(2,1,0)x(0,1,1) 
     with Frequency 260")
plot(fore3,main="ARIMA(2,1,2)x(0,1,1) 
     with Frequency 260")
# Wavelet Transform Based ARIMA Fitting
close.dwt.la8.2<-dwt(close.data[1:4096],"la8",2)
V2<- close.dwt.la8.2$s2
W2<- close.dwt.la8.2$d2
W1<- close.dwt.la8.2$d1
arima.V2<-arima(V2,order=c(1,1,1))
arima.W2<-arima(W2,order=c(1,1,1))
arima.W1<-arima(W1,order=c(1,1,1))
fore.V2<-forecast(arima.V2,h=100)
fore.W2<-forecast(arima.W2,h=100)
fore.W1<-forecast(arima.W1,h=100)
sum(fore.V2$residuals+fore.W2$residuals
    +fore.W1$residuals)
close.modwt.la8.2<-modwt(close.data[1:5344],
                         "la8",2)
class(close.modwt.la8.2)
names(close.modwt.la8.2)
cV2<-close.modwt.la8.2$s2
cW2<-close.modwt.la8.2$d2
cW1<-close.modwt.la8.2$d1
sum(close.data[1:5344]^2)
sum(cW1^2)+sum(cW2^2)+sum(cV2^2)
sum(close.data[1:5344]^2)-(sum(cW1^2)
                        +sum(cW2^2)+sum(cV2^2))
auto.arima(cV2)
auto.arima(cW2)
auto.arima(cW1)
a<-arima(cV2,order=c(5,1,5))
b<-arima(cW2,order=c(2,0,4))
c<-arima(cW1,order=c(0,0,5))
fore0<-forecast(a,h=100)
fore1<-forecast(b,h=100)
fore2<-forecast(c,h=100)
fit.arima<-arima(close.data[1:5344],
                 order=c(1,1,1))
forecast.arima<-forecast(fit.arima,h=100)
sum(forecast.arima$residuals)
sum(fore0$residuals+fore1$residuals
    +fore2$residuals)
sum(fore.V2$residuals+fore.W2$residuals
    +fore.W1$residuals)
#GARCH Fittings
y=diff(log(close.data))*100
y=y-mean(y)
garchFit(~garch(1,0),data=y,
         include.mean=FALSE)
summary(garchFit(~garch(1,0),data=y,
                 include.mean=FALSE))
garchFit(~garch(1,0),data=y,
         include.mean=FALSE,cond.dist="sstd",trace=F)
summary(garchFit(~garch(1,0),data=y,
                 include.mean=FALSE,cond.dist="sstd",
                 trace=F))
garchFit(~garch(4,0),data=y,
         include.mean=FALSE,cond.dist="sstd",trace=F)
summary(garchFit(~garch(4,0),data=y,
                 include.mean=FALSE,cond.dist="sstd",
                 trace=F))
garchFit(~garch(4,1),data=y,include.mean=FALSE,
         cond.dist="sstd",trace=F)
summary(garchFit(~garch(4,1),data=y,
                 include.mean=FALSE,cond.dist="sstd",
                 trace=F))
garchFit(~garch(4,1),data=y,include.mean=FALSE,
         cond.dist="sged",trace=F)
summary(garchFit(~garch(4,1),data=y,
                 include.mean=FALSE,cond.dist="sged",
                 trace=F))
garchFit(~garch(1,1),data=y,include.mean=FALSE,
         cond.dist="sstd",trace=F)
summary(garchFit(~garch(1,1),data=y,
                 include.mean=FALSE,cond.dist="sstd",
                 trace=F))
garchFit(~garch(1,1),data=y,
         include.mean=FALSE,cond.dist="sged",trace=F)
summary(garchFit(~garch(1,1),data=y,
                 include.mean=FALSE,cond.dist="sged",
                 trace=F))
#Other methods
#AAR
library(tsDyn)
a<- window(close.data,1,5344)
fit<-aar(a,m=3,d=1,steps=100)
pred<-predict(fit, n.ahead=100)
accuracy(pred,window(close.data,5344,5444),d=0,D=0)
fit<-aar(a,m=4,d=1)
pred<-predict(fit, n.ahead=100)
accuracy(pred,window(close.data,5344,5444),d=0,D=0)
fit<-aar(a,m=10,d=1)
pred<-predict(fit, n.ahead=100)
accuracy(pred,window(close.data,5344,5444),d=0,D=0)
#LSTAR(Logistic Smooth Transition AutoRegressive Model)
mod.lstar <- lstar(a,m=4, d=1,mTh=c(0,1,2,3), 
                   control=list(maxit=3000))
pred<-predict(mod.lstar, n.ahead=101)
accuracy(pred,window(close.data,5344,5444),d=0,D=0)
#SETAR(Self Threshold Autoregressive model)
selectSETAR(log(close.data), m=4, mL=1:3, mH=1:3, 
            thSteps = 5, thDelay=0:2)
mod <- list()
mod[["linear"]] <- linear(close.data, m=4)
mod[["setar"]] <- setar(close.data, m=4,d=1, thDelay=1)
mod[["lstar"]] <- lstar(close.data, m=4,d=1, thDelay=1)
mod[["nnetTs"]] <- nnetTs(close.data, m=4, size=5)
mod[["aar"]] <- aar(close.data, m=4)
mod[["sarima"]] <- arima(close.data, order=c(0,1,2), 
                         seas=list(order=c(0,1,1), 260))
sapply(mod, AIC)