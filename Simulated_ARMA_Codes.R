# AR(1) Processes
x1 = arima.sim(list(order=c(1,0,0), ar=.8), n=1000)
x2 = arima.sim(list(order=c(1,0,0), ar=-.8), n=1000)
par(mfrow=c(2,1))
plot(x1, main=(expression(AR(1)~~~phi==+.8)))
plot(x2, main=(expression(AR(1)~~~phi==-.8)))
dev.new()
acf2(x1, 999)
acf2(x1, 100)
acf2(x2, 100)
#AR(2) Process
x = arima.sim(list(order=c(2,0,0), ar=c(.5,.4)), n=100)
dev.new()
plot(x,main=(expression(AR(2)~~~phi[1]==.5~~~phi[2]==.4)))
dev.new()
acf2(x)
#MA(1) Process
x = arima.sim(list(order=c(0,0,1), ma=.5), n=1000)
dev.new()
plot(x, main=(expression(MA(1)~~~theta==.5)))
dev.new()
acf2(x,100)
#MA(2) Process
x = arima.sim(list(order=c(0,0,2), ma=c(-.5,-.9)), n=500)
plot(x, main=(expression(MA(2)~~~theta[1]==-.5~~~
                           theta[2]==-.9)))
dev.new()
acf2(x,100)
#ARMA(2,2) Process
x = arima.sim(list(order=c(2,0,2), ar=c(.5,.4),ma=c(-.5,-.9)), 
              n=1000)
plot(x, main=(expression(ARMA(2,2)~~~phi[1]==.5~~~phi[2]==.4~~~
                           theta[1]==-.5~~~theta[2]==-.9)))
dev.new()
acf2(x,100)
#ARIMA(1,1,1) Process
x1 = arima.sim(list(order=c(1,1,1), ar=.9, ma=.5), n=1000)
x2 = arima.sim(list(order=c(1,1,1), ar=.5, ma=-.4), n=1000)
par(mfrow=c(2,2))
plot(x1, main=(expression(ARIMA(1,1,1)~~~phi==.9~~~
                            theta==.5)))
plot(x2, main=(expression(ARIMA(1,1,1)~~~phi==.5~~~
                            theta==-.4)))
acf(x1,100)
acf(x2,100)