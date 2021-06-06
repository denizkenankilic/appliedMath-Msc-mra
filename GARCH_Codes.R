#plotting dax data and its returns
dax<-(EuStockMarkets)[,"DAX"] 
return.dax<-diff(log(dax))
par(mfrow=c(1,2))
plot(dax)
plot(return.dax)
#summary of AR(1)-ARCH(1) of return of DAX
install.packages("fGarch")
library(fGarch)
summary(garchFit(~arma(1,0)+garch(1,0),return.dax)) 
summary(dax.g <- garchFit(~garch(1,1),return.dax))
#Forecast of GARCH(1,1) volatility of DAX return
u = dax.g@sigma.t
plot(window(return.dax), ylim=c(-.12,.12), 
     ylab="DAX Returns")
lines(window(return.dax-2*u), lty=2, col=4)
lines(window(return.dax+2*u), lty=2, col=4)

