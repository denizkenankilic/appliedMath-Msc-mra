library(forecast)
data(EuStockMarkets)
dax<-EuStockMarkets[, "DAX"]
a<- window(dax, start = c(1991, 130), end = 
             c(1994, 130))
fit <- nnetar(a)
fore<-forecast(fit,h=260)
plot(fore)
dev.new()
plot(window(dax, start = c(1991, 130), end = 
              c(1995, 130)))
accuracy(fore,window(dax, start = c(1994, 130), 
                     end = c(1995, 130)))
