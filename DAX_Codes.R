#some descriptive analysis
dax<-(EuStockMarkets)[,"DAX"]
par(mfrow=c(3,2))
plot(dax)
acf(dax)
pacf(dax)
hist(dax)
qqnorm(dax)
qqline(dax)
qqplot(rt(1000,df=3), dax, main="t(3) Q-Q Plot",ylab=
         "Sample Quantiles")
qqline(dax)
par(mfrow=c(1,2)) 
#for comparing the histograms for different number of breakpoints
hist(dax)
hist(dax,100)
summary(dax)
kurtosis(dax)
Mode <- function(x) {
  ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
   }
Mode(dax)
#general look at spectrums of DAX
DAX<-ts(EuStockMarkets[0:1860],frequency=260,
        start=c(1991,130))
par(mfrow=c(2,2))
plot(DAX)
spec.pgram(DAX, taper=0, log="no")
spectrum(DAX)
spectrum(DAX,method="ar")
spectrum(DAX,method="ar",na.action=na.pass)
spec.ar(DAX, plot=TRUE, method = "mle", 
        add = TRUE,col = "forest green")
spec.ar(DAX, plot=TRUE, method = "ols", 
        add = TRUE,col = "blue")
spec.ar(DAX, plot=TRUE, method = "burg", 
        add = TRUE,col = "red")
legend("topleft",c("YuleWalker","MLE","OLS","Burg"),
       lty=c(1,1,1,1),
       lwd=c(2.5,2.5,2.5,2.5),col=c("black",
                      "forest green","blue","red"))
#spanned smooth periodograms(Daniell)
par(mfrow=c(2,2))
spectrum(DAX)
spectrum(DAX, spans = c(3,5))
text(100,1e+05,"widths(3,5)",col="blue")
spectrum(DAX, spans = c(5,7))
text(100,1e+05,"widths(5,7)",col="blue")
spectrum(DAX, spans = c(19,23))
text(100,10000,"widths(19,23)",col="blue")
#kernel smooth periodograms(Daniell)
par(mfrow=c(2,2))
spectrum(DAX)
spectrum(DAX,kernel("daniell", c(3,5)))
text(100,10000,"widths(3,5)",col="blue")
spectrum(DAX,kernel("daniell", c(5,7)))
text(100,10000,"widths(5,7)",col="blue")
spectrum(DAX,kernel("daniell", c(19,23)))
text(100,10000,"widths(19,23)",col="blue")
