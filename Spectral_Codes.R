#Spectral Density of Noise and ARMA
par(mfrow=c(3,1))
arma.spec(log="no", main="White Noise")
arma.spec(ma=.5, log="no", main="Moving Average")
arma.spec(ar=c(-.4,.3), log="no", main="Autoregression")
arma.spec(ar=c(-.4,.3),ma=.5, log="no", main="Autoregression")
#Example Sine Series
x1<-3*sin(2*pi*1:100*5/100)+4*cos(2*pi*1:100*5/100)
x2<-2*sin(2*pi*1:100*25/100)+3*cos(2*pi*1:100*25/100)
x3<-4*sin(2*pi*1:100*85/100)+6*cos(2*pi*1:100*85/100)
sum=x1+x2+x3
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(frequency
                                      ==5/100~~~A^2==25))
plot.ts(x2, ylim=c(-10,10), main=expression(frequency
                                      ==25/100~~~A^2==13))
plot.ts(x3, ylim=c(-10,10), main=expression(frequency
                                      ==85/100~~~A^2==52))
plot.ts(sum, ylim=c(-20,20), main="sum")
P = abs(2*fft(sum)/100)^2
f = 0:99/100
plot(f, P, type="l", xlab="frequency", ylab="periodogram")
x3<-4*sin(2*pi*1:100*75/100)+6*cos(2*pi*1:100*75/100)
sum=x1+x2+x3
par(mfrow=c(1,2))
P = abs(2*fft(sum)/100)^2
f = 0:99/100
plot(f, P, type="l", xlab="frequency", ylab="periodogram")
x3<-4*sin(2*pi*1:100*50/100)+6*cos(2*pi*1:100*50/100)
sum=x1+x2+x3
#periodogram of sum series
P = abs(2*fft(sum)/100)^2
f = 0:99/100
plot(f, P, type="l", xlab="frequency", ylab="periodogram")
