
library(fBasics) # Load the package fBasics.
library(astsa)

#par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
par(mfrow=c(1,1))
#1a
#xt = −0.92xt−2 + wt
w = rnorm(100,0,1)
x = filter(w, filter=c(0,-.92),  sides=1,method="recursive")
plot.ts(x, main=expression(-0.92*xt-2+wt))
# moving average filter
# vt = (xt + xt−1 + xt−2 + xt−3)/4
v = filter(x, sides=1, rep(1/4,4),method="convolution") 
lines(v,lty=2)

#1b 
#xt = cos(2πt/4).
cs = cos(2*pi*1:100 /4)
plot.ts(cs, main=expression(cos((2*pi*t)/4)))
v2 = filter(cs, sides=1, rep(1/4,4),method="convolution") # moving average 
lines(v2,lty=2)

#1c
plot.ts(cs+w, main=expression( cos((2*pi*t)/4) + N(0,1))) 
v3= filter(cs+w, sides=2, rep(1/4,4),method="convolution") # moving average 
lines(v3,lty=2)

#2
w <- rnorm(100)
x <- filter(w, filter=c(1, 2, 1),sides=1,method="convolution")
a <- acf(x, type="correlation",na.action = na.pass)
print(a$acf)