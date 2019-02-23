library(faraway)
par(mfrow=c(1,3))
data(exa)
#?exa # info for this simulated dataset
plot (y ~ x, exa, main="Example A")
lines(m ~ x, exa)
data(exb)
#?exb
plot(y ~ x, exb, main="Example B")
lines(m ~ x, exb)
data(faithful)
#?faithful
plot(waiting ~ eruptions, faithful,main="old Faithful")

# Kernel smoother with different bandwidths for the Old Faithful data.
?ksmooth

par(mfrow=c(1,3))
plot(waiting ~ eruptions,
     faithful,main="bandwidth=0.1", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", 0.1), lwd=2)

plot(waiting ~ eruptions, faithful, main="bandwidth=0.5", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting,"normal", 0.5), lwd=2)

plot(waiting ~ eruptions, faithful, main="bandwidth=2", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", 2), lwd=2)

# Use CV to select bandwidth
library(sm)
?hcv # find smoothing parameter
?sm.options  # check the options
?sm.regression # normal kernel

par(mfrow=c(1,2))
hm = hcv(faithful$eruptions,faithful$waiting,display="lines")
sm.regression(faithful$eruptions, faithful$waiting, h=hm,
              xlab="duration", ylab="waiting")


par(mfrow=c(1,2))
hm=hcv(exa$x, exa$y, display="lines")
sm.regression(exa$x, exa$y, h=hm, xlab="x", ylab="y")

# Error message for exb data since search boundary search is reached.
hcv(exb$x,exb$y)
# try a smaller hstart, still error
par(mfrow=c(1,2))
hm = hcv(exb$x, exb$y, dislay = "lines", hstart=0.005)
hm
sm.regression(exb$x,exb$y,h=0.005)

# kernel or other local regression methods are not suitable for dataset with outliers

# Try Loess

?loess
# default: span = 0.75, degree = 2
par(mfrow=c(1,3))
plot(waiting ~ eruptions, faithful,col="gray", cex=0.5)
f=loess(waiting ~ eruptions, faithful)
i = order(faithful$eruptions)
lines(f$x[i],f$fitted[i], lwd=1.5, col="red")

plot(y ~ x, exa, col="gray", cex=0.5)
lines(exa$x,exa$m,lty=1)
f = loess(y ~ x,exa)
lines(f$x,f$fitted,lty=2, lwd=1.5, col="red")
f = loess(y ~ x, exa, span=0.22)
lines(f$x,f$fitted,lty=5, lwd=1.5, col="blue")

plot(y ~ x, exb, col="gray", cex=0.5)
lines(exb$x,exb$m, lty=1)
f =loess(y ~ x, exb)
lines(f$x,f$fitted,lty=2,lwd=1.5, col="red")
f = loess(y ~ x, exb,span=1)
lines(f$x,f$fitted,lty=5, lwd=1.5, col="blue")


