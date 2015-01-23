library(gains)
library(ggplot2)
library(lattice)
data(ciaScores)
gains <- with(subset(ciaScores,train==0),
     gains(actual=CellPhonesPP, predicted=PredOLS, optimal=TRUE))
summary(gains)


plot(gains,
     main="Test Gains Table Plot")

plot(gains$cume.lift, type="l")
plot(gains$mean.resp, type="l")
plot(gains$depth, type="l")  #No data mining model 
plot(gains$cume.pct.of.total, type="l") #cum response curve.

