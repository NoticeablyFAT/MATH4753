spruce <- read.csv("SPRUCE.csv")

library(ggplot2)

ggplot(aes(x = BHDiameter, y = Height), data = spruce) + geom_point(shape=21, fill="blue", size=1.2) + ggtitle("Plot of Height vs Diameter") + scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1*max(spruce$BHDiameter))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1*max(spruce$Height))) + geom_smooth(method="lm")

library(s20x)

layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
trendscatter(spruce$Height~spruce$BHDiameter,f=.5)
trendscatter(spruce$Height~spruce$BHDiameter,f=.6)
trendscatter(spruce$Height~spruce$BHDiameter,f=.7)
plot(spruce$Height~spruce$BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), data=spruce)
mtext("Height Vs BHDiameter",side=3)
spruce.lm = with(spruce, lm(Height~BHDiameter))
abline(spruce.lm)


layout.show(4)
spruce.lm = with(spruce, lm(Height~BHDiameter))
abline(spruce.lm)

plot(spruce$Height~spruce$BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), data=spruce)
yhat=fitted(spruce.lm)

plot(spruce$Height~spruce$BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), data=spruce)
with(spruce,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})
abline(spruce.lm)

plot(spruce$Height~spruce$BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), data=spruce)
with(spruce,{
  segments(BHDiameter,mean(Height),BHDiameter,yhat)
})
with(spruce, abline(h=mean(Height)))
abline(spruce.lm)

plot(spruce$Height~spruce$BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), data=spruce)
with(spruce,abline(h=mean(Height)))
with(spruce, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))

RSS=with(spruce,sum((Height-yhat)^2))
RSS

MSS=with(spruce,sum((yhat-mean(Height))^2))
MSS

TSS=with(spruce,sum((Height-mean(Height))^2))
TSS

MSS/TSS

TSS
MSS + RSS
