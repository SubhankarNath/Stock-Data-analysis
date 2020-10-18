library(pdfetch)
library(car)
r<-pdfetch_YAHOO("^BSESN",from =as.Date("2007-01-02"),to=as.Date("2018-04-06") )
r<-read.csv("^BSESN.csv")
class(r)
df<-data.frame(r); f<-complete.cases(df)
df<-df[f,];dim(df)
head(df)
par(mfrow=c(1,1)); plot(df[1:100,])
dft=df[,-c(5:6)]
dft<-data.frame(dft,dft$X.BSESN.open^2,dft$X.BSESN.high^2,dft$X.BSESN.low^2,
                dft$X.BSESN.close^2,dft$X.BSESN.open*dft$X.BSESN.high,
                dft$X.BSESN.high*dft$X.BSESN.low,dft$X.BSESN.low*dft$X.BSESN.close)
##===========================================================================
df1=dft[1:2000,]; par(mfrow=c(2,2))
plot(df1$X.BSESN.open,type = 'l',main = 'Daily open price')
plot(df1$X.BSESN.high,type='l',main = "Daily high price")
plot(df1$X.BSESN.low,type='l',main = "Daily low price")
plot(df1$X.BSESN.close,type = 'l',main="Daily closing price")
volatility=(df1$X.BSESN.close - df1$X.BSESN.open)^2/df1$X.BSESN.close
head(volatility)
range(volatility)
plot(volatility,type = "l",main = "Volatility of stock data",col='red')
hist(volatility,col="blue")
par(mfrow=c(1,1))
prob<-function(z){
  s=1/(1+exp(-z))
  return(s)
}
probability=prob(volatility)
head(probability)
range(probability)
plot(probability,ylab="Probability",xlab = "",pch=20)
abline(0.6,0,col='blue')
par(mfrow=c(2,2))
hist(probability,col="blue")
boxplot(probability,ylab="Probability",main="Boxplot of probabilities")
p<-data.frame(probability,df1)
v=NULL
for(i in 1:length(probability)){
    if(probability[i]>0.6)
      v[i]=1 ##  volatile
    else
     v[i]=0 ## non-volatile
}
head(v,10)
df2=data.frame(v,probability)
df2$v=as.factor(df2$v)
head(df2)
par(mfrow=c(1,1))
plot(df2$probability,col=c("red","blue")[df2$v],pch=20, ylim = c(0.4,1),
     ylab="Probability",xlab="",main = "Classification of volatility")
legend(1500,0.485,legend = c("Volatile","Non-volatile"), cex=0.9,pch=20,
       col=c("blue","red"))
df3=data.frame(df2$v,df1)
##===========================================
f1score<-function(a,b){
  tp=length(intersect(a,b))  
  fp=length(b)-tp
  fn=length(a)-tp
  precision= tp/(tp+fp)
  recall=tp/(tp+fn)
  f1=2*precision*recall/(precision+recall)
  return(f1)      ## F1 score           
}
v2<-NULL
analysis<-function(pre,th=0){
  for(i in 1:length(pre)){
    if(pre[i]>th)
      v2[i]=1 ## volatile
    else
      v2[i]=0 ## non-volatile
    
  }
  match=length(which((v2)==df3[1501:2000,1]))
  accuracy=match*100/length(v2)
  error<-(length(v2)-match)/length(v2)
  a=which(df3[1501:2000,1]==1) ## assumed 1
  b=which(v2==1)   ## predicted 1
  f=f1score(a,b)
  d<-data.frame(th,accuracy,error,f)
  colnames(d)<-c("Threshold value", "Accuracy(%)", "Cross validation Error", 
                 "F1 score")
  return(d)
}
##===========================================================================
## Model 1 :
df4=df3[1:1500,]
df5=df4[,c(1,2,3,4,5)]
model1<-glm(df5$df2.v~., data = df5, family = binomial)
summary(model1)
res=resid(model1,type = "deviance")
par(mfrow=c(2,2))
plot(res,pch=20,col="blue",main = "Residuals",ylab = "residuals")
abline(0,0)
hist(res,col="skyblue",main="Histogram of Residuals")
qqPlot(res,main = "Q-Q normal plot",ylab='residuals')
c=cooks.distance(model1)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data<-df3[1501:2000,2:5]
prediction<-predict.glm(model1,pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.3),analysis(prediction,0.34),
                    analysis(prediction,0.35),analysis(prediction,0.36),
                    analysis(prediction,0.40),analysis(prediction,0.41),
                    analysis(prediction,0.42), analysis(prediction,0.43),
                    analysis(prediction,0.44), analysis(prediction,0.45),
                    analysis(prediction,0.48),analysis(prediction,0.5),
                    analysis(prediction,0.55), analysis(prediction,0.6)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model1 will work best with threshold value 0.41 as the corresponding F1 score
    is higher.")
##====================================================
## Model 2:
df4=df3[1:1500,]
df5=df4[,c(1,2,3,4,5,6,7,8,9)]
model2=glm(df5$df2.v~ ., data=df5,family = binomial)
summary(model2)
res=resid(model2,type = "deviance")
par(mfrow=c(2,2))
plot(res,col="blue",pch=20,main="Residuals")
abline(0,0)
hist(res,col="red",main='Histogram of Residuals',xlab='residuals')
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model2)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data<-df3[1501:2000,2:9]
prediction<-predict.glm(model2,pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.3),analysis(prediction,0.4),
                    analysis(prediction,0.45), analysis(prediction,0.46),
                    analysis(prediction,0.47),analysis(prediction,0.48),
                    analysis(prediction,0.49), analysis(prediction,0.5),
                    analysis(prediction,0.55), analysis(prediction,0.6),
                    analysis(prediction,0.62),analysis(prediction,0.63),
                    analysis(prediction,0.65)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model2 will work best with threshold value 0.49")
##===============================================
## Model 3:
df4=df3[1:1500,c(1,2,5)]
model3=glm(df4$df2.v~., data=df4, family = binomial)
summary(model3)
res=resid(model3,type = "deviance")
par(mfrow=c(2,2))
plot(res,pch=20,col="blue",main="Residuals")
abline(0,0)
hist(res,col="green",main="Histogram of residuals")
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model3)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data=df3[1501:2000,-1]
prediction<-predict.glm(model3,newdata = pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.01), analysis(prediction,0.05),
                    analysis(prediction,0.1),analysis(prediction,0.4),
                    analysis(prediction,0.45), analysis(prediction,0.47),
                    analysis(prediction,0.5),analysis(prediction,0.55),
                    analysis(prediction,0.6),analysis(prediction,0.7)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So, model3 can work better with threshold value 0.1 but this is not a good model
    since accuracy is very low.")
##======================================================
## Model 4:
df4=df3[1:1500,]
df5=df4[,c(1,6,7,8,9)]
model4=glm(df5$df2.v~., data=df5, family = binomial)
summary(model4)
res=resid(model4,type = "deviance")
par(mfrow=c(2,2))
plot(res,pch=20,col="red",main='Residuals')
abline(0,0)
hist(res,col="blue")
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model4)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data=df3[1501:2000,-1]
prediction=predict.glm(model4,newdata = pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.3),analysis(prediction,0.31),
                    analysis(prediction,0.32),analysis(prediction,0.33),
                    analysis(prediction,0.3318), analysis(prediction,0.4),
                    analysis(prediction,0.45),analysis(prediction,0.5)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model4 will work best with the threshold value 0.3318")
##======================================================
## Model 5:
df4=df3[1:1500,c(1,2,3)]
model5=glm(df4$df2.v~. , data=df4, family = binomial)
summary(model5)
res=resid(model5,type = "deviance")
par(mfrow=c(2,2))
plot(res,main='Residuals',pch=20,col='blue')
abline(0,0)
hist(res,main="Histogram of residuals",col = "orange")
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model5)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data=df3[1501:2000,-1]
prediction=predict.glm(model5,newdata = pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.3),
                    analysis(prediction,0.4), analysis(prediction,0.5),
                    analysis(prediction,0.55), analysis(prediction,0.56),
                    analysis(prediction,0.57),analysis(prediction,0.58),
                    analysis(prediction,0.59),analysis(prediction,0.6),
                    analysis(prediction,0.65),analysis(prediction,0.7)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model5 can work better with the threshold value 0.59 but it is not a good
    model.")
##======================================================
## Model 6:
df4=df3[1:1500,c(1,2,3,4)]
model6=glm(df4$df2.v~ ., data=df4, family = binomial)
summary(model6)
res<-resid(model6, type = "deviance")
par(mfrow=c(2,2))
plot(res,pch=20,col="orange",main="Residuals")
abline(0,0,col='red')
hist(res,main="Histogram of residuals ",col='deepskyblue')
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model6)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data=df3[1501:2000,-1]
prediction=predict.glm(model6,newdata = pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.35),
                    analysis(prediction,0.4), analysis(prediction,0.44),
                    analysis(prediction,0.45), analysis(prediction,0.46),
                    analysis(prediction,0.47), analysis(prediction,0.5),
                    analysis(prediction,0.55),analysis(prediction,0.6)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model6 can work better with the threshold value 0.44")
##======================================================
## Model 7:
df4=df3[1:1500,c(1,2,3,4,5,6)]
model7=glm(df4$df2.v~ ., data=df4, family = binomial)
summary(model7)
res=resid(model7,type = "deviance")
par(mfrow=c(2,2))
plot(res,main='Residuals',pch=20,col='blue')
abline(0,0)
hist(res,main = "Hidstogram of residuals",col='yellow')
qqPlot(res,main="Q-Q normal plot",ylab='residuals')
c=cooks.distance(model7)
plot(c,type = 'h',col='red',main = "Cook's distance to identify outliers",
     ylab="Cook's distance")
pred.data=df3[1501:2000,-1]
prediction=predict.glm(model7,newdata = pred.data,type = "response")
head(prediction)
D<-data.frame(rbind(analysis(prediction,0.35),
                    analysis(prediction,0.4),analysis(prediction,0.44),
                    analysis(prediction,0.45),analysis(prediction,0.46),
                    analysis(prediction,0.47),analysis(prediction,0.5),
                    analysis(prediction,0.55),analysis(prediction,0.56),
                    analysis(prediction,0.57),analysis(prediction,0.58),
                    analysis(prediction,0.59), analysis(prediction,0.6),
                    analysis(prediction,0.62)))
D
par(mfrow=c(1,1))
plot(D$Threshold.value,D$Cross.validation.Error, type='l',xlab="Threshold",
     ylab="Error",main="Threshold vs. Cross validation error")
cat("So model7 can work better with the threshold value 0.46")
#===============================================================================
 ## Prediction with relatively good models 
a=dft[2001:nrow(dft),]
pred=predict.glm(model1,newdata = a,type = "response") ## Prediction with model1
head(pred)
stock=NULL
volt<-function(th=0,h){
  for (i in 1: length(h))
    
  if(h[i]>th)
    stock[i]=1  ## Volatile
  else
    stock[i]=0  ## Non-volatile
  return(stock)
}
head(volt(0.41,pred),50)
cat('1 implies stock is volatile and 0 implies stock is non-volatile on that day.')
pred=predict.glm(model2,newdata = a,type = "response") ## Prediction with model2
head(pred)
head(volt(0.49,pred),50)
cat('1 implies stock is volatile and 0 implies stock is non-volatile on that day.')
pred=predict.glm(model4,newdata = a,type = "response") ## Prediction with model4
head(pred)
head(volt(0.3318,pred),50)
cat('1 implies stock is volatile and 0 implies stock is non-volatile on that day.')
pred=predict.glm(model6,newdata = a,type = "response") ## Prediction with model6
head(pred)
head(volt(0.44,pred),35)
cat('1 implies stock is volatile and 0 implies stock is non-volatile on that day.')
pred=predict.glm(model7,newdata = a,type = "response") ## Prediction with model7
head(pred)
head(volt(0.46,pred),35)
cat('1 implies stock is volatile and 0 implies stock is non-volatile on that day.')


#==================================================================================
sum=0;
for (i in 1: length(v)){
  if(abs(v[i]-volt(0.46, pred)[i])==0) sum=sum+1;
}
sum
accuracy=sum/length(v)
accuracy

