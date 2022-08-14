rm(list = ls())
library(readxl)
A1 = read_excel('豆象A1_A9data.xls',sheet = 1)
A2 = read_excel('豆象A1_A9data.xls',sheet = 2)
A3 = read_excel('豆象A1_A9data.xls',sheet = 3)
A4 = read_excel('豆象A1_A9data.xls',sheet = 4)
A5 = read_excel('豆象A1_A9data.xls',sheet = 5)
A6 = read_excel('豆象A1_A9data.xls',sheet = 6)
A7 = read_excel('豆象A1_A9data.xls',sheet = 7)
A9 = read_excel('豆象A1_A9data.xls',sheet = 8)
bean_index = function(A){
  y = rep(0,length(A))
  for(i in 1:length(A)){
     if(A[i] == 11||A[i] ==15||A[i] ==51||A[i] ==55){
       y[i]=1 }else if(A[i]==22||A[i] ==23||A[i] ==24||A[i] ==32||A[i] ==33||A[i] ==34||A[i] ==42||A[i] ==43||A[i] ==44){
       y[i]=3} else{
         y[i]=2}
  }
  return(y)
}
a1 = as.data.frame(matrix(c(A1$time,A1$`bout length`,diff(A1$`egg series`),1,A1$`bean visit no.`,A1$`eggs on bean`,bean_index(A1$`bean no.`)),nrow(A1)))
a2 = as.data.frame(matrix(c(A2$time,A2$`bout length`,diff(A2$`egg series`),1,A2$`bean visit no.`,A2$`eggs on bean`,bean_index(A2$`bean no.`)),nrow(A2)))
a3 = as.data.frame(matrix(c(A3$time[1:88],A3$`bout length`[1:88],diff(as.numeric(A3$`egg series`[1:88])),1,A3$`bean visit no.`[1:88],A3$`eggs on bean`[1:88],bean_index(A3$`bean no.`[1:88])),88))
a4 = as.data.frame(matrix(c(A4$Time[1:196],A4$`bout length`[1:196],diff(as.numeric(A4$`egg series`[1:196])),1,A4$`bean visit no.`[1:196],A4$`eggs on bean`[1:196],bean_index(A4$`bean no.`[1:196])),196))
a5 = as.data.frame(matrix(c(A5$time[1:102],A5$`bout length`[1:102],diff(as.numeric(A5$`egg series`[1:102])),1,A5$`bean visit no.`[1:102],A5$`eggs on bean`[1:102],bean_index(A5$`bean no.`[1:102])),102))
a6 = as.data.frame(matrix(c(A6$Time[1:95],A6$`bout length`[1:95],diff(as.numeric(A6$`egg series`[1:95])),0,A6$`bean visit no.`[1:95],A6$`eggs on bean`[1:95],bean_index(A6$`bean no.`[1:95])),95))
a7 = as.data.frame(matrix(c(A7$time[1:85],A7$`bout length`[1:85],diff(as.numeric(A7$`egg series`[1:85])),1,A7$`bean visit no.`[1:85],A7$`eggs on bean`[1:85],bean_index(A7$`bean no.`[1:85])),85))
a9 = as.data.frame(matrix(c(A9$Time,A9$`bout length`,diff(A9$`egg series`),1,A9$`bean visit no.`,A9$`eggs on bean`,bean_index(A9$`bean no.`)),nrow(A9)))
colnames(a1)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a2)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a3)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a4)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a5)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a6)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a7)=c('time','bout.time','ovp','visit.no','egg on bean','position')
colnames(a9)=c('time','bout.time','ovp','visit.no','egg on bean','position')

n = c(nrow(a1),nrow(a2),nrow(a3),nrow(a4),nrow(a5),nrow(a6),nrow(a7),nrow(a9))

#上三種豆的次數
visit = matrix(c(table(a1[,6]),
                 table(a2[,6]),
                 table(a3[,6]),
                 table(a4[,6]),
                 table(a5[,6]),
                 table(a6[,6]),
                 table(a7[,6]),
                 table(a9[,6])),3)
ratio.visi = t(t(visit)/n)

#每隻蟲上去3種豆產卵的次數
bout = matrix(c(0,table(a1[a1[,3]==1,6]),
                table(a2[a2[,3]==1,6]),
                table(a3[a3[,3]==1,6]),
                table(a4[a4[,3]==1,6]),
                table(a5[a5[,3]==1,6]),
                table(a6[a6[,3]==1,6]),
                table(a7[a7[,3]==1,6]),
                table(a9[a9[,3]==1,6])),3)
#卵中三種豆的比例
ratio.bout = t(t(bout)/c(sum(a1[,3]),sum(a2[,3]),sum(a3[,3]),sum(a4[,3]),sum(a5[,3]),sum(a6[,3]),sum(a7[,3]),sum(a9[,3])))

#在三種豆上產卵的機率
p.bout = round(bout/visit,3)

#每次下蛋的時間
ovp.time1 = cbind(a1[a1[,3]==1,],diff_t = c(a1[a1[,3]==1,1][1],diff(a1[a1[,3]==1,1])))
ovp.time2 = cbind(a2[a2[,3]==1,],diff_t = c(a2[a2[,3]==1,1][1],diff(a2[a2[,3]==1,1])))
ovp.time3 = cbind(a3[a3[,3]==1,],diff_t = c(a3[a3[,3]==1,1][1],diff(a3[a3[,3]==1,1])))
ovp.time4 = cbind(a4[a4[,3]==1,],diff_t = c(a4[a4[,3]==1,1][1],diff(a4[a4[,3]==1,1])))
ovp.time5 = cbind(a5[a5[,3]==1,],diff_t = c(a5[a5[,3]==1,1][1],diff(a5[a5[,3]==1,1])))
ovp.time6 = cbind(a6[a6[,3]==1,],diff_t = c(a6[a6[,3]==1,1][1],diff(a6[a6[,3]==1,1])))
ovp.time7 = cbind(a7[a7[,3]==1,],diff_t = c(a7[a7[,3]==1,1][1],diff(a7[a7[,3]==1,1])))
ovp.time9 = cbind(a9[a9[,3]==1,],diff_t = c(a9[a9[,3]==1,1][1],diff(a9[a9[,3]==1,1])))


visit.no1 = matrix(c(trunc(A1$`bean no.`/10),A1$`bean no.`%%10),nrow(A1))
visit.no2 = matrix(c(trunc(A2$`bean no.`/10),A2$`bean no.`%%10),nrow(A1))
visit.no3 = matrix(c(trunc(A3$`bean no.`[1:88]/10),A3$`bean no.`[1:88]%%10),88)
visit.no4 = matrix(c(trunc(A4$`bean no.`[1:196]/10),A4$`bean no.`[1:196]%%10),196)
visit.no5 = matrix(c(trunc(A5$`bean no.`[1:102]/10),A5$`bean no.`[1:102]%%10),102)
visit.no6 = matrix(c(trunc(as.numeric(A6$`bean no.`[1:73])/10),2,trunc(as.numeric(A6$`bean no.`[75:95])/10),as.numeric(A6$`bean no.`[1:73])%%10,3,as.numeric(A6$`bean no.`[75:95])%%10),95)
visit.no7 = matrix(c(trunc(A7$`bean no.`[1:85]/10),A7$`bean no.`[1:85]%%10),85)
visit.no9 = matrix(c(trunc(A9$`bean no.`/10),A9$`bean no.`%%10),nrow(A9))


plot(visit.no1,type = 'l')
par(pty = "s")
points(visit.no1,pch= 1)

#沒分三種豆子logstic
data = rbind(a1,a2,a3,a4,a5,a6,a7,a9)[,2:6]
head(data)
colnames(data)<-c("bout.time","ovp" ,"bean.visit.no.","eggs.on.bean","position")
t = sample(1:nrow(data),round(nrow(data)/5))
test = data[t,]
train = data[-t,]
p = sum(train$ovp)/nrow(train)
model = glm(ovp~bout.time+bean.visit.no.+eggs.on.bean,data = train,family = binomial(logit))
summary(model)
result_train = predict(model,newdata = train,type = 'response')
result_train <- ifelse(result_train > p, 1, 0)
cm = table(train$ovp, result_train, dnn = c("實際", "預測"))
cm
result_train[result_train==1]
p

##test
result_test = predict(model,newdata = test,type = 'response')
result_test <- ifelse(result_test > p, 1, 0)
cm = table(test$ovp, result_test, dnn = c("實際", "預測"))
cm
result_test[result_test==1]

library(caret)
confusionMatrix(data=as.factor(result_test), reference=as.factor(test$ovp))

library("pROC")
test_prob = predict(model, newdata = test, type = "response")
par(pty = "s") #指定畫布繪製成正方形的大小
test_roc = roc(test$ovp ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE)



#分三種豆子logistic
model1 = glm(ovp~.,data = train,family = binomial(logit))
summary(model1)
##train
result_train = predict(model1,newdata = train,type = 'response')
result_train <- ifelse(result_train > p, 1, 0)
cm = table(train$ovp, result_train, dnn = c("實際", "預測"))
cm
result_train[result_train==1]
p
##test
result_test = predict(model1,newdata = test,type = 'response')
result_test <- ifelse(result_test > p, 1, 0)
cm = table(test$ovp, result_test, dnn = c("實際", "預測"))
cm
result_test[result_test==1]

library(caret)
confusionMatrix(data=as.factor(result_test), reference=as.factor(test$ovp))

library("pROC")
test_prob = predict(model1, newdata = test, type = "response")
par(pty = "s") #指定畫布繪製成正方形的大小
test_roc = roc(test$ovp ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE)

##移除不顯著變數
model2 = glm(ovp~bout.time+eggs.on.bean,data = train,family = binomial(logit))
summary(model2)
result_train = predict(model2,newdata = train,type = 'response')
result_train <- ifelse(result_train > p, 1, 0)
cm = table(train$ovp, result_train, dnn = c("實際", "預測"))
cm
##test
result_test = predict(model2,newdata = test,type = 'response')
result_test <- ifelse(result_test > p, 1, 0)
cm = table(test$ovp, result_test, dnn = c("實際", "預測"))
cm
result_test[result_test==1]

library(caret)
confusionMatrix(data=as.factor(result_test), reference=as.factor(test$ovp))

library("pROC")
test_prob = predict(model2, newdata = test, type = "response")
par(pty = "s") #指定畫布繪製成正方形的大小
test_roc = roc(test$ovp ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE)


##存活
library(survival)
a1$bout.time = Surv(a1$bout.time)
a1.train = a1[-test,]
summary(survfit(a1$bout.time~1),censor = F)
KM = survfit(bout.time ~ 1 , type = 'kaplan-meier',data = a1)
KM1 = summary(KM,censor = F)
plot(KM$time,KM1$surv,type = 's') #Kaplan-Meier 曲線

#參數法 (假設服從Weibull)
fitWeib = survreg(bout.time~ovp+position,dist = 'weibull',data = a1.train)
summary(fitWeib)
betaHat = coef(fitWeib) / fitWeib$scale  #COX的參數估計
betaHat

#比較服從EXP
fitExp = survreg(bout.time~ovp+position,dist = 'exponential',data = a1.train)
anova(fitExp,fitWeib)  #這裡感覺很長期(Weibull比較好?)

fitR = survreg(bout.time~ovp+position,dist = 'weibull',data = a1.train)#我只選顯著的
anova(fitR,fitWeib)   #fitR比較好?還是要選Log-lik大的

#生存曲線(測試組)
p = (1:(nrow(a1)/5))/(nrow(a1)/5)

FWeib = predict(fitR,newdata = a1.test,se = T)
A = cbind(a1.test$time,FWeib$fit) #
FWeib$fit = sort(FWeib$fit)
FWeib$se.fit = sort(FWeib$se.fit)

plot(FWeib$fit, 1-p,type="l", main=expression(paste("Weibull-Fit ", hat(S)(t), " mit SE")),
     xlab="t", ylab="Survival", lty=c(1, 2, 2), lwd=2, col="blue")
lines(FWeib$fit - 2*FWeib$se.fit,1-p,lty = 2,col = 'blue',lwd = 2)
lines(FWeib$fit + 2*FWeib$se.fit,1-p,lty = 2,col = 'blue',lwd = 2)
lines(sort(as.numeric(a1.test$bout.time)[1:19]),1-p, col='red', lwd=2)
legend('topright',legend = c('predict','true'),col = c('blue','red'),bty="n",lwd = 2)

model = coxph(bout.time~ovp+position,data = a1)
summary(model)
model1 = coxph(bout.time~position,data = a1)
summary(model1)
anova(model1,model) 
CPH = survfit(model1)
CPH.test = survfit(model1,newdata = a1.test)
plot(CPH, main=expression(paste("Cox PH-estimate ", hat(S)(t), " with CI")),
     xlab="t", ylab="Survival", lwd=2)
lines(sort(as.numeric(a1.test$bout.time))[20:38],1-p, col='red', lwd=2)
legend('topright',legend = c('predict','true'),col = c('black','red'),bty="n",lwd = 2)




