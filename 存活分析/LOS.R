LOS = read.csv('LOS.csv',header = T)[,1:15]
head(LOS)
LOS[,14] = as.numeric(as.Date(LOS[,14]))
LOS[,15] = as.numeric(as.Date(LOS[,15]))
time = LOS[,15]-LOS[,14]
LOS = cbind(LOS,time)
plot(as.numeric(time),type = 'h',xlab = "ID",ylab = "LOS")

boxplot(LOS[,-c(1,3,14,15)])
pairs(LOS[,-c(1,3,14,15)])

cor(LOS[,7:10])
cor(LOS$AGE,LOS$FIM_F)
cor(LOS$AGE,LOS$BAR_F)
cor(LOS$AGE,LOS$FIM_S)
cor(LOS$AGE,LOS$BAR_S)
cor(LOS$AGE,as.numeric(time))
cor(LOS$AGE,LOS$FIM_S-LOS$FIM_F)
cor(as.numeric(time),LOS$FIM_F)
cor(as.numeric(time),LOS$BAR_F)

plot(time,rowSums(LOS[,4:6]),xlab = "LOS",ylab = "治療") #想看治療是否影響住院天數(看不太出來)
plot(time,LOS[,7],type = 'h',xlab = "LOS",ylab = "FIM_F")
plot(time,rowSums(LOS[,c(7,9)]),type = 'h',xlab = "LOS",ylab = "入院分數") #可以看出入院分數會影響住院天數
plot(time,rowSums(LOS[,c(12,13)]),xlab = "LOS",ylab = "慢性病") #感覺慢性疾病影響也不大
table(rowSums(LOS[,12:13]))

plot(time,LOS$ht,xlab = "LOS",ylab = "高血壓") #感覺慢性疾病影響也不大
plot(time,LOS$dm,xlab = "LOS",ylab = "糖尿病") #感覺慢性疾病影響也不大
table(LOS$sex)   #男性335個，女性209

M = LOS[LOS$sex==1,]
FM = LOS[LOS$sex==0,]
table(LOS[,12:13])

sum(M$ht)  #男性有高血壓偏高(較女性)
sum(FM$ht)
sum(M$dm)
sum(FM$dm)

plot(M$ID,time[LOS$sex==1],col = 2,xlim = c(0,544),ylim = c(0,130),xlab = "ID",ylab = "LOS")
par(new = T)
plot(FM$ID,time[LOS$sex ==0],col =4,xlim = c(0,544),ylim = c(0,130),xlab = "",ylab = "")
legend('topright',legend = c('Male','Female'),col = c(2,4),lwd = 2,lty = c(0,0),pch=c(1,1))


y2 = LOS[1:5,]
y3 = LOS[6:37,]
y4 = LOS[38:105,]
y5 = LOS[106:204,]
y6 = LOS[205:380,]
y7 = LOS[381:544,]#6.70歲人很多
time.mean = c(mean(y2$time[y2$sex==1]),mean(y3$time[y3$sex==1]),mean(y4$time[y4$sex==1]),mean(y5$time[y5$sex==1]),mean(y6$time[y6$sex==1]),mean(y7$time[y7$sex==1]))
plot(time.mean,type='h',xlab = "年齡層(男性)",ylab="平均住院時間",xaxt="n")
axis(1,seq(1,6,1),labels=c("20~29歲","30~39歲","40~49歲","50~59歲","60~69歲","70~79歲"))

time.mean = c(mean(y2$time[y2$sex==0]),mean(y3$time[y3$sex==0]),mean(y4$time[y4$sex==0]),mean(y5$time[y5$sex==0]),mean(y6$time[y6$sex==0]),mean(y7$time[y7$sex==0]))
plot(time.mean,type='h',xlab = "年齡層(女性)",ylab="平均住院時間",xaxt="n")#男性的住院時間隨年紀遞增，女性遞減(但女性20-30;30-40資料滿少的)
axis(1,seq(1,6,1),labels=c("20~29歲","30~39歲","40~49歲","50~59歲","60~69歲","70~79歲"))

s.h = as.numeric(LOS$BAR_F>35)
s.l = as.numeric(LOS$BAR_F<=35)#低分人類
table(s.l) #490人在入院時已達可申請看護狀態

hist(time[s.l==1],col = 'pink',main = 'Histogram of time',xlab = 'stay time',ylab = '累積人數')
hist(time[s.l==0],col = 'light blue',add = T )
legend('topright',legend = c('BAR<=35','BAR>35'),col = c('pink','light blue'),bty="n",lwd = 1)#美麗的圖

library(survival)
test = sort(sample(1:544,144,replace = F))
LOS$time = Surv(LOS$time)
LOS.test = LOS[test,]
LOS.train = LOS[-test,]
summary(survfit(LOS$time~1),censor = F)

KM = survfit(time ~ 1 , type = 'kaplan-meier',data = LOS)
KM1 = summary(KM,censor = F)
plot(KM$time,KM1$surv,type = 's',xlab="time",ylab="surv") #Kaplan-Meier 曲線
abline(h=0.5,lty=2,col="red")
survfit(LOS$time~1)
plot(survfit(LOS$time~1))
library(ggfortify)
km_fit<-survfit(LOS$time~1)
autoplot(km_fit)

#參數法 (假設服從Weibull)
fitWeib = survreg(time~AGE+FIM_F+FIM_S+BAR_F+BAR_S+PT+OT+ST+sex+ht+dm,dist = 'weibull',data = LOS.train)
summary(fitWeib)

betaHat = coef(fitWeib) / fitWeib$scale  #COX的參數估計
betaHat

#比較服從EXP
fitExp = survreg(time~AGE+FIM_F+FIM_S+BAR_F+BAR_S+PT+OT+ST+sex+ht+dm,dist = 'exponential',data = LOS.train)
anova(fitExp,fitWeib)  #這裡感覺很長期(Weibull比較好?)

fitR = survreg(time~FIM_F+FIM_S+BAR_F,dist = 'weibull',data = LOS.train)#我只選顯著的
anova(fitR,fitWeib)   #fitR比較好?還是要選Log-lik大的

#生存曲線(測試組)
p = (1:144)/144

FWeib = predict(fitR,newdata = LOS.test,se = T)
A = cbind(LOS.test$time,FWeib$fit) #
FWeib$fit = sort(FWeib$fit)
FWeib$se.fit = sort(FWeib$se.fit)

plot(FWeib$fit, 1-p,type="l", main=expression(paste("Weibull-Fit ", hat(S)(t), " mit SE")),
     xlab="t", ylab="Survival", lty=c(1, 2, 2), lwd=2, col="blue")
lines(FWeib$fit - 2*FWeib$se.fit,1-p,lty = 2,col = 'blue',lwd = 2)
lines(FWeib$fit + 2*FWeib$se.fit,1-p,lty = 2,col = 'blue',lwd = 2)
lines(sort(as.numeric(LOS.test$time))[145:288],1-p, col='red', lwd=2)
legend('topright',legend = c('predict','true'),col = c('blue','red'),bty="n",lwd = 2)

library(survival)
#Cox reg
model = coxph(time~AGE+FIM_F+FIM_S+BAR_F+BAR_S+PT+OT+ST+sex+ht+dm,data = LOS.train)
summary(model)

model1 = coxph(time~FIM_F+FIM_S+BAR_F,data = LOS.train)
summary(model1)

cox_fit <- survfit(model)
cox_fit1 <- survfit(model1)
plot(cox_fit, main = "cph model", xlab="Days")
plot(cox_fit1, main = "cph model", xlab="Days")


anova(model1,model)

CPH = survfit(model1)
CPH.test = survfit(model1,newdata = LOS.test)
plot(CPH, main=expression(paste("Cox PH-estimate ", hat(S)(t), " with CI")),
     xlab="t", ylab="Survival", lwd=2)
lines(sort(as.numeric(LOS.test$time))[145:288],1-p, col='red', lwd=2)
#這條線我有點不確定怎麼畫(我畫的跟網路上看得不太一樣，但感覺好像蠻好的)
legend('topright',legend = c('predict','true'),col = c('black','red'),bty="n",lwd = 2)

