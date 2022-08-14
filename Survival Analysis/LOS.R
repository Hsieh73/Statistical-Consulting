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

plot(time,rowSums(LOS[,4:6]),xlab = "LOS",ylab = "�v��") #�Q�ݪv���O�_�v�T���|�Ѽ�(�ݤ��ӥX��)
plot(time,LOS[,7],type = 'h',xlab = "LOS",ylab = "FIM_F")
plot(time,rowSums(LOS[,c(7,9)]),type = 'h',xlab = "LOS",ylab = "�J�|����") #�i�H�ݥX�J�|���Ʒ|�v�T���|�Ѽ�
plot(time,rowSums(LOS[,c(12,13)]),xlab = "LOS",ylab = "�C�ʯf") #�Pı�C�ʯe�f�v�T�]���j
table(rowSums(LOS[,12:13]))

plot(time,LOS$ht,xlab = "LOS",ylab = "������") #�Pı�C�ʯe�f�v�T�]���j
plot(time,LOS$dm,xlab = "LOS",ylab = "�}���f") #�Pı�C�ʯe�f�v�T�]���j
table(LOS$sex)   #�k��335�ӡA�k��209

M = LOS[LOS$sex==1,]
FM = LOS[LOS$sex==0,]
table(LOS[,12:13])

sum(M$ht)  #�k�ʦ�����������(���k��)
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
y7 = LOS[381:544,]#6.70���H�ܦh
time.mean = c(mean(y2$time[y2$sex==1]),mean(y3$time[y3$sex==1]),mean(y4$time[y4$sex==1]),mean(y5$time[y5$sex==1]),mean(y6$time[y6$sex==1]),mean(y7$time[y7$sex==1]))
plot(time.mean,type='h',xlab = "�~�ּh(�k��)",ylab="�������|�ɶ�",xaxt="n")
axis(1,seq(1,6,1),labels=c("20~29��","30~39��","40~49��","50~59��","60~69��","70~79��"))

time.mean = c(mean(y2$time[y2$sex==0]),mean(y3$time[y3$sex==0]),mean(y4$time[y4$sex==0]),mean(y5$time[y5$sex==0]),mean(y6$time[y6$sex==0]),mean(y7$time[y7$sex==0]))
plot(time.mean,type='h',xlab = "�~�ּh(�k��)",ylab="�������|�ɶ�",xaxt="n")#�k�ʪ����|�ɶ��H�~�����W�A�k�ʻ���(���k��20-30;30-40��ƺ��֪�)
axis(1,seq(1,6,1),labels=c("20~29��","30~39��","40~49��","50~59��","60~69��","70~79��"))

s.h = as.numeric(LOS$BAR_F>35)
s.l = as.numeric(LOS$BAR_F<=35)#�C���H��
table(s.l) #490�H�b�J�|�ɤw�F�i�ӽЬ��@���A

hist(time[s.l==1],col = 'pink',main = 'Histogram of time',xlab = 'stay time',ylab = '�ֿn�H��')
hist(time[s.l==0],col = 'light blue',add = T )
legend('topright',legend = c('BAR<=35','BAR>35'),col = c('pink','light blue'),bty="n",lwd = 1)#���R����

library(survival)
test = sort(sample(1:544,144,replace = F))
LOS$time = Surv(LOS$time)
LOS.test = LOS[test,]
LOS.train = LOS[-test,]
summary(survfit(LOS$time~1),censor = F)

KM = survfit(time ~ 1 , type = 'kaplan-meier',data = LOS)
KM1 = summary(KM,censor = F)
plot(KM$time,KM1$surv,type = 's',xlab="time",ylab="surv") #Kaplan-Meier ���u
abline(h=0.5,lty=2,col="red")
survfit(LOS$time~1)
plot(survfit(LOS$time~1))
library(ggfortify)
km_fit<-survfit(LOS$time~1)
autoplot(km_fit)

#�Ѽƪk (���]�A�qWeibull)
fitWeib = survreg(time~AGE+FIM_F+FIM_S+BAR_F+BAR_S+PT+OT+ST+sex+ht+dm,dist = 'weibull',data = LOS.train)
summary(fitWeib)

betaHat = coef(fitWeib) / fitWeib$scale  #COX���ѼƦ��p
betaHat

#����A�qEXP
fitExp = survreg(time~AGE+FIM_F+FIM_S+BAR_F+BAR_S+PT+OT+ST+sex+ht+dm,dist = 'exponential',data = LOS.train)
anova(fitExp,fitWeib)  #�o�̷Pı�ܪ���(Weibull����n?)

fitR = survreg(time~FIM_F+FIM_S+BAR_F,dist = 'weibull',data = LOS.train)#�ڥu����۪�
anova(fitR,fitWeib)   #fitR����n?�٬O�n��Log-lik�j��

#�ͦs���u(���ղ�)
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
#�o���u�ڦ��I���T�w���e(�ڵe��������W�ݱo���Ӥ@�ˡA���Pı�n���Z�n��)
legend('topright',legend = c('predict','true'),col = c('black','red'),bty="n",lwd = 2)
