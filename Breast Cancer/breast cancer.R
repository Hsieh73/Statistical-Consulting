library(randomForest)
library(rpart)       # performing regression trees
library(rpart.plot)
library(rattle)
library(caret)
library(pROC)
library(ROCit)

#1為惡性，2為良性，3為健康
data = read.csv('serum_with element name.csv',header = T)
data[,1] = as.factor(data[,1])

#資料預處理
data[,-1]=scale(data[,-1])

#拆分樣本，1為訓練組，2為測試組
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
trainset=data[which(ind==1),]
testset=data[which(ind==2),]

#pca
prin_comp <- prcomp(trainset[,-1], scale. = T)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#decision tree
btrainset = trainset[sample(length(trainset$D1),replace = T),] #用訓練組訓練模型
dtree = rpart(D1 ~ .,data = btrainset, method = "class")
rpart.plot(dtree)
fancyRpartPlot(dtree, caption="")
dtree$variable.importance

pred <- predict(dtree, newdata=testset, type="class")
confus.matrix<-table(real=testset$D1, predict=pred)
acc<-sum(diag(confus.matrix))/sum(confus.matrix)
acc

#random forest
rf=randomForest(D1 ~ ., data=trainset,proximity=TRUE)
which.min(rf$err.rate[,1])
confusionMatrix(rf$predicted, trainset$D1)

#fit
rfpred<- predict(rf, testset)
confusionMatrix(rfpred, testset$D1)
MDSplot(rf, trainset$D1)

#ROC
ran_roc <- roc(testset$D1,as.numeric(rfpred))
ran = multiclass.roc(testset$D1,as.numeric(rfpred))
plot(ran$rocs[[1]], print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),type='bar', max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
plot(ran$rocs[[2]], print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),type='bar', max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
plot(ran$rocs[[3]], print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),type='bar', max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

bdata<-data
bdata[which(bdata$D1==3),1]=2
ind <- sample(2, nrow(bdata), replace = TRUE, prob = c(0.8, 0.2))
btrainset=bdata[which(ind==1),]
btestset=bdata[which(ind==2),]
brf=randomForest(D1 ~ ., data=btrainset)
which.min(brf$err.rate[,1])
confusionMatrix(rf$predicted, trainset$D1)
rfpred<- predict(rf, testset)
confusionMatrix(rfpred, testset$D1)
ROCit_obj <- rocit(score=as.numeric(resultD),class=DATA[ind==2,1])
plot(ROCit_obj)


#Error rate of Random Forest
plot(rf)
plot(rf$err.rate[,1],type='l')  #oob error

#求得變數的重要性，並畫出重要性圖
rf$importance
varImpPlot(rf, sort = TRUE)
#透過 Mean Decrease Gini 來衡量變數重要性指數，表示 Gini 係數減少的平均值。在隨機森林中，衡量變數的重要性方法是透過剔除該變數，並將剔除變數的模型與原模型比較，差異越大表示變數越重要

library(LSD)
MDSplot(rf,data$D1)

library(praznik)
MRMR(data[,-1], data$D1, k = 11)

#knn
library(class)
pred<-knn(trainset[,-1], testset[,-1], trainset$D1, k = 3)
confus.matrix<-table(testset$D1, pred, dnn = c("實際", "預測"))
confusionMatrix(pred, testset$D1)

#svm
library(e1071)
svmM <- svm(D1 ~ ., data = trainset, probability = TRUE)
results <- predict(svmM, testset, probability = TRUE)
cm <- table(x = testset$D1, y = results)
cm
SVMaccuracy <- sum(diag(cm)) / sum(cm)
SVMaccuracy

#native bayesian
nbcm <- naiveBayes(D1 ~ ., data = trainset)
results <- predict(nbcm, testset)
cm <- table(x = testset$D1, y = results)
cm
naiveBayesaccuracy <- sum(diag(cm)) / sum(cm)
naiveBayesaccuracy

#kmeans
cluster <- kmeans(data[,-1], 3, nstart = 20)
cm<-table(cluster$cluster, data$D1)
kmeansaccuracy <- sum(diag(cm)) / sum(cm)
kmeansaccuracy

#nnet
install.packages('neuralnet')
library(neuralnet)
bpn <- neuralnet(formula = D1  ~ ., data = trainset, hidden = c(2,4),learningrate = 0.01)
plot(bpn)
bpnpred<-predict(bpn,testset)
confusionMatrix(bpnpred, testset$D1)
model <- train(form = D1  ~ ., data = trainset, method = "neuralnet", 
               tuneGrid = expand.grid(.layer1 = c(1:4), .layer2 = c(1:4), .layer3 = c(0)), learningrate = 0.01)
model
