
##1
bikeshare = read.csv('bikeshare.csv')
attach(bikeshare)

bikeshare$WEATHERSIT <- as.factor(bikeshare$WEATHERSIT)

bikeshare$REG_85 <- ifelse(bikeshare$REGISTERED/bikeshare$COUNT > 0.85,1,0)

set.seed(8726)

num_obs <- nrow(bikeshare)
train_obs <- sample(num_obs, num_obs*0.75)
bikeshare_train <- bikeshare[train_obs,]
bikeshare_test <- bikeshare[-train_obs,]



##2
fit1 <- glm(REG_85~.-COUNT-REGISTERED-CASUAL, data=bikeshare_train, family='binomial')
summary(fit1)

cutoffs=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99)
log_acc=rep(0,13)
log_TPR=rep(0,13)
log_TNR=rep(0,13)
log_preds <- predict(fit1,newdata=bikeshare_test,type='response')


for (i in 1:13) {
  log_class <- ifelse(log_preds>cutoffs[i],1,0)
  confuse_test=table(bikeshare_test$REG_85,log_class)
  log_acc[i]=(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)
  log_TPR[i]=confuse_test[2,2]/(confuse_test[2,1]+confuse_test[2,2])
  log_TNR[i]=confuse_test[1,1]/(confuse_test[1,1]+confuse_test[1,2])
}

#log_class <- ifelse(log_preds>0.45,1,0)
#confuse_test=table(bikeshare_test$REG_85,log_class)

plot(1-log_TNR, log_TPR, type='l', xlab='FPR', ylab='TPR', main='ROC Curve')

plot(cutoffs, log_TPR, col='green', type='l', xlab='Cutoff', ylab='TPR, TNR, Accuracy', main='spider plot')
lines(cutoffs, log_TNR, col='red')
lines(cutoffs, log_acc, col='blue')

cutoff = 0.45

log_class <- ifelse(log_preds>cutoff,1,0)
confuse_test=table(bikeshare_test$REG_85,log_class,dnn=c('Actual','Predicted'))
logistic_acc=(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)

total_cost=confuse_test[1,2]*72+confuse_test[2,1]*135


##3
library(MASS)
lda_model <- lda(REG_85~.-COUNT-REGISTERED-CASUAL, data=bikeshare_train)

lda_predict=predict(lda_model,newdata=bikeshare_test)
lda_preds <- lda_predict$posterior[,2]
lda_class <- ifelse(lda_preds > cutoff,1,0)
confuse_test2 <- table(bikeshare_test$REG_85,lda_class,dnn=c('Actual','Predicted'))
accuracy2 <- (confuse_test2[1,1]+confuse_test2[2,2])/sum(confuse_test2)
total_cost2=confuse_test2[1,2]*72+confuse_test2[2,1]*135


#table(bikeshare_test$REG_85,lda_predict$class,dnn=c('Actual', 'Predicted'))
#xyz_class <- ifelse(lda_preds > 0.5,1,0)
#table(bikeshare_test$REG_85,xyz_class,dnn=c('Actual','Predicted'))

table(bikeshare_train$REG_85, bikeshare_train$WEATHERSIT)


##4
qda_model <- qda(REG_85~.-COUNT-REGISTERED-CASUAL-WEATHERSIT, data=bikeshare_train)

qda_predict <- predict(qda_model,newdata=bikeshare_test)
qda_preds <- qda_predict$posterior[,2]
qda_class <- ifelse(qda_preds > cutoff,1,0)
confuse_test3 <- table(bikeshare_test$REG_85,qda_class,dnn=c('Actual','Predicted'))
accuracy3 <- (confuse_test3[1,1]+confuse_test3[2,2])/sum(confuse_test2)
total_cost3=confuse_test3[1,2]*72+confuse_test3[2,1]*135


##5
countOf1 = sum(ifelse(bikeshare_test$REG_85==1,1,0))/nrow(bikeshare_test)
baseline_test <- ifelse(bikeshare_test$REG_85==1,1,1)
baseline_cf <- table(bikeshare_test$REG_85, baseline_test, dnn=c('Actual', 'Predicted'))
baseline_accuracy <- baseline_cf[2,1]/(baseline_cf[1,1]+baseline_cf[2,1])

baseline_cost <- baseline_cf[1,1]*72

