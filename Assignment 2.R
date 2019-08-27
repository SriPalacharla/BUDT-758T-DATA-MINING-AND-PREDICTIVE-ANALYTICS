
##1
airlines <- read.csv("Airlines_Data.csv")
attach(airlines)


airlines$cc1_miles=as.factor(airlines$cc1_miles)
airlines$cc2_miles=as.factor(airlines$cc2_miles)
airlines$cc3_miles=as.factor(airlines$cc3_miles)


set.seed(14632)

num_obs <- nrow(airlines)
split <- sample(num_obs, 0.65*num_obs)
airlines_train <- airlines[split,]
airlines_test <- airlines[-split,]



##2
boxplot(airlines_train$Bonus_trans~airlines_train$Award, xlab='Award', ylab='Bonus Transactions')

table(airlines_train$Award, airlines_train$cc3_miles, dnn=c('Award','cc3_miles'))



##3
reg1 = glm(Award~Balance+Qual_miles+cc1_miles+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12
           +Days_since_enroll,data=airlines_train)

reg1_pred_train <- predict(reg1, newdata=airlines_train)
reg1_pred_test <- predict(reg1, newdata=airlines_test)

RMSE_train <- sqrt(mean((reg1_pred_train-airlines_train$Award)^2))
RMSE_test <- sqrt(mean((reg1_pred_test-airlines_test$Award)^2))
RMSE_baseline <- sqrt(mean((mean(airlines_test$Award)-airlines_test$Award)^2))



class_train <- ifelse(reg1_pred_train > 0.5,1,0)
class_test <- ifelse(reg1_pred_test > 0.5,1,0)

table(class_train, airlines_train$Award, dnn=c('Prediction','Award'))
table(class_test, airlines_test$Award, dnn=c('Prediction','Award'))
sum(ifelse((class_test==0 & airlines_test$Award==1),1,0))

sum(ifelse(airlines_train$Award==class_train,1,0))/nrow(airlines_train)
sum(ifelse(airlines_test$Award==class_test,1,0))/nrow(airlines_test)
sum(ifelse(airlines_test$Award==0,1,0))/nrow(airlines_test)


new_entry <- data.frame(Balance=10000,Qual_miles=20000,cc1_miles='1',Bonus_miles=20000,Bonus_trans=50,
                        Flight_miles_12mo=25000,Flight_trans_12=25,Days_since_enroll=1000)
predict(reg1,new_entry)



##4
reg2 <- glm(Award~Balance+Qual_miles+cc1_miles+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12
           +Days_since_enroll,data=airlines_train,family='binomial')

reg2_preds <- predict(reg2, newdata=airlines_test,type='response')
reg2_class_test <- ifelse(reg2_preds>0.5,1,0)
table(airlines_test$Award, reg2_class_test, dnn=c('Actual','Predicted'))


sum(ifelse(airlines_test$Award==reg2_class_test,1,0))/nrow(airlines_test)



predict(reg2,new_entry, type='response')


