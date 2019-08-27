

## 0 Data Preparation
college <- read.csv("College.csv")
View(college)

set.seed(91101)

num_obs=nrow(college)
test_obs = sample(num_obs, 0.35*num_obs)
college_test <- college[test_obs,]
college_rest <- college[-test_obs,]


## 1 Principal Components Analysis
pc_rest = prcomp(college_rest[,-c(1,2,19)])
summary(pc_rest)


head(pc_rest$x)
PC8 = pc_rest$x[,c(1:8)]

plot(pc_rest)


lm_model=lm(Grad.Rate~.-X-Private, data=college_rest)
pc_model=lm(college_rest$Grad.Rate~PC8)

summary(lm_model)
summary(pc_model)


## 2 Bootstrap Sample (Single Tree)
num_rest=nrow(college_rest)
bootstrap_sample=sample(seq(1,num_rest),num_rest,replace=T)

library(tree)
bag.tree=tree(Grad.Rate~.-X,data=college_rest[bootstrap_sample,])
summary(bag.tree)
plot(bag.tree)
text(bag.tree,pretty=1)

bag.tree_preds <- predict(bag.tree,newdata=college_test)
RMSE_bag.tree_test <- sqrt(mean((bag.tree_preds-college_test$Grad.Rate)^2))


## 3 Bagging
library(randomForest)
bag.trees=randomForest(Grad.Rate~.-X,data=college_rest,ntree=200,mtry=17,importance=TRUE)

bag.trees
importance(bag.trees)
varImpPlot(bag.trees)

bagging_preds_rest=predict(bag.trees,newdata=college_rest)
RMSE_bag.trees_rest <- sqrt(mean((bagging_preds_rest-college_rest$Grad.Rate)^2))

bagging_preds_test=predict(bag.trees,newdata=college_test)
RMSE_bag.trees_test <- sqrt(mean((bagging_preds_test-college_test$Grad.Rate)^2))


## 4 Random Forest
library(randomForest)
rf.trees=randomForest(Grad.Rate~.-X,data=college_rest[bootstrap_sample,],ntree=200,mtry=4,importance=TRUE)

rf.trees
importance(rf.trees)
varImpPlot(rf.trees)


rf_preds_test=predict(rf.trees,newdata=college_test)
RMSE_rf.trees_test <- sqrt(mean((rf_preds_test-college_test$Grad.Rate)^2))


## 5 Boosting
library(gbm)
college_rest$Private = as.numeric(college_rest$Private)
college_test$Private = as.numeric(college_test$Private)

boost.college=gbm(Grad.Rate~.-X,data=college_rest,distribution="gaussian",n.trees=200)


summary(boost.college)

boost_preds_test = predict(boost.college,newdata=college_test,n.trees=200)
RMSE_boost_test <- sqrt(mean((boost_preds_test-college_test$Grad.Rate)^2))

college_rest$Private = as.factor(college_rest$Private)
college_test$Private = as.factor(college_test$Private)


## 6 
bag.trees_1000=randomForest(Grad.Rate~.-X,data=college_rest,ntree=1000,mtry=17,importance=TRUE)
bagging_preds_test_1000=predict(bag.trees_1000,newdata=college_test)
RMSE_bag.trees_test_1000 <- sqrt(mean((bagging_preds_test_1000-college_test$Grad.Rate)^2))

rf.trees_1000=randomForest(Grad.Rate~.-X,data=college_rest,ntree=1000,mtry=4,importance=TRUE)
rf_preds_test_1000=predict(rf.trees_1000,newdata=college_test)
RMSE_rf.trees_test_1000 <- sqrt(mean((rf_preds_test_1000-college_test$Grad.Rate)^2))

college_rest$Private = as.numeric(college_rest$Private)
college_test$Private = as.numeric(college_test$Private)

boost.college_1000=gbm(Grad.Rate~.-X,data=college_rest,distribution="gaussian",n.trees=1000)
boost_preds_test_1000 = predict(boost.college_1000,newdata=college_test,n.trees=1000)
RMSE_boost_test_1000 <- sqrt(mean((boost_preds_test_1000-college_test$Grad.Rate)^2))

college_rest$Private = as.factor(college_rest$Private)
college_test$Private = as.factor(college_test$Private)



RMSE_bag.trees_test_1000
RMSE_rf.trees_test_1000
RMSE_boost_test_1000

RMSE_bag.trees_test
RMSE_rf.trees_test
RMSE_boost_test




