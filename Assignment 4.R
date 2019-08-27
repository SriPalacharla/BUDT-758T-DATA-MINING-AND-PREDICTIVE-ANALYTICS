

##1
data <- read.csv("movies_data.csv")

data$successful <- as.factor(data$successful)

set.seed(3730)

num_obs <- nrow(data)
test_obs <- sample(num_obs, 0.25*num_obs)
movies_test <- data[test_obs, ]
movies_rest <- data[-test_obs, ]

num_obs <- nrow(movies_rest)
valid_obs <- sample(num_obs, 0.25*num_obs)
movies_valid <- movies_rest[valid_obs, ]
movies_train <- movies_rest[-valid_obs, ]


##2
sapply(data, class)


##3
library(tree)
movies.tree=tree(successful~.-id-title-revenue,movies_train)
summary(movies.tree)
plot(movies.tree)
text(movies.tree,pretty=1)


##4
movies.pruned_1=prune.tree(movies.tree,best=1)
summary(movies.pruned_1)
plot(movies.pruned_1)
text(movies.pruned_1,pretty=1)
tree_preds <- predict(movies.pruned,newdata=movies_valid)
tree_probs=tree_preds[,2]
tree_class=ifelse(tree_probs>0.5,1,0)
table(movies_valid$veryprofitable,tree_class,dnn=c("Actual","Predicted"))
acc_tree=sum(ifelse(tree_class==movies_valid$veryprofitable,1,0))/nrow(movies_valid)
acc_tree


for(i in 2:7) {
  name <- paste("movies.pruned", i, sep="_")
  name=prune.tree(movies.tree,best=i)
  tree_preds <- predict(name,newdata=movies_valid)
  tree_probs=tree_preds[,2]
  tree_class=ifelse(tree_probs>0.5,1,0)
  accuracy <- paste("acc_tree", i, sep="_")
  accuracy=sum(ifelse(tree_class==movies_valid$successful,1,0))/nrow(movies_valid)
  print(c(i,accuracy))
}



movies.tree_retrain=tree(successful~.-id-title-revenue,movies_rest)
movies.pruned_retrain=prune.tree(movies.tree_retrain,best=6)
summary(movies.pruned_retrain)
plot(movies.pruned_retrain)
text(movies.pruned_retrain,pretty=1)
title(main='retrained')

movies.pruned_6=prune.tree(movies.tree,best=6)
summary(movies.pruned_6)
plot(movies.pruned_6)
text(movies.pruned_6,pretty=1)
title(main='only training')


##5
library(class)
colnames(data)
str(data)

train.X=movies_train[,c(4,6,10,11,12,15)]
valid.X=movies_valid[,c(4,6,10,11,12,15)]
test.X=movies_test[,c(4,6,10,11,12,15)]
rest.X=movies_rest[,c(4,6,10,11,12,15)]

train.target=movies_train$successful
valid.target=movies_valid$successful
test.target=movies_test$successful
rest.target=movies_rest$successful


for(i in c(1,3,5,10,25)) {
  knn.pred=knn(train.X,valid.X,train.target,k=i)
  cm = table(valid.target,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}


## 6

## best tree
name <- paste("movies.pruned", i, sep="_")
name=prune.tree(movies.tree,best=i)
tree_preds <- predict(movies.pruned_retrain,newdata=movies_test)
tree_probs=tree_preds[,2]
tree_class=ifelse(tree_probs>0.5,1,0)
tree_accuracy=sum(ifelse(tree_class==movies_test$successful,1,0))/nrow(movies_test)
tree_accuracy


## best knn 
knn.pred=knn(rest.X,test.X,rest.target,k=5)
cm = table(test.target,knn.pred,dnn=c("Actual","Predicted"))
knn_accuracy = (cm[1,1] + cm[2,2])/sum(cm)
knn_accuracy


nrow(movies_valid)
sum(ifelse(movies_valid$successful==0,1,0))/nrow(movies_valid)

