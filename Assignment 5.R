
##1
movies <- read.csv("movies_data.csv")

movies$month <- as.factor(movies$month)

set.seed(6022)

num_obs <- nrow(movies)
test_obs <- sample(num_obs, 0.30*num_obs)
movies_test <- movies[test_obs, ]
movies_rest <- movies[-test_obs, ]

num_obs <- nrow(movies_rest)
valid_obs <- sample(num_obs, 0.20*num_obs)
movies_valid <- movies_rest[valid_obs, ]
movies_train <- movies_rest[-valid_obs, ]

##2
movies_all <- glm(revenue~.-id-title-successful, data=movies_train)
movies_null <- glm(revenue~1, data=movies_train)

backward_model = step(movies_all, direction="backward")
summary(backward_model)

forward_model = step(movies_null, scope=list(upper=movies_all), direction="forward")
summary(forward_model)


backward_pred_val <- predict(backward_model, newdata=movies_valid)
RMSE_back_val <- sqrt(mean((backward_pred_val-movies_valid$revenue)^2))

forward_pred_val <- predict(forward_model, newdata=movies_valid)
RMSE_forw_val <- sqrt(mean((forward_pred_val-movies_valid$revenue)^2))

##3
successful_all <- glm(successful~.-id-title-revenue, family="binomial", data=movies_train)
successful_null <- glm(successful~1, family="binomial", data=movies_train)

backward_model_step = step(successful_all, direction="both")
summary(backward_model_step)

forward_model_step = step(successful_null, scope=list(upper=movies_all), direction="both")
summary(forward_model_step)


for(i in c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8)) {
  back_probs = predict(backward_model_step,newdata = movies_valid,type="response")
  back_class = ifelse(back_probs>i,1,0)
  acc = sum(ifelse(back_class==movies_valid$successful,1,0))/nrow(movies_valid)
  print(c(i, acc))
}


for(i in c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8)) {
  forward_probs = predict(forward_model_step,newdata = movies_valid,type="response")
  forward_class = ifelse(forward_probs>i,1,0)
  acc = sum(ifelse(forward_class==movies_valid$successful,1,0))/nrow(movies_valid)
  print(c(i, acc))
}


##4
mean(movies$successful)

##5
#movies_train_X <- model.matrix( ~ .-1, movies_train[,c(3,4,6:15)])
#movies_valid_X <- model.matrix( ~ .-1, movies_valid[,c(3,4,6:15)])
movies_rest_X <- model.matrix( ~ .-1, movies_rest[,c(3,4,6:15)])
movies_test_X <- model.matrix( ~ .-1, movies_test[,c(3,4,6:15)])


library(glmnet)

glmnet_ridge=glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=0)
glmnet_ridge.cv=cv.glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=0)
plot(glmnet_ridge.cv)
best.lambda_r=glmnet_ridge.cv$lambda.min
best.lambda_r

glmnet_lasso=glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=1)
glmnet_lasso.cv=cv.glmnet(movies_rest_X,movies_rest$successful,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda_l=glmnet_lasso.cv$lambda.min
best.lambda_l

predict(glmnet_ridge,s=best.lambda_r,type="coefficients")
predict(glmnet_lasso,s=best.lambda_l,type="coefficients")


##6
successful_retrained = glm(successful ~ genre + budget + united_states + english + 
                                popularity + vote_average + vote_count + month + year + runtime, 
                              family = "binomial", data = movies_rest)

backward_model_step_retrained = step(successful_retrained, direction="both")
summary(backward_model_step_retrained)

back_probs = predict(backward_model_step_retrained,newdata = movies_test,type="response")
back_class = ifelse(back_probs>0.5,1,0)
acc = sum(ifelse(back_class==movies_test$successful,1,0))/nrow(movies_test)
acc

forward_model_step_retrained = step(successful_retrained, direction="both")
summary(forward_model_step_retrained)

forward_probs = predict(forward_model_step_retrained,newdata = movies_test,type="response")
forward_class = ifelse(forward_probs>0.5,1,0)
acc = sum(ifelse(forward_class==movies_test$successful,1,0))/nrow(movies_test)
acc


lasso_probs = predict(glmnet_lasso,s=best.lambda_l,newx=movies_test_X,type="response")
lasso_class = ifelse(lasso_probs>0.5,1,0)
acc = sum(ifelse(lasso_class==movies_test$successful,1,0))/nrow(movies_test)
acc

ridge_probs = predict(glmnet_ridge,s=best.lambda_r,newx=movies_test_X,type="response")
ridge_class = ifelse(ridge_probs>0.5,1,0)
acc = sum(ifelse(ridge_class==movies_test$successful,1,0))/nrow(movies_test)
acc




