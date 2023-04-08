#@Author: Thomas Sorenson
##Description: This script is to illustrate the procedures needed to perform an ensembling procedure for predictive analysis in R. This example can be used to instruct those who are learning the basics of performing these data nalytics techniques in R.

##First we need to install the right packages for our algorithms

library(glmnet)

install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)




###The Data
##We will use the dataset : College.csv. As a reminder, it’s a famous data set (contained in the ISLr package.  that was collected on U.S. colleges in 1995 to show common statistics on over 700 American universities. The main goal was to find factors that influence the success of the school and determine whether it was worth it for parents to send their child to a given university. The main measure of this success was the graduation rate of the school, because parents and students wanted to be assured of a successful degree. 

###The variables collected in the data set are:
###Variable	Description
###Private	Is college a private college (Yes or No)
###Apps	How many students applied for admission
###Accept	How many students did the college accept
###Enroll	How many students chose to enroll in the college
###Top10perc	Percentage of new students enrolling who were in top 10% of their high school class
###Top25perc	Percentage of new students enrolling who were in top 25% of their high school class
###F.Undergrad	Number of full-time undergraduate students
###P.Undergrad	Number of part-time undergraduate students
###Outstate	Expected tuition cost for students who are out-of-state
###Room.Board	Expected cost of room and board
###Books	Expected cost of books
###Personal	Expected cost for other personal expenses
###PhD	Percentage of faculty who have a PhD
###Terminal	Percentage of faculty with a terminal degree
###S.F.Ratio	Student to faculty ratio
###perc.alumni	Percentage of alumni who donate to the school
###Expend	Instructional expenditure per student (how much does school spend per student on instruction)
###Grad.Rate	Graduation rate of the school



###



### Data Preparation: 
###a.	Read the data set into R.

college = read.csv("i:/Analysis6100 - 6110/Data Sets/College.csv")

###b.	Convert the variable Private to a numeric variable with the value 0 if the college is not private and 1 if the college is private.

college$Private = ifelse(college$Private =="Yes", 1, 0)



###c.	Set the seed to 91101.

set.seed(91101)


###d.	Randomly partition the data set in the following order 

###i.	Split 25% of the observations in the full data set to use as testing data. Using these observations, create a testing data set called college_test.

num_obs = nrow(college)
test_obs = sample(num_obs, 0.25*num_obs)
college_test = college[test_obs,]


###ii.	Save the remaining 75% of the data as college_rest.

college_rest = college[-test_obs,]

###1. Ensembling From Multiple Methods

###•	Forward Selection

college_all = glm(Private~., data = college_rest, family = "binomial")
college_null = glm(Private~ 1, data = college_rest, family = "binomial")

forward_model = step(college_null, scope=list(upper=college_all), direction="forward")
summary(forward_model)

##Stepwise Both model starting with Forward

forward_both = step(college_null, scope=list(upper=college_all), direction="both")
summary(forward_both)


###•	Backward Elimination

backward_model = step(college_all, direction="backward")
summary(backward_model)

##Spepwise model starting with backward model

backward_both =  step(college_all, direction="both")
summary(backward_both)


###•	Ridge

ridge_model = glmnet(as.matrix(college_rest[,-1]),                       college_rest$Private, family = "binomial",  alpha = 0)
summary(ridge_model)

ridge_cv = cv.glmnet(as.matrix(college_rest[,-1]), college_rest$Private, family = "binomial", alpha = 0)
best_ridge = ridge_cv$lambda.min
best_ridge
predict(ridge_model, s = best_ridge,type="coefficients")


###•	LASSO

lasso_model = glmnet(as.matrix(college_rest[,-1]), college_rest$Private, family = "binomial", alpha = 1)
summary(lasso_model)

lasso_cv <- cv.glmnet(as.matrix(college_rest[,-1]), college_rest$Private, family = "binomial",                       alpha = 1)
best_lasso = lasso_cv$lambda.min
best_lasso
predict(lasso_model, s = best_lasso,type="coefficients")



###Rerun these models and consider the testing predictions for each.

##Forward Predictions

forward_preds = predict(forward_model, college_test, type = "response")
summary(forward_preds)

forward_class = ifelse(forward_preds>0.6,1,0)
summary(forward_class)


##Backward Predictions

backward_preds = predict(backward_model, college_test, type = "response")
summary(backward_preds)

backward_class = ifelse(backward_preds>0.6,1,0)
summary(backward_class)


##ridge Predictions

ridge_preds =  predict(ridge_model, s = best_ridge,                        newx = as.matrix(college_test[,-1]), type = "response")
summary(ridge_preds)

ridge_class =  ifelse(ridge_preds>0.6,1,0)
summary()ridge_class)


##Lasso Predictions

lasso_preds = predict(lasso_model, s = best_lasso,                        newx = as.matrix(college_test[,-1]), type = "response") 
summary(lasso_preds)

lasso_class = ifelse(lasso_preds>0.6,1,0)
summary(lasso_class)


###a.	What was your previous testing data accuracy for each of these four models? Report them again here for reference.

## Baseline accuracy

baseline_acc = mean(college_test$Private)
baseline_acc


##backward accuracy

backward_acc = sum(backward_class==college_test$Private)/nrow(college_test)
backward_acc

##Forward Accuracy
forward_acc = sum(forward_class==college_test$Private)/nrow(college_test)
forward_acc

##Ridge Accuracy
ridge_acc = sum(ridge_class==college_test$Private)/nrow(college_test)
ridge_acc

##Lasso Accuracy
lasso_acc = sum(lasso_class==college_test$Private)/nrow(college_test)
lasso_acc

###b.	You should have four distinct models here, each with different variables/coefficients, but you may have gotten very similar testing data accuracies. If the testing data accuracies were all the same, would that indicate that ensembling is unnecessary? Explain.

##Answer:Not necessarily, where cvariation is good for ensembling, it is possible that difference models predicted different variables better than other which just happened to result in a similar accuracy despite predicting different observations. Ensembling can still be potentially beneficial


###c.	Create two different ensembles of these four models:
###i.	An ensemble (i.e. an average) of the predicted probabilities for each school

ensemble_preds = (backward_preds+forward_preds+ridge_preds+lasso_preds)/4 

ensemble_preds_class = ifelse(ensemble_preds>0.6,1,0)
)
###ii.	An ensemble (i.e. an average of the predicted classes for each school

ensemble_class = (backward_class+forward_class+ ridge_class+lasso_class)/4
ensemble_class2 = ifelse(ensemble_class>0.6,1,0) 
###What are the prediction accuracies for these two ensemble methods? 

ensemble_preds_acc = sum(ensemble_preds_class==college_test$Private)/nrow(college_test)
 ensemble_preds_acc

ensemble_class_acc = sum(ensemble_class2==college_test$Private)/nrow(college_test)
ensemble_class_acc


###You don’t need any special code for this but think it through! Remember that you should be using a cutoff of 0.6 to create classifications for these ensembles in order to be able to calculate prediction accuracy.
###d.	Does your answer to part (c) surprise you? Why or why not?
###e.	If your ensemble methods improved over your best accuracy, why do you think that happened for this problem? If your ensemble methods did not improve over your best accuracy, why do you think that happened for this problem?

###2.	(20 points) Bootstrap Sample (Single Tree)

###a.	Create a single bootstrapped sample from the college_rest data.

num_rest=nrow(college_rest)
bootstrap_sample = sample(seq(1,num_rest),num_rest,replace=T)


###b.	Using your single sample, run a single tree to predict Private using all other variables. Is this a bagged tree or a random forest (of size 1)?

bag.tree = tree(as.factor(Private)~., data=college_rest[bootstrap_sample,])
summary(bag.tree)

###c.	Plot your single tree. Does this appear to be a useful tree? Explain.

plot(bag.tree)
text(bag.tree,pretty=1)

###d.	Use your single tree to predict Private for the college_test data with a cutoff of 0.6. What is the accuracy for your predictions?

bag.tree_preds = predict(bag.tree,newdata=college_test)

bag.tree_probs = bag.tree_preds[,2]

bag.tree_class = ifelse(bag.tree_probs>0.6,1,0)

bag.tree_acc = sum(bag.tree_class==college_test$Private)/nrow(college_test)
bag.tree_acc




### Bagging: Use your college_rest data to run a bagging procedure for 200 classification trees, again using Private as the dependent variable and all other variables as independent variables.

###a.	Report the Variable Importance Plot for your bagging procedure. Given your single tree from Question 2, do the results surprise you? Why or why not?

bag.forest = randomForest(as.factor(Private)~., data=college_rest,ntree=200,mtry=17,importance=TRUE)
varImpPlot(bag.forest)
importance(bag.forest)


###b.	Use your bagging results to predict Private on the training data. Remember that the predict() function here will calculate classes for you automatically, not probabilities!

bag.forest_preds = predict(bag.forest, newdata=college_rest, type="prob")[,2]
bag.forest_class = ifelse(bag.forest_preds> 0.6, 1, 0)


bag.forest_acc = sum(bag.forest_class==college_rest$Private)/nrow(college_rest)
bag.forest_acc

###c.	Use your bagging results to predict Private on the test data.

bag.forest_preds_test = predict(bag.forest, newdata=college_test, type="prob")[,2]
bag.forest_class_test = ifelse(bag.forest_preds_test> 0.6, 1, 0)


bag.forest_acc = sum(bag.forest_class_test==college_test$Private)/nrow(college_test)
bag.forest_acc




###i.	Report your test data accuracy.

bag.forest_acc


###ii.	Does there appear to be overfitting happening? Support your answer.

##Answer: No because we are not doing excessively well on traning accuracy compared to our testing accuracy.

###iii.	Does bagging appear to show improvement over the single tree? Support your answer.

##Answer: Yes, Bagging seems to improve our testing data accuracy significantly.

### Random Forest: Use your college_rest data to run a random forest procedure for 200 classification trees using 4 random variables per split, again using Private as the dependent variable and all other variables as independent variables.

## How many variables do we have in our data?
ncol(college_rest)

random.forest = randomForest(as.factor(Private)~., data=college_rest,ntree=200,mtry=4,importance=TRUE)
random.forest
importance(random.forest)
varImpPlot(random.forest)



###a.	Report the Variable Importance Plot for your random forest procedure. Does it appear that any of the variables in the data set is significantly affecting our results more than the others? Support your answer.

varImpPlot(random.forest)


###b.	If we increase the number of variables per split from 4 to 15, would you expect the Variable Importance Plot to change? Support your answer.

##answer: No, because our bagged tree chose the same variable as our random forest as the most important variable therefore there shouldnt be a change at 15 variables in our random forest because our bagging tree used 17 variables.

###c.	Use your random forest results to predict Private for the test data. What is your RMSE here? Does random forest appear to be a better option than bagging for this problem?


random.forest_preds = predict(random.forest, newdata= college_test, type="prob")[,2]
random.forest_class = ifelse(random.forest_preds>0.6,1,0)

random.forest_acc = sum(random.forest_class==college_test$Private)/nrow(college_test)
random.forest_acc

###5.	(10 points) Repeat your ensemble methods from Questions 3 and 4 with 1000 trees instead of 200. (Do not re-report variable importance/relative influence plots.)

bag.forest1000 = randomForest(as.factor(Private)~., data=college_rest,ntree=1000,mtry=17,importance=TRUE)

random.forest1000 = randomForest(as.factor(Private)~., data=college_rest,ntree=1000,mtry=4,importance=TRUE)



###Calculate the new accuracy values on the test data with your bagging and random 	forest procedures. ensemble methods. Have the results of your ensemble methods 	changed? (That is, is there a new method you now prefer based on these new 	accuracy results?)

bag.forest1000_preds = predict(bag.forest1000, newdata=college_test, type="prob")[,2]
bag.forest1000_class = ifelse(bag.forest1000_preds>0.6, 1, 0)
bag.forest1000_acc = sum(bag.forest1000_class==college_test$Private)/nrow(college_test)
bag.forest1000_acc


random.forest1000_preds = predict(random.forest1000, newdata=college_test, type="prob")[,2]
random.forest1000_class = ifelse(random.forest1000_preds>0.6, 1, 0)
random.forest1000_acc = sum(random.forest1000_class==college_test$Private)/nrow(college_test)
random.forest1000_acc


##Answer: Althogh that accuracies are still very similar we will still prefer a random forest approach.








