##### SETTING UP #####

set.seed(1101)

dataset= read.csv("diabetes-dataset.csv")
dim(dataset)
head(dataset)
attach(dataset)
names(dataset)
table(dataset$diabetes)

dataset$hypertension = as.factor(dataset$hypertension)
dataset$heart_disease = as.factor(dataset$heart_disease)
dataset$diabetes = as.factor(dataset$diabetes)


##### CHECKING ASSOCIATION #####


### Gender and Diabetes Association
# Gender: 3 cat, Diabetes: 2 cat -> Joint Proportion Table by gender
con_gen_table = table(gender, diabetes); con_gen_table
prop.table(con_gen_table, "gender")
# Findings:
# 1. Female and Male probability of having diabetes is about the same, so no association 
# 2. 100% of Others don't have diabetes. But only 18 Others are in this datatset, as compared to 41430 male and 58552 female. 
# So I'd say that Others category is too small to conclude any association with the response variables or other 'gender' category
# Thus concludes that gender has no association with the response variable, diabetes.


### Age and Diabetes Assosciation
# Age: (Discrete) Quantitative, Diabetes: 2 Categories -> Boxplot
boxplot_age = boxplot(age ~ diabetes, col = 'pink')
boxplot_age
# Findings:
# 1. Diabetes = 1 has a very high median as compared to diabates = 0 when assosciated with age 
# 2. The IQR of Diabetes = 1 is way smaller than Diabetes = 0. Range is also smaller. 
# 3. Diabetes = 1 has all outliers from the lower-extreme (we will further examine the outliers below)
# 4. Diabetes = 0 is symmetric
# Conclude that having diabetes and age have association considering the higher median and small IQR.


# Examining Outlier Trend
outlier = boxplot_age$out; outlier
length(outlier) # there are 118 outliers in boxplot_age
index = which(age %in% outlier & diabetes == '1'); length(index) # length is also 118, so we confirmed that all outliers are from diabetes = 1
outlier_info = dataset[index,]; outlier_info # extracting outlier info 


table(outlier_info$hypertension) # all age outliers don't have high-blood pressure 
table(outlier_info$heart_disease) # all but only 1 age outliers have heart disease
table(outlier_info$smoking_history) # most age outliers don't smoke 
table(outlier_info$gender) # female and male are about the same amount. age outliers has no association with gender
boxplot(outlier_info$blood_glucose_level) # compared to the whole data glucose level boxplot(blood_glucose_level), the has a slightly higher median and upper quartile. 
# Furthermore, all outliers in boxplot(blood_glucose_level) is present in the outliers. Showing that the age outliers then to have high blood_glucose_level
boxplot(outlier_info$bmi) # compared to the whole bmi boxplot(bmi), this also has a slightly higher median. Although it has little association with the high extreme outliers in boxplot(bmi).
# So I would say bmi has a weak association with the age outlier
boxplot(outlier_info$HbA1c_level) # compared to whole data's HbA1c_level boxplot(HbA1c_level), the outlier has a higher median and it includes the two upper outliers in boxplot(HbA1c_level).
# So I would associate having diabetes with higher HbA1c levels

## CONCLUDING OUTLIER TREND
# The outliers on age when diabetes = 1 is a lower extreme - meaning the younger people who has diabetes are the outliers
# Through the association, I found that hypertension, heart diseases, smoking history and gender does not contribute to the outlier
# However, high blood_glucose_level and HbA1c_level could be the possible reasons why the younger people are getting diabetes. bmi has weak association but could be considered as well. 



### Hypertension and Diabetes Association 
# Hypertension: 2 cat, Diabetes: 2 cat -> odd ratio 
con_hyp_table = table(hypertension, diabetes); con_hyp_table
split_hyp = prop.table(con_hyp_table, "hypertension"); split_hyp
hyp_diab_yes = split_hyp[2]
hyp_diab_no = split_hyp[4]

odds_hyp_y = hyp_diab_yes / (1-hyp_diab_yes)
odds_hyp_n = hyp_diab_no / (1-hyp_diab_no)
OR_hyp = odds_hyp_y / odds_hyp_n; OR_hyp # 6.681
# Findings: 
# 1. the odds of having diabetes amongst having hypertension is 6.681 times more than not having hypertension
# Thus, conclude that hypertension is associated to the response variable, diabetes. 



### Heart_disease and Diabetes Association
# Heart_disease: 2 cat, Diabetes: 2 cat -> odd ratio 
con_heart_table = table(heart_disease, diabetes); con_heart_table
split_heart = prop.table(con_heart_table, "heart_disease"); split_heart
heart_diab_yes = split_heart[2]
heart_diab_no = split_heart[4]

odds_heart_y = heart_diab_yes / (1-heart_diab_yes)
odds_heart_n = heart_diab_no / (1-heart_diab_no)
OR_heart = odds_heart_y / odds_heart_n; OR_heart
# Findings:
# 1. the odds of having diabetes amongst having heart disease is 4.458 times more than not having heart disease
# Thus, conclude that heart disease is associated to the response variable, diabetes 



### Smoking_history and Diabetes Association 
# Smoke_history: 6 cat, Diabetes: 2 cat -> Joint Prop Table by smoking_history
con_smoke_table = table(smoking_history, diabetes); con_smoke_table
prop.table(con_smoke_table, "smoking_history")
# Findings:
# 1. all 6 category under smoke history have a similar ratio for diabetes = 0 and diabetes = 1
# Thus, conclude that smoke history has NO EFFECT on the response variable, diabetes 



### BMI and Diabetes Association 
# BMI: quantitative variable, Diabetes: 2 cat -> Boxplot
boxplot_bmi = boxplot(bmi ~ diabetes, col = 'lightblue')
# Findings:
# 1. Median, Upper Quartile and Lower Quartile for bmi when having diabetes is higher than not having diabetes. 
# 2. A lot of outliers on the upper and bottom extremes on BMI value for having and not having diabetes 
# Thus, conclude that have BMI has association with response variable, diabetes. Although the strength of association is unknown for now. 



### HbA1c_level and Diabetes Association
# HbA1c: quantitative variable, Diabetes: 2cat -> Boxplot
boxplot_hba1c = boxplot(HbA1c_level ~ diabetes, col = 'violet')
# Findings:
# 1. Median, Upper Quartile and Lower Quartile of HbA1c_level is higher for diabetes = 1 than diabetes = 0
# 2. Boxplot of diabetes = 0 is more left-skewed when boxplot of diabetes = 1 is more right-skewed
# 3. Both boxplots have little overlap in HbA1c_levels 
# Thus, concludes that HbA1c have strong association with response variable, diabetes. Where people who have diabetes are more likely to have higher HbA1c level. 



### Blood_glucose_level and Diabetes association
# blood_glucose_level: (discrete) quantitative, diabetes: 2 cat -> box plot
boxplot_glucose = boxplot(blood_glucose_level ~ diabetes, col = 'lightyellow')
# Findings:
# 1. Median, Upper Quartile and Lower Quartile of blood glucose level is higher for diabetes = 1 than diabetes = 0
# 2. Boxplot of diabetes = 0 is more left-skewed when boxplot of diabetes = 1 is more right-skewed
# 3. Both boxplots have little overlap in HbA1c_levels 
# Thus, concludes that blood glucose level have strong association with response variable. diabetes. Where people who have diabetes are more likely to have higher blood glucose level. 



### Conclusion for Association 
# I can conclude to remove gender and smoking_history from the dataset because they have no association to the response variable, diabetes.
dataset = dataset[,-1] # got rid of gender
dataset = dataset[, -4] # got rid of smoking_history



##### SEPARATING THE DATASET #####

n = dim(dataset)[1]; n

index.train = sample(1:n)[1:(0.8*n)]
train.data = dataset[index.train,]
test.data = dataset[-index.train,]
train.data.x = dataset[index.train, (1:6)] 
train.data.y = dataset[index.train, 7] 
test.data.x = dataset[ -index.train, (1:6)]
test.data.y = dataset[-index.train, 7] 


##### MODELLING #####



### 1. LINEAR MODEL (Not Choosing)
# Why? 
# 1. First Assumption for a linear model is that the responds should be quantitative or continuous but the responds here is categorical
# 2. Assuming response is symmetric will not work as well. As there is only values 0 and 1. 


# --------------------------------------------------------------------------------------------------



### 2. DECISION TREE (Choosing)
# Why Choosing? 
# Decision Tree helps decide which features are more important than the other, which could help build and increase the accuracy of our model. While checking whether the features we eliminated during the association part is really ineffective
# Finding the best CP value can remove branches that have week predictive power and prevent overfitting 

# Methods:
# Test every cp value and record the rate of misclassification of each cp. Decide on which cp value is the best. 
# Construct decision tree plot with that cp plot. Checking which feature is most important.
# Can eliminate any features that are unimportant according to the plot
# Test accuracy and ROC, AUC

library(rpart)
library(rpart.plot)

cp = 10^(-5:5)
misC = rep(0, length(cp)) 

for (i in 1:length(cp)) {
  fit <- rpart(diabetes ~ age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level, 
               method = 'class', 
               data = train.data, 
               control = rpart.control(cp = cp[i]), 
               parms = list(split = 'information'))
  
  predictions = predict(fit, newdata = test.data.x, type = "class") 
  misC[i] = sum(predictions != test.data.y) / nrow(test.data) 
}

plot(-log(cp, base = 10), misC, type = 'b') 
best.cp = cp[max(which (misC == min(misC)))] # Find the cp value that minimizes the misclassification rate
best.cp; min(misC) # 0.1, 0.02995
# we have multiple cp values returning the same misclassfication rate, to which we select the largest cp value. Because it gives us a simpler diagram with same misC value 

# fit decision tree with that smallest cp 
fit_small = rpart(diabetes ~ age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level, 
                  method = 'class', 
                  data = train.data, 
                  control = rpart.control(cp = best.cp), 
                  parms = list(split = 'information'))

rpart.plot(fit_small, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

### FINDINGS:
# Decision Tree suggests only using HbA1c_level and blood_glucose_level to form the best model
# However, I think it might risk underfitting. So I will proceed to test each regressor in Logistic Regression model to double confirm each regressor's significance. 



### Goodness of fit DT - accuracy 
predicted_tree = predict(fit_small, newdata = test.data.x, type = "class")
confusion.matrix.tree = table(predicted_tree, test.data.y); confusion.matrix.tree
accuracy_tree = sum(diag(confusion.matrix.tree))/sum(confusion.matrix.tree); accuracy_tree # 0.9701

diab = ifelse(test.data[,7] == 1, "Yes", "No")
diab = as.factor(diab)



# AUC
prob_tree = predict(fit_small, newdata = test.data.x, type ="prob") 
positive_prob_tree = prob_tree[,2] # has diabetes
pred_tree = prediction(positive_prob_tree, test.data.y)
roc_tree = performance(pred_tree, "tpr", "fpr")
auc_tree = performance(pred_tree , measure ="auc")
auc_tree@y.values[[1]]
plot(roc_tree , col = "purple", main = paste(" Area under the curve :", round(auc_tree@y.values[[1]] ,4)))
# AUC Value: 0.8282


### Conclusion for decision tree:
# 1. best_cp = 0.1
# 2. AUC = 0.8282
# 3. Accuracy: 0.9701
# 4. Decision Tree might under-fit, causing over-simplication. 


# --------------------------------------------------------------------------------------------------


### 3. LOGISTIC REGRESSION (Choosing)
# Why Choosing?
# Logistic Regression is suitable for response with Boolean which fits diabetes 
# Interpretability: Logistic Regression provides coefficients for each feature, providing the importance and impact of each variable on the prediction. 

M_log <- glm( diabetes ~., data = train.data,family = binomial)
summary(M_log) # shows that all regressors are important. so we will not discard it despite the results of decision tree

### Goodness of fit Logistic Regression - accuracy  
prob.log = predict(M_log, newdata = test.data.x, type ="response") 
predicted.classes.log = ifelse(prob.log > 0.5, 1, 0) # i will use 0.5 because it is usually the standard. especially for binaries like having diabetes or not
confusion.matrix.log = table(predicted.classes.log, test.data.y); confusion.matrix.log

accuracy_log = sum(diag(confusion.matrix.log))/sum(confusion.matrix.log); accuracy_log # 0.9581


### Goodness of fit Logistic Regression - AUC and ROC
prob_log = predict(M_log, newdata = test.data.x, type ="response") 

pred_log = prediction(prob_log , test.data.y)
roc_log = performance(pred_log , "tpr", "fpr")
auc_log = performance(pred_log , measure ="auc")
auc_log@y.values[[1]]
plot(roc_log, add = TRUE, col = "green", main = paste(" Area under the curve :", round(auc_log@y.values[[1]] ,4)))
# AUC Value: 0.962 


# Conclusion for Logistic Regression:
# 1. Threshold = 0.5 
# 2. Accuracy = 0.9581
# 3. AUC Value = 0.962

# --------------------------------------------------------------------------------------------------


### 4. KNN (Choosing)
# Why Choosing? 
# KNN is suitable for response with Boolean which fits diabetes 
# It is scale sensitive, but we can standardise the regressors to minimize the effects
# KNN is non-parametric, meaning it won't be affected by any statistical assumptions


library(class)

# Changing regressor data type to numeric to scale the regressors 
hypertension.knn = as.numeric(hypertension)
heart_disease.knn = as.numeric(heart_disease)
blood_glucose_level.knn = as.numeric(blood_glucose_level)

dataset.knn = data.frame(age, hypertension.knn, heart_disease.knn, bmi, HbA1c_level, blood_glucose_level.knn, diabetes); dataset.knn

# Scaling the training and testing data
train.data.knn.x = scale(dataset.knn[index.train,(1:6)])
train.data.knn.y = dataset.knn[index.train,7]
test.data.knn.x = scale(dataset.knn[-index.train,(1:6)])
test.data.knn.y = dataset.knn[-index.train,7]

K = 50 
misclass_rate = numeric(K)

for (i in 1:K) {
  knn.pred = knn(train.data.knn.x, test.data.knn.x, train.data.knn.y, k = i)
  confusion.matrix.knn = table(True = test.data.knn.y, Predicted = knn.pred)
  misclass_rate[i] = 1 - sum(diag(confusion.matrix.knn)) / sum(confusion.matrix.knn) # Misclassification rate is 1 - accuracy
}
misclass_rate
best_k <- which.min(misclass_rate); best_k; min(misclass_rate) # k = 9, misclass = 0.0328

### Forming model with k values and comparing results 
knn.pred = knn(train.data.knn.x, test.data.knn.x, train.data.knn.y, k= best_k) 
knn_compare_table = data.frame(knn.pred, test.data.knn.y)
confusion.matrix.knn = table(knn.pred, test.data.knn.y); confusion.matrix.knn

### Goodness of fit KNN - accuracy 
accuracy.knn = sum(diag(confusion.matrix.knn))/sum(confusion.matrix.knn); accuracy.knn # 0.9673

### Goodness of fit KNN - ROC, AUC
library(ROCR)
knn.pred.num = as.numeric(knn.pred)
knn_pred = prediction(as.vector(knn.pred.num), test.data.knn.y)
knn_perf = performance(knn_pred, 'tpr', 'fpr')
knn_auc = performance(knn_pred, 'auc')@y.values[[1]]; knn_auc # 0.8270
plot(knn_perf, add = TRUE, col = "lightblue", main = paste("Area under the curve :", round(knn_auc ,4)))

### Conclusion for knn:
# 1. best k is 9 
# 2. k = 9 gives accuracy of 0.9673
# 3. k = 9 gives AUC value of 0.8270

# --------------------------------------------------------------------------------------------------

### 5. NAIVE BAYES (Choosing)
# Why Choosing? 
# 1. can accept categorical and continuous variables - discretization of continuous variable
# 2. Require to split the regressors into categories, but we already got some splits from decision tree 

### Splitting numerical/continuous variables

# 1. Splitting based on decision tree - HbA1c and blood_glucose_level 
# we will split HbA1c into <6.7 and >= 6.7
# and split blood_glucose_level into <210 and >= 210 

# 2. Splitting based on Quartiles - Age and BMI
summary(age)
# we will split age into before it's 1st Quartile, between 1st and 3rd Quartile, after 3rd Quartile as young, adult, old
# 1st Quartile (Young):  < 24.00, Between (Adult): 24.00 and 60.00, 3rd Quartile (Old): > 60.00
summary(bmi)
# we will also split BMI in the same way 
# 1st Quartile (Under-BMI): 23.63, Between (Normal): 23.63 and 29.58, 3rd Quartile (Over-BMI): >29.58


# Splitting data 
split_HbA1c = ifelse(HbA1c_level < 6.7, "<6.7", ">= 6.7")
split_glucose = ifelse(blood_glucose_level < 210, "<210", ">= 210")
split_age = ifelse(age < 24, "Young", ifelse(age <= 60, "Adult", "Old"))
split_bmi = ifelse(bmi < 23.63, "Under", ifelse(age <= 29.58, "Normal", "Over"))

dataset.nb = data.frame(dataset$hypertension, dataset$heart_disease, split_HbA1c, split_glucose, split_age, split_bmi, dataset$diabetes)

# set new train data 
train.data.nb = dataset.nb[index.train,]
test.data.nb = dataset.nb[-index.train,]
train.data.nb.x = dataset.nb[index.train, (1:6)]
train.data.nb.y = dataset.nb[index.train, 7]
test.data.nb.x = dataset.nb[-index.train, (1:6)]
test.data.nb.y = dataset.nb[-index.train, 7]

# modelling 
library(e1071)
M_nb = naiveBayes(train.data.nb.y ~ ., data = train.data.nb)


# Goodness of fit Naive Bayes - Accuracy  
pred.M_nb = predict(M_nb, test.data.nb.x)
correct_predictions_nb <- sum(pred.M_nb == test.data.nb.y)
accuracy_nb <- correct_predictions_nb / length(test.data.nb.y); accuracy_nb # 0.966


# Goodness of fit Naive Bayes - AUC and ROC
pred.M_nb = predict(M_nb, test.data.nb.x, type ='raw')[,2]
pred_nb = prediction(pred.M_nb, test.data.nb.y) 
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr") 
auc_nb <- performance(pred_nb , "auc")@y.values[[1]]; auc_nb # 0.9281
plot(roc_nb,add = TRUE, col = "red", main = paste("Area under the curve :", round(auc_nb ,4))) 

legend(0.41,0.54, legend=c("Decision Tree", "Logistic Regression", "KNN", "Naive Bayes"),col=c("purple", "green", "lightblue", "red"), pch=c(20,2))


# Conclusion for Naive Bayes 
# 1. Accuracy = 0.966
# 2. AUC = 0.9281
