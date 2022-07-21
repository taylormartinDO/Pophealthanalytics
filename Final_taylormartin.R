# ============================================================ 
# PHAV :: Final Project Questions (20% of total grade)
# ============================================================ 

# your final project includes 150 points (50 points are considered as bonus!)
# if you score 100 out of 150, you will get all of the 20% for the final project


# hint: the columns of the dataset used in the final project include:
# - age is in years; 
# - sex is Male or Female; 
# - plan is the insurance plan type; 
# - columns ami to aids are various diagnosis of the patient (e.g., ami = acute myocardial infarcion)
# - wscore is the weighted 'Charlson Score' of the patient (similar to the count of chronic conditions but much more sophisticated; higher numbers mean the patient is sicker)
# - state is where the patient lives (2 letter state abbreviations)
# - income_k is the income in 1000s of dollars
# - cost_c is the total cost of care in the current year
# - cost_f is the total cost of care in the future year
# - ip_c is the flag for inpatient admission in the current year
# - ip_f is the flag for inpatient admission in the future year
# - er_c is the flag for emergency room admission in the current year
# - er_f is the flag for emergency room admission in the future year

# process to finish the final project
# - this document looks long as our comments to guide you are much longer than the R commands that you will provide
# - add your responses immidiately after the sub-questions noted by a number (do NOT pile all answers at the end of the question group)
# - if a question asks about your comments, simply add your reponse as an R command (starting with #; like this line of code)
# - take it one question at time -- don't overwhelm yourself jumping from one question to another
# - question groups A and B should be completed before other question groups
# - question group C is recommended to be completed befor other question groups
# - other questions groups (D to I) can be completed separately/independently 

# ------------------------------------------------------------- 
# question group (A) [2 pts] :: loading the data
# -------------------------------------------------------------
# download the "data_new.zip" file from CoursePlus online library and unzip on your computer (data_new.rds)
# import the data as "data_new" variable and print its column names
library(data.table)
file_rds = 'data_new'
data_new <- readRDS(file = file_rds)                                    # reading an RDS file (need to check the content; in this case, contains a data.table)
data_new[, .N]  
colnames(data_new)
    
# hint: this dataset contains a data.table
# hint: make sure not to mix data_pat with data_new (we used data_pat in our mini-assignment, but the final project only uses data_new)
# hint: see code_slide6.R in CoursePlus online library for sample code reading RDS files

# ------------------------------------------------------------- 
# question group (B) [1x13 = 13 pts] :: cleaning the data
# ------------------------------------------------------------- 
# hint: you need the '%in%' command in some of the following questions
# e.g.: data_new[col %in% c('A','B','C'),] means find rows that only contain col = A or B or C
# hint: you can simply select the rows that fit your inclusion criteria and put the new results back in data_new (e.g., data_new <- data_new[....])
# hint: you should be able to answer each subquestion in one line of code, but OK if you use more lines
# hint: see code_slide5.R in CoursePlus online library for sample code on using data tables in R

# 1 - drop all rows that have an age >= 110 or age = NA
data_new <- data_new[age <= 110, ]
data_new <- data_new[!age == "NA", ]

# 2 - drop all rows that do NOT have a sex of 'M' or 'F' (drop everything else including NAs)
data_new <- data_new[sex == "M" | sex == "F", ] 

# 3 - drop all rows that do NOT have a race of 'W','B','A','O' (drop everything else including NAs)
data_new <- data_new[race == "W" | race == "B" | race == "A" | race == "O", ] 

# 4 - drop all rows that do NOT have a plan of 'medicare','medicaid','commercial' (drop everything else including NAs)
data_new <- data_new[plan == "medicare" | plan == "medicaid" | plan == "commercial", ] 

# 5 - drop all rows that do NOT have a state of 'DC','DE','IL','MA','MD','RI','TN','TX','VA','WV' (drop everything else including NAs)
data_new <- data_new[state %in% c("DC","DE","IL","MA","MD","RI","TN","TX","VA","WV"), ]  

# 6 - drop all rows that have a negative current cost or a missing current cost (cost_c)
data_new <- data_new[data_new$cost_c >= 0, ]
data_new <- data_new[!cost_c == "NA", ]

# 7 - drop all rows that have a negative future cost or a missing future cost (cost_f)
data_new <- data_new[data_new$cost_f >= 0, ]
data_new <- data_new[!cost_f == "NA", ]

# 8 - drop all rows that have a negative current inpatient admission or a missing current inpatient admission (ip_c)
data_new <- data_new[data_new$ip_c >= 0, ]
data_new <- data_new[!ip_c == "NA", ]

# 9 - drop all rows that have a negative future inpatient admission or a missing future inpatient admission (ip_f)
data_new <- data_new[data_new$ip_f >= 0, ]
data_new <- data_new[!ip_f == "NA", ]

# 10 - drop all rows that have a negative current ER admission or a missing current ER admission (er_c)
data_new <- data_new[data_new$er_c >= 0, ]
data_new <- data_new[!er_c == "NA", ]

# 11 - drop all rows that have a negative future ER admission or a missing future ER admission (er_f)
data_new <- data_new[data_new$er_f >= 0, ]
data_new <- data_new[!er_f == "NA", ]

# 12 - drop all rows that have a negative (less than 0) or missing income (income_k)
# hint: you should have 10,000 rows left! (if not, check your prior steps)
data_new <- data_new[data_new$income_k >= 0, ]
data_new <- data_new[!income_k == "NA", ]

# 13 - what percentage of the original rows were dropped? (you can calculate manually) 
(2000/12000)*100 #16.7% were dropped


# -------------------------------------------------------------
# question group (C) [1x25 = 25 pts] :: exploratory data analysis
# -------------------------------------------------------------
# hint: using data.table features is preferred; however, it is OK if you complete these tasks using other functions
# hint: see code_slide8.R in CoursePlus online library for sample code on data exploration and basic visualization
# hint: see code_slide9.R in CoursePlus online library for sample code on measuring skewness of distributions
# hint: see code_slide12.R in CoursePlus online library for sample code on passing regression lines to abline commands

# 1 - show the summary of age; what is the mean? 
#59.34
summary(data_new$age)

# 2 - show the histogram of age; does it have any skewness (just by visually looking at it)? 
# It is left Skewed
hist(data_new$age, col=rgb(.8,1,.8), main='', xlab='Age (yrs)')  

# 3 - does applying the LOG function to age make it normally distributed? (run another histogram and describe what you visually see) 
# No it worsens the left skew
data_new[, age_log := log(age)]  
hist(data_new$age_log, col=rgb(.8,1,.8), main='', xlab='log Age (yrs)') 

# 4 - show the number of patients in each sex category (M vs. F)
# F 5636 M 4364
data_new[, .N, by = sex][order(sex)] 

# 5 - create two overlapping histograms to show the distribution of age in each sex category
# do you see higher number of males or females in the 65+ population (just visually)
# hint: give the F patients the following color: rgb(1,0,0,.3)
# hint: give the M patients the following color: rgb(0,0,1,.3)
# hint: make sure to filter the patients based on their sex (using data.table features) before passing the data to the histogram function
# hint: use the 'add = TRUE' parameter in the second histogram to overlay it on top of the first one
# hint: you should be able to answer this question in two lines of code, but OK if you use more lines
hist(data_new[data_new$sex=='F',]$age, main='', xlab='age (years)', col=rgb(1,0,0,.3))                                         # histogram of age for F 
hist(data_new[data_new$sex=='M',]$age, col=rgb(0,0,1,.3), add=TRUE) 

# 6 - calculate the number and mean age of F vs M patients in the >= 65 age group (65+)
# hint: you should be able to answer this question in one line of code (using data.table features), but OK if you use more lines
data_new[age >=65, .(ct = .N, age_avg = round(mean(age),1)), by = sex]

# 7 - calculate the percentage of female population in each of the races (e.g., what percentage of Whites are female within the total White population) (round percentages by 1 digit)
# hint: you can answer this question using data.table features, but OK if you use additional commands
data_new[ , .(sex_M = (sum(sex == 'M')/.N)*100, 
           sex_F = (sum(sex == 'F')/.N)*100),
          by = race] 

# 8 - calculate how many patients exist in each insurance plan, as well as the average income of those patients (round to zero decimals)
# hint: you should be able to answer this question in one line of code (using data.table features), but OK if you use more lines
data_new[, .( ct = .N, income_avg = round(mean(income_k),1)), 
         by = plan]

# 9 - visually inspect the skewness of income; then measure the exact skewness of income (is it high?)
# hint: you may need the 'moments' library!
install.packages('moments')                                             				      # installing the 'moments' package
library(moments)                                                        				      # activating the "moments" library/command -- make sure that you have run this command once: install.packages("moments")
hist(data_new$income_k, main='', xlab='Income', col='lightblue', labels=TRUE)                
skewness(data_new$income_k)
# 0.38 not high

# 10 - show the box plots of incomes for each state (all in one plot) -- any state with a considerably different income level (judging visually)?
boxplot(income_k ~ state, data = data_new, col=c(rgb(1,.8,1, .5),rgb(.8,1,1, .5))) 
# No not really that different.

# 11 - show the box plots of wscore for each race (all in one plot) -- any race having a considerably different wscore (judging visually)?
boxplot(wscore ~ race, data = data_new, col=c(rgb(1,.8,1, .5),rgb(.8,1,1, .5)))
# No significant difference of wscore based on race

# 12 - depict the scatterplot of wscore vs. age
# hint: use 'pch = 20' and 'cex = 0.2' as additional parameters to make it visually appealing
plot(wscore ~ age, data = data_new, pch=20, cex = .2, xlab='Age', ylab='wscore')  

# 13 - show the histogram of current cost (cost_c) -- is it normally distributed (judging visually)?
hist(data_new$cost_c, main='', xlab='current cost', col='lightblue', labels=TRUE) 
# No it is right skewed

# 14 - LOG transform current cost and see if the distribution has changed (visually)
data_new$logcost_c <- log(data_new$cost_c)
hist(data_new$logcost_c, main='', xlab='log current cost', col='lightblue', labels=TRUE)
# Yes it looks about normal now

# 15 - create two columns to store the LOG transformation of current and future cost and add them to the data_new table
# hint: call these columns as cost_c_log & cost_f_log
# hint: make sure that none of the costs are equal to 0 before creating these two new columns
data_new <- data_new[cost_c >= 0, ]
data_new <- data_new[cost_f >= 0, ]
data_new$cost_c_log <- log(data_new$cost_c)
data_new$cost_f_log <- log(data_new$cost_f)

# 16 - depict the QQ plot of cost_f_log (also show the QQ line on the plot)
# hint: specific commands are available for QQ plot + QQ line
# hint: use 'pch = 20' and 'cex = 0.2' as additional parameters to make it visually appealing
qqnorm(data_new$cost_f_log, pch = 20, cex = .2, main = '') 
qqline(data_new$cost_f_log[1:5000], col = "red")  

# 17 - depict the scatterplot of wscore vs. cost_c_log
# hint: use 'pch = 20' and 'cex = 0.2' as additional parameters to make it visually appealing
plot(wscore ~ cost_c_log, data = data_new, pch=20, cex = .2, xlab='cost_c_log', ylab='wscore')  

# 18 - add a regression line to the previous scatterplot
# hint: use a simple linear regression of cost_c_log vs. wscore and then pass it to the 'abline' command
abline(lm(wscore ~ cost_c_log, data = data_new))    

# 19 - what is the coefficient of wscore in the prior regression line
m_lm_tax = lm(wscore ~ cost_c_log, data = data_new)  
summary(m_lm_tax) 
coef(m_lm_tax) 

# 20 - depict the scatterplot of cost_c_log vs. cost_f_log + add a linear regression line to the plot
plot(cost_c_log ~ cost_f_log, data = data_new, pch=20, cex = .2, xlab='cost_c_log', ylab='wscore')
abline(lm(cost_c_log ~ cost_f_log, data = data_new))    

# 21 - add a new column to data_new table, and for each patient insert 'red' if sex is F, and 'blue' if sex is M
# hint: call this new column sex_color
data_new$sex_color[data_new$sex == "F"] <- "red"
data_new$sex_color[data_new$sex == "M"] <- "blue"

# 22 - redo the previous scatterplot + regression line, but show the sex_color to each of the dots (i.e., blue vs. red dots)
# hint: use the 'col' parameter to plot the colors
plot(cost_c_log ~ cost_f_log, data = data_new, pch=20, cex = .2, xlab='cost_c_log', ylab='wscore', col=data_new$sex_color)
abline(lm(cost_c_log ~ cost_f_log, data = data_new)) 

# 23 - calculate the top 1% of cost for current and future costs (using the raw cost value; do not use the LOG transformed ones)
# hint: use a probability of .99 in the quantile function
quantile(data_new$cost_c, seq(0,1,.01))
#65804 top 1% of current cost
quantile(data_new$cost_f, seq(0,1,.01))
#44312 top 1% of future cost

# 24 - add two new columns to data_new table indicating if the patient had a current or future cost in top 1% of not (1 for being in top 1%)
# hint: call these columns t1c and t1f (they should only contain 0 or 1)
# hint: use the ifelse command in data.table to assign 1s and 0s depending on cost vs. cost_t1 values (both current and future numbers)
data_new$sex_t1c[data_new$cost_c >= 65804] <- 1
data_new$sex_t1c[data_new$cost_c <= 65804] <- 0

data_new$sex_t1f[data_new$cost_f >= 65804] <- 1
data_new$sex_t1f[data_new$cost_f <= 65804] <- 0

# 25 - make sure that ip_c, ip_f, er_c, and er_f outcomes are binary
# hint: just show the unique values in each of these columns
unique(data_new$ip_c)
unique(data_new$ip_f)
unique(data_new$er_c)
unique(data_new$er_f)
#confirmed


# -------------------------------------------------------------
# question group (D) [2x15 = 30 pts] :: linear regressions
# -------------------------------------------------------------
# hint: see code_slide9.R in CoursePlus online library for sample code of linear regression + cross-validation process

# 1 - develop a linear regression line to predict cost_c_log using age, sex, race, plan, wscore, and income_k -- can your model explain at least 50% of the variance in the outcome?
# hint: use a simple linear regression command
# hint: give the adj-R2 as an R comment (like this hint!)
cost_c_log_mod <- lm(cost_c_log ~ age + sex + race + plan + wscore + income_k, data = data_new)
summary(cost_c_log_mod)
#0.4435 adjusted rsquared

# 2 - which predictor had the highest coefficient? (provide your answer as an R comment)
#Medicaid plan

# 3 - should any of the predictors be exluded from the regression model? (provide your answer as an R comment)
# no, they should all be included. Each predictor has a statistically significant coefficient

# 4 - develop a linear regression line to predict cost_f_log using age, sex, race, plan, wscore, and income_k
cost_f_log_mod <- lm(cost_f_log ~ age + sex + race + plan + wscore + income_k, data = data_new)
summary(cost_f_log_mod)

# 5 - did your previous model achieve a higher explanation of the outcome's variance compared to model developed in # 1
# No it acheived a lower explanation of the variance, because adjusted rsquared was lower

# 6 - predict cost_f_log using only cost_c_log -- is this model better than the model developed in # 4?
cost_f_log2_mod <- lm(cost_f_log ~ cost_c_log, data = data_new)
summary(cost_f_log2_mod)

# 7 - add cost_c_log as a predictor to model developed in # 4 -- did your model's prediction power improve?
cost_f_log3_mod <- lm(cost_f_log ~ age + sex + race + plan + wscore + income_k + cost_c_log, data = data_new)
summary(cost_f_log3_mod)
# yes the prediction improved

# 8 - why your new model's predictive power is not the sum of predictive powers in #4 and #6 ?
# hint: why adding cost_c_log to the rest of the predictors did not improve the adjusted R2 by .2374
# Because they arent the same thing, cost_c_log on its own does not adjust for all other variables, while running it with multiple variables does

# 9 - based on the last model (developed in # 7), which race has the highest significant coefficient?
# raceB

# 10 - print the diagnostic plots of the last regression model (developed in # 7) -- do you see any patterns in the residual vs. fitted plot?
plot(cost_f_log3_mod)  
# Fairly even residuals with a bit more variance at the higher fitted values

# 11 - cut your data into two halves for training and validation purposes (50% - 50%)
# hint: use seed = 299 for random sampling purposes
# hint: use a random 50% of your data in each bin (instead of cutting it in the middle)
# (2) concurrent :: train + validation --> split-half
                                                                                                    # using a fixed seed number for random comments (to get the same results in the each run)
train_id_50x50 = sample(1:data_new[, .N], size = floor(data_new[, .N]*.5), replace = FALSE)                          # randomly give 50% of the rows to the train_id list
data_new_train_50x50 = data_new[train_id_50x50, ]                                                                    # creating a new data.table based on any row that has train_id rows
data_new_valid_50x50 = data_new[-train_id_50x50, ]                                                                   # creating a new data.table based on any row that has NOT train_id rows
data_new[, .N] == data_new_train_50x50[, .N] + data_new_valid_50x50[, .N]                                            # double checking that the number of rows are equal (8393 = 5875 + 2518) [note that this is a == sign (not one equal) to check as a boolean test]

# 12 - retrain the linear regression model developed in #7 using the training half of the data -- what is the training R2? (simple R2; not adjusted)
m_cost_train_50x50 = lm(cost_f_log ~ age + sex + race + plan + wscore + income_k + cost_c_log, data = data_new_train_50x50)       # model training on training set (50%) only
summary(m_cost_train_50x50) 

# 13 - predict the values of the validation half of the data and store them back in the validation table
# hint: name the new column: cost_f_log_predict
data_new_train_50x50$cost_f_log_predict = predict(object = m_cost_train_50x50,                           # predicting outcome (current cost) in the validation dataset (50% of the data)
                                                              newdata = data_new_valid_50x50)
summary(data_new_train_50x50$cost_f_log_predict)
# 14 - calculate the R2 of the model using the prediction results stored in the validation table -- what is the validation R2?
# hint: find the formula to calculate R2 in the validation data set in the past screen recordings / mini-assignments
# hint: reminder about the formula: R2 = 1 - ( sum((predicted - observed)^2) / sum((observed - mean(observed))^2) )
# hint: you only need the validation dataset's predicted and observed outcomes to calculate R2
func_r2 <- function(prd, obs)																						                                             # function to caluclate R2 for the validation dataset
{
  rss = sum((prd - obs)^2)                                       
  tss = sum((obs - mean(obs))^2)
  r2 = 1 - (rss/tss)
  return(r2)
}																				                                             # function to caluclate R2 for the validation dataset
func_r2(data_new_valid_50x50$cost_f_log_predict, data_new_valid_50x50$cost_f_log)

# 15 - did you achieve a higher or lower R2 when applying the model on the validation dataset? what could be the potential reason?
# Lower, less sample size

# -------------------------------------------------------------
# question group (E) [2x10 = 20 pts] :: logistic regressions
# -------------------------------------------------------------
# hint: see code_slide10.R in CoursePlus online library for sample code for logistic regression


# 1 - predict current inpatient admission (ip_c) using age, sex, race, plan, wscore, and income_k -- what is the AIC of the model?
# hint: develop a binomial model using the generalized linear methodology (logistic model)

# 2 - store the predicted values of ip_c in the data_new variable/table
# hint: name the new column ip_c_pred
# hint: you can either use the 'predict' command or other means to get the results

# 3 - calculate the AUC of the model -- compared to a flip of a coin, how much better you are predicting the current inpatient admissions (ip_c)?
# hint: you can use the pROC library

# 4 - plot the ROC curve of the model, and print the 'thresholds' on it 

# 5 - redo the model developed in #1, however add cost_c_log to the list or predictors -- what is the AIC of the new model?

# 6 - is the AIC of the model developed in #5 more or less than the model developed in #1? what can be concluded?

# 7 - using the model developed in #5, store the predicted values of ip_c in the data_new table
# hint: name the new column as ip_c_pred2

# 8 - calculate the AUC of the model -- compared to a flip of a coin, how much better you are predicting the current inpatient admissions (ip_c)?
# hint: you can use the pROC library

# 9 - plot the ROC curve of the model, and print the 'thresholds' on it 

# 10 - print the most optimal threshold that create the maximum sensitivity and specificity at the same time -- does your model achieve a higher sensitivityor a higher specificity?

# -------------------------------------------------------------
# question group (F) [1x10 = 10 pts] :: penalized regressions
# -------------------------------------------------------------
# hint: see code_slide11.R in CoursePlus online library for sample code of penalized regression

# 1 - create two separate data matrices: one data matrix containing cost_f_log; and, another data matrix containing age, wscore, sex, plan, race, income_k, cost_c_log, and state 
# hint: data matrix only accepts numerical and factor-basis variables -- convert categorical ones to factor using as.factor command
# hint: call the matrix containing cost_f_log as "data_y"; and the other matrix "data_x"

# 2 - find the 1se (1 standard deviation) lambda using data_x and data_y matrices  (assuming a lasso regression)
# hint: use glmnet library
# hint: use seed = 100
# hint: use type.measure = 'mse' and nfolds = 10 parameters in the command to calcualte the lambda(s)

# 3 - plot the results of the lambda calculation, and try to find the 1-se on the diagram -- how many variables are kept at this level of lambda?

# 4 - execute a lasso penalized regression using data_x and data_y, but no specific lambda 
# hint: it is a lasso regression -- use the appropriate alpha
# hint: use seed = 100

# 5 - plot the model from the last question and try to find log of 1se lambda on it (visually or by drawing a line)
# hint: the plot function should include the xvar=c('lambda') parameter

# 6 - execute a lasso penalized regression using the 1se lambda found in question # 2 -- show the degree of freedom in the model
# hint: no need for train/validation
# hint: it is a lasso regression -- use the appropriate alpha
# hint: use seed = 100
# hint: degree of freedom (Df) is stored in the variables that holds the regression results (simply print the variable)

# 7 - print the coefficients of the lasso model (question #6) -- how many of the predictors are left in the model (except the intercept), and which predictors were dropped?

# 8 - caluclate the R2 and RMSE of the lasso model developed in question #6
# hint: predict using the predictors in data_x, and then compare results with data_y to calcualte the measures

# 9 - conduct a simple linear regression predicting cost_f_log using age, wscore, plan, race, and cost_c_log -- what is your R2? 

# 10 - compare the R2s from #8 to #9; which one is larger? does this mean that penalized (lasso) regression is weaker?

# -------------------------------------------------------------
# question group (G) [2x10 = 20 pts] :: multi-level regressions
# -------------------------------------------------------------
# hint: see code_slide12.R in CoursePlus online library for sample code of multi-level regression

# 1 - plot cost_c_log against income_k (scatterplot), but give each of the dots a different color based patient's state
# hint: find the code doing a very similar plot in the multi-level screen recording

# 2 - add regressions lines of cost_c_log vs. income_k for each of the states to the previous plot
# hint: find the code doing a very similar plot in the multi-level screen recording

# 3 - develop a linear regression model to predict cost_c_log using income_k -- what is your R2?

# 4 - develop a linear regression model to predict cost_c_log using income_k and state -- what is your R2?

# 5 - develop a multi-level regression model to predict cost_c_log using income_k while using state as the grouper for random intercepts -- what is your 'conditional' R2?
# hint: use the lme4, lmerTest, and sjstats packages

# 6 - comparing the R2 in #4 and #5, did the multi-level modeling improve the predictive power of the model?

# 7 - develop a linear regression model to predict cost_c_log using age, wscore, sex, plan, race, income_k, and state -- what is your R2?

# 8 - develop a multi-level regression model to predict cost_c_log using same predictors of #7 but using state as the grouper for random intercepts -- what is your 'conditional' R2?

# 9 - comparing the R2 in #7 and #8, did the multi-level modeling improve the predictive power of the model?

# 10 - try redoing #8 but adding income_k as a random intercept as well -- can you get the conditional R2? (what is "isSingular"?)

# -------------------------------------------------------------
# question group (H) [4x5 = 20 points] :: supervised learning
# -------------------------------------------------------------
# hint: see code_slide13.R in CoursePlus online library for sample code of supervised learning AI

# 1 - develop a logistic regression to predict ip_f based on age, sex, race, plan, wscore, income_k, and ip_c -- calculate the AUC [no need for cross-validation]
# hint: see the code used multiple times in the recordings to predict the values, apply the ROC function, and then calculate the AUC
m_ipf <- glm(ip_f ~ age + sex + race + plan + wscore + income_k + ip_c, data = data_new, family='binomial')
summary(m_ipf)
library(pROC)
data_new$ipf_predict = predict(m_ipf, type=c("response"))
m_ipf_roc <- roc(ip_f ~ ipf_predict, data = data_new)
plot(m_ipf_roc)
as.numeric(m_ipf_roc$auc) # 0.655

# 2 - develop a decision tree to predict ip_f using the same predictors (use minsplit = 30 & cp = .001) -- calculate AUC & plot the tree [no need for cross-validation]
# hint: see the code used multiple times in the recordings to predict the values, apply the ROC function, and then calculate the AUC
library(rpart)
library(rpart.plot)
m_erf_dt <- rpart(formula = ip_f ~ age + sex + race + plan + wscore + income_k + ip_c, data = data_new, method = 'class', 
                  control = rpart.control(minsplit = 30, cp = .001))
rpart.rules(m_erf_dt)
rpart.plot(m_erf_dt, extra = 1)
data_new$erf_dt_predict = as.data.table(predict(m_erf_dt, data_new, type = c('prob')))[[2]]
m_erf_dt_roc <- roc(ip_f ~ erf_dt_predict, data = data_new)
plot(m_erf_dt_roc)
as.numeric(m_erf_dt_roc$auc) # 0.644

# (3) develop a random forrest to predict ip_f using the same predictors (use ntree = 100 & mtry = 2 & seed = 100) -- calculate AUC & plot the OOB errors  [no need for cross-validation]
# hint: see the code used multiple times in the recordings to predict the values, apply the ROC function, and then calculate the AUC
# hint: make sure to create factor-type columns for categorical variables (sex, race, and plan)
library(randomForest)
data_new[, ip_f_factor := as.factor(ip_f)]
data_new[, sex_factor := as.factor(sex)]
data_new[, race_factor := as.factor(race)]
data_new[, plan_factor := as.factor(plan)]
m_ip_f_rf <- randomForest(ip_f_factor ~ age + sex_factor + race_factor + plan_factor + wscore + income_k + ip_c, data = data_new, ntree = 100, mtry = 2)
m_ip_f_rf_roc<-roc(data_new$ip_f_factor,m_ip_f_rf$votes[,2])
plot(m_ip_f_rf_roc)
auc(m_ip_f_rf_roc) # 0.6278
errors <- as.data.table(m_ip_f_rf$err.rate)
plot(errors$OOB)
lines(errors$OOB)

# (4) develop a neural network to predict ip_f using the same predictors (use hidden layer = 3 & seed = 100 & other parameters used in the recording) -- calculate AUC & plot the model [no need for cross-validation]
# hint: make sure to create numeric columns for categorical variables (sex, race, and plan)
library(neuralnet)
set.seed(100)
data_new[, sex_num := as.numeric(sex_factor)]
data_new[, race_num := as.numeric(race_factor)]
data_new[, plan_num := as.numeric(plan_factor)]
m_erf_nn4 <- neuralnet(formula = ip_f ~ age + sex_num + race_num + plan_num + wscore + income_k + ip_c, data = data_new, hidden=c(3), act.fct="logistic", err.fct="ce", rep=10, stepmax=10000, lifesign="minimal", lifesign.step=5000, threshold=2, linear.output=F)
plot(m_erf_nn4, rep="best", show.weights=T, intercept=F)
m_erf_nn4_predict <- compute(m_erf_nn4, data_new)
m_erf_nn4_roc <- roc(data_new$ip_f, as.numeric(m_erf_nn4_predict$net.result))
auc(m_erf_nn4_roc) # 0.6449
plot(m_erf_nn4_roc)

# (X) which model generated the highest AUC? (not graded)
# the neural network

# -------------------------------------------------------------
# question group (I) [2x5 = 10 points] :: unsupervised learning
# -------------------------------------------------------------
# hint: see code_slide14.R in CoursePlus online library for sample code of unsupervised learning AI

# (1) run a k-means algorithm to find clusters of cost_f vs. cost_c -- what k is optimal? 
# hint: develop a 'for' loop to test k ranging from 1 to 20; and then select the optimal k based on sum of squares (within and between)
# hint: set seed to 100; iter.max = 100; and, nstart = 1
# clustering 2 variables
k = c()
ss_within = c()
ss_between = c()

for(i in 1:20)
{
  set.seed(100)
  m_km = kmeans(x = data_new[, .(cost_f, cost_c)], centers = i, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
  k = c(k, i)
  ss_within = c(ss_within, m_km$tot.withinss)
  ss_between = c(ss_between, m_km$betweenss)
}

par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(k, ss_within)
lines(ss_within)
plot(k, ss_between)
lines(ss_between)

#K of 5 is optimal

# (2) use the optional k from the previous question to find the clusters (set seed = 100)
# hint: add the cluster information to a linear regression predicting cost_f (future total cost log) using age, sex, race, plan, wscore, income_k, and cost_c
# hint: compare the adj-R2 before and after adding the cluster information to the regression -- did your model improve?
# the adjusted R2 improved after adding the 'clust' variable to the model

set.seed(100)
m_km = kmeans(x = data_new[, .(cost_f, cost_c)], centers = 5, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
data_new$clust = m_km$cluster
summary(lm(cost_f ~ age + sex + race + plan + wscore + income_k + cost_c, data = data_new)) # .1467
summary(lm(cost_f ~ age + sex + race + plan + wscore + income_k + cost_c + clust, data = data_new)) # .1998
# Yes, the R2 improved to 0.1998


