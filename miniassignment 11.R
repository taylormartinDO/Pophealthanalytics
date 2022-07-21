# ============================================================ 
# 	Mini-assignment # 11
# ============================================================ 
# install.packages("MASS")
library(MASS)
m_cft_full <- lm(cost_future_total_log ~ age + sex_num + hcc_ct + plan_num + cost_current_total_log + urban_rural + zip_income, data = data_pat)
summary(m_cft_full)
m_cft_full_forw <- stepAIC(object = m_cft_full, direction = 'forward')                                                        # forward stepwise regression
summary(m_cft_full_forw)
m_cft_full_back <- stepAIC(object = m_cft_full, direction = 'backward')                                                       # backward stepwise regression
summary(m_cft_full_back)



# load the data_pat_reg.csv file and perform the following modeling
library(data.table)                                                                                                     # activating the data.table library 
data_pat <- fread(file = "data_pat_reg.csv") # loading the data -- please adjust path to file based on your folder structure
colnames(data_pat)                                                                                                      # showing the column names of this data.table

# (1) predict cost_future_pr (log) using age, sex_num, hcc_ct, plan_num, urban_rural, zip_income (no penalization; simple linear regression) [make sure to drop rows with outcome = 0]
# calcualte the adj-r-square and RMSE of the model
data_pat <- data_pat[cost_future_rx > 0, ]
data_pat[, .N]
data_pat[, cost_future_rx_log := log(cost_future_rx)]
m_full <- lm(cost_future_rx_log ~ age + sex_num + hcc_ct + plan_num + urban_rural + zip_income, data = data_pat)
length(coef(m_full)) # 7 (includes intercept)
summary(m_full)$adj.r.sq # Adj-r-square = 0.2117406
sqrt(mean(m_full$residuals^2)) # RMSE = 1.547695 
sqrt(mean((data_pat$cost_future_rx_log - m_full$fitted.values)^2))

# (2) repeat #1 but use backward stepwise regression to prune the predictors -- does it give you a more optimal model (fewer predictors, higher R2 and/or lower RMSE)?
# calcualte the adj-r-square and RMSE of the model
# This model is a bit more parsimonious with 1 less predictor (urban_rural). Otherwise, these 2 models are relatively the same, this models rsuared and RSME are a small bit larger than model1. 
library(MASS)
m_back <- stepAIC(object = m_full, direction = 'backward')
length(coef(m_back)) # 6
summary(m_back)$adj.r.sq # Adj-r-square = 0.211788
sqrt(mean(m_back$residuals^2)) # RMSE = 1.547865

# (3) repeat #1 but use lasso regression -- does it give you a more optimal model (fewer predictors while having a higher R2 and/or lower RMSE)? (use seed = 100; nfolds = 10; lambda.1se)
# calcualte the adj-r-square and RMSE of the model
# of the models so far, this has the lowest adj-r-square and highest RMSE
library(glmnet) 
data_x = data.matrix(data_pat[, .(age, sex_num, hcc_ct, plan_num, urban_rural, zip_income)])
data_y = data.matrix(data_pat[, .(cost_future_rx_log)])

set.seed(100)
m_lasso_cv <- cv.glmnet(x = data_x, y = data_y, family = 'gaussian', alpha = 1, type.measure = 'mse', nfolds = 10) 
set.seed(100)
m_lasso <- glmnet(x = data_x, y = data_y, family = 'gaussian', alpha = 1, lambda = m_lasso_cv$lambda.1se)
length(coef(m_lasso)) # 7
lasso_pred <- predict(m_lasso, newx=data_x)
1 - sum((data_y - lasso_pred)^2) / sum((data_y - mean(data_y))^2) # 0.1915545
sqrt(mean((data_y - lasso_pred)^2)) # 1.568702

# (4) which model gave you the highest performance; which model performances are more reliable if the model is applied to external data? (no need to calcualte anything; just discuss)
#Model 2 is the highest performing model with the largest rsquared. It is the most parsimonious. It does have a slightly larger RSME than model 1, which means that model 1 is probably the most reliable model when applied to external data. 
  



