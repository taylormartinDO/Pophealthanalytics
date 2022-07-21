
# ============================================================ 
# 	Mini-assignment # 9
# ============================================================ 

library(moments)    

# variables of interest: Y = cost_future_total_log ~ Xs = age, sex, plan, urban_rural, hcc_ct, zip_income, cost_current_total_log
# continues variables:   Y = cost_future_total_log ~ Xs = age, hcc_ct, zip_income, cost_current_total_log

# linearity of data (looking at skewness and kurtosis)


# activating the "moments" library/command -- make sure that you have run this command once: install.packages("moments")

par(mfrow=c(1,1))                                                       				      # setting a simple 1x1 plot
set.seed(100)                                                           				      # setting the seed for randomization -- to get the same results
x = rnorm(100000, mean = 0, sd = 1)                                     				      # creating a perfect normal distribution (with 100k points; mean = 0; sd = 1)
hist(x)                                                                 				      # histogram of x shows a normal distribution 
skewness(x)                                                             				      # skewness close to 0 (ideal)
kurtosis(x)  

par(mfrow=c(5,2), mar=c(2,1,1,1))                                                   	# setting a page with 5x2 diagram (to fit all of the following histograms)           
hist(data_pat$age, main = 'age', col = 'lightblue')                                 	# histogram of age (somehow skewed with certain drops)
hist(log(data_pat$age), main = 'age (log)', col = 'lightblue')                      	# histogram of age log (often complicates the interpretation of the model)
hist(data_pat$hcc_ct, main = 'hcc', col = 'lightblue')                              	# histogram of hcc (somehow skewed)
hist(log(data_pat$hcc_ct), main = 'hcc (log)', col = 'lightblue')                   	# histogram of hcc log (no specific distribution)
hist(data_pat$zip_income, main = 'income', col = 'lightblue')                       	# histogram of zip-based income (partially skewed -- can potentially be LOG transformed)  
hist(log(data_pat$zip_income), main = 'income (log)', col = 'lightblue')            	# histogram of zip-based income log (skewness is minimal)
hist(data_pat$cost_current_total, main = 'cost cur', col = 'lightblue')             	# histogram of total current cost (highly skewed)  
hist(data_pat$cost_current_total_log, main = 'cost cur (log)', col = 'lightblue')   	# histogram of total current cost (most of skewness is gone)
hist(data_pat$cost_future_total, main = 'cost fut', col = 'lightblue')              	# histogram of total future cost (highly skewed)       
hist(data_pat$cost_future_total_log, main = 'cost fut (log)', col = 'lightblue')    	# histogram of total future cost (most of skewness is gone)

skewness(data_pat[, .(age, hcc_ct, zip_income, log(zip_income), cost_current_total
                      , cost_current_total_log, cost_future_total, cost_future_total_log)])       # skewness of age (-0.346), hcc count (2.00), income based on zip (1.01), log income based on zip (-0.43), current cost total (11.52), current cost total log (-0.14) [lower than the raw current total cost], future cost total (11.60), future cost total log (-0.14) [lower than the raw future total cost]

kurtosis(data_pat[, .(age, hcc_ct, zip_income, log(zip_income), cost_current_total
                      , cost_current_total_log, cost_future_total, cost_future_total_log)])       # skewness of age (2.53), hcc count (10.25), income based on zip (6.81), log income based on zip (3.48), current cost total (202.18), current cost total log (3.19) [much lower than the raw current total cost], future cost total (216.88), future cost total log (3.22) [lower than the raw future total cost]

# kurtosis close to 3 (ideal)

# collinearity of data
plot(data_pat[1:1000, .(age, hcc_ct, zip_income, cost_current_total_log, cost_future_total_log)], pch = 20, cex = .1)    # multiple scatterplots to see the interaction of each pair of the variables

par(mfrow=c(4,2), mar=c(2,1,1,1))                                       				# setting a page with 4x2 diagram (to fit all of the following QQ diargams)            
qqnorm(data_pat$age, main = 'age')                                      				# qqplot of age (somehow close to the diagornal line)         
qqnorm(data_pat$hcc_ct, main = 'hcc')                                   				# qqplot of hcc (not close to the diagonal line; makes a specifc curve)
qqnorm(data_pat$zip_income, main = 'income')                            				# qqplot of zip-based income (off the diagonal line, with a sharp run at the right)
qqnorm(log(data_pat$zip_income), main = 'income (log)')                 				# qqplot of zip-based income log (closer to the diagonal line, but still with a sharp run at the right)
qqnorm(data_pat$cost_current_total, main = 'cost cur')                  				# qqplot of total current cost (far from the diagonal line)  
qqnorm(data_pat$cost_current_total_log, main = 'cost cur (log)')        				# qqplot of total current cost log (getting closer to the diagonal line)
qqnorm(data_pat$cost_future_total, main = 'cost fut')                   				# qqplot of total future cost (does not fit the diagonal line; which shows the normal distribution)       
qqnorm(data_pat$cost_future_total_log, main = 'cost fut (log)')         				# qqplot of total future cost log (closer to the diagonal line)


# load the data_pat_reg.csv file and perform the following modeling
# do not drop any rows/patients from this dataset throughout your analysis
library(data.table)
data_pat <- fread(file = "data_pat_reg.csv")
colnames(data_pat)
data_pat[, .N]

# (1) run a series of regressions to predict cost_future_ip, cost_future_op, cost_future_pr, cost_future_rx, and cost_future_total using age, sex, plan, hcc_ct, urban_rural, zip_income -- which of the outcomes can be predicted better? (use adj.r.square to compare)
# call your models: m_ip, m_op, m_pr, m_rx, m_tt
m_ip <- lm(cost_future_ip ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_op <- lm(cost_future_op ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_pr <- lm(cost_future_pr ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_rx <- lm(cost_future_rx ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_tt <- lm(cost_future_total ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)

summary(m_ip)$adj.r.squared
summary(m_op)$adj.r.squared
summary(m_pr)$adj.r.squared
summary(m_rx)$adj.r.squared
summary(m_tt)$adj.r.squared

#Cost future total predicts the best, having the highest rsquared

# (2) add predict cost_current_ip, cost_current_op, cost_current_pr, cost_current_rx, and cost_current_total predictors to the corresponding models developed in #1 (e.g., add cost_current_op to the model that predicts cost_future_op and so on) -- which of the models improved the most? (use adj.r.square to compare)
# call your models: m_ip_enh, m_op_enh, m_pr_enh, m_rx_enh, m_tt_enh
m_ip_enh <- lm(cost_current_ip ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_op_enh <- lm(cost_current_op ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_pr_enh <- lm(cost_current_pr ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_rx_enh <- lm(cost_current_rx ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)
m_tt_enh <- lm(cost_current_total ~ age + sex + plan + hcc_ct + urban_rural + zip_income, data = data_pat)

summary(m_ip)$adj.r.squared
summary(m_op)$adj.r.squared
summary(m_pr)$adj.r.squared
summary(m_rx)$adj.r.squared
summary(m_tt)$adj.r.squared
summary(m_ip_enh)$adj.r.squared
summary(m_op_enh)$adj.r.squared
summary(m_pr_enh)$adj.r.squared
summary(m_rx_enh)$adj.r.squared
summary(m_tt_enh)$adj.r.squared

#total cost improved the most

# (3) drop insignificant variables (@ p-value < .05) in the regression models developed in #2 and cacluate the model adj.r.square one again -- any significant drops in performance?
# call your models: m_ip_mod, m_op_mod, m_pr_mod, m_rx_mod, m_tt_mod

summary(m_ip_enh)
#hcc_ct and age
summary(m_op_enh)
#sexM and hcc_ct and age
summary(m_pr_enh)
#sexM, age, planSF,  and hcc_ct
summary(m_rx_enh)
#age, planSF, hcc_ct, zip_income
summary(m_tt_enh)
#hcc_ct and age

m_ip_mod <- lm(cost_current_ip ~ age + hcc_ct, data = data_pat)
m_op_mod <- lm(cost_current_op ~ age + sex + hcc_ct, data = data_pat)
m_pr_mod <- lm(cost_current_pr ~ age + sex + plan + hcc_ct, data = data_pat)
m_rx_mod <- lm(cost_current_rx ~ age + plan + hcc_ct + zip_income, data = data_pat)
m_tt_mod <- lm(cost_current_total ~ age + hcc_ct, data = data_pat)

summary(m_ip_mod)$adj.r.squared
summary(m_op_mod)$adj.r.squared
summary(m_pr_mod)$adj.r.squared
summary(m_rx_mod)$adj.r.squared
summary(m_tt_mod)$adj.r.squared

#no significant drops

# (4) how do you explain the list of predictors used to predict cost_future_rx, and the model's performance in #3? (elaborate on potential data issues, correlations, and other items affecting your analysis) [this question is NOT graded]
#The pvalues for all regressions were signficant. The best predictor was total cost, which was enhanced by using current instead of future. However, it was still not very predictive, with the highest rsquared of 0.21 which means 21% of cost could be explained by variables age and hcc_ct

