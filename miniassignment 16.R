# ============================================================ 
# 	Mini-assignment # 16
# ============================================================ 

# load the data_pat_all.csv file and perform the following analytics
library(data.table)                                                                                                         # initializing the data.table library (make sure to install if not already done so)
data_pat <- fread('data_pat_all.csv')                                             # loading the data (make sure to adjust path to the csv file based on your local folders)

# (1) plot the box plot of ctf for each of the hcc counts (in one command)
boxplot(ctf ~ hcc_ct, data = data_pat)

# (2) develop a linear regression to predict ctf based on age, sex, plan, hcc_ct, and urban_rural predictors -- what is your R2? which variables are not significant predictors?
m_lm <- lm(ctf ~ age + sex + plan + hcc_ct + urban_rural, data = data_pat)
summary(m_lm) # R2 = .2008 & planSF and urban_rural are not significant predictors

# (3) develop a logistic regression to predict t1f based on age, sex, plan, hcc_ct, urban_rural, and ctf predictors -- what went wrong? explain the issue by a boxplot
m_log <- glm(t1f ~ age + sex + plan + hcc_ct + urban_rural + ctf, data = data_pat, family = 'binomial')
# can't use ctf to predict t1f -- there is a total separation of outcomes based on ctf
boxplot(ctf ~ t1f, data = data_pat) # the two boxplots are totally separated

# (4) develop a logistic regression to predict t1f based on age, sex, plan, hcc_ct, and urban_rural predictors -- what is your AUC? which variable is the most significant predictor?
m_log_t1f <- glm(t1f ~ age + sex + plan + hcc_ct + urban_rural, data = data_pat, family = 'binomial')
summary(m_log_t1f) # hcc_ct is the most significant predictor
library(pROC)
data_pat$t1f_predict = predict(m_log_t1f, type=c("response"))
m_log_t1f_roc <- roc(t1f ~ t1f_predict, data = data_pat)
as.numeric(m_log_t1f_roc$auc) # AUC = .862

# (5) plot the box plot of hcc_ct for each t1f category (in one command) -- does this explain your logistic regression results?
boxplot(hcc_ct ~ t1f, data = data_pat) # t1f group has a much higher hcc_ct compared to 0 ... there is almost a total separation, hence the high importance of hcc_ct and high AUC

