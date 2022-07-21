# ============================================================ 
# 	Mini-assignment # 13
# ============================================================ 

# load the data_pat_reg.csv file and perform the following modeling (no train/validation in any of the questions)
# create duplicate columns with these abbreviated column names before answering the questions
# ============================================================ 
# Sup-ML :: Loading data
# ============================================================ 

library(data.table)                                                                                                     # activating the data.table library 
data_pat <- fread(file = "data_pat_reg.csv")                                          # loading the data -- please adjust path to file based on your folder structure
data_pat$ipf = data_pat$admit_flg_future
data_pat$ipc = data_pat$admit_flg_current
# erf = er_visit_flg_future
# erc = er_visit_flg_current
data_pat$erf = data_pat$er_visit_flg_future
data_pat$erc = data_pat$er_visit_flg_current
data_pat$ctf = data_pat$cost_future_total_log
data_pat$ctc = data_pat$cost_current_total_log
colnames(data_pat)
data_pat[, .N]                                                                       # ctc == current total cost (log)
colnames(data_pat)                                                                                                      # showing the column names of this data.table

# (1) develop a logistic model (glm) to predict erc using age, sex, and hcc_ct -- calculate AUC
m_erc <- glm(erc ~ age + sex + hcc_ct, data = data_pat, family='binomial')
summary(m_erc)
library(pROC)
data_pat$erc_predict = predict(m_erc, type=c("response"))
m_erc_roc <- roc(erc ~ erc_predict, data = data_pat)
plot(m_erc_roc)
as.numeric(m_erc_roc$auc) # 0.599

# (2) develop a decision tree to predict erc using the same predictors (use minsplit = 5 & cp = .001) -- calculate AUC & plot the tree
library(rpart)
library(rpart.plot)
m_erc_dt <- rpart(formula = erc ~ age + sex + hcc_ct, data = data_pat, method = 'class', 
                  control = rpart.control(minsplit = 5, cp = .001))
rpart.rules(m_erc_dt)
rpart.plot(m_erc_dt, extra = 1)
data_pat$erc_dt_predict = as.data.table(predict(m_erc_dt, data_pat, type = c('prob')))[[2]]
m_erc_dt_roc <- roc(erc ~ erc_dt_predict, data = data_pat)
plot(m_erc_dt_roc)
as.numeric(m_erc_dt_roc$auc) # 0.5123

# (3) develop a random forrest to predict erc using the same predictors (use ntree = 10 & mtry = 2 & seed = 100) -- calculate AUC & plot the OOB errors
library(randomForest)
data_pat[, erc_factor := as.factor(erc)]
data_pat[, sex_factor := as.factor(sex)]
m_erc_rf <- randomForest(erc_factor ~ age + sex_factor + hcc_ct, data = data_pat, ntree = 10, mtry = 2)
m_erc_rf_roc<-roc(data_pat$erc_factor,m_erc_rf$votes[,2])
plot(m_erc_rf_roc)
auc(m_erc_rf_roc) # 0.511
errors <- as.data.table(m_erc_rf$err.rate)
plot(errors$OOB)
lines(errors$OOB)

# (4) develop a neural network to predict erc using the same predictors (use hidden layer = 2 & seed = 100 & other parameters used in the recording) -- calculate AUC & plot the modelcalculate AUC & plot the model
library(neuralnet)
set.seed(100)
m_erc_nn4 <- neuralnet(formula = erc ~ age + sex_num + hcc_ct, data=data_pat, hidden=c(2), act.fct="logistic", err.fct="ce", rep=10, stepmax=10000, lifesign="minimal", lifesign.step=5000, threshold=2, linear.output=F)
plot(m_erc_nn4, rep="best", show.weights=T, intercept=F)
m_erc_nn4_predict <- compute(m_erc_nn4, data_pat)
m_erc_nn4_roc <- roc(data_pat$erc, as.numeric(m_erc_nn4_predict$net.result))
auc(m_erc_nn4_roc) # 0.5519
plot(m_erc_nn4_roc)

# (5) which model performed the best? (regardless of overfitting and cross-validation issues)
#Model 1 with the highest AUC 0.599
