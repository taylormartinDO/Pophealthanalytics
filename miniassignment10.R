# ============================================================ 
# 	Mini-assignment # 10
# ============================================================ 

# load the data_pat_reg.csv file and perform the following modeling
# do not drop any rows/patients from this dataset throughout your analysis
library(data.table)
data_pat <- fread(file = "data_pat_reg.csv")
colnames(data_pat)
data_pat[, .N]

# (1) predict pcp_visit_flg_future (rename it to pcpf) using age, sex, hcc_ct, pcp_visit_flg_current (rename it to pcpc), and zip_income
# call your model m_pcpf
data_pat[, unique(pcp_visit_flg_future)]
data_pat[, unique(pcp_visit_flg_current)]
data_pat$pcpf = data_pat$pcp_visit_flg_future
data_pat$pcpc = data_pat$pcp_visit_flg_current
colnames(data_pat)

m_pcpf <- glm(pcpf ~ age + sex + hcc_ct + pcpc + zip_income, data = data_pat, family = "binomial")
summary(m_pcpf)

# (2) plot roc, calculate auc, best sens/spec for question # 1 (no validation set needed)
library(pROC) 

data_pat$pcpf_predict = predict(m_pcpf, type=c("response"))
m_pcpf_roc <- roc(pcpf ~ pcpf_predict, data = data_pat)
plot(m_pcpf_roc, print.thres=TRUE)
as.numeric(m_pcpf_roc$auc) # .6314
coords(m_pcpf_roc, 'best', 'threshold', transpose = FALSE)

# (3) train the model based on a random 50% validation set (set seed to 100)
# set the seed to 100; call your model m_pcpf_50x50
set.seed(100)
train_id_50x50 = sample(1:data_pat[, .N], size = floor(data_pat[, .N]*.5), replace = FALSE)
data_pat_train_50x50 = data_pat[train_id_50x50, ]
data_pat_valid_50x50 = data_pat[-train_id_50x50, ]

m_pcpf_50x50 = glm(pcpf ~ age + sex + hcc_ct + pcpc + zip_income , data = data_pat_train_50x50, family = "binomial")


# (4) plot roc, calculate auc, best sens/spec for question # 3 using the validation data -- did auc improve compared to # 2? what was the reason for the change?
data_pat_valid_50x50$pcpf_predict = predict(m_pcpf_50x50, newdata = data_pat_valid_50x50, type=c("response"))
m_pcpf_50x50_valid <- roc(pcpf ~ pcpf_predict, data = data_pat_valid_50x50)
plot(m_pcpf_50x50_valid, print.thres='best')
as.numeric(m_pcpf_50x50_valid$auc) # .5968

#yes there was a little bit of improvement from 0.92 to 0.93. This was with a 50% training set. The AUC could possibly be increased by even more training (70% training, 30% validation)
