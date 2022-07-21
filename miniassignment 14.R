# ============================================================ 
# 	Mini-assignment # 14
# ============================================================ 

# load the data_pat_ai.csv file and perform the following unsupervised clustering
# drop all rows/patients with either cost_current_op == 0 or cost_future_op == 0 (3884 patients/rows should remain)
library(data.table)                                                                                                     # activating the data.table library 
data_pat <- fread(file = "data_pat_ai.csv") 
library(dplyr)
data_pat <- filter(data_pat, cost_current_op != 0)
data_pat <- filter(data_pat, cost_future_op != 0)
# create the following columns before answering the questions (note the log transformation)
# cco = log(cost_current_op)
data_pat$cco = log(data_pat$cost_current_op) 
# cfo = log(cost_future_op)
data_pat$cfo = log(data_pat$cost_future_op)     
                                                                       
# (1) run a k-means algorithm to find clusters of cfo vs. cco -- what k is optimal? - 5 is optimal
# develop a 'for' loop to test k ranging from 1 to 20; and then select the optimal k based on sum of squares (within and between)
# set seed to 100; iter.max = 100; and, nstart = 1
k = c()
ss_within = c()
ss_between = c()

for(i in 1:20)
{
  set.seed(100)
  m_km = kmeans(x = data_pat[, .(cfo, cco)], centers = i, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
  k = c(k, i)
  ss_within = c(ss_within, m_km$tot.withinss)
  ss_between = c(ss_between, m_km$betweenss)
}

par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(k, ss_within)
lines(ss_within)
plot(k, ss_between)
lines(ss_between)

# (2) use the optional k from #1 to find the clusters (set seed = 100)
# add the cluster information to a linear regression predicting cfo (the log of future total cost of outpatient settings) using age, hcc_ct, and cco
# compare the adj-R2 before and after adding the cluster information to the regression -- did your model improve?
set.seed(100)
m_km = kmeans(x = data_pat[, .(cfo, cco)], centers = 3, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
data_pat$clust = m_km$cluster
summary(lm(cfo ~ age + hcc_ct + cco, data = data_pat)) # .1884
summary(lm(cfo ~ age + hcc_ct + cco + clust, data = data_pat)) # .2044
# the adjusted R2 improved after adding the 'clust' variable to the model from 0.14 to 0.57

