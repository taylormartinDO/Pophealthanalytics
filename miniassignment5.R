# ============================================================ 
# 	Mini-assignment # 5
# ============================================================ 

# using the following data.table, calculate the following (use built-in data.table functions):
set.seed(1234)
id = 1:5000
age = abs(rnorm(5000,40,20))
race = sample(c('W','B','A',NA),5000,replace=T)
dxct = floor(sample(1:3,5000,replace=T)*(age/10))
cost = rnbinom(5000, size = 1, mu = 10000)*age
dt = data.table(id = id, age = age, race = race, dxct = dxct, cost = cost)

# (1) count patients with missing race info
5000 - ((dt[race == 'W', .N]) + (dt[race == 'B', .N]) + (dt[race == 'A', .N]))

# (2) count patients designated as Asian, more than 65 with a cost over 10k
dt[race == 'A' & age > 65 & cost > 10000, .N]
dt[race == 'A'][age > 65][cost > 10000, .N] # stacking format

# (3) count patients over 65 with either a cost over 500k "or" more than 5 diagnoses
dt[age > 65][cost > 500000 | dxct > 5, .N] # stacking format

# (4) count patients in the top 1% of cost (use the quantile command)
dt[cost > quantile(dt$cost, c(.99)), .N]

# (5) calculate the following per race: count of patients, average and SD of age, average and SD of number of diseases, and average and SD of cost [using built-in data.table functions]
dt[, .(ct = .N, age_avg = mean(age), age_SD = sd(age), dxct_avg = mean(dxct), dxct_SD = sd(dxct), cost_avg = mean(cost), cost_SD = sd(cost)), by = race]

# (6) join the data.table with a new data.table that provides some lab information (explain the "NA" in the new merged data.table)
id = 1000:5000
lab = round(runif(4001, 50, 100))
dt_lab = data.table(id = id, lab = lab)

merge(dt, dt_lab, by = c('id'), all = T)
