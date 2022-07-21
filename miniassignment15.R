# ============================================================ 
# 	Mini-assignment # 15
# ============================================================ 

# load the data_pat_reg.csv file and perform the following visualization techniques
# ============================================================ 
# Viz :: Loading data
# ============================================================ 

library(data.table)                                                                                                     # initializing the data.table library (make sure to install if not already done so)
data_pat <- fread('data_pat_reg.csv')                                         # loading the data (make sure to adjust path to the csv file based on your local folders)
# create a sex_color column (red for F and blue for M)
data_pat$sex_color = ifelse(data_pat$sex == 'F', 'red', 'blue')                                                         # adding a column to the dataset (F > red, M > blue)                                                                        # adding abbreviated column names
# create abbreviated columns of ctc and ctf for log of current/future total cost
data_pat$ctc <- data_pat$cost_current_total_log                                                                         # adding abbreviated column names
data_pat$ctf <- data_pat$cost_future_total_log 

# (1) plot the box plot of ctc for each of the hcc counts
boxplot(ctc ~ hcc_ct, data = data_pat)
# do you see a trend? 
# Yes, increasing log cost with an increase in hcc count

# (2) plot the scatterplot of ctf vs. ctc for all females over 65 + add a regression line to it
plot(ctf ~ ctc, data = data_pat[sex == 'F' & age > 65])
abline(lm(ctf ~ ctc, data = data_pat[sex == 'F' & age > 65]))
# do you see a trend?
# Yes, an increase in log future cost with an increase in log current cost

# (3) plot a 3 dimensional scatterplot (using lattice) of hcc_ct (z) vs. ctf vs. ctc colored by pcp_visit_flg_current
# create flg_color column to distinguish pcp_visit_flg_current == 1 vs. 0
# use z = 0 and x = -60 for the 3d plot
library(lattice)
data_pat[, flg_color := 'lightblue']
data_pat[pcp_visit_flg_current == 1, flg_color := 'red']
cloud(hcc_ct ~ ctc * ctf, data = data_pat, col = data_pat$flg_color, screen = list(z=0, x=-60))
# do you see a trend between ctc and pcp_visit_flg_current?
# Yes, patients who have pcp visit flg currently have higher current and future costs

# (4) redo the same plot as #3 using the plotly command
library(plotly) 
plot_ly(data = data_pat, type = 'scatter3d', mode = 'markers', x = ~ctc, y = ~ctf, z = ~hcc_ct, marker = list(size = 2, color = data_pat$flg_color))
# do you see a trend between ctf and pcp_visit_flg_current? 
# yes there are higher log future costs in the patients with pcp visit flags.
