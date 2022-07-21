
# ============================================================ 
# 	Mini-assignment # 6
# ============================================================ 

# (1) import the data_pat.csv file as a data.table
file_csv = '~/Desktop/data_pat.csv'  # comma separated values
dat1 <- read.table(file = file_csv, header = T, sep = ',')           # reading as simple table
dat2 <- read.csv(file = file_csv, header = T, sep = ',')             # reading as a CSV file (as a data.frame)
dat3 <- read.csv2(file = file_csv, header = T, sep = ',')            # reading as a CSV file (with additional optoins; and faster)
dat4 <- read.delim(file = file_csv, header = T, sep = ',')           # reading as a CSV file (with more control on the delimiter)
dat5 <- read.delim2(file = file_csv, header = T, sep = ',')          # reading as a CSV file (with more control on the delimiter; and faster)
# (2) show the histogram of "cost_current_total"; do you see a pattern?
file_txt = '~/Desktop/data_pat.csv'
dt <- fread(file_txt)
hist(dt$cost_current_total)

# (3) add a column to store the log of "cost_future_total" -- call the new column as "cost_future_total_log"
dt[, cost_future_total_log := log(cost_future_total)]
##It is extremely right skewed

# (4) show the histogram of "cost_future_total_log"; do you see a pattern?
hist(dt$cost_future_total_log)
##It is now normalized to a normal distribution


# (5) count how many of the "cost_future_total_log" values are "-Inf", and then replace all of them with 0
dt[cost_future_total_log == -Inf, .N]
dt[cost_future_total_log == -Inf, cost_future_total_log := 0]

# (6) show the histogram of "cost_future_total_log" once more; do you see a new pattern?
hist(dt$cost_future_total_log)
##Yes, there is quite a significant amount of INF future costs, 1099


# (7) save the new data.table as data_pat_log.csv and also data_pat_log.rds, but use "\t" (tab) as the separator
file_csv = '~/Desktop/data_pat_log.csv'
fwrite(dt, file_csv, sep = "\t")
