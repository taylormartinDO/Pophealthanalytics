# ============================================================ 
# 	Mini-assignment # 3
# ============================================================ 

# (1) create a vector containing numbers 0 to 10, 100 to 200, and 900 to 1000 (without entering all numbers individually)
1:10
100:200
900:1000


# (2) find the min, max, sum, average and quartiles of the last vector
z<-900:1000
print(z)
min(z)
max(z)
sum(z)
quantile(z)

# (3) find the min, max, sum, average and quartiles of the following vector: c(rnorm(1000,500,10))  [note that rnorm command is generating random numbers every time you run it!]
y<-c(rnorm(1000,500,10))
print(y)
min(y)
max(y)
sum(y)
quantile(y)

# (4) create a data.frame with four columns (age, sex, cost, risk) with 1000 rows, as follow: 
# first column randomly picked from 18 to 65; 
# second column random 'F' or 'M' or NA (i.e., missing); 
# third column with random numbers between 1000 and 100,000 (round to 1 digit); and, 
# fourth column with random number between 0 and 1 (do NOT round)

data.frame(age = sample(18:65, 1000, replace = T),
           sex = sample(c('M','F', 'NA'), 1000, replace = T),
           cost = round(runif(100, min = 1000, max = 1000000)),
           binary = runif(1000, min = 0, max = 1))


# (5) assign the previous data.frame to a variable such as x
# get the mean of all columns in the data.frame
# count the number of F and M and NA (find online how to count NA in the table command)
# set your seed number to 100

set.seed(100) # to make sure random numbers staying the same
x = data.frame(age = sample(18:65, 1000, replace = T),
           sex = sample(c('M','F', 'NA'), 1000, replace = T),
           cost = round(runif(100, min = 1000, max = 1000000)),
           binary = runif(1000, min = 0, max = 1))

mean(x$age)
table(x$sex) # counting a categorical column
mean(x$cost)
mean(x$binary)

# (6) get the quartiles of the cost and risk columns before and after log transformation

quantile(x$cost)
quantile(log(x$cost))

quantile(x$binary)
quantile(log(x$binary))

# (7) count how many of the costs are more than mean of cost 
# avoid entering a plain number in the comparison

x$cost > mean(x$cost)
sum(x$cost > mean(x$cost))
