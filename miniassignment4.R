# ============================================================ 
# 	Mini-assignment # 4
# ============================================================ 

# (1) calculate and store the min, max, mean, and standard deviation of all numerical columns for the following data.table:
# set.seed(100)
# a = sample(65:110,1000,replace=T)
# b = sample(c('M','F','F'),1000,replace=T)
# c = sample(c('W','W','W','B','B','A','O',NA),1000,replace=T)
# d = sample(100:5000,1000)*a
# e = runif(1000,0,1)*(a/110)
# dt = data.table(age = a, sex = b, race = c, cost = d, risk = e)

library(data.table) # loading the data.table library (assuming it's installed)
set.seed(100) # so the random numbers are reproducible
a = sample(65:110,1000,replace=T)
b = sample(c('M','F','F'),1000,replace=T)
c = sample(c('W','W','W','B','B','A','O',NA),1000,replace=T)
d = sample(100:5000,1000)*a
e = runif(1000,0,1)*(a/110)
dt = data.table(age = a, sex = b, race = c, cost = d, risk = e)

print(dt)
print(min(dt$a))
print(max(dt$a))
print(mean(dt$a))
print(sd(dt$a))

# (2) print mean and standard deviation (2 decimals) of the numerical columns as below (replace XXX with actual variables):
# The study population has an average age of XXX years with a standard deviation of XXX years
# The study population has an average cost of XXX dollars with a maximum of XXX dollars
# The total risk ranged between minimum of XXX and maximum of XXX

a_min = min(dt$a)
a_max = max(dt$a)
a_avg = mean(dt$a)
a_std = sd(dt$a)
paste0('The study population has an average age of ', 
       a_avg, 
       ' years with a standard deviation of', 
       round(a_std,2), 
       ') years.')

c_min = min(dt$c)
c_max = max(dt$c)
c_avg = mean(dt$c)
c_std = sd(dt$c)
paste0('The total risk ranged between minimum of ', 
       c_avg, 
       ' dollars with a maximum of', 
       round(c_std,2), 
       ') dollars.')

d_min = min(dt$d)
d_max = max(dt$d)
d_avg = mean(dt$d)
d_std = sd(dt$d)
paste0('The study population has an average cost of ', 
       d_min, 
       ' dollars with a maximum of', 
       round(d_max,2), 
       ') dollars.')

# (3) calculate the mean age, cost, and risk of each race as well as populations with missing race separately
# try using the short/alternate built-in data.table syntax (although base R syntax is also acceptable)

dt$age[1:10] # the first 10 patient's ages
dt$sex[1:10] # the first 10 patient's sexes
which(dt$sex[1:10] == 'M') # index of all males in the first 10 elements/rows
which(dt$sex == 'M') # index of all males
dt[which(dt$sex == 'M')] # all rows of Males
dt$age[which(dt$sex == 'M')] # only show the age of Males (method 1)
dt[which(dt$sex == 'M')]$age # only show the age of Males (method 2)
mean(dt$age[which(dt$sex == 'M')]) # calculate the mean of male's age
mean(dt$age[which(dt$sex == 'F')]) # calculate the mean of female's age

dt[sex == 'M', mean(age)] # an alternative (faster way) to calculate male's mean age
dt[sex == 'F', mean(age)] # an alternative (faster way) to calculate female's mean age

# (4) show the histogram of age for the female population

hist(dt$age)
hist(dt$cost)

# (5) show the scatter plot of age vs. risk for the male population

plot(dt$cost ~ dt$age)
plot(cost ~ age, data = dt)