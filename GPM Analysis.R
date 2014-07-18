
setwd("C:/Users/Anup/Downloads")
working.data = read.csv("FuelEfficiency.csv")
attach(working.data)

str(working.data)

#Descriptive analysis of each variable
for (i in 1:7){
  cat(sprintf("Currently calculating summary for: %s", 
              colnames(working.data[i])), sprintf("\n"))
  stats = summary(working.data[i])
  cat(sprintf("%s",stats),sprintf("\n"),sprintf("\n"))
}

table(ET)

#Plotting the data variables as histograms
#For MPG
ggplot(aes(x=MPG), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9, 
                 color = 'dark red')

#For GPM
ggplot(aes(x=GPM), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9,
                 color = 'dark red')

#For WT
ggplot(aes(x=WT), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9,
                 color = 'dark red')

#For DIS
ggplot(aes(x=DIS), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=7,
                 color = 'dark red')

#For NC
ggplot(aes(x=factor(NC)), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9,
                 color = 'dark red')

#For HP
ggplot(aes(x=HP), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=2,
                 color = 'dark red')

#For ACC
ggplot(aes(x=MPG), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9,
                 color = 'dark red')

#For ET
ggplot(aes(x=ET), data=working.data) + 
  geom_histogram(aes(fill = ..count..), binwidth=0.9,
                 color = 'dark red')

#Showing relationship between all the variables using a scatter plot
pairs(working.data)

#Plotting the relationship of GPM with other variables as scatterplots
#For MPG
ggplot(aes(x=GPM, MPG), data=working.data) + 
  geom_point(aes(color = MPG), size=3)

#For WT
ggplot(aes(x=GPM,WT), data=working.data) + 
  geom_point(aes(color = WT), size=3)

#For DIS
ggplot(aes(x=GPM, DIS), data=working.data) + 
  geom_point(aes(color = DIS), size=3)

#For NC
ggplot(aes(x=GPM, NC), data=working.data) + 
  geom_point(aes(color = NC), size=3)

#For HP
ggplot(aes(x=GPM, HP), data=working.data) + 
  geom_point(aes(color = HP), size=3)

#For ACC
ggplot(aes(x=GPM, ACC), data=working.data) + 
  geom_point(aes(color = ACC), size=3)

#For ET
ggplot(aes(x=GPM, ET), data=working.data) + 
  geom_point(aes(color = ET), size=3)


## Predictive Analysis of GPM using ANNOVA
annova.model = aov(GPM ~ MPG + WT + DIS + NC + HP + ACC + ET)
summary(annova.model)

## Here we can see that the most significant variable in the 
## data set is MPG and the second most significant variable is WT

# Now let us do analysis using regression model with MPG and WT 
# as significant variables 

regression.model = lm(GPM ~ MPG + WT)
summary(regression.model)

# Checking attributes of the regression model
attributes(regression.model)

# Exctracting the coefficients of the most signifact variables for 
# predictive model

regression.model$coefficients

# Thus, our predictive model for this dataset will be

GPM = 0.3593*WT - 0.1381*MPG + 6.7219




  
  







