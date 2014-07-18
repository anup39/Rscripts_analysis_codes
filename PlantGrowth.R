

data(PlantGrowth)
plantgrowth.data = PlantGrowth #Loading the data into workspace
attach(plantgrowth.data)

View(plantgrowth.data)

str(plantgrowth.data)

summary(plantgrowth.data)
sd(weight)
var(weight)

library("ggplot2", lib.loc="C:/Users/Anup/Documents/R/win-library/3.1")
ggplot(aes(x=group, y=weight), data=PlantGrowth) + 
  geom_boxplot(aes(fill=factor(group)),position = "dodge", 
               outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, notch = FALSE, 
               notchwidh = 0.5)

#Regression model
model = lm(weight~group)



group.ctrl = plantgrowth.data[1:10,]  #Group1 = ctrl
group.trt1 = plantgrowth.data[11:20,] #Group2 = trt1
group.trt2 = plantgrowth.data[21:30,] #Group3 = trt2

summary(group.ctrl)
summary(group.trt1)
summary(group.trt2)

sd(group.ctrl$weight)
var(group.ctrl$weight)

sd(group.ctrl$weight)
var(group.trt1$weight)

sd(group.ctrl$weight)
var(group.trt2$weight)


#Difference in the mean yeild for the first and second group
#Ho = mean of ctrl = mean of trt1
table = aov(weight ~ group)
summary(table)

# Looking up the table attributes
attributes(table)

# Accessing the model coefficients
table$coefficients

# Thus we have the final model as
Weight =  0.494*grouptrt2 - 0.371*grouptrt1 + 5.032




