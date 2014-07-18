setwd('C:/Users/Anup/Downloads')

cars.data = read.csv('ToyotaCorolla.csv')
attach(cars.data)

for (i in 1:3){
  summary = summary(cars.data[,i]);
  cat(sprintf("Currently calculating for: %s", colnames(cars.data[i])),
      sprintf("\n"),sprintf("Min  1st Qu.  Median  Mean  3rd Qu.  Max. "),
      sprintf("\n"), sprintf("%s", summary), sprintf("\n"),
      sprintf("Var: %s", var(cars.data[,i])), sprintf("\t"),
      sprintf("SD: %s", sd(cars.data[,i])), sprintf("\n"), sprintf("\n"));
}

table(FuelType)
table(MetColor)
table(Automatic)

for (i in 8:10){
  summary = summary(cars.data[,i]);
  cat(sprintf("Currently calculating for: %s", colnames(cars.data[i])),
      sprintf("\n"),sprintf("Min  1st Qu.  Median  Mean  3rd Qu.  Max. "),
      sprintf("\n"), sprintf("%s", summary), sprintf("\n"),
      sprintf("Var: %s", var(cars.data[,i])), sprintf("\t"),
      sprintf("SD: %s", sd(cars.data[,i])), sprintf("\n"), sprintf("\n"));
}

#Plotting the interdepence of variables on each other
pairs(cars.data)

#Applying regression model
model1 = lm(Price ~ Age + KM + HP + CC + Doors +  Weight)
summary(model1)

# We saw from the summary of the model that Age, KM, HP, CC
# & Weight are the most significant factors

#Apply regression model based on the most significant factors
model2 = lm(Price ~ Age + KM + HP + CC + Doors + Weight)
summary(model2)

attributes(model2)

model2$coefficients

#From the above multiple regression model we get the final modelling as

Price = -122.093*Age - 0.01675*KM + 32*HP - 1.67*CC - 58.761*Doors 
        + 22.771*Weight





