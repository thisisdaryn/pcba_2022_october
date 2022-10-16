library(tidyverse)

cars2020 <- read.csv("data/cars2020.csv")



## Exploratory graph of relationship between 
## mpg and displacement
plot1 <- ggplot(data = cars2020, 
       aes(x = disp, y = mpg)) + 
  geom_point() + geom_smooth()

plot1

## Simple model1: using 1 numerical variable to predict another 
## numerical variable using linear regression (OLS),
## using the lm function

model1 <- lm(mpg~disp, data = cars2020)

cars2020$model1 <- predict(model1, newdata = cars2020) 

cars2020 <- mutate(cars2020, 
         Acura_ilx = if_else(make == "Acura" & model == "ILX", 
                               "Acura ILX", "Everything else"))


ggplot(data = cars2020, 
       aes(x = mpg, y = model1)) + 
  geom_point(alpha = 0.2) + geom_abline(slope = 1, intercept = 0) + 
  #geom_vline(xintercept = 27.9249, 
  #           linetype = "dashed", color = "brown") + 
  #geom_hline(yintercept = 25.86070, linetype = "dashed", 
  #           color = "brown") + 
  xlim(c(0, 60)) + ylim(c(0, 60)) + theme_minimal()


ggplot(data = cars2020, 
       aes(x = mpg, y = model1,
           color = Acura_ilx)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + 
  geom_vline(xintercept = 27.9249, 
             linetype = "dashed", color = "brown") + 
  geom_hline(yintercept = 25.86070, linetype = "dashed", 
             color = "brown") + 
  xlim(c(0, 60)) + ylim(c(0, 60)) + theme_minimal()


## Using transmission type to predict mpg

model2 <- lm(mpg~transmission, data = cars2020)
model2

cars2020$model2 <- predict(model2, newdata = cars2020)

ggplot(data = cars2020,
       aes(x = mpg, y = model2)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0, 60))

summary(model2)


model3 <- lm(mpg~transmission+disp, data = cars2020)

cars2020$model3 <- predict(model3, newdata = cars2020)


## Relocating model values closer to actual mpg value for easy comparison
cars2020 <- relocate(cars2020, make, model,
                     mpg, model1, model2, model3, disp, 
                     transmission)

cars2020_pivot <- pivot_longer(cars2020, model1:model3,
                               values_to = "prediction")

ggplot(data = cars2020_pivot, 
       aes(x = mpg, y = prediction)) + 
  geom_point(alpha = 0.2) + facet_wrap(~name) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0, 60)) + theme_bw()

model4 <- lm(mpg~transmission+disp+atvType+class, data = cars2020)

cars2020$model4 <- predict(model4, newdata = cars2020)

cars2020_pivot2 <- pivot_longer(cars2020, c(model1, model2, model3, model4),
                               values_to = "prediction")

ggplot(data = cars2020_pivot2,
       aes(x = mpg, y = prediction)) + 
  geom_point(alpha = 0.2) + facet_wrap(~name) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0, 60)) + theme_bw()

### Syntax for Splitting the data 

library(rsample)

split <- initial_split(cars2020, prop = .8)

training_data <- training(split)
testing_data <- testing(split)

model5 <- lm(mpg~transmission+disp+atvType+class, 
             data = training_data)

testing_data$model5 <- predict(model5, newdata = testing_data)

## Plotting model performance
ggplot(data = testing_data,
       aes(x = mpg, y = model5)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0, 60)) + theme_bw()

### Making a decision tree model on the testing data

## to make a decision tree we use the rpart function of the rpart package

library(rpart)
library(rpart.plot)

model6_dt <- rpart(mpg~transmission+disp+atvType, data = training_data)

rpart.plot(model6_dt)


testing_data$model6_dt <- predict(model6_dt, newdata = testing_data)

testing_pivot <- pivot_longer(testing_data, model5:model6_dt) |> 
  relocate(make, model, mpg, name, value)

ggplot(data = testing_pivot, 
       aes(x = mpg, y = value)) + 
  geom_point(alpha = 0.2) + facet_wrap(~name) + 
  geom_abline(intercept = 0, slope = 1, color = "green") + 
  xlim(c(0, 60)) + ylim(c(0, 60)) + theme_bw()










































