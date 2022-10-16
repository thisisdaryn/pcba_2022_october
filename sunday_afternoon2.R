library(tidyverse)
library(rsample) #would use for data splitting

so <- read_csv("data/stackoverflow.csv",
               show_col_types = FALSE)

set.seed(1729)
data_split <- initial_split(so, prop = 0.8)
training_data <- training(data_split)
testing_data <- testing(data_split)

ggplot(data = training_data,
       aes(x = country)) + 
  geom_bar(fill = 'orange',
           color = "orange", alpha = 0.4) + 
  theme_minimal()

ggplot(data = training_data,
       aes(x = country, y = salary)) + 
  geom_boxplot()

ggplot(data = training_data,
       aes(x = country, fill = remote)) + 
  geom_bar(position = "dodge") + theme_bw() +
  scale_fill_manual(values = c("#DB7F8E", "#72195A"))


### Creating a 0-1 variable for prediction

training_data <- mutate(training_data, 
                        remote2 = if_else(remote == "Not remote", 
                                          0, 1))

pred_model1 <- glm(remote2~country, data = training_data, 
    family = binomial(link = "logit"))

pred_model1

training_data$model1 <- predict(pred_model1, 
                                newdata = training_data,
                                type = "response")

## rebalancing data 

## split into remote and not remote
remote <- filter(so, remote == "Remote")
not_remote <- filter(so, remote == "Not remote")


# take 90% of remote for training
remote_split <- initial_split(remote, 0.9)
remote_train <- training(remote_split)


# take 10% of not remote for training
notremote_split <- initial_split(not_remote, 0.1)
notremote_train <- training(notremote_split)

# combine training data sets
training_data <- bind_rows(remote_train, notremote_train) |>
  mutate(remote2 = if_else(remote == "Not remote", 
                           0, 1))
  
## exploratory graph
ggplot(data = training_data,
       aes(x = country, fill = remote)) + 
  geom_bar(position = "dodge") + theme_bw() +
  scale_fill_manual(values = c("#DB7F8E", "#72195A"))

## train the model on training data set
model2 <- glm(remote2~country+years_coded_job+open_source, 
              data = training_data)
model2

## create the testing data set by combining the remaining 
## 10% of the remote and 90% of the not remote

remote_testing <- testing(remote_split)
notremote_testing <- testing(notremote_split)

## combine testing data sets
testing_data <- bind_rows(remote_testing, notremote_testing)

## making predictions on the testing set
testing_data$prediction <- predict(model2, 
                                   newdata = testing_data)


## observing results (kinda)

ggplot(data = testing_data,
       aes(x = remote, y = prediction)) + 
  geom_boxplot()

### thisisdaryn@gmail.com



