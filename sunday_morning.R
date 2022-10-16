library(tidyverse)

penguins <- read_csv("data/penguins.csv",
                     show_col_types = FALSE)


### Some graphs in base R 

hist(penguins$body_mass_g, col = "pink", 
     main = "Body masses of penguins", breaks = 50)

### Scatter plot of bill length vs body mass
plot(bill_length_mm~body_mass_g, data = penguins)

plot(penguins$body_mass_g, penguins$bill_length_mm)


### Boxplot 

boxplot(body_mass_g~species, data = penguins)

### Making a scatter plot of flipper length vs bill depth


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm,
                     y = flipper_length_mm)) + 
  geom_point() + theme_minimal()

### adding color for species

library(ggplot2)
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm,
                     y = flipper_length_mm,
                     color = species,
                     shape = sex)) + 
  geom_point()


sub_str <- str_wrap("Some scientists measured some penguins in Antarctica between 2007 and 2009. Here is what they found.", 80)

filter(penguins, sex == "female") |>
  ggplot(mapping = aes(x = bill_length_mm,
                     y = flipper_length_mm,
                     color = species)) + 
  geom_point() + 
  labs(title = "Relationship between bill length and flipper length in female penguins",
       x = "Bill length", y = "Flipper length")

## Plotting Bill depth vs flipper length

ggplot(data = penguins,
       mapping = aes(y = bill_depth_mm,
                     x = flipper_length_mm)) + 
  geom_point() + theme_minimal()

ggplot(data = penguins,
       mapping = aes(y = bill_depth_mm,
                     x = flipper_length_mm,
                     color = species)) + 
  geom_point() + theme_minimal()

## Make a scatter plot showing the relationship between flipper length and 
## body mass. Use colours to show the species

ggplot(data = penguins, 
       aes(x = flipper_length_mm, 
           y = body_mass_g,
           colour = species)) + geom_point() 

ggplot(data = penguins, 
       aes(y = flipper_length_mm, 
           x = body_mass_g,
           colour = species)) + 
  geom_point(shape = 21) + theme_classic()
  
  
## Making a histogram of bill lengths

ggplot(penguins,
       aes(x = bill_length_mm)) + geom_histogram()

## Change the fill 

ggplot(penguins, 
       aes(x = bill_length_mm)) + 
  geom_histogram(fill = "#89608E", color = "black")

## Showing histograms for different groups

ggplot(penguins,
       aes(x = bill_length_mm)) + 
  geom_histogram(color = "black", 
                 fill = "blue") + 
  facet_wrap(~species, ncol = 1)


## Using boxplots to show distributions

ggplot(data = penguins, 
       aes(x = species, y = bill_length_mm)) + 
  geom_boxplot()

ggplot(data = penguins,
       aes(y = species, x = bill_length_mm)) + 
  geom_boxplot()


## Can you make a box plot to compare body masses 
## of different species?

ggplot(data = penguins,
       aes(y = species, x = body_mass_g)) +
  geom_boxplot()


## Removing rows where sex is NA
filter(penguins, !is.na(sex)) |> 
  ggplot(aes(y = species, x = body_mass_g, 
             color = sex)) +
  geom_boxplot()


penguins_knownsex <- filter(penguins, !is.na(sex))

penguins <- mutate(penguins, sexmissing = is.na(sex))

## Violin plot

ggplot(data = penguins, 
       aes(x = species, y = bill_length_mm)) + geom_violin()

library(ggbeeswarm)


ggplot(data = penguins, 
       aes(x = species, y = bill_length_mm)) + 
       geom_beeswarm(alpha = 0.8)

### 

ggplot(data = penguins,
       aes(x = species)) + 
  geom_bar(fill = "#D64045") + theme_light()

ggplot(data = penguins,
       aes(x = species, fill = island)) + 
  geom_bar()

ggplot(data = penguins,
       aes(x = species, fill = island)) + 
  geom_bar(position = "fill")


































