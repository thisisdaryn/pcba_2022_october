libary(tidyverse)

ames <- read.csv("data/AmesHousing.csv")

nbhd_counts <- count(ames, Neighborhood, sort = TRUE)

ames <- mutate(ames, nbhd2 = fct_lump(Neighborhood, 5))

ggplot(data = ames, 
       aes(x = Longitude, 
           y = Latitude,
           color = nbhd2)) + 
  geom_point()


library(sf)
story <- read_sf("Iowa_County_Boundaries/IowaCounties.shp") |>
  filter(CountyName == "Story")

iowa <- read_sf("Iowa_County_Boundaries/IowaCounties.shp")

ggplot(story) + geom_sf()+ 
  geom_point(data = ames,
             aes(x = Longitude, y = Latitude,
                 color = nbhd2))

## Distribution of house prices

## Histogram of prices
ggplot(data = ames, 
       aes(x = Sale_Price)) + 
  geom_histogram(fill = "#0EAD69",
                 color = "darkgreen")

## Scatterplot of Sale Price vs Living area
ggplot(data = ames,
       aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.2)

## Faceted by Neighbourhood
ggplot(data = ames,
       aes(x = Gr_Liv_Area, 
           y = Sale_Price,
           color = nbhd2)) +
  geom_point(alpha = 0.2) + facet_wrap(~nbhd2)

## Box plot comparing prices in top 5 common neighborhoods (and other)
ggplot(data = ames,
       aes(x = nbhd2, y = Sale_Price)) + 
  geom_boxplot()


## Box plots of all 28 neighborhoods

ggplot(data = ames,
       aes(y = Neighborhood, 
           x = Sale_Price)) + 
  geom_boxplot()

### Box plots of all 28 neighborhoods sorted by Sale Price


ames2 <- mutate(ames, 
                Neighborhood = fct_reorder(Neighborhood,
                                           Sale_Price))

ggplot(data = ames2,
       aes(y = Neighborhood, 
           x = Sale_Price)) + 
  geom_boxplot()

