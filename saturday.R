library(tidyverse) 

cars2020 <- read_csv("data/cars2020.csv",
                     show_col_types = FALSE)

tr_counts <- count(cars2020, transmission)

tr_fuel_counts <- count(cars2020, transmission, fuel)


## Getting all cars with manual transmission
manual <- filter(cars2020, transmission == "Manual")


# All cars with 4 or more cylinders
cyl_4plus <- filter(cars2020, cyl >= 4)


## All cars with manual transmissions and 4 or more cylinders

manual_4plus <- filter(cars2020, transmission == "Manual", 
                       cyl >=4) 

auto_lt6 <- filter(cars2020, transmission == "Automatic", 
                   gears < 6)

count(cars2020, transmission, gears)

## Using select to get only some of the columns 

cars_narrow <- select(cars2020, model, mpg, transmission, cyl)


## Using - to say which columns you want to drop 

cars_alt <- select(cars2020,  -startStop, -aspiration)


## Using arrange to sort cars by mpg

## ascending order

cars_sorted <- arrange(cars2020, mpg)

## descending order

cars_sorted <- arrange(cars2020, desc(mpg))

## also descending order

cars_sorted <- arrange(cars2020, -mpg)

## Sort by transmission type then within each group sort by mpg (descending)

cars_transmpg <- arrange(cars2020, transmission, desc(mpg))


##start with the original data set (all vehicles)
### restrict to CVT transmission vehicles
### remove the sidi, aspiration and startStop columns
### order the data in descending order of mpg
### if cars have the same mpg order them by model alphabetically


cvt <- filter(cars2020, transmission == "CVT")
cvt_narrow <- select(cvt, -sidi, -aspiration, -startStop)
cvt_answer <- arrange(cvt_narrow, desc(mpg), model)

cvt_answer2 <- filter(cars2020, transmission == "CVT") |>
  select(-sidi, -aspiration, -startStop) |> 
  arrange(desc(mpg), model)

## Using mutate to add a column/variable to the data set

cars_30 <- mutate(cars2020, above30 = if_else(mpg > 30, TRUE, FALSE))

## alternative using pipes

cars_30_alt <- select(cars2020, make, model, mpg) |> 
  mutate(above30 = if_else(mpg > 30, TRUE, FALSE))

## Other alternative using pipes
cars_30_alt2 <- cars2020 |> select(make, model, mpg) |>
  mutate(above30 = if_else(mpg > 30, TRUE, FALSE))

### 

ggplot(cars2020, 
       aes(x = transmission, y = mpg)) + 
  geom_boxplot() + theme_minimal()

ggplot(cars2020, 
       aes(x = disp, y = mpg, color = transmission)) + 
  geom_point() + theme_bw()

### Summarise number of cars and average mpg 

report <- summarise(cars2020, num_cars = n(),
          avg_mpg = mean(mpg, na.rm = TRUE),
          med_mpg = median(mpg, na.rm = TRUE))


### Using group_by

grouped_cars <- group_by(cars2020, transmission)
report2 <- summarise(grouped_cars, num_cars = n(),
          avg_mpg = mean(mpg, na.rm = TRUE),
          med_mpg = median(mpg, na.rm = TRUE))

report3 <- cars2020 |> group_by(transmission) |> 
  summarise(num_cars = n(), 
            avg_mpg = mean(mpg, na.rm = TRUE),
            med_mpg = median(mpg, na.rm = TRUE))

## Can you do the same for make? (Make and transmission?)

make_report <- cars2020 |> group_by(make) |>
  summarise(num_cars = n(), 
            avg_mpg = mean(mpg, na.rm = TRUE),
            med_mpg = median(mpg, na.rm = TRUE)) |> 
  arrange(desc(avg_mpg))

make_report2 <- cars2020 |> group_by(make, transmission) |>
  summarise(num_cars = n(), 
            avg_mpg = mean(mpg, na.rm = TRUE),
            med_mpg = median(mpg, na.rm = TRUE)) |> 
  arrange(desc(avg_mpg))






