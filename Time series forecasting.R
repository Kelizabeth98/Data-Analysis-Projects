###Installing the required package
install.packages(c("tidyverse",
                   "fpp3",
                   "GGally",
                   "sugrrants",
                   "astsa"))
install.packages("sugrrants")
library(fpp3)
library(tidyverse)
library(GGally)
library(sugrrants)
library(astsa)
library(ggplot2)

##Global economy dataset
global_economy

###Creating a simple tsibble index
global_economy |>
  filter(Year == 1960)

as_tibble(global_economy)
tourism


mydata <- tsibble(
  year = 2012:2016,
  y = c(123, 39, 78, 52, 110),
  index = year
)
mydata

mydata |>
  filter(year == 2012)

as_tsibble(tourism)

#Read a csv file and convert to a tsibble
prison <- readr::read_csv("http://OTexts.com/fpp3/extrafiles/prison_population.csv")

  ##Convert to tsibble
prison |>
  mutate(Quarter = yearquarter(Date)) |>
  as_tsibble(index = Quarter, key = c(State, Gender, Legal, Indigenous))


##PBS
PBS |>
  index_by(Year = year(Month)) |>
  summarise(Cost = sum(Cost)) |>
  slice_max(Year, n =10)

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(total_cost = sum(Cost)) |>
  mutate(total_cost = total_cost/1e6)

##Plotting the number of passengers for ADL-PER Airport
ansett |>
  filter(Airports == "ADL-PER", Class == "Economy") |>
  autoplot(Passengers)

ansett |>
  filter(Airports == "ADL-PER") |>
  autoplot(Passengers)

##Plotting the total number of passengers for each Airport
ansett |>
  group_by(Airports) |>
  summarise(Passengers = sum(Passengers)) |>
  autoplot(Passengers)

##Plotting the total cost of A10 Drug
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(total_cost = sum(Cost)) |>
  mutate(total_cost = total_cost/1e6) |>
  autoplot(total_cost) 

###Time Plots
aus_production |>
  autoplot(Beer)

####Seasonal Plots

##Using the gg_season() for seasonality
aus_production |>
  gg_season(Beer)

aus_production |>
  autoplot(Cement)

aus_production |>
  gg_season(Cement)

####Multiple Seasonal plots
vic_elec |> gg_season(Demand)

###Making the plot better
vic_elec |>
  index_by(month = yearmonth(Time)) |>
  #index_by(date = as.Date(Time)) |>
  #index_by(week = yearweek(Time)) |>
  summarise(Demand = sum(Demand)) |>
  gg_season(Demand)


vic_elec |>
  gg_season(Demand, period = "week")

vic_elec |>
  gg_season(Demand, period = "1 week")

vic_elec |>
  gg_season(Demand, period = "day")

vic_elec |>
  gg_season(Demand, period = "1 day")

vic_elec |>
  index_by(month = yearmonth(Time)) |>
  summarise(Demand = sum(Demand)) |>
  autoplot(Demand)

aus_production |>
  autoplot(Beer)

recent_production <- aus_production |>
  filter_index("1995 Q1"~.) 

recent_production |>
  autoplot(Beer)

recent_production |>
  gg_season(Beer)

##Sub-serires function
recent_production |>
  gg_subseries(Beer)

##Tourisim dataset
holiday <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holiday |>
  autoplot(Trips)

holiday |>
  gg_season(Trips)

holiday |>
  gg_subseries(Trips)

###Lag and Autocorr plots
##Lag
recent_production |>
  gg_lag(Beer, geom = "point")

recent_production |>
  ACF(Beer) |>
  autoplot()

holiday |>
  filter(State == "Victoria") |>
  ACF(Trips) |>
  autoplot()

##Specify the number of lags
holiday |>
  filter(State == "Victoria") |>
  ACF(Trips, lag_max = 40) |>
  autoplot()

holiday |>
  filter(State == "Northern Territory") |>
  ACF(Trips, lag_max = 40) |>
  autoplot()

##White Noise
pelt |>
  ACF(Hare) |>
  autoplot()
  
pelt |>
  autoplot(Hare)

as_tsibble(USAccDeaths) |>
  ACF(value) |>
  autoplot()


##Creating a random tsibble
random <- tsibble(
  year = 1990:2023,
  y = rnorm(34),
  index = year
)

random |>
  autoplot(y)

random |>
  ACF(y) |>
  autoplot()
###White noise, implying no pattern

library(fpp3)
library(ggplot2)

food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarize(Turnover = sum(Turnover))

autoplot(food)

#Time series transformation
#Square root
food |> autoplot(sqrt(Turnover)) +
  labs(y = "Square root turnover")

#Cube root
food |> autoplot(Turnover^(1/3)) +
  labs(y = "Square root turnover")

#Inverse
food |> autoplot(-1/Turnover) +
  labs(y = "Inverse turnover")


##Box-Cox Transformations
#Introduces the lambda parameter

##Getting lambda value automatically
food |>
  features(Turnover, features = guerrero)

food |>
  autoplot(box_cox(Turnover,0.0895)) +
  labs(y = "Box-Cox transformed turnover")


global_economy |>
  filter(Country %in% c("Kenya", "Tanzania", "Uganda", "Rwanda", "Ethiopia", "Somalia")) |>
  autoplot(GDP)

#GDP Per Capita
global_economy |>
  filter(Country %in% c("Australia", "United Kingdom", "China")) |>
  autoplot(GDP/Population)
