# Week 2 Day 1 Dates and Times in R

install.packages("tidyverse")

library(tidyverse)

library(lubridate)

today()

now()

ymd("2021-01-20")

mdy("January 20th, 2021")

Glimpse(penguins)
glimpse(penguins)

# Week 3 Day 2 Cleaning up with the basics

library(ggplot2)
data("diamonds")
View(diamonds)

install.packages("here")
library("here")
install.packages("skimr")
library("skimr")
install.packages("janitor")
library("janitor")
install.packages("dplyr")
library("dplyr")

install.packages("palmerpenguins")
library("palmerpenguins")

skim_without_charts(penguins)

head(penguins)

penguins %>%
    select(-species)

penguins %>%
  rename(island_new=island)

rename_with(penguins,toupper)

rename_with(penguins,tolower)

clean_names(penguins)

# Week 3 Day 2: Cleaning Data 

  # Video: Organize Your Data

library(tidyverse)
penguins %>% arrange(bill_length_mm)

penguins %>% arrange(-bill_length_mm)

penguins2 <- penguins %>% arrange(bill_length_mm)
view(penguins2)

penguins %>% group_by(island) %>% 
  drop_na() %>% 
  summarize(mean_bill_length_mm = mean(bill_length_mm)) 

penguins %>% group_by(island) %>%
  drop_na() %>% 
  summarize(max_bill_length_mm = max(bill_length_mm))

penguins %>% group_by(species, island) %>%
  drop_na() %>% 
  summarize(max_bl = max(bill_length_mm), mean_bl = mean(bill_length_mm))

penguins %>% filter(species == "Adelie")

# Hands-on Activity: Cleaning Data in R

## Step 1: Load packages

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)

## Step 2: Import data (manual)

bookings_df <- read_csv("hotel_bookings.csv")

## Step 3: Getting to know your data

head(bookings_df)

str(bookings_df)

glimpse(bookings_df)

colnames(bookings_df)

skim_without_charts(bookings_df)

## Step 4: Cleaning your data

trimmed_df <- bookings_df %>% 
  select("hotel","is_canceled" ,"lead_time" )

trimmed_df %>% 
  select(hotel, is_canceled, lead_time) %>% 
  rename(hotel_type = hotel)

example_df <- bookings_df %>% 
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")

head(example_df)

## Step 5: Calculations

calculated_df <- bookings_df %>% 
  summarize(number_canceled = sum(is_canceled), 
            average_lead_time = mean(lead_time))

head(calculated_df)

# Reading: Manually create a data frame

id <- c(1:10)

name <- c("John Mendes", "Rob Stewart", "Rachel Abrahamson", "Christy Hickman",
          "Johnson Harper", "Candace Miller", "Carlson Landy", "Pansy Jordan",
          "Darius Berry", "Claudia Garcia")

job_title <- c("Professional", "Programmer", "Management", "Clerical",
               "Developer", "Programmer", "Management", "Clerical", "Developer",
               "Programmer")

employee <- data.frame(id, name, job_title)

head(employee)


# Video: Transforming Data (already created df)

print(employee)

separate(employee, name,into=c('first_name', 'last_name'), sep=' ')

unite(employee, 'name',first_name,last_name,sep=' ')

View(penguins)

penguins %>% 
  mutate(body_mass_kg=body_mass_g/1000, flipper_length_m=flipper_length_mm/1000)

# Week 3 Day 3: Take a Closer Look at the Data 

  # Video: Same data, different outcome

install.packages('Tmisc')
library(Tmisc)
data(quartet)
View(quartet)

quartet %>% 
  group_by(set) %>% 
  summarize(mean(x),sd(x),mean(y),sd(y),cor(x,y))

ggplot(quartet,aes(x,y)) + geom_point() + geom_smooth(method=lm,se=FALSE) + facet_wrap('set')

install.packages('datasauRus')
library('datasauRus')

ggplot(datasaurus_dozen,aes(x=x,y=y,colour=dataset)) + 
  geom_point()+theme_void()+theme(legend.position = "none")+facet_wrap('dataset',ncol=3)

  # Video: The Bias Function

install.packages("SimDesign")
library(SimDesign)

actual_temp <- c(68.3, 70, 72.4, 71, 67, 70)
predicted_temp <- c(67.9, 69, 71.5, 70, 67,69)
bias(actual_temp, predicted_temp)

actual_sales <- c(150, 203, 137, 247, 116, 287)
predicted_sales <- c(200, 300, 150, 250, 150, 300)
bias(actual_sales, predicted_sales)


# Hands on Activity: Changing Your Data

# Steps 1 and 3 complete from earlier exercise

head(hotel_bookings)

# Practice Quiz:  1. There are 32 columns (variables) in the dataset.
  #               2. True, it is character data

str(hotel_bookings)
glimpse(hotel_bookings)
colnames(hotel_bookings)

arrange(hotel_bookings, desc(lead_time))

# Practice Quiz: Highest lead time is A: 737

head(hotel_bookings)

hotel_bookings_v2 <- arrange(hotel_bookings, desc(lead_time))

head(hotel_bookings_v2)

max(hotel_bookings$lead_time)
min(hotel_bookings$lead_time)

mean(hotel_bookings$lead_time)

mean(hotel_bookings_v2$lead_time)

# Practice Quiz: Average lead time is D: 104.0114

hotel_bookings_city <- filter(hotel_bookings, hotel_bookings$hotel=="City Hotel")

head(hotel_bookings_city)

mean(hotel_bookings_city$lead_time)

hotel_summary <- 
  hotel_bookings %>% 
  group_by(hotel) %>% 
  summarise(average_lead_time=mean(lead_time),
            min_lead_time=min(lead_time),
            max_lead_time=max(lead_time))

head(hotel_summary)
