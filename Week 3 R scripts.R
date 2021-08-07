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

































