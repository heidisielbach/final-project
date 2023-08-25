# Heidi Sielbach final project


here::here()

install.packages("tidyverse","here","tidytuesdayR","gtsummary","knirt")

library(tidyverse)
library(here)
library(tidytuesdayR)
library(gtsummary)
library(knitr)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

names(squirrel_data)

names(squirrel_data) <- c("X", "Y" ,"Unique_Squirrel_ID","Hectare","Shift","Date","Hectare_Squirrel_Number",
               "Age" ,"Primary_Fur_Color", "Highlight_Fur_Color", 
               "Combination_of_Primary_and_Highlight_Color", "Color_notes","Location", 
               "Above_Ground_Sighter_Measurement", "Specific_Location","Running","Chasing",
               "Climbing","Eating","Foraging","Other_Activities","Kuks","Quaas","Moans", "Tail_flags",
               "Tail_twitches","Approaches","Indifferent","Runs_from","Other_Interactions",                       
               "Lat/Long")


tbl_summary(
  squirrel_data,
  by= "Shift",
  include= c("Age", "Primary_Fur_Color", "Location", "Foraging", "Approaches", "Runs_from"),
  label= list(
    "Age" ~ "Age",
    "Primary Fur Color" ~ "Primary Fur Color",
    "Location" ~ "Location Seen At",
    "Foraging" ~ "Foraging Observed",
    "Approaches" ~ "Approached Humans",
    "Runs_from" ~ "Ran From Humans"
  ),
  missing_text = "Observation Not Recorded")  |> 
  add_overall(col_label = "**All Sightings**") |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  modify_header(label = "**Squirrel Characteristic**"
 )

table(squirrel_data$Runs_from)


squirrel_data$Age1 <- ifelse(squirrel_data$Age=="Adult", 2, ifelse(squirrel_data$Age=="Juvenile", 1, NA))
squirrel_data$Primary_Fur_Color1 <- ifelse(squirrel_data$Primary_Fur_Color=="Black", 3, ifelse(squirrel_data$Primary_Fur_Color=="Cinnamon", 1, 0))
squirrel_data$Location1 <- ifelse(squirrel_data$Location=="Above Ground", 2, 1)
squirrel_data$Foraging1 <- ifelse(squirrel_data$Foraging=="True", 2, 1)
squirrel_data$Approaches1 <- ifelse(squirrel_data$Approaches=="True", 2, 1)
squirrel_data$Runs_from1 <- ifelse(squirrel_data$Runs_from=="True", 2, 1)

table(squirrel_data$Age, squirrel_data$Age1)
table(squirrel_data$Primary_Fur_Color, squirrel_data$Primary_Fur_Color1)
table(squirrel_data$Location, squirrel_data$Location1)
table(squirrel_data$Foraging, squirrel_data$Foraging1)
table(squirrel_data$Approaches, squirrel_data$Approaches1)
table(squirrel_data$Runs_from, squirrel_data$Runs_from1)


tbl_uvregression(
  squirrel_data, 
  y = Shift,
  include = c(Age1, Primary_Fur_Color1, Runs_from1),
  method = lm) 

c(Age, Primary_Fur_Color, Runs_from)

linear_model_sd <- lm(Shift ~ Age1 + Primary_Fur_Color1 + Runs_from1,
                   data = squirrel_data)

logistic_model <- glm(Shift ~ Age + Primary_Fur_Color + Runs_from, 
                      data = squirrel_data, family = binomial())

?glm()

table(squirrel_data$Age)

class(squirrel_data$Age)

?hist()

squirrel_data_age <- factor(x=squirrel_data$Age, 
                            levels=c(0,1,2),
                            labels=c("Unknown","Juvenile","Adult"))

