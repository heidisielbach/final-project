# Heidi Sielbach final project

here::here()

install.packages("tidyverse","here","tidytuesdayR","gtsummary")

library(tidyverse)
library(here)
library(tidytuesdayR)
library(gtsummary)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

str(squirrel_data)

names(squirrel_data)

#1

tbl_summary(
  squirrel_data,
  by= "Shift",
  include= c("Age", "Primary Fur Color", "Location", "Foraging", "Approaches", "Runs from"),
  label= list(
    "Age" ~ "Age",
    "Primary Fur Color" ~ "Primary Fur Color",
    "Location" ~ "Location Seen At",
    "Foraging" ~ "Foraging Observed",
    "Approaches" ~ "Approached Humans",
    "Runs from" ~ "Ran From Humans"
  ),
  missing_text = "Observation Not Recorded")  |> 
  add_overall(col_label = "**All Sightings**") |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  modify_header(label = "**Squirrel Characteristic**"
 )
