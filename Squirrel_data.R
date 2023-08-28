# Heidi Sielbach final project


here::here()

install.packages("tidyverse","here","tidytuesdayR","gtsummary","knirt")

library(tidyverse)
library(here)
library(tidytuesdayR)
library(gtsummary)
library(knitr)

squirrel_data <- read_csv(here::here("data", "squirrel_data.csv"))


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
    "Primary_Fur_Color" ~ "Primary Fur Color",
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

table(squirrel_data$Shift)


squirrel_data$Age1 <- ifelse(squirrel_data$Age=="Adult", 2, ifelse(squirrel_data$Age=="Juvenile", 1, NA))
squirrel_data$Primary_Fur_Color1 <- ifelse(squirrel_data$Primary_Fur_Color=="Black", 3, ifelse(squirrel_data$Primary_Fur_Color=="Cinnamon", 2, 1))
squirrel_data$Location1 <- ifelse(squirrel_data$Location=="Above Ground", 2, 1)
squirrel_data$Foraging1 <- ifelse(squirrel_data$Foraging=="True", 2, 1)
squirrel_data$Approaches1 <- ifelse(squirrel_data$Approaches=="True", 2, 1)
squirrel_data$Runs_from1 <- ifelse(squirrel_data$Runs_from=="True", 2, 1)
squirrel_data$Shift1 <- ifelse(squirrel_data$Shift=="AM", 2, 1)

table(squirrel_data$Age, squirrel_data$Age1)
table(squirrel_data$Primary_Fur_Color, squirrel_data$Primary_Fur_Color1)
table(squirrel_data$Location, squirrel_data$Location1)
table(squirrel_data$Foraging, squirrel_data$Foraging1)
table(squirrel_data$Approaches, squirrel_data$Approaches1)
table(squirrel_data$Runs_from, squirrel_data$Runs_from1)
table(squirrel_data$Shift, squirrel_data$Shift1)


linear_model_squirrel <- lm(Shift1 ~ Age1 + Primary_Fur_Color1 + Location1, 
                   data = squirrel_data)

tbl_regression(
  linear_model_squirrel, 
  intercept = TRUE,
  label = list(
    Age1 ~ "Age",
    Primary_Fur_Color1 ~ "Primary Fur Color",
    Location1 ~ "Location Seen At"
  ))

hist(squirrel_data$Primary_Fur_Color1, breaks=c(0,1,2,3), col="pink", 
     main="Squirrel Colors (n=2698)", border="black", 
     xlab= "Fur Color (1=Grey, 2=Cinnamon, 3=Black)", ylab="Number of Squirrels with Color")


x <- c(1,2,6,8,17)


data_spread <- function(x) {
  a <- max(x, na.rm=TRUE)
  b <- min(x, na.rm=TRUE)
  spread <- abs(a-b)
  return(spread)
}

data_spread(squirrel_data$Primary_Fur_Color1)

list(n = nrow(data),
     mean_age = mean(data$age))

usethis::use_readme_rmd


?read_csv()
