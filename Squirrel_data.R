# Heidi Sielbach final project

```{r}

here::here()

install.packages("tidyverse","here","tidytuesdayR","gtsummary","knirt")

library(tidyverse)
library(here)
library(tidytuesdayR)
library(gtsummary)
library(knitr)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')
```

#gtsummary table

```{r}
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

tbl_uvregression(
  squirrel_data, 
  y = "Shift",
  include = c("Age", "Primary Fur Color", "Runs from"),
  method = lm)  
```

```{r}

table(squirrel_data$Age)

class(squirrel_data$Age)

?hist()

squirrel_data_age <- factor(x=squirrel_data$Age, 
                            levels=c(0,1,2),
                            labels=c("Unknown","Juvenile","Adult"))
#| label: fig-hist
#| fig-cap: "Squirrel Data"
hist(squirrel_data_age)
```

```{r}

```