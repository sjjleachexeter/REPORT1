##########################
### Question 1 b #########
##########################

## First we load the tidyverse library
library(tidyverse)

## now we use read_delim to read the csv file
x <- read_delim("test_results.csv", delim = ",")
filtered_x <- filter(x, age != "18andUnder" & duration>180)

## we have or filtered list so it is time to take data from this list
filtered_count = nrow(filtered_x) # = 3178
## finding duplicates
duplicates <- filtered_x %>%
    count(PID) %>%
    filter(n>1)
nrow(duplicates) #843
