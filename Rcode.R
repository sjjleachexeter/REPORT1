##########################
### Question 1 b #########
##########################

## First we load the tidyverse library
library(tidyverse)

## now we use read_delim to read the csv file
x = read_delim("test_results.csv", delim = ",")
filtered_x = filter(x, age != "18andUnder" & duration>180)

## we have or filtered list so it is time to take data from this list
filtered_count = nrow(filtered_x) # = 3178
## finding duplicates
duplicates = filtered_x %>%
    count(PID) %>%
    filter(n>1)
nrow(duplicates) # = 843



###################################
##### Question 2 a ################
###################################

## To compare the data of the 2 educational groups, we'll
## create a graph comparing their scores on a bar chart
further_filtered = filtered_x %>%
    filter(edu == "College/university degree (4 yr)" | edu == "Less than 12 years")

graph = further_filtered %>%
    ggplot(aes(x=age, y=prop_correct,fill=edu)) +
    geom_col(position = "dodge") + 
    labs (x="Age", y="Score", fill = "Education")

ggsave("question2a.pdf", plot = graph)
