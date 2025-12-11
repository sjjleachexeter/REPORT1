######################################
############## Part 1 ################
######################################

####### Question 2 #######

## First we load the tidyverse library
library(tidyverse)

## now we use read_delim to read the csv file
x = read_delim("test_results.csv", delim = ",")
filtered_x = filter(x, age != "18andUnder" & duration>180)

## we have or filtered list so it is time to take data from this list
filtered_count = nrow(filtered_x) # = 3178
## finding duplicates
duplicates = filtered_x %>%
    #count() counts the observations of each element in the specified column
    count(PID) %>%
    filter(n>1)
nrow(duplicates) # = 843



###################################
############ Part 2 ###############
###################################

########## Question 1 #############

## To compare the data of the 2 educational groups, we'll
## create a graph comparing their scores on a bar chart
further_filtered = filtered_x %>%
    filter(edu == "College/university degree (4 yr)" | edu == "Less than 12 years")


edugraph = further_filtered %>%
    ggplot(mapping = aes(x = edu, y = prop_correct)) +
    # to compare 2 discrete groups against each other based on continuous data, box plots are the best visualiser
    geom_boxplot() +
    labs(x = "Education", y = "Score")

ggsave("ComparativeEducations.png", plot = edugraph)


############ Question 2 ##############

# using the whole filtered group found in Part 1, not the education groups in Part 2 question 1
duration_summary = filtered_x %>%
    # The IQR, mean, standard deviation are all essential for describing duration
    summarise(mean_duration = mean(duration),
              sd_duration = sd(duration),
              q1_duration = quantile(duration, probs=0.25),
              q2_duration = quantile(duration, probs=0.5),
              q3_duration = quantile(duration, probs=0.75),
              smallestval = min(duration),
              largestval = max(duration)
    )


###################################################
##################### Part 3 ######################
###################################################

######### Question 1 ###########

y = read_delim("tda_key.csv", delim = ",")
all_correct = filter(y, prop_item_correct == 1)


######## Question 2 (b) ########

# noValue = filter(y, gbooks_freq = NA)
# percentageNA = (count(noValue)/count(y))*100

noValue = filter(y, is.na(gbooks_freq))
intnoValue = nrow(noValue)
totalRows = nrow(y)
percentNA = (intnoValue/totalRows)*100

######### Question 3 ###########

adjectiveRelationship = y %>%
    # here we remove rows with no data
    drop_na(gbooks_freq) %>%
    ggplot(mapping = aes(x = gbooks_freq, y = prop_item_correct)) +
    geom_point() +
    geom_smooth()
ggsave("AdjectiveRelationship.png", plot = adjectiveRelationship)