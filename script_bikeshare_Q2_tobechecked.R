#   2 Popular stations and trip
# What is the most common start station?
# What is the most common end station?
# What is the most common trip from start to end (i.e., most frequent combinat
#

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rlang)

# import original data:
# chicago <- read_csv("C:\\PATH\\chicago.csv")
# new_york <- read_csv("C:\\PATH\\new-york-city.csv")
# washington <- read_csv("C:\\PATH\\washington.csv")

# add city as prefix to column names of each city table:
colnames(chicago) <- paste("Chicago", colnames(chicago), sep = " ")
colnames(new_york) <- paste("New York", colnames(new_york), sep = " ")
colnames(washington) <- paste("Washington", colnames(washington), sep = " ")

# Concatenate city tables to list:
city_list01 <- c(chicago, new_york, washington)
# Convert list to tibble:
city_tibble01 <- as_tibble(city_list01)

####
#Stats Q2:
# create column trip from start station and end station:
city_tibble_Q2 <- city_tibble01 %>% 
  mutate(`Chicago Trip` = str_c(`Chicago Start Station`, `Chicago End Station`, sep = " - "),
         `New York Trip` = str_c(`New York Start Station`, `New York End Station`, sep = " - "),
         `Washington Trip` = str_c(`Washington Start Station`, `Washington End Station`, sep = " - "))

# save start station, end station, and trip as list:
list_stations_trips <- city_tibble_Q2 %>% 
  select(`Chicago Start Station`, `Chicago End Station`, `Chicago Trip`, 
         `New York Start Station`, `New York End Station`, `New York Trip`, 
         `Washington Start Station`, `Washington End Station`, `Washington Trip`)

# turn list into tibble:
tibble_stations_trips <- as_tibble(list_stations_trips) 

####
#HERE IS WHERE PROBLEM STARTS:

#create empty list for loop output:
list_output_stats_Q2 <- c()

# Iterate along grouping variables and save to list_output_stats_Q2: 
for (i in seq_along(list_stations_trips)) {
  list_output_stats_Q2[[i]] <- tibble_stations_trips %>%
    group_by(!!list_stations_trips[[i]]) %>%
    summarize(count01 = n()) %>%
    arrange(desc(count01)) %>% 
    head() 
}

#pass column names as names of elements in list:
names(list_output_stats_Q2) <- colnames(tibble_stations_trips)
# call result list with list names:
list_output_stats_Q2

############
# Q2 plots from Q2 stats output (from above): DOES NOT WORK
#############

tibble_output_stats_Q2 <- as_tibble(list_output_stats_Q2) 

# Subset first column of first data frame of list:
list_output_stats_Q2[[1]][,1]
# same with tibble:
tibble_output_stats_Q2[[1]][,1]
# Subset whole first data frame:
list_output_stats_Q2[[1]]

# --> subsetting works fine, but cannot be used to make plots from list (below):
###############
# tried to make 1 specific plot from specific data frame from list:
list_output_stats_Q2[[1]] %>%
  ggplot(aes(x = list_output_stats_Q2[[1]][,1], y = list_output_stats_Q2[[1]][,2])) +
  geom_bar(stat = "identity") +
  coord_flip()

################
# also tried to make a for loop to make all plots from list_output_stats
# indexing does not work:

#create empty list for loop output (to put plots in):
list_output_plots_Q2 <- c()

# for loop plots Q2:
for (i in seq_along(tibble_output_stats_Q2)) {
  list_output_plots_Q2[[i]] <- tibble_output_stats_Q2 %>%
    ggplot(aes(x = !!tibble_output_stats_Q2[[i]][,i], 
               y = !!tibble_output_stats_Q2[[i]][,i+1])) +
    geom_bar(stat = "identity") +
    coord_flip()
}
