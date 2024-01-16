no_source()

library(tidyverse)

setwd(r4projects::get_project_wd())

# Read data
###data1 is historical data, and data 2 is predicted data
data1 <- readr::read_csv("2-data/age_structure_data_historical.csv")
data2 <- readr::read_csv("2-data/age_structure_data_predicted.csv")

data1$`Reference area`[data1$`Reference area` == "China (People’s Republic of)"] <- "China"

data1 <-
data1  %>% 
dplyr::rename(Country = "Reference area",
Time_horizon = "Time horizon",
Time = "TIME_PERIOD",
Value = OBS_VALUE)  %>% 
dplyr::select(Country, Sex, Time_horizon, Age, Time, Value) %>% 
    dplyr::filter(Age %in% c("4 years or less", 
    "From 5 to 9 years",
    "From 10 to 14 years",
    "From 15 to 19 years",
    "From 20 to 24 years",
    "From 25 to 29 years",
    "From 30 to 24 years",
    "From 35 to 39 years",
    "From 40 to 44 years",
    "From 45 to 49 years",
    "From 50 to 54 years",
    "From 55 to 59 years",
    "From 60 to 64 years",
    "From 65 to 69 years",
    "From 70 to 74 years",
    "From 75 to 79 years",
    "From 80 to 84 years",
    "85 years or over")) %>%
    dplyr::filter(Sex == "Total") %>%
        dplyr::mutate(Age_range = case_when(Age %in% c("4 years or less", "From 5 to 9 years", 
    "From 10 to 14 years", "From 15 to 19 years") ~ "0-19",
    Age %in% c("From 20 to 24 years", "From 25 to 29 years", "From 30 to 34 years", "From 35 to 39 years", "From 40 to 44 years",
    "From 45 to 49 years", "From 50 to 54 years", "From 55 to 59 years", "From 60 to 64 years") ~ "20-64",
    Age %in% c("From 65 to 69 years", "From 70 to 74 years", "From 75 to 79 years", "From 80 to 84 years", "85 years or over") ~ "Over 65")) %>%
    dplyr::filter(Country %in% c("Singapore", "China", "Japan", "Korea", "United States", "Germany"))

data1 <-
data1 %>%
    dplyr::group_by(Country, Time) %>%
    dplyr::summarize(rate = Value / sum(Value) * 100,
                                     Age_range = Age_range) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country, Age_range, Time) %>%
    dplyr::summarize(rate = sum(rate))

# This code takes the 'data1' dataframe and adds a new column 'Age_range' as a factor variable.
# The levels of the 'Age_range' variable are set to "Over 65", "20-64", and "0-19".
# It then creates a bar plot using ggplot, where the x-axis represents 'Time' and the y-axis represents 'rate'.
# The bars are filled based on the 'Age_range' variable.
# The plot is displayed in a grid of facets, with each facet representing a different 'Country'.
# The grid has 3 columns.
data1 %>%
    dplyr::mutate(Age_range = factor(Age_range, levels = c("Over 65", "20-64", "0-19"))) %>%
    ggplot(aes(Time, rate)) +
    geom_bar(stat = "identity", aes(fill = Age_range)) +
    theme_bw() +
    facet_wrap(~Country, ncol = 3)

# This code filters the 'data1' dataframe to include only the rows where the 'Age_range' column is "Over 65".
# It then creates a line plot using ggplot, where the x-axis represents the 'Time' column and the y-axis represents the 'rate' column.
# Each data point is colored based on the 'Country' column, and lines are drawn to connect the data points for each country.

data1  %>% 
    dplyr::filter(Age_range == "Over 65")  %>% 
    ggplot(aes(Time, rate)) +
    geom_point(aes(color = Country)) +
    geom_line(aes(group = Country, color = Country))



data2$`Reference area`[data2$`Reference area` == "China (People’s Republic of)"] <- "China"

data2 <-
data2  %>% 
dplyr::rename(Country = "Reference area",
Time_horizon = "Time horizon",
Time = "TIME_PERIOD",
Value = OBS_VALUE)  %>% 
dplyr::select(Country, Sex, Time_horizon, Age, Time, Value) %>% 
    dplyr::filter(Age %in% c("4 years or less", 
    "From 5 to 9 years",
    "From 10 to 14 years",
    "From 15 to 19 years",
    "From 20 to 24 years",
    "From 25 to 29 years",
    "From 30 to 24 years",
    "From 35 to 39 years",
    "From 40 to 44 years",
    "From 45 to 49 years",
    "From 50 to 54 years",
    "From 55 to 59 years",
    "From 60 to 64 years",
    "From 65 to 69 years",
    "From 70 to 74 years",
    "From 75 to 79 years",
    "From 80 to 84 years",
    "85 years or over")) %>%
    dplyr::filter(Sex == "Total") %>%
        dplyr::mutate(Age_range = case_when(Age %in% c("4 years or less", "From 5 to 9 years", 
    "From 10 to 14 years", "From 15 to 19 years") ~ "0-19",
    Age %in% c("From 20 to 24 years", "From 25 to 29 years", "From 30 to 34 years", "From 35 to 39 years", "From 40 to 44 years",
    "From 45 to 49 years", "From 50 to 54 years", "From 55 to 59 years", "From 60 to 64 years") ~ "20-64",
    Age %in% c("From 65 to 69 years", "From 70 to 74 years", "From 75 to 79 years", "From 80 to 84 years", "85 years or over") ~ "Over 65")) %>%
    dplyr::filter(Country %in% c("Singapore", "China", "Japan", "Korea", "United States", "Germany"))

data2 <-
data2 %>%
    dplyr::group_by(Country, Time) %>%
    dplyr::summarize(rate = Value / sum(Value) * 100,
                                     Age_range = Age_range) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country, Age_range, Time) %>%
    dplyr::summarize(rate = sum(rate))

# This code takes the 'data2' dataframe and adds a new column 'Age_range' as a factor variable.
# The levels of the 'Age_range' variable are set to "Over 65", "20-64", and "0-19".
# It then creates a bar plot using ggplot, where the x-axis represents 'Time' and the y-axis represents 'rate'.
# The bars are filled based on the 'Age_range' variable.
# The plot is displayed in a grid of facets, with each facet representing a different 'Country'.
# The grid has 3 columns.
data2 %>%
    dplyr::mutate(Age_range = factor(Age_range, levels = c("Over 65", "20-64", "0-19"))) %>%
    ggplot(aes(Time, rate)) +
    geom_bar(stat = "identity", aes(fill = Age_range)) +
    theme_bw() +
    facet_wrap(~Country, ncol = 3)

# This code filters the 'data2' dataframe to include only the rows where the 'Age_range' column is "Over 65".
# It then creates a line plot using ggplot, where the x-axis represents the 'Time' column and the y-axis represents the 'rate' column.
# Each data point is colored based on the 'Country' column, and lines are drawn to connect the data points for each country.

data2  %>% 
    dplyr::filter(Age_range == "Over 65")  %>% 
    ggplot(aes(Time, rate)) +
    geom_point(aes(color = Country)) +
    geom_line(aes(group = Country, color = Country))

rbind(data1, data2)  %>% 
dplyr::filter(Age_range == "Over 65")  %>% 
dplyr::filter(Country == "Singapore")  %>%
ggplot(aes(Time, rate)) +
geom_point(aes(color = Country)) +
geom_line(aes(group = Country,
color = Country)) +
theme_bw()
