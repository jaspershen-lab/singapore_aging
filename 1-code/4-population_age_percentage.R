no_source()

library(tidyverse)

setwd(r4projects::get_project_wd())

# Read data
data1 <- readr::read_csv("2-data/age_structure_data_historical.csv")
data2 <- readr::read_csv("2-data/age_structure_data_predicted.csv")


data1 <-
data1  %>% 
dplyr::select(Country, Sex, Age, Time, Value)  %>% 
dplyr::filter(Age %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29",
                                     "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                     "50 to 54", "55 to 59", "60 to 64", "65 to 69",
                                     "70 to 74", "75 to 79", "80 to 84", "85 and over")) %>%
                                     dplyr::filter(Sex == "Total")  %>% 
dplyr::filter(Country %in% c("Singapore", "China (People's Republic of)", "Japan", "Korea", "United States", "Germany"))

data1 <-
data1  %>% 
dplyr::group_by(Country, Time)  %>% 
dplyr::summarize(rate = Value/sum(Value) * 100,
Age = Age)

###for data1, I want to dra
data1  %>% 
dplyr::filter(Country == "Singapore")  %>%
ggplot(aes())


total <-
data2  %>% 
dplyr::filter(Country == "Singapore")  %>% 
dplyr::filter(Age == "Total",
Sex == "Total")  %>%
dplyr::select(Time, Age, Value)  %>% 
dplyr::arrange(Time)

over60 <- 
data2  %>% 
dplyr::filter(Country == "Singapore")  %>%
dplyr::filter(AGE == "65_OVER",
Sex == "Total")  %>% 
dplyr::select(Time, Age, Value)  %>% 
dplyr::arrange(Time)

data2 <-
total  %>% 
dplyr::mutate(rate = over60$Value / total$Value * 100)

# data2 <-
# data2  %>% 
# dplyr::filter(Country == "Singapore")  %>% 
# dplyr::filter(Age != "Total")  %>% 
# dplyr::filter(AGE %in% c("0_4", "05_9", "10_14", "15_19", "20_24",
#          "25_29", "30_34", "35_39", "40_44", "45_49",
#          "50_54", "55_59", "60_64", "65_69",
#          "70_74", "75_79", "80_84", "85_OVER"))  %>% 
#          dplyr::group_by(AGE)  %>% 
#          dplyr::summarize(AGE = AGE,
#          TIME = TIME,
#          Value = sum(Value))


dir.create("3-data1_analysis/4-population_age_percentage", showWarnings = FALSE)

setwd("3-data1_analysis/3-singapore_population_age_percentage")

data1$`Data Series`

data1 <-
data1  %>% 
dplyr::select(-"Data Series")  %>% 
head(1)  %>% 
t()  %>% 
as.data.frame()  %>% 
tibble::rownames_to_column(var = "year" ) 

colnames(data1)[2] <- "rate"

data1 <-
data1  %>% 
dplyr::filter(year != "1957")

data1$rate <- as.numeric(data1$rate)

data1$year


data2$Time <-
as.character(data2$Time)

data1  %>% 
dplyr::full_join(data2[,c("Time", "rate")],
by = c("year" = "Time"))  %>% 
dplyr::filter(!is.na(rate.x) & !is.na(rate.y))  %>% 
ggplot(aes(rate.x, rate.y)) +
geom_point() +
geom_abline(intercept = 0, slope = 1)


plot <-
data1 %>% 
    ggplot(aes(year, rate)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    # geom_hline(yintercept = 65,
    # color = "red") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Year",
    y = "Percentage of population ages 65 or older")
    # scale_y_continuous(limits = c(0, 6))


plot

ggsave(plot, filename = "singapore_population_age_percentage.pdf", 
width = 10, height = 6, units = "in")
