no_source()

library(tidyverse)

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

# Read in data
data <- readxl::read_xlsx("2-data/singapore_life_expectancy.xlsx")
data$`Data Series`

data <-
data  %>% 
dplyr::select(-"Data Series")  %>% 
head(1)  %>% 
t()  %>% 
as.data.frame()  %>% 
tibble::rownames_to_column(var = "year" ) 

colnames(data)[2] <- "rate"

data <-
data  %>% 
dplyr::filter(year != "1957")

data$rate <- as.numeric(data$rate)

data$year

life_expanctancy <-
data

# Read in data
data <- readxl::read_xlsx("2-data/singapore_fertility_rate.xlsx")

data$`Data Series`

data <-
data  %>% 
dplyr::select(-"Data Series")  %>% 
head(1)  %>% 
t()  %>% 
as.data.frame()  %>% 
tibble::rownames_to_column(var = "year" ) 

colnames(data)[2] <- "rate"

data$rate <- as.numeric(data$rate)

data$year <- as.numeric(data$year)

fertility_data <-
data

dir.create("3-data_analysis/2-singapore_fertility_life_expectancy", showWarnings = FALSE)
setwd("3-data_analysis/2-singapore_fertility_life_expectancy")

fertility_data
life_expanctancy

colnames(fertility_data)[2] <- "fertility_rate"
colnames(life_expanctancy)[2] <- "life_expectancy"

life_expanctancy$year <-
as.numeric(life_expanctancy$year)

data <-
fertility_data  %>% 
left_join(life_expanctancy, by = "year") 

plot <-
ggplot() +
geom_point(aes(x = year, y = fertility_rate), 
color = fertility_rate_color  ,
data = filter(data, !is.na(fertility_rate))) +
geom_line(aes(group = 1, 
x = year,
y = fertility_rate), 
color = fertility_rate_color,
data = filter(data, !is.na(fertility_rate))) +
geom_line(aes(group = 1, 
x = year,
y = life_expectancy / 20), 
color = life_expanctancy_color,
data = filter(data, !is.na(life_expectancy))) +
geom_point(aes(x = year, 
y = life_expectancy / 20), 
color = life_expanctancy_color,
data = filter(data, !is.na(life_expectancy))) +
 geom_hline(yintercept = 2.1,
    color = fertility_rate_color) +
geom_vline(xintercept = 1975,
    color = fertility_rate_color) +
geom_hline(yintercept = 65 / 20,
    color = life_expanctancy_color) +
geom_vline(xintercept = 1970,
    color = life_expanctancy_color) +
  scale_y_continuous(   
    "Fertility rate",
    sec.axis = sec_axis(~ . * 20, name = "Life expectancy")
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1)) +
  scale_x_continuous(breaks = seq(1950, 2020, 5),
  labels = seq(1950, 2020, 5))  +
  labs(x = "")

plot

ggsave(plot, filename = "singapore_fertility_life_expectancy.pdf", 
width = 6, height = 6, units = "in")
