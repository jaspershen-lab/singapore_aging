no_source()

library(tidyverse)

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

# Read in data
data <- readxl::read_xlsx("2-data/singapore_population_age_percentage.xlsx")

dir.create("3-data_analysis/3-singapore_population_age_percentage", showWarnings = FALSE)

setwd("3-data_analysis/3-singapore_population_age_percentage")

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

plot <-
data %>% 
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
