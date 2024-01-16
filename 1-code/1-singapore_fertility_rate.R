no_source()

library(tidyverse)

setwd(r4projects::get_project_wd())

# Read in data
data <- readxl::read_xlsx("2-data/singapore_fertility_rate.xlsx")

dir.create("3-data_analysis/1-singapore_fertility_rate", showWarnings = FALSE)

setwd("3-data_analysis/1-singapore_fertility_rate")

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

data$year

plot <-
data %>% 
    ggplot(aes(year, rate)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    geom_hline(yintercept = 2.1,
    color = "red") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Year",
    y = "Total Fertility Rate") +
    scale_y_continuous(limits = c(0, 6))


plot

ggsave(plot, filename = "singapore_fertility_rate.pdf", 
width = 10, height = 6, units = "in")
