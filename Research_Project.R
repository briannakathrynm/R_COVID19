# Importing packages
library(readr)
library(tidyverse)
library(plotly)
library(ggthemes)
library(gganimate)
library(geosphere)
library(DT)
library(scales)
library(openair)
library(corrplot)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(ggdark)

# Importing data
ts_confirmed <- read_csv(file = "time_series_covid_19_confirmed.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_recovered <- read_csv(file = "time_series_covid_19_recovered.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_deaths <- read_csv(file = "time_series_covid_19_deaths.csv",
                      col_types = cols(
                        .default = col_double(),
                        `Province/State` = col_character(),
                        `Country/Region` = col_character()
                      ))

codes <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv',
                  col_types = cols(
                    COUNTRY = col_character(),
                    `GDP (BILLIONS)` = col_double(),
                    CODE = col_character()
                  ))
# Preprocessing Data
ts_confirmed <- ts_confirmed %>%
  gather("Date", "Confirmed", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_recovered <- ts_recovered %>%
  gather("Date", "Recovered", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_deaths <- ts_deaths %>%
  gather("Date", "Deaths", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

# Organizing data
ts_total <- ts_confirmed %>%
  left_join(ts_deaths) %>%
  left_join(ts_recovered) %>%
  mutate(Recovered = replace_na(Recovered, replace = 0))

## We all know "Diamond Princess" and "MS Zaandam" are cruises, So we have to remove them from the data

ts_total <- ts_total %>%
  filter(`Country/Region` != "Diamond Princess") %>%
  filter(`Country/Region` != "MS Zaandam")

ts_total$Deaths[is.na(ts_total$Deaths)] <- 0

## Created a dataset including latest news of COVID-19

cases_latest <- ts_total %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed  = sum(Confirmed),
            Recovered = sum(Recovered),
            Deaths = sum(Deaths)) %>%
  mutate("New Cases" = Confirmed - lag(Confirmed, 1) ) %>%
  filter(Date == max(Date))

day_latest <- max(cases_latest$Date)

cases_total_date <- ts_total %>%
  rename(Region = `Country/Region`) %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate("New_Cases" = Confirmed - lag(Confirmed, 1))

cases_total_date$New_Cases[is.na(cases_total_date$New_Cases)] <- 0 

cases_total_latest <- cases_total_date %>%
  filter(Date == max(Date))

# Visualization
cases_all <- cases_total_date %>%
  select(-Confirmed, -New_Cases) %>%
  gather("Status", "Cases", -"Date")

barchart <- ggplot(data = cases_total_date, aes(x = Date)) +
  geom_bar(aes(y = Confirmed), position = "stack", stat = "identity", fill = "#ff5050") +
  geom_bar(data = cases_all, aes(y = Cases, fill = Status), position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#000000", "#009900")) +
  scale_y_continuous(breaks = seq(0, 21000000, by = 1000000), labels = comma) +
  theme_solarized(base_size = 10, light = TRUE)+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
        panel.background = element_rect(fill = "White"),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("World COVID-19 Total Cases by Day")

ggplotly(barchart) %>%
  layout(legend = list(orientation = 'h'))