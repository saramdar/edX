library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Q1
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max()

#Q2
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% min()
temp_carbon %>% filter(year == 1751 & !is.na(carbon_emissions)) %>% select(year, carbon_emissions)

#Q3
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()
p

temp_carbon %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% max()
temp_carbon %>% filter(year == 2018 & !is.na(temp_anomaly)) %>% select(year, temp_anomaly)

#Q4, 5, 6, 7
p + geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

#Q7
#create combined data table
library(reshape)
melt_data <- melt(temp_carbon, id = 'year')
melt_data

#plot temp, land, ocean, and mean on same graph
variables <- c("temp_anomaly", "land_anomaly", "ocean_anomaly")
melt_data %>%
  filter(variable %in% variables) %>%
  ggplot(aes(year, value, color = variable))+
  geom_line() +
  xlim(1875, 2020) +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

#Q8 and 9
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(rows = vars(gas), scales = "free") + 
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


greenhouse_gases %>%
  ggplot(aes(year, concentration, col = gas)) +
  geom_line() + 
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#Q10
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

temp_carbon %>% 
  filter(year %in% c(1960, 2014)) %>%
  select(year, carbon_emissions)
9855/2569

temp_carbon %>% 
  filter(year %in% c(1978, 2014)) %>%
  select(year, carbon_emissions)
9855/5074

#Q11
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line()

co2_time +
  xlim(-3000, 2018)

