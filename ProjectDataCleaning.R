library(tidyverse)
library(stringr)
GDP <- read_csv("gdp_pcap.csv")
Emissions <- read.csv("co2_pcap_cons.csv")


#GDP data set:
GDP_clean <- GDP |>
  mutate_at(vars(`1800`:`2100`), as.numeric)

GDP_longer <- pivot_longer(GDP_clean, cols = `1800`:`2100`,
                            names_to ="Year", values_to = "GDP")


#Emissions data set:
Emissions_clean <- Emissions |>
  rename_with(~gsub("^X", "", .), everything()) |>
  mutate_at(vars(`1800`:`2022`), as.numeric)

  
Emissions_longer <- pivot_longer(Emissions_clean, cols = `1800`:`2022`, 
                                  names_to = "Year", values_to = "CO2")


#Merge the two datasets:
merged_data <- full_join(GDP_longer, Emissions_longer, by = c("Year", "country"))


#Build the regression:
regression_model <- lm(CO2 ~ GDP, data = merged_data)
summary(regression_model)


