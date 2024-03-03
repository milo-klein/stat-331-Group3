library(tidyverse)
library(stringr)

#setwd('C:/Users/sdege/OneDrive - Cal Poly/Masters Classes/STAT 531/Final')
GDP <- read_csv("gdp_pcap.csv")
Emissions <- read.csv("co2_pcap_cons.csv")

summary(GDP)
class(GDP[["1801"]])
GDP[[5,"2015"]]

#GDP data set:



#### WINNER WINNER CHICKEN DINNER
country_names<-GDP[1]

clean_gdp_value <- function(df) {
  df |>
    mutate(across(everything(), ~case_when(is.character(.) & str_detect(., "k") ~ as.numeric(str_replace(., "k", "")) * 1000,
                            TRUE ~ as.numeric(.))))
}

GDP_clean <- clean_gdp_value(GDP)
GDP_clean[1] <- country_names
view(GDP_clean)


GDP_longer <- pivot_longer(GDP_clean, cols = `1800`:`2100`,
                            names_to ="Year", values_to = "GDP")
view(GDP_longer)



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

ggplot(CO2, GDP)


