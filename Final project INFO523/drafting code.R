setwd("C:/Users/carms/OneDrive/Desktop/Final project INFO523")
library(readr)
library(ggplot2)
library(dplyr)

Hospital_Inpatient_Discharges_SPARCS_De_Identified_Maternal_Sepsis_by_County_and_Demographics_2016_2018 <- read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)
View(Hospital_Inpatient_Discharges_SPARCS_De_Identified_Maternal_Sepsis_by_County_and_Demographics_2016_2018,show_col_types = FALSE)
sepsis<-read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)

summary(sepsis)
names(sepsis)

county<-sepsis%>%
  filter(`Patient County` != "Statewide")

hispanic_patients_county<-county_patients%>%
  filter(`Demographic Characteristics`==)
         
         
# Data Transformation step for Hispanic-specific mining
hispanic_sepsis_map <- sepsis %>%
  filter(`Patient County` != "Statewide", 
         `Demographic Characteristics` == "Race/Ethnicity",
         `Demographic Strata` == "Hispanic") %>%
  # Convert suppressed "SS" to NA and rates to numeric
  mutate(Sepsis_Rate = as.numeric(as.character(`Any Sepsis Events Peripartum (per 100,000)`)))     
         
         
map_data_clean <- sepsis %>%
  # Filter out statewide data as discussed for your geospatial project
  filter(`Patient County` != "Statewide", 
         `Demographic Strata` == "Total") %>%
  # Use backticks to handle the spaces and parentheses exactly
  mutate(Sepsis_Rate = as.numeric(as.character(`Any Sepsis Events Peripartum (per 100,000)`)))         
         

