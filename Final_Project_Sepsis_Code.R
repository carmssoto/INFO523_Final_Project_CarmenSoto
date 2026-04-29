setwd("C:/Users/carms/OneDrive/Desktop/Final project INFO523")

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)

Hospital_Inpatient_Discharges_SPARCS_De_Identified_Maternal_Sepsis_by_County_and_Demographics_2016_2018 <- read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)
#View(Hospital_Inpatient_Discharges_SPARCS_De_Identified_Maternal_Sepsis_by_County_and_Demographics_2016_2018,show_col_types = FALSE)

sepsis_raw<-read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)


#Data Quality and clean process

colnames(sepsis_raw)
#"Any Sepsis Events During Pregnancy (N)" 
#total # of maternal sepsis events- including both severe sepsis and septic shock
#at any points beween start  of preg and 42 days after delivery 

#check total Patient Sepsis Cases
class(sepsis_raw$`Any Sepsis Events Peripartum (N)`)#[1] "character"

#change to numeric
sepsis_raw <- sepsis_raw %>%
  mutate(`Any Sepsis Events Peripartum (N)` =as.numeric(`Any Sepsis Events Peripartum (N)`))
class(sepsis_raw$`Any Sepsis Events Peripartum (N)`)#"numeric"

#Check which columns I will be working with:
#Patient county,Demographics,#of Live Births,# total Sepsis Cases
unique(sepsis_raw$`Patient County`)
unique(sepsis_raw$`Demographic Strata`)
unique(sepsis_raw$`Year(s) of Live Birth`)
unique(sepsis_raw$`Live Births\n(N)`)#"2016-2018"
unique(sepsis_raw$`Any Sepsis Events Peripartum (N)`)
sum(is.na(sepsis_raw$`Any Sepsis Events Peripartum (N)`))#13 missing values


#Make a small data with my selected columns 

sepsis_small<-sepsis_raw%>%
  select(`Patient County`,`Demographic Strata`,`Live Births\n(N)`,`Any Sepsis Events Peripartum (N)`)%>%
rename(
  Patient_County= `Patient County`,
  Demographic_Strata= `Demographic Strata`,
  Total_Number_of_Live_Births= `Live Births\n(N)`,
  Total_Cases_Any_Maternal_Sepsis= `Any Sepsis Events Peripartum (N)`
)

#checktotal births& total sepsis cases for NA values
sum(is.na(sepsis_small$Total_Cases_Any_Maternal_Sepsis))#13  
sum(is.na(sepsis_small$Total_Number_of_Live_Births))#0


#remove statewide data to keep county data only
#cases by county and demographic group 

county_total.sepsis<-sepsis_small%>%
  filter(Patient_County != "Statewide")%>%
  group_by(Patient_County,Demographic_Strata)%>%
  summarise(
    Total_Cases.Any.Maternal.Sepsis= sum(Total_Cases_Any_Maternal_Sepsis, na.rm = TRUE),
    Total_Live.Births = sum(Total_Number_of_Live_Births, na.rm = TRUE),
    .groups = "drop"
  )%>%
  arrange(desc(Total_Cases.Any.Maternal.Sepsis))

#check out which demographic group I want to focus on
Hispanic.patients_county<-county_total.sepsis%>%
  filter(Demographic_Strata=="Hispanic")
head(Hispanic.patients_county)

#Black.NonHispanic.patients_county<-county_total.sepsis%>%
  #filter(Demographic_Strata=="Black, Non-Hispanic")

#Calculate Sepsis Rate

  
  

  

