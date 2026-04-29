library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)


#1st step:Upload NYSOH_Maternal Sepsis SPARCS:2016-2018 Data Set
sepsis_raw<-read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)


#2nd Step:Data Quality and clean process
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

#Data Cleaning
#   Make a small data frame with my selected columns 
sepsis_small<-sepsis_raw%>%
  select(`Patient County`,`Demographic Strata`,`Live Births\n(N)`,`Any Sepsis Events Peripartum (N)`)%>%
rename(
  Patient_County= `Patient County`,
  Demographic_Strata= `Demographic Strata`,
  Total_Number_of_Live_Births= `Live Births\n(N)`,
  Total_Cases_Any_Maternal_Sepsis= `Any Sepsis Events Peripartum (N)`
)

#check total births& total sepsis cases for NA values
sum(is.na(sepsis_small$Total_Cases_Any_Maternal_Sepsis))#13  
sum(is.na(sepsis_small$Total_Number_of_Live_Births))#0

#remove statewide data to keep county data only

#cases by county and demographic group 

County_total.sepsis<-sepsis_small%>%
  filter(Patient_County != "Statewide")%>%
  group_by(Patient_County,Demographic_Strata)%>%
  summarise(
    Total_Cases.Any.Maternal.Sepsis= sum(Total_Cases_Any_Maternal_Sepsis, na.rm = TRUE),
    Total_Live.Births = sum(Total_Number_of_Live_Births, na.rm = TRUE),
    .groups = "drop"
  )%>%
  arrange(desc(Total_Cases.Any.Maternal.Sepsis))

#check out which Minority Group demographic group I want to focus on

Hispanic_County<-County_total.sepsis%>%
  filter(Demographic_Strata=="Hispanic")

Black.NonHispanic_County<-County_total.sepsis%>%
  #filter(Demographic_Strata=="Black, Non-Hispanic")


#Calculate Sepsis Rate
# *Pattern Evaluation in the Hispanic Patient Data frame
  
Hispanic_Sepsis.rate<-Hispanic_County%>%
  mutate(Sepsis_Rate=(Total_Cases.Any.Maternal.Sepsis/Total_Live.Births)*100000)%>%
  filter(!is.na(Sepsis_Rate))%>%
  arrange(desc(Sepsis_Rate))#list the counties based on highest sepsis rates among Hisp. Population


#Geospatial Worked Example Rubric Step 4

#Geospatial Analysis- Mapping disparities across regions to guide targeted interventions

#62 counties in New York  State 
#only 61 states report sepsis data during the years of 2016-2018

#Counties
top10_counties <- Hispanic_Sepsis.rate %>%
  slice(1:10)

#Visual code-
ggplot(top10_counties,
       aes(x = reorder(Patient_County, Sepsis_Rate),
           y = Sepsis_Rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Counties: Maternal Sepsis Rate (Hispanic Population)",
    x = "County",
    y = "Rate per 100,000 Live Births"
  )



#Region data

