#Load R libraries 
library(readr)
library(ggplot2)
library(reshape2)
library(dplyr)

#STEPS 1-3 I was able to solve on my own after many attempts
#STEP 4- GEOSPATIAL VISUAL was difficult to create on my own AI USE DISCLOSURE listed. 

#I Upload NYSOH_Maternal Sepsis SPARCS:2016-2018 Data Set
sepsis_raw<-read_csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___Maternal_Sepsis_by_County_and_Demographics__2016-2018.csv",show_col_types = FALSE)

#**1ST STEP Data Quality and clean process
#Read through the Sources the data dictionary and Overview documents
colnames(sepsis_raw)
#All Sepsis counts presented in the data set is used as the numerator for calculation of sepsis rates within each maternal window
#"Any Sepsis Events During Pregnancy (N)" - The number of all maternal sepsis events
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

#**2ND STEP Smaller data frames Data Cleaning

#SMALL RAW DATA 
#I first make my small data frame with my selected columns 
#rename my columns to clean up my code

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

#COUNTY DATA
#I then made a county data frame cases by county 
#group by demographic group #The strata of the demographic characteristic 
##remove statewide data to keep county data only

county.sepsis<-sepsis_small%>%
  filter(Patient_County != "Statewide")%>%
  group_by(Patient_County,Demographic_Strata)%>%
  summarise(
    Total_Cases.Any.Maternal.Sepsis= sum(Total_Cases_Any_Maternal_Sepsis, na.rm = TRUE),
    Total_Live.Births = sum(Total_Number_of_Live_Births, na.rm = TRUE),
    .groups = "drop"
  )%>%
  arrange(desc(Total_Cases.Any.Maternal.Sepsis))

#MINORITY DATA- HISPANIC 
# I then check out which Minority demographic group of my interest I want to focus on for HD research
unique(county.sepsis$Demographic_Strata)
# Interested in looking into the minority group for Black non Hispanic patients and Hispanic patients

#Black.NonHispanic_county<-county.sepsis%>%
# filter(Demographic_Strata=="Black, Non-Hispanic")

hispanic_county<-county.sepsis%>%
  filter(Demographic_Strata=="Hispanic")
# My preference for the minority group selection for this projects focus will be Hispanic patient population

#*STEP 3 HISPANIC SEPSIS RATES 
#I then Calculate Sepsis Rate in Hispanic Patients Per County 
#DATA DICTONARY
#The rate of all maternal sepsis events (including severe sepsis/septic 
#shock) identified via diagnosis coding at any point between the 
#beginning of pregnancy through 42 days post-delivery per 100,000 
#eligible live births within the specified patient county and demographic 
#strata.

#HISPANIC.SEPSIS RATES DATA
hispanic_sepsis.rate<-hispanic_county%>%
  mutate(Sepsis_Rate=(Total_Cases.Any.Maternal.Sepsis/Total_Live.Births)*100000)%>%
  filter(!is.na(Sepsis_Rate))%>%
  arrange(desc(Sepsis_Rate))#list the counties based on highest sepsis rates among Hisp. Population

##^This will be my filter code for my worked visuals 


#*STEP 4 Geospatial Worked Example

#all 61 counties too large 
ggplot(hispanic_sepsis.rate,
       aes(x=Patient_County,y=Sepsis_Rate))+
  geom_col(fill="navy")+
  labs(title="Maternal Sepsis Rates by County Among Hispanic Patients in New York State (SPARCS,2016-2018)",
       x = "County",
       y="Maternal Sepsis Cases per 100,000 Eligible Live Births"
  )

#Top 10 higher rates of Maternal Sepsis in Hispanic patients higher risk 
top10.hisp.counties<-hispanic_sepsis.rate%>%
  slice(1:10)

#top 10counties xHisp.sepsisrate Bar chart Visual
ggplot(top10.hisp.counties,
       aes(x=Patient_County,y=Sepsis_Rate))+
  geom_col(fill="navy")+
  coord_flip()+
  labs(
    title="Top 10 Counties in New York State with the Highest Maternal Sepsis Rates Among Hispanic Patients in 2016-2018 (SPARCS)",
    x="County",
    y="Maternal Sepsis Rate per 100,000 Eligible Live Births"
  )


#This will be used to explain and display regional data viz
#Geospatial Packages:
#library(raster)
#library(sf)
#library(maps)
#Installed maps to work on New York Maps for Counties and Regions
#library(maps)
#this is to create a heat map 
#the guided resourced used https://www.geeksforgeeks.org/r-language/geospatial-data-analysis-with-r/
#Another resourced used for GEODATA
#https://gis.ny.gov/civil-boundaries

#Rubric Step 4
#I selected the method of Geospatial Analysis- Mapping disparities across regions to guide targeted interventions

#Notes
#62 counties in New York  State 
#only 61 counties report sepsis data during the years of 2016-2018

#Stopping point- 04/30/2026 12:24-cs 
#drafting 
library(maps)
NY<-map_data("county")%>%
  filter(region=="new york")%>%
  left_join(hispanic_sepsis.rate%>%
              mutate())

library(maps)

library(maps)

ggplot(
  map_data("county") %>%
    filter(region == "new york") %>%
    left_join(
      hispanic_sepsis.rate %>%
        mutate(subregion = tolower(Patient_County)),
      by = "subregion"
    ),
  aes(long, lat, group = group, fill = Sepsis_Rate)
) +
  geom_polygon() +
  coord_fixed() +
  theme_void()

# 