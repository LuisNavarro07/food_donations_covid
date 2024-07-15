# Census Data: 
# Indiana Population Example 

# Setup: clean environment and load libraries 
# Get Census Data 
rm(list = ls())
### Setup 
if(!require(pacman)) {install.packages("pacman")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(sf)) {install.packages("sf")}
if(!require(lubridate)) {install.packages("lubridate")}
if(!require(tidycensus)) {install.packages("tidycensus")}
if(!require(tigris)) {install.packages("tigris")}
if(!require(purrr)) {install.packages("purrr")}
if(!require(rio)) {install.packages("rio")}
library(pacman)
pacman::p_load(tidyverse, sf, lubridate, 
               tidycensus, tigris, purrr)
options(scipen = 999)
#path = "C:/Users/luise/OneDrive - Indiana University/Research/School_Districts/"
path <- here("/Users/luisenriquenavarro/Library/CloudStorage/OneDrive-SharedLibraries-IndianaUniversity/[Sec-E] BL-SPEA-FoodDonations - [Sec-E] BL-SPEA-FoodDonations")

bi = here(path,'stata','build', 'input')
bc = here(path,'stata','build', 'code')
bo = here(path,'stata','build', 'output')
bt = here(path,'stata','build', 'temp')
ai = here(path,'stata','analysis','input')
ac = here(path,'stata','analysis','code')
ao = here(path,'stata','analysis','output')
at = here(path,'stata','analysis','temp')

# Check this book for details on how to use this library 
# https://walker-data.com/census-r/an-introduction-to-tidycensus.html

# Load data dictionary of ACS 5 year data 
censusvars <- tidycensus::load_variables(2017, "acs5", cache = TRUE)


# Function to Download Chosen Variables from the ACS 5 (More Comprehensive)
get_acs_counties <- function(year){
  
  acs_data <- get_acs(geography = "county",  #set up your desired geographical aggreagtion
                      variables = c(population = 'B01003_001', 
                                    female = 'B01001_026', 
                                    white = 'B02001_002',
                                    black = 'B02001_003',
                                    asian = 'B02001_005', 
                                    age_median = 'B01002_001', 
                                    hhincome_median = 'B19013_001'),
                                    #housing_units = 'B25001_001', 
                                    #home_value_mortgage = 'B25082_002', 
                                    #home_value_nomortgage = 'B25082_003', 
                                    #property_taxes_median = 'B25103_001', 
                                    #property_taxes_median_mortgage = 'B25103_002',
                                    #property_taxes_median_nomortgage = 'B25103_003'),  # write in vector format the variable codes 
                      year = year, 
                      cache_table = TRUE, 
                      # Choose the states you want to obtain data from. Here I am choosing California and Texas 
                      state = c("Indiana"), # Write desired states inside a vector 
                      survey = "acs5")
  # Express Variables as Percentages and Create Key Variables 
  acs_clean <- acs_data %>%
    select(-moe) %>% # Remove moe variable 
    mutate(year = year) # create variable to identify year 
  return(acs_clean)  
}

## Function to obtain all the data across available geographies (in this case counties )
# you can write this as a sequence 
years <- 2011:2022
# Obtain the data for all desired years 
indiana_data <- map_df(years, get_acs_counties)
# export the data 
saveRDS(indiana_data, paste(bt,"indiana_data.Rds", sep =""))


#---------------------------------------------------------------
gleaners <- rio::import(file = paste("/Users/luisenriquenavarro/Downloads/", "2022 Pie Chart Database.xlsx", sep = ""),
                        which = "Feeding America Food Bank", skip = 1, column_names = 2) %>% 
  select(`Food Bank`, `Total Pounds`, `Federal Pounds`, `Total Non Federal Pounds`, `County...2` ) %>% 
  rename(County = `County...2`) %>% 
  filter(is.na(`Food Bank`) == FALSE) %>% 
  mutate(gleaners = ifelse(grepl("Gleaners", `Food Bank`), "Gleaners", "Rest of Food Banks")) %>% 
  mutate(County = str_to_title(County)) %>% 
  # St Joseph, Dekalb, LaGrange, LaPorte. Change the names. 
  mutate(County = case_when(County == "Saint Joseph" ~"St. Joseph", 
                            County == "Dekalb" ~"DeKalb", 
                            County == "Lagrange" ~"LaGrange", 
                            County == "Laporte" ~"LaPorte", 
                            TRUE ~ County)) 
                        
gleaners_pounds <- gleaners %>% 
  mutate(total_pounds = sum(`Total Pounds`), 
         fed_pounds = sum(`Federal Pounds`), 
         nonfed_pounds = sum(`Total Non Federal Pounds`)) %>% 
  mutate(total_pct  = 100*(`Total Pounds`/ total_pounds),
         fed_pct    = 100*(`Federal Pounds`/ fed_pounds),
         nonfed_pct = 100*(`Total Non Federal Pounds`/ nonfed_pounds)) %>% 
  group_by(gleaners) %>% 
  summarize(nonfed_pct = sum(nonfed_pct), 
            fed_pct = sum(fed_pct))

gleaners_counties <- gleaners %>% 
  select(County, gleaners) %>% 
  mutate(NAME = paste(County, " County, Indiana", sep = "")) 

#----------------------------------------------------------

# Population Served 
indiana_data_clean <- indiana_data %>% 
  filter(year == 2022) %>% 
  filter(variable == 'population') %>% 
  left_join(gleaners_counties, by = "NAME", relationship = "one-to-one") %>% 
  mutate(total_pop = sum(estimate, na.rm = TRUE)) %>% 
  mutate(pop_percent = 100*estimate/total_pop) %>% 
  group_by(gleaners) %>% 
  summarize(pop_percent = sum(pop_percent), 
            n = n()) %>% 
  mutate(counties_tot = sum(n)) %>% 
  mutate(counties_pct = 100*n/counties_tot) %>% 
  select(-counties_tot)

# Indiana Population 
indiana_data %>% 
  summarize(`Population (millions)` = sum(estimate)/1000000) %>% 
  print()



