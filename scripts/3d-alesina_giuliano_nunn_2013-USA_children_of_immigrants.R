library(haven)
library(dplyr)
library(lfe)
source('scripts/my_utils.R')

# Choose variable to run code on and controls
outcome = "ilf"
exposure = "plow_m"

individual_controls = 
  " + age + age_sq + high_sc + less_hs" %+%
  " + my_data$single +  factor(year) + factor(metro)"

historical_country_controls = 
  " + agricultural_suitability_m + tropical_climate_m + large_animals_m" %+%
  " + economic_complexity_m + political_hierarchies_m"

contemporary_country_controls = " + ln_income_m + ln_income2m"
  
controls = 
  " + factor(statefip)"         %+%
  individual_controls           %+%
  historical_country_controls   #%+%
  #contemporary_country_controls

clusters = "mbpl"

# Prepare data
my_data <- read_dta("datasets/3_alesina_et_al_2013_data/cps_dataset.dta")
my_data <- my_data[complete.cases(pull(my_data, exposure)),]

## Add state latitude and longitude
gaz_counties_national <- read.delim("datasets/2019_Gaz_counties_national.txt")

gaz_counties_national$statefip <- floor(gaz_counties_national$GEOID/1000)
gaz_counties_national <- gaz_counties_national %>% select(c(statefip, INTPTLAT, INTPTLONG))
gaz_counties_national <- aggregate(gaz_counties_national, by=list(gaz_counties_national$statefip), FUN=mean)
my_data <- merge(x=my_data, y=gaz_counties_national, by.x="statefip", by.y="statefip", all.x=TRUE)

# Run regression
f = outcome %+% " ~ " %+% exposure %+% controls %+% " | 0 | 0 | " %+% clusters 
my_felm <- felm(as.formula(f), data=my_data, keepModel=TRUE)

my_summary(
  my_felm,
  outcome, 
  exposure, 
  clusters, 
  expected_effect_size = 0.03, 
  n_hypothesis = 5)

# Compute spatial autocorrelation statistics
f = outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f), my_data, na.action = na.exclude)

my_moran(
  residuals(my_lm), 
  my_data$INTPTLAT, 
  my_data$INTPTLONG, 
  my_data$mbpl)
