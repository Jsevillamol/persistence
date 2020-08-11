library(haven)
library(dplyr)
source('scripts/my_utils.R')

# Choose outcome and controls
outcome = "FLFP15_64" # FLFP15_64 jobs_scarce men_pol_leaders
exposure = "plow"

fixed_effects = "+ factor(countrycode)" ## regioncode countrycode continent
individual_controls = " + age + age_sq + primary + secondary + married" %+%
  ifelse(outcome == "FLFP15_64", "", " + factor(sex)")
historical_district_controls = 
  " + agricultural_suitability + tropical_climate + large_animals" %+%
  " + economic_complexity + political_hierarchies"
contemporary_country_controls = " + ln_income + ln_income2"
controls = 
  fixed_effects %+%
  individual_controls %+%
  historical_district_controls %+%
  contemporary_country_controls
  
# Prepare data
my_data <- read_dta("datasets/3_alesina_et_al_2013_data/WVS_dataset.dta")

## Add country coordinates to data
country_centroids <- read.csv("datasets/country_centroids_az8.csv")
keeps <- c("iso_n3", "Longitude", "Latitude")
country_centroids <- country_centroids[keeps]
my_data <- merge(x = my_data, y = country_centroids, by.x = "countrycode", by.y = "iso_n3")#, all.x=TRUE)
### Caution: The previous line drops some observations

# Regression
f = outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f), data=my_data, na.action=na.exclude)

my_summary(my_lm, outcome, exposure, n_hypothesis = 5)

# Compute clustered Moran
my_moran(
  residuals(my_lm),
  my_data$Latitude,
  my_data$Longitude,
  my_data$countrycode
)