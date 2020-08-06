library(haven)
library(dplyr)
library(lfe)
source('scripts/my_utils.R')

# Replication of table X
# DETERMINANTS OF GENDER ATTITUDES OF EUROPEAN CHILDREN OF IMMIGRANTS

# Choose variable to run code on and controls
outcome = "job_scarce" # job_scarce reg job_scarce_0to1
exposure = "plow"

individual_controls = 
  " + age + agesq + male + married + yedu + smallcity" %+%
  " + factor(essround) + factor(country_s) + factor(mother_isocode_3digits)"

historical_country_controls = 
  " + agricultural_suitability + tropical_climate + large_animals" %+%
  " + economic_complexity + political_hierarchies"

contemporary_country_controls = " + ln_income + ln_income2"
  
controls = 
  individual_controls           %+%
  historical_country_controls   %+%
  contemporary_country_controls

clusters = "mother_isocode_3digits"

# Prepare data
my_data <- read_dta("datasets/3_alesina_et_al_2013_data/ess_mother_dataset.dta")
my_data <- my_data[complete.cases(pull(my_data, exposure)),]

## Add state latitude and longitude
country_centroids <- read.csv("datasets/country_centroids_az8.csv")
keeps <- c("iso_a3", "Longitude", "Latitude")
country_centroids <- country_centroids[keeps]
my_data <- merge(x = my_data, y = country_centroids, by.x = "mother_isocode_3digits", by.y = "iso_a3")#, all.x=TRUE)

# Run regression
f = outcome %+% " ~ " %+% exposure %+% controls %+% " | 0 | 0 | " %+% clusters 
my_felm <- felm(as.formula(f), data=my_data, keepModel=TRUE)
my_summary(
  my_felm,
  outcome, 
  exposure, 
  clusters, 
  expected_effect_size = 0.03)

# Compute spatial autocorrelation statistics
f = outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f), my_data, na.action = na.exclude)

my_moran(
  residuals(my_lm), 
  my_data$Latitude, 
  my_data$Longitude, 
  my_data$mother_isocode_3digits)
