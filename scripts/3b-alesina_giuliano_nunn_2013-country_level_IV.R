library(haven)
library(geosphere)
library(ape)
library(dplyr)
source('scripts/my_utils.R')

# Choose variable to run code on and controls
outcome = "flfp2000" ## flfp2000 female_ownership women_politics
exposure = "plow"

continent_fixed_effects = " + factor(continent)"
historical_controls = 
  " + agricultural_suitability + tropical_climate" %+%
  " + large_animals + political_hierarchies + economic_complexity"
contemporary_controls = 
  " + ln_income + ln_income_squared"

additional_historical_controls = 
  " + intensity_agriculture"    %+%
  " + husbandry + hunting"      %+%
  " + abs_inherit + patrilocal" %+%
  " + matrilocal + nuclear_fam" %+%
  " + extended_fam + year_obs"

religion_controls =
  " + cath00 + prot00"    %+%
  " + othchrist00"        %+%
  " + muslim00 + hindu00"

additional_contemporary_controls = 
  " + years_civil_conflict"            %+%
  " + years_interstate_conflict"       %+%
  " + rugged + communist_dummy"        %+%
  " + european_descent + oil_pc"       %+%
  " + agr_va_gdp2000 + man_va_gdp2000" %+%
  " + serv_va_gdp2000 "                #%+%
#religion_controls

controls = 
  continent_fixed_effects          %+% 
  historical_controls              %+%
#  contemporary_controls            %+% 
  additional_historical_controls   %+%
  additional_contemporary_controls 

# Prepare data
## Load main dataset
my_data <- read_dta("datasets/3_alesina_et_al_2013_data/crosscountry_dataset.dta")

## Add country coordinates to data
country_centroids <- read.csv("datasets/country_centroids_az8.csv")
keeps <- c("iso_a3", "Longitude", "Latitude")
country_centroids <- country_centroids[keeps]
my_data <- merge(x = my_data, y = country_centroids, by.x = "isocode", by.y = "iso_a3")#, all.x=TRUE)
### Caution: The previous line drops some observations

# Run 2SLS
f1 = exposure %+% " ~ plow_positive_crops + plow_negative_crops" %+% controls
first_stage <- lm(as.formula(f1), data=my_data, na.action=na.exclude)
exposure.hat <- fitted.values(first_stage)

f2 = outcome %+% " ~ exposure.hat" %+% controls
my_lm <- lm(as.formula(f2), data=my_data, na.action=na.exclude)

# Print useful statistics
my_summary(
  my_lm,
  outcome, 
  "exposure.hat", 
  expected_effect_size = 0.1
)

## Print first stage F-stat 
F1 <- summary(first_stage)$fstatistic["value"]
print(sprintf('First stage F-stat = %.2f', F1))

# Compute Moran's statistic
my_moran(
  residuals(my_lm),
  my_data %>% pull("Longitude"),
  my_data %>% pull("Latitude")
)