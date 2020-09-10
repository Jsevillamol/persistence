library(haven)
library(dplyr)
source('scripts/my_utils.R')

# Select outcome, response and controls
outcome = "ln_maddison_pcgdp2000"
exposure = "ln_export_area"
instruments = 
     "atlantic_distance_minimum + indian_distance_minimum" %+%
  " + saharan_distance_minimum + red_sea_distance_minimum"

colony_fixed_effects = 
  " + colony0 + colony1 + colony2" %+%
  " + colony3 + colony4 + colony5" %+%
  " + colony6 + colony7"

geographical_controls=
  " + abs_latitude + longitude + rain_min"      %+%
  " + humid_max + low_temp + ln_coastline_area" %+%
  " + island_dum + legor_fr + region_n"
  #"region_s + region_e + region_w + region_c"

precolonial_prosperity_controls=
  " + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop" %+%
  " + ln_pop_dens_1400"

controls = 
  colony_fixed_effects  %+%
  geographical_controls %+%
  precolonial_prosperity_controls
  
# Load data
my_data <- read_dta("datasets/1_nunn_2008_data/slave_trade_QJE.dta")

## Add latitude and longitude
country_centroids <- read.csv("datasets/country_centroids_az8.csv")
country_centroids <- country_centroids %>% select(c(iso_a3,Latitude, Longitude))
my_data <- merge(my_data, country_centroids, by.x = "isocode", by.y = "iso_a3", all.x=TRUE)

# Compute regression
f1 = exposure %+% " ~ " %+% instruments %+% controls
first_stage <- lm(as.formula(f1), my_data)
exposure.hat <- first_stage$fitted.values

f = outcome %+% " ~ " %+% "exposure.hat" %+% controls
my_lm <- lm(as.formula(f),data=my_data)

# Print statistics
my_summary(
  my_lm, 
  outcome, 
  "exposure.hat", 
  expected_effect_size = 0.13)

## Print first stage F value
## Print first stage F-stat 
F1 <- summary(first_stage)$fstatistic["value"]
print(sprintf('First stage F-stat = %.2f', F1))

# Measure spatial autocorrelation of residuals
my_moran(resid(my_lm), my_data$Longitude, my_data$Latitude)