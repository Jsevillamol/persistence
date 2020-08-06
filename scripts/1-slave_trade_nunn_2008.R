library(haven)
library(dplyr)
source('scripts/my_utils.R')

# Select outcome, response and controls
outcome = "ln_maddison_pcgdp2000"
exposure = "ln_export_area"

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

# Compute regression
f = outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f),data=my_data)
my_summary(my_lm, outcome, exposure, expected_effect_size = 0.13)

# Measure spatial autocorrelation of residuals
my_moran(resid(my_lm), my_data$longitude, my_data$abs_latitude) # not quite right