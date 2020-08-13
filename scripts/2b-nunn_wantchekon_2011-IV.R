library(haven)
library(lfe)
library(dplyr)
source('scripts/my_utils.R')

# Choose outcome, exposure and controls
## List of target outcomes:
## trust_relatives trust_neighbors trust_local_council 
## intra_group_trust inter_group_trust 
outcome = "trust_neighbors"
exposure = "ln_export_area"
instrument = "distsea"

country_fixed_effects = " + factor(isocode)"
individual_controls = 
  " + age + age2 + male" %+%
  " + factor(living_conditions) + factor(education)" %+%
  " + factor(religion) + factor(occupation)" %+%
  " + urban_dum"
district_controls = " + district_ethnic_frac + frac_ethnicity_in_district"
ethnic_controls =
  " + malaria_ecology + cities_1400_dum" %+%
  " + factor(v30) + v33"%+%
  " + railway_contact + explorer_contact + total_missions_area" %+%
  " + ln_init_pop_density"
public_goods_controls = 
  " + school_present + electricity_present" %+%
  " + piped_water_present + sewage_present" %+%
  " + health_clinic_present" 

controls = 
  " + loc_ln_export_area" %+% 
  country_fixed_effects %+% 
  individual_controls %+%
  district_controls %+%
  ethnic_controls #%+%
  #public_goods_controls

clusters = "ethnicity + district"

# Load data
my_data <- read_dta("datasets/2_nunn_and_wantchekon_2011_data/Nunn_Wantchekon_AER_2011.dta")

# Compute IV regression with cluster-adjusted errors
f1 <- exposure %+% " ~ " %+% instrument
first_stage <- lm(as.formula(f1), my_data, na.action=na.exclude)
exposure.hat <- fitted.values(first_stage)

f = outcome %+% " ~ " %+% "exposure.hat" %+% controls %+% "| 0 | 0 | " %+% clusters
my_felm <- felm(formula=as.formula(f), data=my_data, na.action=na.omit, keepModel=TRUE)

# Print results
my_summary(my_felm, 
           outcome, 
           "exposure.hat", 
           "district", # hack to make the script work with multiple controls
           expected_effect_size = 0.13)

F1 <- summary(first_stage)$fstatistic["value"]
print(sprintf('First stage F-stat = %.2f', F1))

# Compute the clustered Moran statistic
f = outcome %+% " ~ " %+% "exposure.hat" %+% controls
my_lm <- lm(formula=as.formula(f), data=my_data, na.action=na.exclude)
my_moran(
  my_lm %>% residuals() %>% c(), 
  my_data %>% pull("centroid_lat"), 
  my_data %>% pull("centroid_long"), 
  my_data %>% pull("murdock_name")
)