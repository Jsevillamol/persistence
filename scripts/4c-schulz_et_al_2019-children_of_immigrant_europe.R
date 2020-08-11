library(haven)
library(dplyr)
source('scripts/my_utils.R')

# Choose outcome and controls
outcome = "individualism" # individualism conformityI2 ppltrst pplfair
exposure = "m_lin_christ_dur1500_adj_w_sc" # m_lin_christ_dur1500_adj_w_sc m_KII 

baseline_controls = 
  " + m_nu_rugged + m_ag_distcr" %+%
  " + m_du_caloricsui + m_ag_abslat"

individual_controls = 
  " + gndr + ageb + ageb_sqrt"

fixed_effects = 
  " + factor(cntry) + factor(essround)"

controls = 
  #" + m_lin_christ_dur1500_adj_e_sc" %+%
  baseline_controls %+%
  individual_controls %+%
  fixed_effects

cluster = "cntry"

# Prepare data
my_data <- read_dta("datasets/4_schulz_et_al_data/ESS_immigrants.dta")
country_data <- read.csv("datasets/country_centroids_az8.csv")
country_data <- country_data %>% select(c(Longitude, Latitude, iso_a2))
my_data <- merge(my_data, country_data, by.x="cntry", by.y="iso_a2", all.x=TRUE)

# Regression
f <- outcome %+% " ~ " %+% exposure %+% controls %+% " | 0 | 0 | " %+% cluster
my_felm <- felm(as.formula(f), my_data, keepModel=TRUE)
my_summary(
  my_felm, 
  outcome, 
  exposure, 
  cluster,
  n_hypothesis = 4)

# Compute spatial autocorrelation
f2 <- outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f2), my_data, na.action = na.exclude)
my_moran(
  residuals(my_lm),
  my_data$Latitude,
  my_data$Longitude,
  my_data$cntry
)