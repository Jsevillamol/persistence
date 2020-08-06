library(haven)
source('scripts/my_utils.R')

# Choose outcome and controls
outcome = "IIP"
exposure = "ChurchExpWest" # ChurchExpWest ChurchExpEast KII fs_cousin_pref_cont_est2
controls = 
  " + nu_rugged + ag_distcr " %+%
  " + du_caloricsui + ag_abslat" 

# Prepare data
my_data <- read_dta("datasets/4_schulz_et_al_data/alldata.dta")

# Regression
f <- outcome %+% " ~ " %+% exposure %+% controls 
my_lm <- lm(as.formula(f), my_data, na.action=na.exclude)
my_summary(my_lm, outcome, exposure)

# Compute Moran
my_moran(
  residuals(my_lm),
  my_data$nu_lat,
  my_data$nu_lon
)