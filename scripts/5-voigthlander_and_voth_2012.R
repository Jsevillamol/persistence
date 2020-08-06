library(haven)
library(dplyr)
library(lfe)
source('scripts/my_utils.R')

# Choose outcome, exposure and controls
outcome = "pog20s" # pog20s NSDAP_1928 frac_deportations stuer syn33?
exposure = "pog1349"
controls = 
  " + ln_pop"  %+% # pop33 is this right?
  " + frac_juden" %+% # jews33 Shouldnt this be a percentage?
  " + frac_prot"
cluster = "kreis"

# Prepare data
my_data <- read_dta("datasets/5_voigthlander_and_voth_2012_data/Dataset_QJE_Replicate_with_Cities.dta")

## Filter data
my_data$exist1349 = my_data$judaica==1 | my_data$comm1349==1 
my_data$exist1349[is.na(my_data$exist1349)] <- FALSE
my_data <- my_data[my_data$exist1349, ]

## Define additional variables
my_data$NSDAP_1928 = my_data$n285nsda / my_data$n285gs
my_data$frac_deportations = 100*(my_data$deptotal/my_data$jews33)
my_data$stuer = my_data$stuer1 +my_data$stuer2 + my_data$stuer3
my_data$ln_pop = my_data$c25pop %>% log()
my_data$frac_juden = my_data$c25juden / my_data$c25pop
my_data$frac_prot = my_data$c25prot / my_data$c25pop

# Run regression
f <- outcome %+% " ~ " %+% exposure %+% controls %+% " | 0 | 0 | " %+% cluster
my_felm <- felm(as.formula(f), my_data, keepModel=TRUE)
my_summary(my_felm, outcome, exposure, cluster, expected_effect_size = 0.13)

# Compute spatial autocorrelation of residuals
f <- outcome %+% " ~ " %+% exposure %+% controls
my_lm <- lm(as.formula(f), )
moran_out <- my_moran(
  my_felm %>% residuals(), 
  my_data$Latitude, 
  my_data$Longitude)