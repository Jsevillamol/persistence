
# The Church, intensive kinship, and global psychological variation
# Replication of table 4
# https://science.sciencemag.org/content/366/6466/eaau5141

# Libraries ---------------------------------------------------------------

library(haven)
library(lfe)
library(dplyr)
source("scripts/my_utils.R") 

# Variable selection ------------------------------------------------------

outcome = "individualism" # individualism conformityI2 ppltrst pplfair
exposure = "ChurchExp" # ChurchExp FirstCousin Ln_FirstCousin

baseline_controls = 
  " + rugged + dist_coast" %+%
  " + caloricsui + abs_latitude"

individual_controls = 
  " + female + ageb + ageb_sqrt"

controls = 
  baseline_controls %+%
  individual_controls %+%
  " + factor(country) + factor(essround)"

cluster = "region_name"


# Data preparation --------------------------------------------------------

my_data <- read_dta("datasets/4_schulz_et_al_data/EuropeanRegions.dta")
country_data <- read_dta("datasets/4_schulz_et_al_data/alldata.dta")
country_data <- country_data %>% select(c(nu_lat, nu_lon, js_code))
my_data <- merge(my_data, country_data, by.x="iso", by.y="js_code")


# Regression --------------------------------------------------------------

f <- outcome %+% " ~ " %+% exposure %+% controls %+% " | 0 | 0 | " %+% cluster
my_felm <- felm(as.formula(f), my_data, na.action=na.omit, keepModel=TRUE)

## Compute relevant statistics
my_summary(my_felm, 
           outcome, 
           exposure, 
           cluster, 
           expected_effect_size = 0.13,
           n_hypothesis = 4
           )

## Compute spatial autocorrelation of residuals

f <- outcome %+% " ~ " %+% exposure %+% controls 
my_lm <- lm(as.formula(f), my_data, na.action=na.exclude)

my_moran(
  residuals(my_lm),
  my_data %>% pull("nu_lat"),
  my_data %>% pull("nu_lon"),
  my_data %>% pull("iso")
  )
