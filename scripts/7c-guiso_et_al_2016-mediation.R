# Long-term persistence
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jeea.12177

# Libraries ---------------------------------------------------------------

library(haven)
library(lfe)
library(dplyr)
library(tidyr)
source("scripts/my_utils.R") 

# Variable selection ------------------------------------------------------

outcome = "empowerment" # totassoc_p sede_aido cheating_mat_area_rap gold_medal
exposure = "libero_comune_allnord" # libero_comune_allnord libero_comune_principale signoria_indipendente_allnord 
instrument = "bishopcity"

controls = 
  #" + signoria_indipendente_allnord" %+%
  " + altitudine + escursione + costal + nearsea" %+%
  " + population + pop2" %+%
  " + nordest + nordovest + centro" %+%
  " + gini_land  + gini_income"

# Data preparation --------------------------------------------------------

my_data <- read_dta("datasets/7_guiso_et_al_data/ltp1F.dta")
n <- nrow(my_data)
self_efficacy_data <- read_dta("datasets/7_guiso_et_al_data/ltp3F.dta")
my_data <- merge(my_data, self_efficacy_data)
print(nrow(my_data) - n) # Why are some extra rows added?


## Add names of municipalities
istat_codes <- read.csv("~/R/persistence/datasets/7_guiso_et_al_data/Codici-statistici-e-denominazioni-al-01_07_2020 (1).csv", sep=";")
istat_codes <- istat_codes %>% select(c(Codice.Comune.formato.numerico, Denominazione.in.italiano))
my_data <- merge(my_data, istat_codes, by.x="istcom", by.y = "Codice.Comune.formato.numerico")

## Add coordinates of municipalities
coordinate_data <- read.csv("~/R/persistence/datasets/7_guiso_et_al_data/places.csv") 
coordinate_data <- coordinate_data %>% select(c(X,Y,name))
my_data <- merge(my_data, coordinate_data, by.x="Denominazione.in.italiano", by.y="name")

## Exclude Roma, region 20 and towns with no nonprofits
my_data <- my_data %>% filter(dummyroma == 0) %>% 
  filter(my_data$regione<20 || is.na(my_data$regione)) %>% 
  filter(totassoc_p > 0 %>% replace_na(TRUE))

# Two stage regression --------------------------------------------------------------

f <- exposure %+% " ~ " %+% instrument %+% controls
first_stage <- lm(as.formula(f), my_data, na.action = na.exclude)
exposure.hat <- fitted.values(first_stage)

f <- outcome %+% " ~ exposure.hat" %+% controls 
my_lm <- lm(as.formula(f), my_data, na.action=na.exclude) 

## Compute relevant statistics
my_summary(my_lm, 
           outcome, 
           "exposure.hat", 
           #cluster, 
           expected_effect_size = 0.1
)

print("First stage F-value = " %+% summary(first_stage)$fstatistic["value"])

## Compute spatial autocorrelation of residuals

my_moran(
  residuals(my_lm),
  my_data %>% pull("Y"),
  my_data %>% pull("X")
)