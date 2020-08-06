# Long-term persistence
# Replication of table 4, panel B
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jeea.12177

# Libraries ---------------------------------------------------------------

library(haven)
library(lfe)
library(dplyr)
library(tidyr)
source("scripts/my_utils.R") 

# Variable selection ------------------------------------------------------

outcome = "gold_medal" # totassoc_p sede_aido cheating_mat_area_rap gold_medal
exposure = "libero_comune_allnord" # libero_comune_allnord libero_comune_principale signoria_indipendente_allnord 

controls = 
  #" + signoria_indipendente_allnord" %+%
  " + altitudine + escursione + costal + nearsea" %+%
  " + population + pop2" %+%
  " + nordest + nordovest + centro" %+%
  " + gini_land  + gini_income"

# Data preparation --------------------------------------------------------

my_data <- read_dta("datasets/7_guiso_et_al_data/ltp1F.dta")
n <- nrow(my_data)

## Add names of municipalities
istat_codes <- read.csv("datasets/7_guiso_et_al_data/Codici-statistici-e-denominazioni-al-01_07_2020 (1).csv", sep=";")
istat_codes <- istat_codes %>% select(c(Codice.Comune.formato.numerico, Denominazione.in.italiano))
my_data <- merge(my_data, istat_codes, by.x="istcom", by.y = "Codice.Comune.formato.numerico")

## Add coordinates of municipalities
coordinate_data <- read.csv("datasets/7_guiso_et_al_data/places.csv") 
coordinate_data <- coordinate_data %>% select(c(X,Y,name))
my_data <- merge(my_data, coordinate_data, by.x="Denominazione.in.italiano", by.y="name")

## Exclude Roma, region 20 and towns with no nonprofits
my_data <- my_data %>% filter(dummyroma == 0) %>% 
  filter(my_data$regione<20 || is.na(my_data$regione)) %>% 
  filter(totassoc_p > 0 %>% replace_na(TRUE))

# Regression --------------------------------------------------------------

f <- outcome %+% " ~ " %+% exposure %+% controls #%+% " | 0 | 0 | " %+% cluster
my_lm <- lm(as.formula(f), my_data, na.action=na.exclude) #, keepModel=TRUE)

## Compute relevant statistics
my_summary(my_lm, 
           outcome, 
           exposure, 
           #cluster, 
           expected_effect_size = 0.1
)

## Compute spatial autocorrelation of residuals

my_moran(
  residuals(my_lm),
  my_data %>% pull("Y"),
  my_data %>% pull("X")
)