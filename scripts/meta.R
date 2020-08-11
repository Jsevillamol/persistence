# Meta analysis using the random effects model
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
library(meta)
library(metafor)
library(dplyr)

# Fetch data
data <- read.csv("datasets/results.csv")

# Reverse negative correlations
data$Identified.persistent.effect<-data$Identified.persistent.effect %>% abs()

# Effect size pooling

m.hksj <- metagen(data$Identified.persistent.effect,
                  data$Standard.error,
                  #data = madata,
                  #studlab = paste(Author),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  #method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "MD")
print(m.hksj)