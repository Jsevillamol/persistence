# Meta analysis using the random effects model
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
library(meta)
library(metafor)
library(dplyr)

# Fetch data
data <- read.csv("datasets/results.csv")

# Reverse negative correlations
data$Identified.persistent.correlation.abs<-data$Identified.persistent.correlation %>% abs()

# Correlation effect size pooling
m.hksj <- metagen(data$Identified.persistent.correlation.abs,
                  data$Standard.error,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  #method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "MD"
                  )
print(sprintf("Pooled correlation effect size = %.2f (%.2f)", m.hksj$TE.random, m.hksj$seTE.predict))

# Causal effect size pooling
m.hksj <- metagen(data$Identified.persistent.correlation.abs,
                  data$Standard.error,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  #method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "MD"
)
print(sprintf("Pooled correlation effect size = %.2f (%.2f)", m.hksj$TE.random, m.hksj$seTE.predict))