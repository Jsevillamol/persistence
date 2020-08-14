# Meta analysis using the random effects model
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
library(meta)
library(metafor)
library(dplyr)

# Fetch data
data <- read.csv("datasets/results.csv")

# Correlation effect size pooling
m.hksj <- metagen(abs(data$Identified.persistent.correlation),
                  data$Correlation.standard.error,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  #method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "MD"
                  )
print(sprintf("Pooled correlation effect size = %.2f (%.2f)", m.hksj$TE.random, m.hksj$seTE.predict))

# Causal effect size pooling
data$Identified.persistent.causation <- data$Identified.persistent.causation %>% as.numeric()
data$Causation.standard.error <- data$Causation.standard.error %>% as.numeric()
mask <- !is.na(data$Causation.standard.error)
data <- data %>% subset(mask)
m.hksj <- metagen(abs(data$Identified.persistent.causation),
                  data$Causation.standard.error,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  #method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "MD"
)
print(sprintf("Pooled causation effect size = %.2f (%.2f)", m.hksj$TE.random, m.hksj$seTE.predict))