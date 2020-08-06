# Meta analysis
library(meta)
library(metafor)

# Fetch data
data <- read.csv("datasets/results.csv")

# Reverse negative correlations
data$Identified.persistent.effect<-data$Identified.persistent.effect %>% abs()

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