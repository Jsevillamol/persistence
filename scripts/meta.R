# Meta analysis using the random effects model
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
library(meta)
library(metafor)
library(metaplotr)
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

# Calculate min and max span
data$min.span = data$Start.of.outcome-data$End.of.exposure
data$max.span = data$End.of.outcome - data$Start.of.exposure

# Calculate central span
data$central.span = (data$max.span - data$min.span)/2

# Plot persistence span vs error bars
x = data$central.span
se.x = data$max.span - data$central.span
y = data$Identified.persistent.effect
se.y = data$Standard.error

crosshairs(x, y, se.x, se.y, confint = .7,
           main_lab = "",
           x_lab = "Persistence span", 
           #x_lim = c(0, 2000),
           y_lab = "Effect size",
           #y_lim = c(0, 1),
           mdrtr = data$Paper, 
           mdrtr_lab = 'Paper',
           annotate = TRUE)
