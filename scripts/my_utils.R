#########################

# External libraries

library(ape)

#########################################

# Inline string concatenation
`%+%` <- function(a, b) paste(a, b, sep="")

# Error function and inverse
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)

################################################################################

# Computing usual statistics
my_summary <- function(
    my_lm, 
    outcome, 
    exposure, 
    clusters = "", 
    expected_effect_size=0.1,
    n_hypothesis = 1,
    significance_level = 0.05
  ){
  
  n <- nobs(my_lm)
  p <- coef(summary(my_lm))[exposure, 'Pr(>|t|)']
  R2 <- summary(my_lm)$adj.r.squared
  
  # Compute standardized regression coefficient
  r <- coef(summary(my_lm))[exposure, "Estimate"]
  if(clusters == ""){
    s <- coef(summary(my_lm))[exposure, "Std. Error"]
  } else {
    s <- coef(summary(my_lm))[exposure, "Cluster s.e."]
    n_clusters <- n_distinct(my_lm$clustervar[[clusters]])
  }
  
  # Standardize regression coefficient
  sigma_x <- my_lm$model %>% pull(exposure) %>% sd() # Need to fix this for 2SLS
  sigma_y <- my_lm$model %>% pull(outcome) %>% sd()
  
  cohen_d <- r / sigma_y
  cohen_ds <- s / sigma_y
  
  beta <- r * sigma_x / sigma_y
  s_standard <- s * sigma_x / sigma_y
  
  # Apply Sidak MHT correction
  ## Compute critical number of hypothesis
  if(p < significance_level){
    critical_hypothesis_n <- ceiling(log(1 - significance_level) / log(1-p))
  } else {
    critical_hypothesis_n <- 0
  }
  
  ## Adjust p value
  p.adj <- 1-(1-p)^n_hypothesis
  
  ## Adjust standard error
  alpha.p <- erf(1/ sqrt(2))
  alpha <- alpha.p^(1/n_hypothesis)
  s_adjusted <- s_standard * qnorm((1+alpha)/2)
  
  # Compute power, type-S error ratio and exaggeration ratio
  retrodesign_out <- retrodesign(
    expected_effect_size, 
    s_adjusted,
    alpha=significance_level)
  
  # Print results
  print(sprintf('outcome = %s', outcome))
  print(sprintf('exposure = %s', exposure))
  print(sprintf('n = %d', n))
  if (clusters != ""){print(sprintf('n clusters = %d', n_clusters))}
  print(sprintf('r = %.2f (%.2f)', r, s))
  print(sprintf('d = %.2f (%.2f)', cohen_d, cohen_ds))
  print(sprintf('beta = %.2f (%.2f) [%.2f], adjusted p = %.2e', beta, s_standard, s_adjusted, p.adj))
  print(sprintf('unadjusted p = %.2e', p))
  print(sprintf('# hypotheses tested = %i', n_hypothesis))
  print(sprintf('Adjusted p = %.2e', p.adj))
  print(sprintf('Critical hypothesis number = %.2e', critical_hypothesis_n))
  print(sprintf('Adjusted R2 = %.2f', R2))
  print(sprintf('Expected effect size = %.2f', expected_effect_size))
  print(sprintf('Power = %.2f', retrodesign_out$power))
  print(sprintf('Type-S error rate = %.2f', retrodesign_out$typeS))
  print(sprintf('Exaggeration ratio = %.2f', retrodesign_out$exaggeration))
}

################################################################################
# Moran statistic
my_moran <- function(my_residuals,
                     latitudes, 
                     longitudes, 
                     cluster_tags=c()){
  
  # Store relevant information in data frame
  my_data <- data.frame(
    my_residual = my_residuals, 
    latitude =  latitudes, 
    longitude = longitudes)
  
  # Aggregate if analysis is clustered
  if (length(cluster_tags) > 0){
    my_data$cluster_tags <- cluster_tags
    f <- as.formula("cbind(my_residual, latitude, longitude) ~ cluster_tags")
    my_data <- aggregate(f, my_data, FUN=mean, na.rm=TRUE)
  }
  
  # Compute inverse distance matrix
  coordinates = cbind(my_data$latitude, my_data$longitude)
  ww <- as.matrix(dist(coordinates))
  ww <- 1/ww
  diag(ww) <- 0
  ww[which(!is.finite(ww))] <- 0
  
  # Compute Moran using ape
  moran_out <- Moran.I(my_data$my_residual, ww, na.rm = TRUE) # not sure if na.rm = TRUE is masking an error
  moran_out$z.value <- (moran_out$observed - moran_out$expected) / moran_out$sd
  
  # Print results
  print(sprintf("Moran's Z = %.2f", moran_out$z.value))
  print(sprintf("Moran's p = %.2e", moran_out$p.value))
}

############################################
# I have the power
## Beyond Power Calculations: Assessing Type S (Sign) and Type M (Magnitude) Errors
## Gelman and Carlin 2014

retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
  A <- abs(A)
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}
