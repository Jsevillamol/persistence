# Some analysis on the results reported in
# Jha, 2013
# Table 4, column 11

source("scripts/my_utils.R")

# Results
outcome = "Any H-M Riot, 1850-1950?"
exposure = "Medieval Overseas Port"

# Results
r = -0.266
s = 0.071
n= 248

sigma_x = 0.31 
sigma_y = 0.49

# Standardize coefficients
cohen_d = r / sigma_y
cohen_ds = s / sigma_y

beta = r * sigma_x / sigma_y
s_standard = s * sigma_x / sigma_y

# p value
z = beta / s_standard
p = pnorm(z)

significance_level = 0.05

# MHT adjustment
n_hypotheses_tested = 2
if(p < significance_level){
  critical_hypothesis_n <- floor(log(1 - significance_level) / log(1-p))
} else {
  critical_hypothesis_n <- 0
}

## Adjust p value
p.adj <- 1-(1-p)^n_hypotheses_tested

## Adjust standard error 
alpha <- erf(1/ sqrt(2))
alpha.p <- 1-(1-alpha)^n_hypotheses_tested
s_adjusted <- s_standard * (sqrt(2) * erfinv(alpha.p))

# Post design power analysis
expected_effect_size = 0.10
retrodesign_out <- retrodesign(
  expected_effect_size, 
  s_adjusted,
  alpha=significance_level)

# Print results
print(sprintf('outcome = %s', outcome))
print(sprintf('exposure = %s', exposure))
print(sprintf('n = %d', n))
print(sprintf('r = %.2f (%.2f)', r, s))
print(sprintf('d = %.2f (%.2f)', cohen_d, cohen_ds))
print(sprintf('beta = %.2f (%.2f) [%.2f]', beta, s_standard, s_adjusted))
print(sprintf('p = %.2e', p))
print(sprintf('# hypotheses tested = %i', n_hypotheses_tested))
print(sprintf('Adjusted p = %.2e', p.adj))
print(sprintf('Critical hypothesis number = %.2e', critical_hypothesis_n))
print(sprintf('Expected effect size = %.2f', expected_effect_size))
print(sprintf('Power = %.2f', retrodesign_out$power))
print(sprintf('Type-S error rate = %.2f', retrodesign_out$typeS))
print(sprintf('Exaggeration ratio = %.2f', retrodesign_out$exaggeration))