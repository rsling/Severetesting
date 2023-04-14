library(pwr)

# Known variance, sample size and sig.
sd <- 2
n <- 100
sig <- 0.05

# Assume we test the following at sig = 0.05:
# H0: mu = 1
# H1: mu > 1
mu <- 1

# Standard error:
se <- sd/sqrt(n)
cat("SE = ", se, "\n", sep="")

# At which discrepancy would we reject H0? (Note: single-sided test!)
discrepancy <- abs(qnorm(sig)) * se
cat("Discepancy lower bound at sig = ", discrepancy, "\n", sep="")

# We now need the Type 2 error rate: P(H0 not rejected; H0 is false)
#
# Be mu0 the measured mean, mu the assumed lower bound of the  population mean
# under H1. We do not reject H0 if mu0 < mu + discrepancy, in other words if
# the measured mean is smaller than mu under the H0 PLUS the discrepancy
# required to reach sig = 0.05.
#
# We assume an effect size of 0.5. The true mean (in order to constitute a
# discrepancy worthy of mentioning) should thus be at least 1.5.
effect <- 0.5

# We now need: p(mu0 < mu + discrepancy) if mu = 1.5. This is the type 2 error
# rate. If mu = 1.5 and we reject the H0 then z.t2 < (discrepancy - effect)/se
z.t2 <- (discrepancy-effect)/se
err.t2 <- pnorm(z.t2, lower.tail = T)
cat("T2 error rate = ", err.t2, "\n", sep="")

# Power = 1 - type 2 error rate.
power <- 1 - err.t2
cat("Power = ", power, "\n", sep="")

# Which is confirmed by the function from pwr.
print(pwr.norm.test(effect/sd, n, sig, NULL, alternative = "greater"))

