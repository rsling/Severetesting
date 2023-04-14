# Sig level.
sig <- 0.05

# Hypothesis.
# H0 : mu = 0
# H1 : mu > 0

# Parameters of the experiment.
sd <- 2
n <- 100

# The three outcomes.
mu1 <- 0.4
mu2 <- 0.6
mu3 <- 1.0

# Significance tests for the tree outcomes.
se <- sd/sqrt(n)

z1 <- mu1/se
p1 <- 1-pnorm(z1)

z2 <- mu2/se
p2 <- 1-pnorm(z2)

z3 <- mu3/se
p3 <- 1-pnorm(z3)

p1 <= sig
p2 <= sig
p3 <= sig

### Severity curves for the outcomes.

# 1. Get z statistics for a range of discrepancies from H0.
ds <- seq(0, 1, 0.01)
zs <- ds/se

# 2. Get difference between z for observed and hypothetical discrepancy.
z1s <- zs-z1
z2s <- zs-z2
z3s <- zs-z3

# 3. Get corresponding p value.
sevs1 <- 1-pnorm(z1s)
sevs2 <- 1-pnorm(z2s)
sevs3 <- 1-pnorm(z3s)

# 4. Make sure you understand the results bloody well. — Thanks, Jackie!

# 5. Plot it to impress sharp-minded scientists.
plot(sevs1, type = "l", xaxt = "n", bty = "n", ylab = "", xlab = "",
     col = 1, lty = 1, lwd = 2)
lines(sevs2, col = 2, lty =2, lwd = 2)
lines(sevs3, col = 3, lty = 3, lwd = 2)

abline(v = 0.2 * 100, col = 4, lty = 4)
text(x = 0.21 * 100, y = 0.4, labels = "discrepancy\nunder H1", col = 4, adj = 0)

lines(x = c(mu1 * 100, mu1 * 100), y = c(0, sevs1[mu1*100]), col = 1, lty = 1, lwd = 2)
text(x = (mu1 + 0.01) * 100, y = 0.4, labels = "", col = 1, adj = 0)

lines(x = c(mu2 * 100, mu2 * 100), y = c(0, sevs2[mu2*100]), col = 2, lty = 2, lwd = 2)
text(x = (mu2 + 0.01) * 100, y = 0.4, labels = "", col = 2, adj = 0)

lines(x = c(mu3 * 100, mu3 * 100), y = c(0, sevs3[mu3*100]), col = 3, lty = 3, lwd = 2)
text(x = (mu3 + 0.01) * 100, y = 0.4, labels = "", col = 3, adj = 0)

axis(1, at = seq(0, 100, 10), labels = seq(0, 1, 0.1))
title("Severity for H0:mu=0; H1:mu>0", xlab="Inferred discrepancy from H0", ylab="SEV")
legend("bottomleft", c("Xbar=0.4", "Xbar=0.6", "Xbar=1.0"),
       bty = "n", col = 1:3, lty = 1:3, cex = 0.75, lwd = 2)


