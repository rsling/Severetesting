save.pdf = F

# Sig level.
sig <- 0.05

# Hypothesis.
# H0 : mu = 0
# H1 : mu > 0

# Parameters.
sd <- 2
n <- 100

# The three outcomes.
mus <- c( mu1 <- 0.4 # 2σ
          , mu2 <- 0.6 # 3σ
          , mu3 <- 1.0 # 5σ
)

# Significance tests for the tree outcomes.
se <- sd/sqrt(n)

zs <- mus/se
ps <- 1-pnorm(zs)
sigs <- ps <= sig


cat("=== Sig tests for potential outcomes ===\n"
    , "H0 : mu = 0\n", "H1 : mu > 0\n"
    , "sd = ", sd, "\nn = ", n, "\n"
    , "se = ", sd, "/√", n, " = ", se, "\n\n"
    , sep=""
)
for (i in 1:length(mus)) {
  cat("mu_obs = ", mus[i]
      , "\n z = ", zs[i]
      , "\n p = ", ps[i]
      , "\n sig = ", sigs[i]
      , "\n\n"
      , sep = ""
  )
}


if (save.pdf) pdf("severity1.pdf")

# Now set some discrepancy from the null for which sig-level has been
# reached. For example 0.4. It's two SEs from the mean under the Null.
# Is such an outcome good evidence for mu > 0.2? The goal is to tell us more
# about the probabilities of concrete deviations from the Null, not just
# "It's something larger than 0."
1-pnorm(1)
xs <- seq(-1, 1.5, 0.01)
sev <- 1-qnorm(p = 0.4, mean = 0.2, sd = se, lower.tail = T)

plot(dnorm(xs, mean = 0, sd = se), type = "l", xaxt = "n", col = 1, lwd = 2,
     main = "Severity for observed mu[obs] = 0.4 ; H0: mu = 0 ; H1': mu = 0.2",
     ylab="", xlab = "", yaxt = "n", frame.plot = F)
lines(rep(which.min(abs(xs - 0)), 2), c(0, dnorm(0, mean = 0, sd = se)), col = 1, lwd = 1, lty = 3)

lines(dnorm(xs, mean = 0.2, sd = se), col = "darkred", lwd = 2)
lines(rep(which.min(abs(xs - 0.2)), 2), c(0, dnorm(0.2, mean = 0.2, sd = se)), col = "darkred", lwd = 1, lty = 3)

# lines(rep(which.min(abs(xs - 0.4)), 2), c(0, ), col = 3, lwd = 2)
x.start <- which.min(abs(xs - 0.4))
x.end <- length(xs)
y1.all <- dnorm(seq(0.4, 1.5, 0.01), mean = 0, sd = se)
y2.all <- dnorm(seq(0.4, 1.5, 0.01), mean = 0.2, sd = se)
polygon(c(x.start, x.start:x.end), c(0, y2.all), col = "darkgreen")
polygon(c(x.start, x.start:x.end), c(0, y1.all), col = "darkblue")

axis(1, at = seq(1:length(xs))[c(TRUE, rep(FALSE, 9))], labels = xs[c(TRUE, rep(FALSE, 9))])
text(x = 35, y = 1.6, labels = paste0("Norm dist for\nH0: mu = 0 at SE = ", se))
text(x = 35, y = 1, labels = paste0("Tail for rejection of H0\np = ", round(ps[1], 2)), col = "darkblue")
text(x = 200, y = 1.6, labels = paste0("Norm dist for\nH1': mu <= 0.2 at SE = ", se), col ="darkred")
text(x = 200, y = 0.8, labels = paste0("SEV tail for H1': mu <= 0.2\nif mu[obs] = 0.4\nSEV = 1 - ", round(1-sev, 2), " = ", round(sev,2)), col ="darkgreen")

if (save.pdf) dev.off()
