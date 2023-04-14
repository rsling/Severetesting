library(pwr)

true.sd <- 2
n <- 30
sig <- 0.05

# Nehmen wir an, wir testen bei sig=0.05 auf Folgendes:
# H0: mu = 1
# H1: mu > 1
mu <- 1

# Der SE ist:
se <- true.sd/sqrt(n)
cat("SE=", se, "\n", sep="")

# Ab welcher Diskrepanz verwerfen wir die H0? | Einseitiger test ("greater")…
discrepancy <- abs(qnorm(sig)) * se
cat("Discepancy=", discrepancy, "\n", sep="")

# Wir brauchen die Typ 2-Fehlerrate: P(H0 nicht zurückgewiesen; H0 ist falsch)
#
# mu0 sei das gemessene Mittel.
# Wir weisen die H0 nicht zurück, wenn mu0 < mu + discrepancy, also wenn
#   das gemessene Mittel kleiner ist als mu unter H0 plus die für sig = 0.05
#   nötige Diskrepanz.
# Wir nehmen eine Effektgröße von 0.5 an. Das wahre Mittel soll also bei
#   mindestens 1.5 liegen.
effect <- 0.5

# Wir brauchen nun: p(mu0 < mu + discrepancy) wenn mu = 1.5, denn das wäre
# nach meinem Verständnis die Typ 2-Fehlerrate.
# Wenn mu = 1.5 und wir weisen H0 zurück: z.t2 < (discrepancy - effect)/se.
z.t2 <- (discrepancy-effect)/se
err.t2 <- pnorm(z.t2, lower.tail = T)

# Power = 1-Fehlerrate
power <- 1 - err.t2
cat("Power=", power, "\n", sep="")

# Aber das gibt was völlig anderes aus.
print(pwr.norm.test(effect/true.sd, n, sig, NULL, alternative = "greater"))

