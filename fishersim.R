# Fisher with true null.

set.seed <- 324

# Sample size range per category 1st var.
n.low <- 10
n.high <- 1000

# Minimum count per cell.
mincell <- 10

reps <- 10000

results <- data.frame()

for (i in 1:reps) {

  # Generate independent counts as RNs from a uniform distribution.
  .n1 <- sample(n.low:n.high, 1)
  .n2 <- sample(n.low:n.high, 1)
  .mat <- table(
    c(rep(0, .n1), rep(1, .n2)),
    c(sample(0:1, size = .n1, replace = TRUE),
      sample(0:1, size = .n2, replace = TRUE))
  )

  if (all(c(.mat)>=mincell)) {
  
    # Get rest results.
    .ft <- fisher.test(.mat)
    results[i, "OR"] <- .ft$estimate
    results[i, "Fisher"] <- .ft$p.value
    results[i, "Chisq"] <- chisq.test(.mat)$p.value
  } 
}

# Proportion alpha results should meet the alpha level.
alpha <- 0.05
cat("Fisher @alpha: ", length(which(results$Fisher <= alpha))/reps, "\n")
cat("Chisq @alpha: ", length(which(results$Chisq <= alpha))/reps, "\n")

# p values should be uniformly distributed.
par(mfrow=c(3,1))
plot(density(results$Chisq, na.rm=T), bty="n", main = "Distribution of Chisq p values.")
plot(density(results$Fisher, na.rm=T))
plot(density(results$OR, na.rm=T))
par(mfrow=c(1,1))

