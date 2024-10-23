# Fisher with true or false null.

# library(Barnard)

set.seed <- 324

# Sample size range per category 1st var.
n.low <- 10
n.high <- 1000

# Probabilities for outcomes (2nd var).
# This is where you set H0 true (equal probs) or false (non-egal probs).
p1 <- 0.45
p2 <- 0.55

# Minimum count per cell.
mincell <- 10

# Number of simlulations.
reps <- 10000


results <- data.frame()
for (i in 1:reps) {

  # Generate independent counts as RNs from a uniform distribution.
  .n1 <- sample(n.low:n.high, 1)
  .n2 <- sample(n.low:n.high, 1)
  .mat <- table(
    c(rep(0, .n1), rep(1, .n2)),
    c(sample(0:1, size = .n1, replace = TRUE, c(p1, 1-p1)),
      sample(0:1, size = .n2, replace = TRUE, c(p2, 1-p2)))
  )

  if (all(c(.mat)>=mincell)) {
  
    # Get rest results.
    .ft <- fisher.test(.mat)
    results[i, "OR"] <- .ft$estimate
    results[i, "Fisher"] <- .ft$p.value
    .ct <- chisq.test(.mat)
    results[i, "Chisq"] <- .ct$p.value
    results[i, "Phi"] <- sqrt(.ct$p.value/sum(.ct$observed))
    #.bt <- barnard.test(.mat[1,1], .mat[2,1], .mat[1,2], .mat[2,2])
    #results[i, "Barnard"] <- .bt$.pvalue[1]
  } 
}

# Proportion alpha results should meet the alpha level.
alpha <- 0.05
cat("Fisher @alpha: ", length(which(results$Fisher <= alpha))/reps, "\n")
cat("Chisq @alpha: ", length(which(results$Chisq <= alpha))/reps, "\n")
#cat("Barnard @alpha: ", length(which(results$B <= alpha))/reps, "\n")

# p values should be uniformly distributed.
par(mfrow=c(3,1))
plot(density(results$Chisq, na.rm=T), bty="n", main = paste0("Dist. of Chisq p | Prop(p<", alpha, ")=", round(length(which(results$Chisq <= alpha))/reps, 3), "\n True p(sucess|A)=", p1, " | True p(success|B)=", p2),
     xlab = paste0(reps, " simulations; minimal count per cell=", mincell, "; group size A/B [", n.low, "..", n.high, "]"))
plot(density(results$Fisher, na.rm=T), bty="n", main = paste0("Dist. of Fisher p | Prop(p<", alpha, ")=", round(length(which(results$Fisher <= alpha))/reps, 3), "\n True p(sucess|A)=", p1, " | True p(success|B)=", p2),
     xlab = paste0(reps, " simulations | Minimal count per cell=", mincell, " | Group size A/B [", n.low, "..", n.high, "]"))
plot(density(results$OR, na.rm=T), bty="n", main = paste0("Dist. of Odds Ratios\n True p(sucess|A)=", p1, " | True p(success|B)=", p2),
     xlab = paste0(reps, " simulations | Minimal count per cell=", mincell, " | Group size A/B [", n.low, "..", n.high, "]"))
par(mfrow=c(1,1))

#plot(results$Phi~abs(log(results$OR)), bty="n", main = "Distribution of Odds Ratios.")
#plot(results$Phi~results$Fisher, bty="n", main = "Distribution of Odds Ratios.")


