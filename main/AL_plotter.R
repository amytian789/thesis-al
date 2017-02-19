# Since each simulation took a while, I did them separately
# (instead of running all of simulation.R at once)
# This file consolidates each results file to plot the Error Ratio

setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/")

# Random Sampling
load("results/rs_2017-02-15.RData")
random_vec <- random_vec

# Uncertainty Sampling
load("results/us_2017-02-15.RData")
us_vec <- us_lda_vec

# Clustering
load("results/cluster_2017-02-15.RData")
cluster_vec <- cluster_vec

# Query by Bagging
load("results/qbb_voteentropy_2017-02-16.RData")
qbb_vec <- qbb_voteentropy_vec

# Query by Committee
load("results/qbc_majority_2017-02-17.RData")
qbc_majority_vec <- qbc_majority_vec
load("results/qbc_rf_2017-02-18.RData")
qbc_rf_vec <- qbc_rf_vec
load("results/qbc_rf_noprune_2017-02-18.RData")
qbc_rf_noprune_vec <- qbc_rf_noprune_vec
load("results/qbc_majority_noprune_2017-02-19.RData")
qbc_majority_noprune_vec <- qbc_majority_noprune_vec

# Select best QBC output
# if (length(which(qbc_majority_vec < qbc_rf_vec)) > length(which(qbc_majority_vec > qbc_rf_vec))){
#   qbc_vec <- qbc_majority_vec
# } else if (length(which(qbc_majority_vec < qbc_rf_vec)) < length(which(qbc_majority_vec > qbc_rf_vec))){
#   qbc_vec <- qbc_rf_vec
# } else{
#   # select one at random
#   rr <- sample(c(0,1),1)
#   if (rr == 0) qbc_vec <- qbc_majority_vec
#   else qbc_vec <- qbc_rf_vec
# }

# Select best QBC output (with pruning)
if (length(which(qbc_majority_vec < qbc_rf_vec)) > length(which(qbc_majority_vec > qbc_rf_vec))){
  qbc_prune_vec <- qbc_majority_vec
} else if (length(which(qbc_majority_vec < qbc_rf_vec)) < length(which(qbc_majority_vec > qbc_rf_vec))){
  qbc_prune_vec <- qbc_rf_vec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_prune_vec <- qbc_majority_vec
  else qbc_prune_vec <- qbc_rf_vec
}
# Select best QBC output (with no pruning)
if (length(which(qbc_majority_noprune_vec < qbc_rf_noprune_vec)) > length(which(qbc_majority_noprune_vec > qbc_rf_noprune_vec))){
  qbc_noprune_vec <- qbc_majority_noprune_vec
} else if (length(which(qbc_majority_noprune_vec < qbc_rf_noprune_vec)) < length(which(qbc_majority_noprune_vec > qbc_rf_noprune_vec))){
  qbc_noprune_vec <- qbc_rf_noprune_vec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_noprune_vec <- qbc_majority_noprune_vec
  else qbc_noprune_vec <- qbc_rf_noprune_vec
}
# Select best overall QBC output
if (length(which(qbc_prune_vec < qbc_noprune_vec)) > length(which(qbc_prune_vec > qbc_noprune_vec))){
  qbc_vec <- qbc_prune_vec
} else if (length(which(qbc_prune_vec < qbc_noprune_vec)) < length(which(qbc_prune_vec > qbc_noprune_vec))){
  qbc_vec <- qbc_noprune_vec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_vec <- qbc_prune_vec
  else qbc_vec <- qbc_noprune_vec
}

################################### Plot the results

date <- Sys.Date()
pdf(file=paste0("results/rf_", date, ".PDF"), 
    height = 6, width = 10)

# Plot all AL performance
ymax <- max(c(us_vec, random_vec, qbc_vec,cluster_vec))
graphics::plot(1:iter, qbc_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="Various Active Learning Error Ratios with Random Forest Classifier", 
               xlab="Iterations", ylab="Error", col = "green")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, us_vec, lwd = 2, col = "black")
graphics::lines(1:iter, qbb_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, cluster_vec, lwd = 2, col = "orange")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=
                 c("Random Sampling","Uncertainty Sampling","Query by Committee","Query by Bagging","Min-Max Clustering"),
                 col=c("red","black","green","blue","orange"))

# Plot QBC performance on same scale as earlier
graphics::plot(1:iter, qbc_majority_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="Query by Committee AL Error Ratio with Various Classifiers", 
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, qbc_rf_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, qbc_majority_noprune_vec, lwd = 2, col = "red")
graphics::lines(1:iter, qbc_rf_noprune_vec, lwd = 2, col = "orange")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,
                 legend=c("Majority Committee Vote (Pruning)", "Majority Committee Vote (No Pruning)",
                          "Random Forest (Pruning)", "Random Forest (No Pruning)"),
                 col=c("black","red","blue","orange"))

graphics.off()
save.image(file = paste0("results/rf_", date, ".RData"))
