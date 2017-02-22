# Since each simulation took a while, I did them separately
# (instead of running all of simulation.R at once)
# This file consolidates each results file to plot the Error Ratio

setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/")

# Random Sampling
load("results/rs_2017-02-15.RData")
randomvec <- random_vec

# Uncertainty Sampling
load("results/us_2017-02-15.RData")
usvec <- us_lda_vec

# Clustering
load("results/cluster_2017-02-15.RData")
clustervec <- cluster_vec

# Query by Bagging
load("results/qbb_voteentropy_2017-02-16.RData")
qbbvec <- qbb_voteentropy_vec

# Query by Committee
load("results/qbc_majority_ALL_2017-02-21.RData")
qbc_majorityvec <- qbc_majority_vec
qbc_majority_noprunevec <- qbc_majority_noprune_vec

load("results/qbc_rf_ALL_2017-02-22.RData")
qbc_rfvec <- qbc_rf_vec
qbc_rf_noprunevec <- qbc_rf_noprune_vec

# Select best QBC output
# if (length(which(qbc_majorityvec < qbc_rfvec)) > length(which(qbc_majorityvec > qbc_rfvec))){
#   qbcvec <- qbc_majorityvec
# } else if (length(which(qbc_majorityvec < qbc_rfvec)) < length(which(qbc_majorityvec > qbc_rfvec))){
#   qbcvec <- qbc_rfvec
# } else{
#   # select one at random
#   rr <- sample(c(0,1),1)
#   if (rr == 0) qbcvec <- qbc_majorityvec
#   else qbcvec <- qbc_rfvec
# }

# Select best QBC output (with pruning)
if (length(which(qbc_majorityvec < qbc_rfvec)) > length(which(qbc_majorityvec > qbc_rfvec))){
  qbc_prunevec <- qbc_majorityvec
} else if (length(which(qbc_majorityvec < qbc_rfvec)) < length(which(qbc_majorityvec > qbc_rfvec))){
  qbc_prunevec <- qbc_rfvec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_prune_vec <- qbc_majorityvec
  else qbc_prune_vec <- qbc_rfvec
}
# Select best QBC output (with no pruning)
if (length(which(qbc_majority_noprunevec < qbc_rf_noprunevec)) > length(which(qbc_majority_noprunevec > qbc_rf_noprunevec))){
  qbc_noprune_vec <- qbc_majority_noprunevec
} else if (length(which(qbc_majority_noprunevec < qbc_rf_noprunevec)) < length(which(qbc_majority_noprunevec > qbc_rf_noprunevec))){
  qbc_noprune_vec <- qbc_rf_noprunevec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_noprune_vec <- qbc_majority_noprunevec
  else qbc_noprune_vec <- qbc_rf_noprunevec
}
# Select best overall QBC output
if (length(which(qbc_prune_vec < qbc_noprune_vec)) > length(which(qbc_prune_vec > qbc_noprune_vec))){
  qbcvec <- qbc_prune_vec
} else if (length(which(qbc_prune_vec < qbc_noprune_vec)) < length(which(qbc_prune_vec > qbc_noprune_vec))){
  qbcvec <- qbc_noprune_vec
} else{
  # select one at random
  rr <- sample(c(0,1),1)
  if (rr == 0) qbcvec <- qbc_prune_vec
  else qbcvec <- qbc_noprune_vec
}

################################### Plot the results

date <- Sys.Date()
pdf(file=paste0("results/rf_", date, ".PDF"), 
    height = 6, width = 10)

# Plot all AL performance
ymax <- max(c(usvec, randomvec, qbcvec, clustervec))
graphics::plot(1:iter, qbcvec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="Various Active Learning Error Ratios with Random Forest Classifier", 
               xlab="Iterations", ylab="Error", col = "green")
graphics::lines(1:iter, randomvec, lwd = 2, col = "red")
graphics::lines(1:iter, usvec, lwd = 2, col = "black")
graphics::lines(1:iter, qbbvec, lwd = 2, col = "blue")
graphics::lines(1:iter, clustervec, lwd = 2, col = "orange")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=
                 c("Random Sampling","Uncertainty Sampling","Query by Committee","Query by Bagging","Min-Max Clustering"),
                 col=c("red","black","green","blue","orange"))

# Plot QBC performance on same scale as earlier
graphics::plot(1:iter, qbc_majorityvec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="Query by Committee AL Error Ratio with Various Classifiers", 
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, qbc_rfvec, lwd = 2, col = "blue")
graphics::lines(1:iter, qbc_majority_noprunevec, lwd = 2, col = "red")
graphics::lines(1:iter, qbc_rf_noprunevec, lwd = 2, col = "orange")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,
                 legend=c("Majority Committee Vote (Pruning)", "Majority Committee Vote (No Pruning)",
                          "Random Forest (Pruning)", "Random Forest (No Pruning)"),
                 col=c("black","red","blue","orange"))

graphics.off()
save.image(file = paste0("results/results_", date, ".RData"))
