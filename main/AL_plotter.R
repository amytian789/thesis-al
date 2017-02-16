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
qbc_vec <- rep(0,50)

################################### Plot the results

date <- Sys.Date()
pdf(file=paste0("results/rf_", date, ".PDF"), 
    height = 6, width = 10)

#plot
ymax <- max(c(us_vec, random_vec, qbc_vec,cluster_vec))
graphics::plot(1:iter, qbc_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="AL Error Ratio with Random Forest Classifier", xlab="Iterations", ylab="Error", col = "green")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, us_vec, lwd = 2, col = "black")
graphics::lines(1:iter, qbb_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, cluster_vec, lwd = 2, col = "orange")

graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=
                   c("Random Sampling","Uncertainty Sampling","Query by Committee","Query by Bagging","Min-Max Clustering"),
                 col=c("red","black","green","blue","orange"))

graphics.off()
save.image(file = paste0("results/rf_", date, ".RData"))
