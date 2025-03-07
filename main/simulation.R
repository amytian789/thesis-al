setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/")
source("main/AL_header.R")
source("main/AL_engine.R")
source("main/AL_engine_noprune.R")
source("main/AL_data.R")




################################## Overall classifier and return methods

# MAIN class.model that will train on the data once AL selection is completed
# X and y contain labeled points
classifier_method <- function(X, y, ...) {
  caret::train(X,y,method="rf")
}

# MAIN prediction method for the data once AL selection is completed
# X contains all points to predict
classifier_predict <- function(classifier, X, ...) {
  stats::predict(classifier, X)
}

# Majority Committee Vote classification model
# X and y contain labeled points
qbc_majority <- function(X, y, committee, ...) {
  tout <- vector("list",length(committee))
  for (i in 1:length(committee)){
    tout[[i]] <- caret::train(X,y,committee[i])
  }
  tout
}

# Generic error ratio
# X contain all points. y are known labels (unknown to the learning algorithm)
return_method <- function(classifier, X, y, ...) {
  p <- stats::predict(classifier, X)
  length(which(p != y))/length(y)
}

# QBC error ratio
# X contain all points. y are known labels (unknown to the learning algorithm)
qbc_m_return <- function(tout, X, y, committee, ...) {
  p <- vector("list",length(committee))
  for (i in 1:length(committee)) {
    p[[i]] <- predict(tout[[i]],newdata=X)
  }
  # Aggregate prediction
  ap <- rep(0,length(y))
  for (i in 1:length(y)){
    temp <- as.numeric(as.character(p[[1]][i]))
    for (j in 1:length(committee)){
      temp <- c(temp,as.numeric(as.character(p[[j]][i])))
    }
    # error checking if a value doesn't appear at all
    if (is.na(as.numeric(sort(table(temp),decreasing=TRUE)[2]))) {
      ap[i] <- as.numeric(names(sort(table(temp),decreasing=TRUE)[1]))
    } else {
      # pick one at random if there is a tie
      if (as.numeric(sort(table(temp),decreasing=TRUE)[1]) == as.numeric(sort(table(temp),decreasing=TRUE)[2])){
        temp <- c(0,1)
        ap[i] <- sample(temp,1)
      } else {
        # Otherwise, insert the first one
        ap[i] <- as.numeric(names(sort(table(temp),decreasing=TRUE)[1]))
      }
    }
  }
  length(which(ap != y))/length(y)
}




################################ Set up the data

load_mnist()
names(train)

# Randomly select the dataset. a and b are the labels which we want to compare 
# (a,b in [0,10]. We are only interested bivariate classification)
a <- 7
b <- 9
n <- 250 # desired dataset size
init <- 10 # desired number of points to initialize with
set.seed(10)
idx <- c(sample(which(train$y == a),n/2),sample(which(train$y == b),n/2))
X <- train$x[idx,]
X <- t(apply(X,1,compressImg)) # compress from 28x28 to 14x14 pixels
y <- as.factor(train$y[idx]) # y contains the "true" labels. y is never seen by the AL algorithms 

# Randomly select the initial points given to the AL algorithms
y_unlabeled <- y
set.seed(10)
y_unlabeled[sample(1:n,n-init)] <- NA

# Visual representation of the data
plotTable(13,20,y,X)

rm(train)




###################################

s <- 15 # Number of random unlabeled points to "pool"
# n = 0 indicates that the "pool" should sample from all data points
k <- 25 # Number of simulations to run
iter <- 50  # Number of AL algorithm iterations (the "budget")




# Classifier performance given all data is labeled
# pred <- classifier_method(X,y)
# perf_results <- rep(return_method(pred,X,y),iter)
# This has been shown to yield perfect results, so it is commented out

# Uncertainty Sampling
us_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  us_results[i,] <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", 
                              classifier_method = classifier_method, return_method = return_method, 
                              iter = iter, n = s, classifier = "rf")
  print(c("Trial ",i,"complete"))
}

# Query by Committee with "Majority Committee Vote" model
qbc_majority_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  ### To change the committee, you must set it in the AL_engine
  qbc_majority_results[i,] <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", 
                                        classifier_method = qbc_majority,return_method = qbc_m_return, 
                                        iter = iter, n = s, dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with "Majority Committee Vote" model
# no pruning
qbc_majority_noprune_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  ### To change the committee, you must set it in the AL_engine_noprune
  qbc_majority_noprune_results[i,] <- AL_engine_noprune(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", 
                                                        classifier_method = qbc_majority, return_method = qbc_m_return, 
                                                        iter = iter, n = s, dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Random Forest" model
qbc_rf_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  ### To change the committee, you must set it in the AL_engine
  qbc_rf_results[i,] <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", 
                                  classifier_method = classifier_method, return_method = return_method, 
                                  iter = iter, n = s, dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Random Forest" model
# no pruning
qbc_rf_noprune_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  ### To change the committee, you must set it in the AL_engine_noprune
  qbc_rf_noprune_results[i,] <- AL_engine_noprune(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", 
                                                  classifier_method = classifier_method, return_method = return_method, 
                                                  iter = iter, n = s, dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Bagging
qbb_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  qbb_results[i,] <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbb", 
                               classifier_method = classifier_method,return_method = return_method, 
                               iter = iter,n = s,classifier="rf",dis = "vote_entropy",num_class=5,r=0.75)
  print(c("Trial ",i,"complete"))
}

# Min-Max Clustering
cluster_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  cluster_results[i,] <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "cluster", 
                                   classifier_method = classifier_method, return_method = return_method, 
                                   iter = iter,n = s, dis = "euclidean")
  print(c("Trial ",i,"complete"))
}

# Random Sampling
random_results <- matrix(0,nrow=k,ncol=iter)
for (i in 1:k){
  set.seed(i)
  random_results[i,] <- AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method, return_method, iter, s)
  print(c("Trial ",i,"complete"))
}

# Average
us_vec <- apply(us_results,2,mean)
random_vec <- apply(random_results,2,mean)
qbc_majority_vec <- apply(qbc_majority_results,2,mean)
qbc_majority_noprune_vec <- apply(qbc_majority_noprune_results,2,mean)
qbc_rf_vec <- apply(qbc_rf_results,2,mean)
qbc_rf_noprune_vec <- apply(qbc_rf_noprune_results,2,mean)
qbb_vec <- apply(qbb_results,2,mean)
cluster_vec <- apply(cluster_results,2,mean)

# Select best QBC output (with pruning)
if (length(which(qbc_majority_vec < qbc_rf_vec)) > length(which(qbc_majority_vec > qbc_rf_vec))){
  qbc_prune_vec <- qbc_majority_vec
} else if (length(which(qbc_majority_vec < qbc_rf_vec)) < length(which(qbc_majority_vec > qbc_rf_vec))){
  qbc_prune_vec <- qbc_rf_vec
} else{
  # select one at random
  set.seed(1)
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
  set.seed(2)
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
  set.seed(3)
  rr <- sample(c(0,1),1)
  if (rr == 0) qbc_vec <- qbc_prune_vec
  else qbc_vec <- qbc_noprune_vec
}


################################### Plot the results

date <- Sys.Date()
pdf(file=paste0("results/results_", date, ".PDF"), height = 6, width = 10)

### Plot all AL performance
ymax <- max(c(us_vec, random_vec, qbc_vec,cluster_vec))
graphics::plot(1:iter, qbc_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="AL Error Ratios with Random Forest classification model*",  
               xlab="Iterations", ylab="Error", col = "green")
mtext("*Query by Committee uses Majority Committee Vote classification model")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, us_vec, lwd = 2, col = "black")
graphics::lines(1:iter, qbb_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, cluster_vec, lwd = 2, col = "orange")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,
                 title="Active Learning method",
                 legend=c("Random Sampling","Uncertainty Sampling","Query by Committee (best)",
                          "Query by Bagging","Min-Max Clustering"),
                 col=c("red","black","green","blue","orange"))

### Plot QBC performance
graphics::plot(1:iter, qbc_majority_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="Query by Committee AL Error Ratio with various parameters", 
               xlab="Iterations", ylab="Error", col = "red")
graphics::lines(1:iter, qbc_majority_noprune_vec, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, qbc_rf_vec, lwd = 2, col = "blue")
graphics::lines(1:iter,qbc_rf_noprune_vec, lwd = 2, lty = 2, col = "blue")
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,
                 title=" Main Classification Model | Committee Pruning",
                 legend=c("Majority Committee Vote | Yes", "Majority Committee Vote | No",
                          "Random Forest | Yes", "Random Forest | No"),
                 col=c("red","red","blue","blue"), lty=c(1,2,1,2))

graphics.off()

### Plot 95% confidence intervals (using t-distribution)

pdf(file=paste0("results/results_ci_", date, ".PDF"), height = 8, width = 8)
par(mfrow=c(3,2))
tt <- rep(0,iter)
ymax <- 0.3

# Random Sampling
for (i in 1:iter){
  tt[i] <- qt(0.975,k-1) * sd(random_results[,i]) / sqrt(k)
}
graphics::plot(1:iter, random_vec, ylim = c(0, ymax), lwd = 2, 
               type = "l", main="Confidence Intervals for Random Sampling",  
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, random_vec + tt, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, random_vec - tt, lwd = 2, lty = 2, col = "red")

# Uncertainty Sampling
for (i in 1:iter){
  tt[i] <- qt(0.975,k-1) * sd(us_results[,i]) / sqrt(k)
}
graphics::plot(1:iter, us_vec, ylim = c(0, ymax), lwd = 2, 
               type = "l", main="Confidence Intervals for Uncertainty Sampling",  
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, us_vec + tt, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, us_vec - tt, lwd = 2, lty = 2, col = "red")

# Min-Max Clustering
for (i in 1:iter){
  tt[i] <- qt(0.975,k-1) * sd(cluster_results[,i]) / sqrt(k)
}
graphics::plot(1:iter, cluster_vec, ylim = c(0, ymax), lwd = 2, 
               type = "l", main="Confidence Intervals for Min-Max Clustering",  
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, cluster_vec + tt, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, cluster_vec - tt, lwd = 2, lty = 2, col = "red")

# Query by Bagging
for (i in 1:iter){
  tt[i] <- qt(0.975,k-1) * sd(qbb_results[,i]) / sqrt(k)
}
graphics::plot(1:iter, qbb_vec, ylim = c(0, ymax), lwd = 2, 
               type = "l", main="Confidence Intervals for Query by Bagging",  
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, qbb_vec + tt, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, qbb_vec - tt, lwd = 2, lty = 2, col = "red")

# Query by Committee (best)
for (i in 1:iter){
  tt[i] <- qt(0.975,k-1) * sd(qbc_majority_noprune_results[,i]) / sqrt(k)
}
graphics::plot(1:iter, qbc_vec, ylim = c(0, ymax), lwd = 2, 
               type = "l", main="Confidence Intervals for Query by Committee (best)",  
               xlab="Iterations", ylab="Error", col = "black")
graphics::lines(1:iter, qbc_vec + tt, lwd = 2, lty = 2, col = "red")
graphics::lines(1:iter, qbc_vec - tt, lwd = 2, lty = 2, col = "red")

graphics.off()
save.image(file = paste0("results/results_", date, ".RData"))