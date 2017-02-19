setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/")
source("main/AL_header.R")
source("main/AL_engine.R")
source("main/AL_data.R")




################################## Overall classifier and return methods

# Generic classifier method: X and y contain labeled points
# This is the MAIN overall classifier that will train on the data once the AL selection is completed
classifier_method <- function(X, y, ...) {
  caret::train(X,y,method="rf")
}

# Generic classifier method prediction: X contain all points to predict
# This is the MAIN overall classifier that will train on the data once the AL selection is completed
classifier_predict <- function(classifier, X, ...) {
  stats::predict(classifier, X)
}

# QBC classifier method: X and y contain labeled points
qbc_majority <- function(X, y, committee, ...) {
  tout <- vector("list",length(committee))
  for (i in 1:length(committee)){
    tout[[i]] <- caret::train(X,y,committee[i])
  }
  tout
}

# Generic error ratio: X contain all points. y are known labels (unknown to the learning algorithm)
return_method <- function(classifier, X, y, ...) {
  p <- stats::predict(classifier, X)

  length(which(p != y))/length(y)
}

# QBC error ratio: X contain all points. y are known labels (unknown to the learning algorithm)
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
set.seed(10)
a <- 7
b <- 9
n <- 250 # desired dataset size
init <- 10 # desired number of points to initialize with
idx <- c(sample(which(train$y == a),n/2),sample(which(train$y == b),n/2))
X <- train$x[idx,]
X <- t(apply(X,1,compressImg)) # compress from 28x28 to 14x14 pixels
y <- as.factor(train$y[idx]) # y contains the "true" labels. y is never seen by the AL algorithms 

# Randomly select the initial points given to the AL algorithms
set.seed(10)
y_unlabeled <- y
y_unlabeled[sample(1:n,n-init)] <- NA

# Visual representation of the data
plotTable(13,20,y,X)





###################################
 
s <- 15 # Number of random unlabeled points to "stream" to the AL method
        # n = 0 indicates that the AL stream should sample from all data points
k <- 25 # Number of simulations to run
iter <- 50  # Number of AL algorithm iterations (the "budget")




# Classifier performance given all data is labeled
# pred <- classifier_method(X,y)
# perf_results <- rep(return_method(pred,X,y),iter)
# This has been shown to yield perfect results, so it is commented out

# Uncertainty Sampling
us_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  us_results <- us_results + 
                    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, n = s, 
                              classifier = "rf")
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Committee Majority Vote" classifier
qbc_majority_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  ### To change the committee, you must set it in the AL_engine
  qbc_majority_results <- qbc_majority_results + 
    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = qbc_majority,
              return_method = qbc_m_return, iter = iter, n = s, 
              dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Committee Majority Vote" classifier
# no pruning
qbc_majority_noprune_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  ### To change the committee, you must set it in the AL_engine
  qbc_majority_noprune_results <- qbc_majority_noprune_results + 
    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = qbc_majority,
              return_method = qbc_m_return, iter = iter, n = s, 
              dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Random Forest" classifier
set.seed(10)
qbc_rf_results <- rep(0, iter)
for (i in 1:k){
  ### To change the committee, you must set it in the AL_engine
  qbc_rf_results <- qbc_rf_results + 
    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = classifier_method,
              return_method = return_method, iter = iter, n = s, 
              dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Committee with overall "Random Forest" classifier
# no pruning
qbc_rf_noprune_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  ### To change the committee, you must set it in the AL_engine
  qbc_rf_noprune_results <- qbc_rf_noprune_results + 
    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = classifier_method,
              return_method = return_method, iter = iter, n = s, 
              dis = "vote_entropy", pt = 0.5)
  print(c("Trial ",i,"complete"))
}

# Query by Bagging
qbb_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  qbb_results <- qbb_results + 
                 AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbb", classifier_method = classifier_method,
                           return_method = return_method, iter = iter, n = s, 
                           classifier="rf", dis = "vote_entropy", num_class=5, r=0.75)
  print(c("Trial ",i,"complete"))
}

# Min-Max Clustering
cluster_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  set.seed(10)
  cluster_results <- cluster_results + 
                     AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "cluster", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, 
                              n = s, dis = "euclidean")
  print(c("Trial ",i,"complete"))
}

# Random Sampling
random_results <- rep(0, iter)
set.seed(10)
for (i in 1:k){
  random_results <- random_results + 
                    AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method, return_method, iter, s)
  print(c("Trial ",i,"complete"))
}

# Average
us_vec <- us_results / k
random_vec <- random_results / k
qbc_majority_vec <- qbc_majority_results / k
qbc_rf_vec <- qbc_rf_results / k
qbb_vec <- qbb_results / k
cluster_vec <- cluster_results / k

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
graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=c("Random Forest","Majority Committee Vote"),col=c("black","blue"))

graphics.off()
save.image(file = paste0("results/rf_", date, ".RData"))