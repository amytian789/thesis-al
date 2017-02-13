source("../main/AL_header.R")

library(caret)
library(entropy)

################################
# set up the data

date <- Sys.Date()
iter <- 50 

set.seed(10)
X <- rbind(MASS::mvrnorm(100, rep(0,2), diag(2)),
           MASS::mvrnorm(100, c(0.5,0), diag(2)))
y <- rep(0,200)
for (i in 1:200) {
  if (X[i,1] > 0 & X[i,2] > 0) y[i] = 1
}
y <- factor(y)

# Randomly set up the unlabeled data
set.seed(10)
y_unlabeled <- y
y_unlabeled[sample(1:200,180)] <- NA

##################################

# Generic classifier method: X and y contain labeled points
classifier_method <- function(X, y, ...) {
  MASS::lda(X, y)
}

# QBC classifier method: X and y contain labeled points
qbc_majority <- function(X, y, committee, ...) {
  tout <- vector("list",length(committee))
  for (i in 1:length(committee)){
    tout[[i]] <- caret::train(X,y,committee[i])
  }
  tout
}

# Generic return method: X contain all points. y are known labels (unknown to the learning algorithm)
return_method <- function(classifier, X, y, ...) {
  p <- stats::predict(classifier, X)$class

  length(which(p != y))/length(y)
}

# QBC return method: X contain all points. y are known labels (unknown to the learning algorithm)
qbc_m_return <- function(tout, X, y, committee, ...) {
  p <- vector("list",length(committee))
  for (i in 1:length(committee)) {
    p[[i]] <- predict(tout[[i]],newdata=X)
  }
  # Aggregate prediction
  ap <- rep(0,length(y))
  for (i in 1:length(y)){
    temp <- as.numeric(as.character(p[[1]][i]))
    for (j in 2:length(committee)){
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

###################################

# The number of random unlabeled points it "streams" to the AL / RS method
# n = 0 indicates that the AL should sample from all data points 
n = 15
k = 10

set.seed(10)

#initialize
us_lda_results <- rep(0, iter)
qbc_results <- rep(0, iter)
cluster_results <- rep(0, iter)
random_results <- rep(0, iter)

# classifier performance given all data is labeled
pred <- classifier_method(X,y)
perf_results <- rep(return_method(pred,X,y),iter)

#run the engine (average over k = 1000 random samples)
for (i in 1:k){
  us_lda_results <- us_lda_results + 
                    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, n = n, classifier = "lda")
  
  ### To change the committee, you must set it in the AL_engine
  qbc_results <- qbc_results + 
                 AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = qbc_majority,
                          return_method = qbc_m_return, iter = iter, n = n, dis = "vote_entropy", pt = 0.75)
  
  cluster_results <- cluster_results + 
                     AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "cluster", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, n = n, dis = "euclidean")
  
  random_results <- random_results + 
                    AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method, return_method, iter, n)
  
  print(c("Trial ",i,"complete"))
}

# Average
us_lda_vec <- us_lda_results / k
random_vec <- random_results / k
qbc_vec <- qbc_results / k
cluster_vec <- cluster_results / k

###################################

pdf(file=paste0("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/results/lda_", date, ".PDF"), 
    height = 6, width = 10)

#plot
ymax <- max(c(us_lda_vec, random_vec, qbc_vec,cluster_vec))
graphics::plot(1:iter, random_vec, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="AL Error Ratio with LDA Classifier", xlab="Iterations", ylab="Error", col = "red")
graphics::lines(1:iter, us_lda_vec, lwd = 2, col = "black")
graphics::lines(1:iter, qbc_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, cluster_vec, lwd = 2, col = "orange")
graphics::lines(1:iter, perf_results, lwd = 2, col = "green")

graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=
                 c("Random Sampling","Uncertainty Sampling", "Query by Committee","Min-Max Clustering","Given all data with labels"),
                 col=c("red","black","blue","orange","green"))

graphics.off()
save.image(file = paste0("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/results/lda_", date, ".RData"))