setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/")
source("main/AL_header.R")
source("main/AL_engine.R")
source("main/AL_data.R")




################################## Overall classifier and return methods

# Generic classifier method: X and y contain labeled points
# This is the MAIN overall classifier that will train on the data once the AL selection is completed
classifier_method <- function(X, y, ...) {
  MASS::lda(X, y)
}

# Generic classifier method prediction: X contain all points to predict
# This is the MAIN overall classifier that will train on the data once the AL selection is completed
classifier_predict <- function(classifier, X, ...) {
  stats::predict(classifier, X)$class
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
  p <- stats::predict(classifier, X)$class

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




################################ Set up the data

load_mnist()
names(train)

# Randomly select the dataset. a and b are the labels which we want to compare 
# (a,b in [0,10]. We are only interested bivariate classification)
set.seed(10)
a <- 0
b <- 1
n <- 500 # desired dataset size
idx <- c(sample(which(train$y == a),n/2),sample(which(train$y == b),n/2))
X <- train$x[idx,]
X <- t(apply(X,1,compressImg)) # compress from 28x28 to 14x14 pixels
y <- train$y[idx] # y contains the "true" labels. y is never seen by the AL algorithms 

# Randomly select the initial points given to the AL algorithms
set.seed(10)
y_unlabeled <- y
y_unlabeled[sample(1:n,180)] <- NA

# Visual representation of the data
plotTable(25,20,y,X)





###################################
 
s <- 15 # Number of random unlabeled points to "stream" to the AL method
        # n = 0 indicates that the AL stream should sample from all data points
k <- 1 # Number of simulations to run
iter <- 50  # Number of AL algorithm iterations (the "budget")

#initialize
us_lda_results <- rep(0, iter)
qbc_results <- rep(0, iter)
qbb_results <- rep(0, iter)
cluster_results <- rep(0, iter)
random_results <- rep(0, iter)

# classifier performance given all data is labeled
pred <- classifier_method(X,y)
perf_results <- rep(return_method(pred,X,y),iter)

#run the engine (average over k = 1000 random samples)
for (i in 1:k){
  set.seed(10)
  us_lda_results <- us_lda_results + 
                    AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, n = s, 
                              classifier = "lda")
  
  ### To change the committee, you must set it in the AL_engine
  set.seed(10)
  qbc_results <- qbc_results + 
                 AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = qbc_majority,
                          return_method = qbc_m_return, iter = iter, n = s, 
                          dis = "vote_entropy", pt = 0.75)
  
  set.seed(10)
  qbb_results <- qbb_results + 
                 AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbb", classifier_method = classifier_method,
                           return_method = return_method, iter = iter, n = s, 
                           classifier_train=classifier_method, classifier_predict=classifier_predict, 
                           num_class=10, r=0.75, dis = "vote_entropy")
  
  set.seed(10)
  cluster_results <- cluster_results + 
                     AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "cluster", classifier_method = classifier_method,
                              return_method = return_method, iter = iter, 
                              n = s, dis = "euclidean")
  
  set.seed(10)
  random_results <- random_results + 
                    AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method, return_method, iter, s)
  
  print(c("Trial ",i,"complete"))
}

# Average
us_lda_vec <- us_lda_results / k
random_vec <- random_results / k
qbc_vec <- qbc_results / k
qbb_vec <- qbb_results / k
cluster_vec <- cluster_results / k





################################### Plot the results

date <- Sys.Date()
pdf(file=paste0("results/lda_", date, ".PDF"), 
    height = 6, width = 10)

#plot
ymax <- max(c(us_lda_vec, random_vec, qbc_vec,cluster_vec))
graphics::plot(1:iter, perf_results, ylim = c(0, ymax), lwd = 2, type = "l", 
               main="AL Error Ratio with LDA Classifier", xlab="Iterations", ylab="Error", col = "green")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, us_lda_vec, lwd = 2, col = "black")
graphics::lines(1:iter, qbc_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, qbb_vec, lwd = 2, col = "darkturquoise")
graphics::lines(1:iter, cluster_vec, lwd = 2, col = "orange")

graphics::legend(x="bottomleft",lwd=2,cex = 0.75,legend=
                 c("Random Sampling","Uncertainty Sampling","Query by Committee","Query by Bagging","Min-Max Clustering",
                   "Given all data with labels"),
                 col=c("red","black","blue","darkturquoise","orange","green"))

graphics.off()
save.image(file = paste0("results/lda_", date, ".RData"))