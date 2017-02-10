source("../main/AL_header.R")

library(caret)
library(entropy)

################################
# set up the data

date <- Sys.Date()
iter <- 100 

set.seed(10)
X <- rbind(MASS::mvrnorm(100, rep(0,2), diag(2)),
           MASS::mvrnorm(100, c(0.5,0), diag(2)))
y <- rep(0,200)
for (i in 1:200) {
  if (X[i,1] > 0 & X[i,2] > 0) y[i] = 1
}
y <- factor(y)

#set up the unlabeled data
y_unlabeled <- y
y_unlabeled[c(11:100, 111:200)] <- NA

##################################

# Generic classifier method: X and y contain labeled points
classifier_method <- function(X, y, ...) {
  MASS::lda(X, y)
}

# QBC classifier method: X and y contain labeled points
qbc_majority <- function(X, y, committee, ...) {
  tout <- list()
  for (i in 1:length(committee)){
    tout[[i]] <- train(X,y,committee[i])
  }
}

# Generic return method: X contain all points. y are known labels (unknown to the learning algorithm)
return_method <- function(classifier, X, y, ...) {
  p <- stats::predict(classifier, X)$class

  length(which(p != y))/length(y)
}

# QBC return method: X contain all points. y are known labels (unknown to the learning algorithm)
qbc_m_return <- function(tout, X, y, committee, ...) {
  p <- list()
  for (i in 1:length(committee)) {
    p[[i]] <- predict(tout[[i]],newdata=X)
  }
  # Aggregate prediction
  ap <- vector()
  for (i in 1:length(committee)){
    # Pick one at random if there is a tie
    if (as.numeric(sort(table(test),decreasing=TRUE)[1]) == as.numeric(sort(table(test),decreasing=TRUE)[2])) {
      temp <- c(0,1)
      ap[i] <- sample(temp,1)
    } else {
      # Otherwise, insert the first one
      ap[i] <- as.numeric(names(sort(table(test),decreasing=TRUE)[1]))
    }
  }
  length(which(ap != y))/length(y)
}

###################################

# The number of random unlabeled points it "streams" to the AL / RS method
n = 15

#run the engine (average over 1000 random samples)
set.seed(10)
us_lda_results <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", classifier_method = classifier_method,
                            return_method = return_method, iter = iter, n = n, classifier = "lda")

### To change the committee, you must set it in the AL_engine
set.seed(10)
qbc_results <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = qbc_majority,
                            return_method = qbc_m_return, iter = iter, n = n, dis = "vote_entropy", pt = 0.75)

set.seed(10)
random_results <- AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method,
                            return_method, iter, n)

# classifier performance given everything
pred <- stats::predict(MASS::lda(X, y), X)$class
perf_results <- rep(length(which(pred != y))/length(y),iter)

###################################

#unwind the results
us_lda_vec <- unlist(us_lda_results)
random_vec <- unlist(random_results)
qbc_vec <- unlist(qbc_results)

#plot
ymax <- max(c(us_lda_vec, random_vec, qbc_vec))
graphics::plot(1:iter, us_lda_vec, ylim = c(0, ymax), lwd = 2, type = "l", main="Performance of AL", xlab="Interations", ylab="Error")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, qbc_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, perf_results, lwd = 2, col = "green")

########## This code doesn't seem too work right now
# save.image(file = paste0("../results/us_", date, ".RData"))