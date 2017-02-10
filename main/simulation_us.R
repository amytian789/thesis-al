source("../main/AL_header.R")

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
# set up the methods

al_method <- function(X, y, n){
  active_learning(X, y, almethod = "us", n)
}

random_method <- function(X, y, n){
  unlabel_index_c <- sample(which(is.na(y)), n)
  sample(unlabel_index_c,1)
}

classifier_method <- function(X, y){
  MASS::lda(X, y)
}

#set up the return method
return_method <- function(classifier, X, y){
  pred <- stats::predict(classifier, X)$class

  length(which(pred != y))/length(y)
}

###################################

# The number of random unlabeled points it "streams" to the AL / RS method
n = 15

#run the engine
set.seed(10)
us_results <- AL_engine(X, y, y_unlabeled, al_method, classifier_method,
                         return_method, iter, n)

set.seed(10)
random_results <- AL_engine(X, y, y_unlabeled, random_method, classifier_method,
                         return_method, iter, n)

###################################

#unwind the results
us_vec <- unlist(us_results)
random_vec <- unlist(random_results)

#plot
ymax <- max(c(us_vec, random_vec))
graphics::plot(1:iter, us_vec, ylim = c(0, ymax), lwd = 2, type = "l", main="Performance of AL", xlab="Interations", ylab="Error")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")

########## This code doesn't seem too work right now
# save.image(file = paste0("../results/us_", date, ".RData"))