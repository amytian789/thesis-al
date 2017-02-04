rm(list = ls())
load("../results/lda_2017-02-04.RData")

#unwind the results
lda_vec <- unlist(lda_results)
random_vec <- unlist(random_results)

#plot
ymax <- max(c(lda_vec, random_vec))

graphics::plot(1:iter, lda_vec, ylim = c(0, ymax), lwd = 2, type = "l")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
