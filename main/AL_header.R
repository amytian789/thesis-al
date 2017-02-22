rm(list=ls())

library(devtools)

# used in development to load all the required packages and source all the R code in your current package
# -> replace with the below once ready to "ship"
devtools::load_all("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-al/thesisAL")

######################

# load package so that you can run the simulation code
# devtools::install_github("amytian789/thesis-al", ref = "amy",subdir = "thesisAL", force = T)