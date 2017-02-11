rm(list=ls())

library(devtools)
# load package so that you can run the simulation code
devtools::install_github("amytian789/thesis-al", ref = "master",
                         subdir = "thesisAL", force = T)

source("../main/AL_engine.R")
