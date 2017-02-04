rm(list=ls())

library(devtools)
install_github("amytian789/thesis-al", ref = "master", subdir = "thesisAL",
               force = T)

source("AL_engine.R")
