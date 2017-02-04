rm(list=ls())

library(devtools)
devtools::install_github("amytian789/thesis-al", ref = "master",
                         subdir = "thesisAL", force = T)

source("../main/AL_engine.R")
