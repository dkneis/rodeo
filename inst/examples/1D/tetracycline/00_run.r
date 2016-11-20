rm(list=ls())

ofile_stoi <- "output/tetra_stoi.pdf" 
ofile_dyn <- "output/tetra_dyn.pdf"
ofile_std <- "output/tetra_std.pdf"
ofile_std_full <- "output/tetra_std_full.pdf"
ofile_sen <- "output/tetra_sen.pdf"

options(warn=1)

source("01_initModel.r")
source("02_setData.r")
source("03_solveStd.r")
source("04_solveDyn.r")
#source("05_sensitivity.r")
source("06_cleanUp.r")

