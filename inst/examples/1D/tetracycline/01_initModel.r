# Load packages
library("rodeo")            # provides the code generator 
library("deSolve")          # provides ODE solvers
library("rootSolve")        # provides solvers for non-linear equations

# Properties of the reach not being parameters of the core model
len <-   125000             # reach length (m)
uL  <- 0.5 * 86400          # flow velocity (m/d)
dL  <- 300 * 86400          # longitudinal dispersion coefficient (m2/d)
xsArea <- 0.6 * 15          # wet cross-section area (m2)

# Computational parameters
nTanks <- trunc(uL * len / dL / 2) + 1 # number of tanks; see Elgeti (1996)
dt_max <- 0.5 * len / nTanks / uL      # max. time step (d); Courant criterion

# Initialization of rodeo object from spreadsheet data
readTable <- function(f) {
  read.table(file=paste0("modelDefinition/tables/",f), header=TRUE, sep="\t")
}
dfToMatrix <- function(df) {
  matrix(unlist(df[,2:ncol(df)]), nrow=nrow(df), ncol=ncol(df)-1,
    dimnames=list(df[,1], names(df)[2:ncol(df)]))
}
model <- rodeo$new(vars=readTable("vars.txt"), pars=readTable("pars.txt"),
  funs=readTable("funs.txt"), pros=readTable("pros.txt"),
  stoi=dfToMatrix(readTable("stoi.txt")), asMatrix=TRUE, dim=c(nTanks))

# Generation + compilation of Fortran code, using hand-coded functions as well
lib <- model$compile(sources="modelDefinition/functions/functions.f95")
dyn.load(lib["libFile"])                                          # load library

