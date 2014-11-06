#TODO Path might need an update
setwd("/home/dkneis/tudd/dev/r_packages/rodeo/test/omexdia")

# load model from JSON file
library(rodeo)
x=new("rodeo",file="data/omexdia_model.json")

# set parameters and initialize state variables
tmp= read.table(file="data/omexdia_pars.txt",header=T)
x$setPars(setNames(tmp$value, tmp$name))
tmp= read.table(file="data/omexdia_vars.txt",header=T)
x$setVars(setNames(tmp$value, tmp$name))
rm(tmp)
#x$plot_stox()

# generate a function to compute the derivatives at SIZE spatial levels
SIZE=2
tf=tempfile()
write(file=tf, x=x$generate(name="derivs",size=SIZE,return_proc=TRUE))
source(tf)
print(paste0("Generated code is in file '",tf,"'"))
# deactivate next statement to see the generated code
if (file.remove(tf)) print("... but now it's already removed.")

# integrate
library(deSolve)
nDays= 10
initial= rep(x$getVars(),each=SIZE)
result= lsoda(y=initial, times=seq(0, nDays, 1/24),
  func=derivs, parms=x$getPars())
if (attr(result,which="istate",exact=TRUE)[1] != 2) stop("Integration failed.")

# adapt format and names of output
result= as.data.frame(result)
names(result)= c("time",
  paste(rep(names(x$getVars()),each=SIZE),rep(1:length(x$getVars()),length=SIZE),sep="_"),
  paste(rep(names(x$getProc()),each=SIZE),rep(1:length(x$getProc()),length=SIZE),sep="_")
)

# plot states
LEVEL=1
if (LEVEL > SIZE)
  stop(paste0("plotting was requested for level ",LEVEL," but model has only ",SIZE))
layout(matrix(1:length(x$getVars()), nrow=2, byrow=TRUE))
for (i in 1:length(x$getVars())) {
  col= paste(names(x$getVars())[i], LEVEL, sep="_")
  plot(result$time, result[,col], type="l", bty="n",
    xlab="Days",ylab=col)
}
# plot process rates
layout(matrix(1:length(x$getProc()), nrow=2, byrow=TRUE))
for (i in 1:length(x$getProc())) {
  col= paste(names(x$getProc())[i], LEVEL, sep="_")
  plot(result$time, result[,col], type="l", bty="n",
    xlab="Days",ylab=col)
}

