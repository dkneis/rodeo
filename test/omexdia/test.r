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
SIZE=1
tf=tempfile()
write(file=tf, x=x$generate(name="derivs",size=SIZE))
source(tf)
print(paste0("Generated code is in file '",tf,"'"))
# deactivate next statement to see the generated code
if (file.remove(tf)) print("... but now it's already removed.")

# wrapper for compatibility with deSolve integrators
# --> This is where the transport terms can be added later
dydt= function(t,y,p) { list(derivs(t,y,p)) }

# integrate
library(deSolve)
nDays= 2
result= lsoda(y=rep(x$getVars(),each=SIZE), times=seq(0, nDays, 1/1440),
  func=dydt, parms=x$getPars())
if (attr(result,which="istate",exact=TRUE)[1] != 2) stop("Integration failed.")

# visualize
LEVEL=1
if (LEVEL > SIZE)
  stop(paste0("plotting was requested for level ",LEVEL," but model has only ",SIZE))
result= as.data.frame(result)
layout(matrix(1:length(x$getVars()), nrow=2, byrow=TRUE))
for (i in 1:length(x$getVars())) {
  col= match(names(x$getVars())[i], names(result)) - 1 + LEVEL
  plot(result$time, result[,col], type="l", bty="n",
    xlab="Days",ylab=names(result)[i])
}

