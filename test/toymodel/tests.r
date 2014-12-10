library(rodeo)

# load model
data(toymodel)
tf= tempfile()
write(x=toymodel, file=tf)
x=new("rodeo", file=tf)
data(toymodel_vars)
x$setVars(setNames(toymodel_vars$value, toymodel_vars$name))
data(toymodel_pars)
x$setPars(setNames(toymodel_pars$value, toymodel_pars$name))
DOSAT= function(t) {14.652 - 0.41022*t + 0.007991*t^2 - 0.000077774*t^3}
k2= function(u, d) {(0.728*sqrt(u) - 0.317*u + 0.0372*u^2) / d / 86400}

# generate code and test
nLevels=3
tf= tempfile()
write(file=tf, x=x$generate(name="derivs", lang="r"))
source(tf)

y= rep(x$getVars(),each=nLevels)
names(y)= paste(rep(names(x$getVars()),each=nLevels),rep(1:nLevels,length(x$getVars())),sep="_")

library(deSolve)
result= lsoda(y=y, times=seq(0, 30*86400, 3600), func=derivs, parms=x$getPars(), nLVLS=nLevels)
if (attr(result,which="istate",exact=TRUE)[1] != 2) stop("Integration failed.")
result= as.data.frame(result)
result$time= result$time / 86400

showLevels=1:2
layout(matrix(showLevels, ncol=2))
for (i in showLevels) {
  par(mar=c(4,4,4,4)+0.1) # Extend right margin for secondary y-axis
  plot(result$time, result[,paste("c_z",i,sep="_")], col="red", type="l", lty=1, bty="n",
    yaxt="n", xlab="Day",ylab="c_z")
  axis(side=2, col="red")
  par(new=T)
  plot(result$time, result[,paste("c_do",i,sep="_")], col="blue", type="l", lty=4, bty="n",
    xaxt="n", yaxt="n", xlab="", ylab="")
  axis(side=4, col="blue")
  mtext("c_do", side=4, line=3)
  legend("top", bty="n", lty=c(1,4), col=c("red","blue"), legend=c("c_z","c_do"))
  mtext(side=3, paste("Level",i))
}
layout(1)

