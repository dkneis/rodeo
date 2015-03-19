rm(list=ls())

################################################################################
# load all package code
setwd("/home/dkneis/tudd/dev/r_packages/rodeo/test")
for (f in list.files(path="../R", pattern=".+[.]r", full.names=TRUE))
  source(f)
identifiers= read.table(file="../data/identifiers.txt", header=TRUE)
processes= read.table(file="../data/processes.txt", header=TRUE)
stoichiometry= read.table(file="../data/stoichiometry.txt", header=TRUE)

################################################################################

model= new("rodeo",
  vars=subset(identifiers,type=="v"),
  pars=subset(identifiers,type=="p"),
  funs=subset(identifiers,type=="f"),
#  auxs=data.frame(name=c(), units=c(), description=c(), expression=c()), 
  pros=processes,
  stoi=stoichiometry
)

print(model)


subst= rbind(
  c("c_z", "var[1]"),
  c("c_do", "var[2]"),
  c("kd", "p[1]"),
  c("h_do", "p[2]")
)

for (i in 1:nrow(model$PROS)) {
  expr= model$PROS$expression[i]
  print(expr)
  print(substituteIdentifiers(expr, sub=setNames(subst[,2], subst[,1]), all=FALSE))
}

print(model$STOI)
print(model$stoichiometryMatrix())


O2sat= function(t) {14.652 - 0.41022*t + 0.007991*t^2 - 0.000077774*t^3}
ka= function(u, d) {(0.728*sqrt(u) - 0.317*u + 0.0372*u^2) / d / 86400}
monod= function(s,h) {s / (s + h)}

#model$plot(c(X=1, Z=1, kd=0.1, ka=0.1, s=1))

code= model$generate(name="reac",lang="f95")
write(x=code, file="gen.f95",ncolumns=1)
code= model$generate(name="reac",lang="r")
write(x=code, file="gen.r",ncolumns=1)

