# Assignment of initial values to state variables
vars <- matrix(rep(as.numeric(model$getVarsTable()$initial), each=nTanks),
  ncol=model$lenVars(), nrow=nTanks, dimnames=list(NULL, model$namesVars()))
model$setVars(vars)

# Set / update values of parameters; River flow is assumed to be steady
# and uniform (i.e. constant in space and time); Settling and resuspension
# velocities are computed from steady-state mass balance as in Hellweger (2011)
pars <- matrix(rep(suppressWarnings(as.numeric(model$getParsTable()$default)), each=nTanks),
  ncol=model$lenPars(), nrow=nTanks, dimnames=list(NULL, model$namesPars()))
pars[,"V"] <- xsArea * len/nTanks                           # tank volumes
pars[,"Q"] <- c(0, rep(uL * xsArea, nTanks-1))              # inflow from upstr.
pars[,"Q_in"] <- c(uL * xsArea, rep(0, nTanks-1))           # inflow to tank 1
pars[,"us"] <- pars[,"kh_s"] * pars[,"ds"] /                # settling velocity
  ((vars[,"POM_w"] / vars[,"POM_s"]) -
  (vars[,"TSS_w"] / vars[,"TSS_s"]))
pars[,"ur"] <- pars[,"us"] * vars[,"TSS_w"] /               # resuspension velo.
  vars[,"TSS_s"]
model$setPars(pars)

##----------------------------------------BeginHide
# Plot of symbolic stoichiometry matrix
m <- model$stoichiometry(box=1)
clr <- function(x, ignoreSign=FALSE) {
  res <- rep("transparent", length(x))
  if (ignoreSign) {
    res[x != 0] <- "black"
  } else {
    res[x < 0] <- "lightgrey"
    res[x > 0] <- "white"
  }
  return(res)
}
sym <- function(x, ignoreSign=FALSE) {
  res <- rep(NA, length(x))
  if (ignoreSign) {
    res[x != 0] <- 21
  } else {
    res[x < 0] <- 25
    res[x > 0] <- 24
  }
  return(res)
}
pdf(file=ofile_stoi, width=4, height=7)
omar <- par("mar")
par(mar=c(1,6,6,1))
plot(c(1,ncol(m)), c(1,nrow(m)), bty="n", type="n", xaxt="n", yaxt="n",
  xlab="", ylab="")
abline(h=1:nrow(m), v=1:ncol(m), col="grey")
for (ir in 1:nrow(m)) {
  ignoreSign <- grepl(pattern="^transport.*", x=rownames(m)[ir]) ||
    grepl(pattern="^diffusion.*", x=rownames(m)[ir])
  points(1:ncol(m), rep(ir,ncol(m)), pch=sym(m[ir,1:ncol(m)], ignoreSign),
    bg=clr(m[ir,1:ncol(m)], ignoreSign))
}
mtext(side=2, at=1:nrow(m), rownames(m), las=2, line=0.5, cex=0.8)
mtext(side=3, at=1:ncol(m), colnames(m), las=2, line=0.5, cex=0.8)
par(mar=omar)
rm(m)
graphics.off()
##----------------------------------------EndHide

