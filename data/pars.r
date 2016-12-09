pars <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  "mu", "1/hour", "intrinsic bacteria growth rate",
  "half", "mg/ml", "half saturation concentration of substrate",
  "yield", "mg/mg", "biomass produced per amount of substrate",
  "vol", "ml", "volume of reactor",
  "flow", "ml/hour", "rate of through-flow",
  "sub_in", "mg/ml", "substrate concentration in inflow"
  ), ncol=3, byrow=TRUE,
  dimnames=list(NULL, c("name", "unit", "description")))
)

