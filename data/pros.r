pros <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  "growth", "mg/ml/hour", "bacteria growth", "mu * monod(sub, half) * bac",
  "inout", "1/hour", "water in-/outflow", "flow/vol"
  ), ncol=4, byrow=TRUE,
  dimnames=list(NULL, c("name", "unit", "description", "expression")))
)

