vars <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  "bac", "mg/ml", "bacteria density",
  "sub", "mg/ml", "substrate concentration"
  ), ncol=3, byrow=TRUE,
  dimnames=list(NULL, c("name", "unit", "description")))
)

