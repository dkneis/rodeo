stoi <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  "bac", "growth", "1",
  "bac", "inout", "-bac",
  "sub", "growth", "-1 / yield",
  "sub", "inout", "(sub_in - sub)"
  ), ncol=3, byrow=TRUE,
  dimnames=list(NULL, c("variable", "process", "expression")))
)

