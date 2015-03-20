rodeo$methods( stoichiometryMatrix = function() {
  "Builds a stoichiometry matrix from its tabular representation"
  m= matrix("0", ncol=nrow(.self$VARS), nrow=nrow(.self$PROS))
  colnames(m)= .self$VARS$name
  rownames(m)= .self$PROS$name
  for (i in 1:nrow(.self$STOI)) {
    m[.self$STOI$process[i], .self$STOI$variable[i]]= .self$STOI$expression[i]
  }
  return(m)
})

