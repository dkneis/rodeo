
rodeo$methods( getStox= function() {
    "Returns the stoichiometry matrix as a data frame (columns = variables, rows = processes)"
    return(stox)
})
rodeo$methods( getProc= function() {
    "Returns a named vector of process rate expressions"
    return(proc)
})
rodeo$methods( getAuxx= function() {
    "Returns a named vector of auxiliary expressions referenced in, e.g., \\code{stox} or \\code{proc}"
    return(auxx)
})
rodeo$methods( getVars= function() {
    "Returns the values of state variables in a named vector"
    return(vars)
})
rodeo$methods( getPars= function() {
    "Returns the values of parameters in a named vector"
    return(pars)
})
rodeo$methods( getFuns= function() {
    "Returns a vector of function names appearing in \\code{auxx}, \\code{stox}, or \\code{proc}"
    return(funs)
})

