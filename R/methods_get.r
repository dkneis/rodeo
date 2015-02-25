
rodeo$methods( getStox= function() {
    "Returns the stoichiometry matrix as a data frame (columns = variables, rows = processes)"
    return(stox)
})
rodeo$methods( getPros= function() {
    "Returns a named vector of process rate expressions"
    return(pros)
})
rodeo$methods( getAuxs= function() {
    "Returns a named vector of auxiliary expressions referenced in, e.g., \\code{stox} or \\code{pros}"
    return(auxs)
})
rodeo$methods( getVars= function() {
    "Returns a vector holding the names of state variables"
    return(vars)
})
rodeo$methods( getPars= function() {
    "Returns a vector holding the names of parameters"
    return(pars)
})
rodeo$methods( getFuns= function() {
    "Returns a vector of function names appearing in \\code{auxs}, \\code{stox}, or \\code{pros}"
    return(funs)
})

