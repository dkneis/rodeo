
# Methods to return names
rodeo$methods( namesVars= function() { as.character(.self$.vars$name) })
rodeo$methods( namesPars= function() { as.character(.self$.pars$name) })
rodeo$methods( namesFuns= function() { as.character(.self$.funs$name) })
rodeo$methods( namesPros= function() { as.character(.self$.pros$name) })

# Methods to return lengths
rodeo$methods( lenVars= function() { nrow(.self$.vars) })
rodeo$methods( lenPars= function() { nrow(.self$.pars) })
rodeo$methods( lenFuns= function() { nrow(.self$.funs) })
rodeo$methods( lenPros= function() { nrow(.self$.pros) })

# Methods to return entire tables
rodeo$methods( getVars= function() { .self$.vars })
rodeo$methods( getPars= function() { .self$.pars })
rodeo$methods( getFuns= function() { .self$.funs })
rodeo$methods( getPros= function() { .self$.pros })
rodeo$methods( getStoi= function() { .self$.stoi })

