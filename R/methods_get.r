
# Methods to return names
rodeo$methods( namesVars= function() { as.character(.self$VARS$name) })
rodeo$methods( namesPars= function() { as.character(.self$PARS$name) })
rodeo$methods( namesFuns= function() { as.character(.self$FUNS$name) })
rodeo$methods( namesPros= function() { as.character(.self$PROS$name) })

# Methods to return lengths
rodeo$methods( lenVars= function() { nrow(.self$VARS) })
rodeo$methods( lenPars= function() { nrow(.self$PARS) })
rodeo$methods( lenFuns= function() { nrow(.self$FUNS) })
rodeo$methods( lenPros= function() { nrow(.self$PROS) })

# Methods to return entire tables
rodeo$methods( getVars= function() { .self$VARS })
rodeo$methods( getPars= function() { .self$PARS })
rodeo$methods( getFuns= function() { .self$FUNS })
rodeo$methods( getPros= function() { .self$PROS })
rodeo$methods( getStoi= function() { .self$STOI })

