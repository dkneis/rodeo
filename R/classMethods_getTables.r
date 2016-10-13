
# Methods to return names
rodeo$set("public", "namesVars", function() { as.character(private$varsTbl$name) })
rodeo$set("public", "namesPars", function() { as.character(private$parsTbl$name) })
rodeo$set("public", "namesFuns", function() { as.character(private$funsTbl$name) })
rodeo$set("public", "namesPros", function() { as.character(private$prosTbl$name) })

# Methods to return lengths
rodeo$set("public", "lenVars", function() { nrow(private$varsTbl) })
rodeo$set("public", "lenPars", function() { nrow(private$parsTbl) })
rodeo$set("public", "lenFuns", function() { nrow(private$funsTbl) })
rodeo$set("public", "lenPros", function() { nrow(private$prosTbl) })

# Methods to return entire tables
rodeo$set("public", "getVarsTable", function() { private$varsTbl })
rodeo$set("public", "getParsTable", function() { private$parsTbl })
rodeo$set("public", "getFunsTable", function() { private$funsTbl })
rodeo$set("public", "getProsTable", function() { private$prosTbl })
rodeo$set("public", "getStoiTable", function() { private$stoiTbl })

