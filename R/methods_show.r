
rodeo$methods( show = function() {
  "Prints all fields of a \\code{rodeo} object"
  print("--- State variables ---")
  print(getVars())
  print("--- Parameters ---")
  print(getPars())
  print("--- Functions ---")
  print(getFuns())
  print("--- Auxiliary expressions ---")
  print(getAuxx())
  print("--- Process rates ---")
  print(getProc())
  print("--- Stoichiometry matrix ---")
  print(getStox())
})

