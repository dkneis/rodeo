
rodeo$methods( print = function() {
  "Prints all fields of a \\code{rodeo} object"
  base::print("--- State variables ---")
  base::print(getVars())
  base::print("--- Parameters ---")
  base::print(getPars())
  base::print("--- Functions ---")
  base::print(getFuns())
  base::print("--- Auxiliary expressions ---")
  base::print(getAuxs())
  base::print("--- Process rates ---")
  base::print(getPros())
  base::print("--- Stoichiometry matrix ---")
  base::print(getStox())
})

