
rodeoConst= list(

  # Regular expressions to detect valid identifiers
  identifierPatterns= list(
    core="[a-zA-Z]([a-zA-Z0-9_])*",
    before= "(^|[^a-zA-Z0-9_])",
    after= "([^a-zA-Z0-9_]|$)"
  ),

  # Identifiers used in generated code
  genIdent= c(
    # vectors with actual values of vars, pars, rates, derivatives
    vecVars= "var", vecPars= "par", vecPros= "pro", vecDrvs= "dydt",
    # index vectors for the 0D case (or 1st level, respectively)
    ivecVars0D= "ivar0D", ivecPars0D= "ipar0D", ivecPros0D= "ipro0D",
    # index vectors for the current level
    ivecVars= "ivar", ivecPars= "ipar", ivecPros= "ipro",
    # (pointers to) indices of variables in first level
    ilistVars0D= "v0D", ilistPars0D= "p0D", ilistPros0D= "r0D",
    # (pointers to) indices of variables in current level
    ilistVars= "v", ilistPars= "p", ilistPros= "r",
    # dimensions
    lenVars=  "NVAR", lenPars=  "NPAR", lenPros=  "NPRO", lenLevels="NLVL",
    # current level
    levelIndex= "level"
  ),

  reservedNames= c(
    time="time",     # external time
    left="left",     # reference to left element in a 1D model
    right="right"    # reference to right element in a 1D model
  ),

  lang= c(r="r", fortran="f95")

) # End of list

