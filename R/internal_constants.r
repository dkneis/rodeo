
rodeoConst= list(

  # Regular expressions to detect valid identifiers
  identifierPatterns= list(
    core="[a-zA-Z]([a-zA-Z0-9_])*",
    before= "(^|[^a-zA-Z0-9_])",
    after= "([^a-zA-Z0-9_]|$)"
  ),

  # Identifiers used in generated code
  genIdent= c(
    vecVars= "var", vecPars= "par", vecPros= "pro", vecDrvs= "dydt",
    ivecVars0D= "ivar0D", ivecPars0D= "ipar0D", ivecPros0D= "ipro0D",
    ivecVars= "ivar", ivecPars= "ipar", ivecPros= "ipro",
    ilistVars0D= "v0D", ilistPars0D= "p0D", ilistPros0D= "r0D",
    ilistVars= "v", ilistPars= "p", ilistPros= "r",
    lenVars=  "NVAR", lenPars=  "NPAR", lenPros=  "NPRO",
    lenLevels="NLVL", levelIndex= "level"
  ),

  nameTime="time"

) # End of list

