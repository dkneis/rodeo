

# Specific comments:
#
# In the generated code, auxiliary expressions (auxx) are replaced by their values. On the one
# hand, this increases the number of terms to be computed for process rates and
# stoichiometry factors (and this may slow down computations). On the other hand
# this avoids memory allocation for additional constants inside the function
# what may also slow down computations (especially in spatially distributed models).
#
# Parameters are scalar. This remains true even for spatially distributed models.
# In the case of spatially variable parameters, one needs to introduce a
# state variable (with all derivatives being zero).


rodeo$methods( generate = function(name="derivs", nLevels=1, lang="r",
  nameVecVars= "Y_", nameVecPars= "P_", nameSpatialLevelIndex= "i_",
  nameLenVars= "NVARS", nameLenPars= "NPARS", nameLenProc="NPROC", nameLenLevels="N_",
  nameVecDrvs= "dYdt",   nameVecProc= "proc"
) {
    "Generate code to compute the variables' derivatives with respect to time.
    \\bold{Arguments:} \\code{name}: A string giving the name for the generated
    function; \\code{nLevels}: An integer (default 1) specifying the number of
    instances of each state variable, e.g. in spatially distributed models.
    \\code{lang}: The language of generated code (currently 'r' or 'f').
    \\bold{Returns:} The generated function as a character string.
    "
  # Constants
  newline= ifelse(.Platform$OS.type=="windows","\r\n","\n")
  # Language-specific code features
  if (lang == "r") {
    L= list(com="#", cont="", eleOpen="[", eleClose="]", vecOpen="c(", vecClose=")")
  } else if (lang == "f") {
    L= list(com="!", cont="&", eleOpen="(", eleClose=")", vecOpen="(/", vecClose="/)")
  } else {
    stop("requested language not supported")
  }
  # Function to break long lines in Fortran
  breakline= function(text, conti, newline) {
    minlen= 65
    buf=""
    from=1
    k= 0
    for (i in 1:nchar(text)) {
      k= k+1
      if (substr(text,i,i) %in% c("+","-","*","/",",") && (k >= minlen)) {
        k= 0
        buf= paste0(buf,substr(text,from,i),conti,newline)
        from=i+1
      }
    }
    if (from <= nchar(text))
      buf= paste0(buf,substr(text,from,nchar(text)))
    return(buf) 
  }

  # We work with local copies (since this is a reference class!)
  AUXX= auxx
  PROC= proc
  STOX= stox

  # Regexp patterns that preceed/follow valid identifier names. These patterns
  # are important to avoid partial matches, e.g. to properly distinguish between
  # two identifiers "temp" and "temperature". 
  beforeName= "(^|[^a-zA-Z0-9_])"
  afterName= "([^a-zA-Z0-9_]|$)"

################################################################################

  # Replace aux. expressions in other aux. expressions in a forward manner,
  #  i.e. assuming that an aux. expression is defined BEFORE it is referenced.
  for (i in 1:length(AUXX)) {
    if (i < length(AUXX)) {
      patt= paste0(beforeName,names(AUXX)[i],afterName)
      repl= paste0("\\1","(",AUXX[i],")","\\2")
      AUXX[(i+1):length(AUXX)]= gsub(pattern=patt, replacement=repl,
        x=AUXX[(i+1):length(AUXX)])
    }
  }
  # Check whether the above assumption was actually fulfilled.
  for (i in 1:length(AUXX)) {
    patt= paste0(beforeName,names(AUXX)[i],afterName)
    if (any(grepl(pattern=patt, x=AUXX))) {
      stop(paste0("auxiliary expression '",names(AUXX)[i],
        "' is referenced before it is defined"))
    }
  }

  # Substitute aux. expression names by their values
  for (i in 1:length(AUXX)) {
    patt= paste0(beforeName,names(AUXX)[i],afterName)
    repl= paste0("\\1","(",AUXX[i],")","\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }

################################################################################

# NOTE: This is de-activated. This implementation does not properly deal with
#       numbers in scientific format like, e.g. "1.e-06". This is not trivial.
#       As a workaround, the input is expected not to contain numeric constants.
#       Named constants must be used instead.
#  # In case of Fortran code: Convert numerical constants to double precision
#  if (lang == "f") {
#    patt= "(^|[-+*/^(, ])([0123456789.]+)($|[-+*/^), ])"
#    repl= "\\1dble(\\2)\\3"
#    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
#    for (n in 1:ncol(STOX)) {
#      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
#    }
#  }

################################################################################

  # Turn names of variables into references to vector elements
  #
  # Notes:
  # - The original indices of variables refer to 0-dimensional case.
  #
  # - In a spatially distributed model with k variables and n spatial levels,
  #   the array holding the variables is organized as follows:
  #   { var[1,1], var[1,2], ... var[1,n],
  #     var[2,1]  var[2,2], ... var[2,n],
  #     ... ,
  #     var[k,1]  var[k,2], ... var[k,n] }
  #
  # - Consequently, the position of the i-th variable at the p-th spatial level
  #   is computed as "(i-1)*n+p"

  for (i in 1:length(vars)) {
    patt= paste0(beforeName,names(vars)[i],afterName)
    repl= paste0("\\1",nameVecVars,L$eleOpen,"(",
      names(vars)[i],"-1)*",nameLenLevels,"+",nameSpatialLevelIndex,L$eleClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }

  # Turn names of parameters into references to vector elements
  # Note: Parameters don't have a spatial resolution (as opposed to state vars)
  for (i in 1:length(pars)) {
    patt= paste0(beforeName,names(pars)[i],afterName)
    repl= paste0("\\1",nameVecPars,L$eleOpen,
      names(pars)[i],L$eleClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }  

  # In a spatially distributed model, the righthand side of each derivative
  # should contain a reference to a (spatially resolved) state variable.
  #
  # Reason: In a spatially distributed model (nLevels > 1), the derivative of a
  #   particular state variable must, on evaluation, expand to vectors of
  #   length nLevels (for the sake of computationally efficient R code).
  #   This expansion is (implicitly) caused by the appearance of a state variable
  #   on the righthand side.
  #
  # Note:  It is reasonable to assume that variables are referenced in the
  #   process rate expressions rather than in the stoichiometry factors.
  #
  # Hint: In cases where a process rate expression is a constant, one can make
  #   the expression compliant with the above-mentioned requirement by adding a
  #   term like '+ X * ZERO' where 'X' is an existing state variable and ZERO is
  #   a constant initialized to zero. 
  if (nLevels > 1) {
    patt=paste0(nameVecVars,L$eleOpen)
    if (!all(grepl(pattern=patt, x=PROC, fixed=TRUE))) {
      stop("in a spatially distributed model, a reference to a state variable",
        " must appear at the righthand side of any process rate expression")
    }
  }
 
################################################################################

  # Generate constructor code for the derivatives vector
  # Note: The length of the vector equals the number of state variables in the
  #       0-dimensional case, i.e. a spatially lumped model
  code_drvs=""
  code_drvs=paste0(code_drvs,"  ",nameVecDrvs,"0D","= ",L$vecOpen,L$cont,newline)
  for (n in 1:ncol(STOX)) {
    if (n > 1) {
      code_drvs=paste0(code_drvs,"    ,",L$cont,newline)
    }
    code_drvs=paste0(code_drvs,"    ",L$com," Variable '",names(STOX)[n],"'",newline)
    # Assemble expressions
    buffer=""
    for (k in 1:length(PROC)) {
      # Suppress terms where a stoichiometry factor is exactly zero (usually because
      # it has not been set explicitly)
      if (grepl(pattern="[^0]", x=STOX[k,n])) {
        if (nchar(buffer) > 0) {
          buffer= paste0(buffer," + ")
        }
        buffer=paste0(buffer," (",PROC[k],") * (",STOX[k,n],")")
      }
    }
    # Treat case where all stoichiometry factors are zero. Note: We cannot simply
    # set the derivative to a constant of zero because, in a distributed model,
    # the value of the derivative would not automatically expand to a vector of
    # length nLevels.
    if (nchar(buffer) == 0) {
      stop(paste0("expecting at least one non-zero stoichiometry factor",
        " for state variable '",names(STOX)[n],"'"))
    }
    # Break fortran lines
    if (lang == "f") { buffer= breakline(text=buffer, conti=L$cont, newline=newline) }
    # Add to code
    code_drvs= paste0(code_drvs,buffer,L$cont,newline)
  }
  code_drvs=paste0(code_drvs,"  ",L$vecClose,newline) # End of derivatives vector

  # Generate constructor code for the processes vector
  # Note: The length of the vector equals the number of processes in the
  #       0-dimensional case, i.e. a spatially lumped model
  code_proc=""
  code_proc=paste0(code_proc,"  ",nameVecProc,"0D","=",L$vecOpen,L$cont,newline)
  for (n in 1:length(PROC)) {
    if (n > 1) {
      code_proc=paste0(code_proc,"    ,",L$cont,newline)
    }
    code_proc=paste0(code_proc,"    ",L$com," Process rate '",names(proc)[n],"'",newline)
    buffer= PROC[n]
    # Break fortran lines
    if (lang == "f") { buffer= breakline(text=buffer, conti=L$cont, newline=newline) }
    # Add to code
    code_proc= paste0(code_proc,"      ",buffer,L$cont,newline)
  }
  code_proc=paste0(code_proc,"  ",L$vecClose,newline)

################################################################################

  # Embed the vector constructor codes in appropriate language-specific functions
  return(create_code(name, nameVecDrvs, nameVecProc, nameVecVars, nameVecPars,
      nameSpatialLevelIndex, names(vars), names(pars),
      code_drvs, code_proc, 
      nameLenVars, nameLenPars, nameLenProc, nProc=length(proc),
      nameLenLevels, nLevels,
      importFuns=(length(funs)>0), newline, lang))

})

