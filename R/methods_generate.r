rodeo$methods( generate = function(lang, name="derivs") {
  "Generates code to compute the variables' derivatives with respect
  to time. \\bold{Arguments:} \\code{name}: A string giving the name for the
  generated function/routine. \\bold{Returns:} The generated code as a string."

  newline="\n"

  # Set language-specific code elements
  L= codeElem(lang)

  # Check names of identifiers used in generated code for conflicts with
  # user-defined names
  names2check= c(.self$FUNS$name)
  conflicts= names2check %in% rodeoConst$genIdent
  if (any(conflicts))
    stop(paste0("identifier name(s) in generated code conflict(s) with name(s)",
      " of user-defined item(s); conflicting names(s): '",
      paste(names2check[which(conflicts)], collapse="', '"),"'"))

  # Define array indices for all items --> these refer to the 0D case
  indexVars= setNames(1:nrow(.self$VARS), .self$VARS$name)
  indexPars= setNames(1:nrow(.self$PARS), .self$PARS$name)
  indexPros= setNames(1:nrow(.self$PROS), .self$PROS$name)

  # Define substitutes for identifiers
  substVars= setNames(paste0(rodeoConst$genIdent["vecVars"],L$eleOpen,
    rodeoConst$genIdent["ilistVars"],L$listElem,names(indexVars),
    L$eleClose), names(indexVars))
  substPars= setNames(paste0(rodeoConst$genIdent["vecPars"],L$eleOpen,
    rodeoConst$genIdent["ilistPars"],L$listElem,names(indexPars),
    L$eleClose), names(indexPars))
  substPros= setNames(paste0(rodeoConst$genIdent["vecPros"],L$eleOpen,
    rodeoConst$genIdent["ilistPros"],L$listElem,names(indexPros),
    L$eleClose), names(indexPros))
  substFuns= setNames(.self$FUNS$name, .self$FUNS$name) # means no substitution

  # Make constructor code for the vector of process rates
  code_pros=""
  code_pros=paste0(code_pros,rodeoConst$genIdent["vecPros"],"0D","=",L$vecOpen,L$cont,newline)
  for (n in 1:nrow(.self$PROS)) {
    if (n > 1) code_pros=paste0(code_pros,"    ,",L$cont,newline)
    code_pros=paste0(code_pros,"      ",L$com," Process rate '",.self$PROS$name[n],"'",newline)
    buffer= .self$PROS$expression[n]
    # Substitute original identifiers by references to vector elements
    tryCatch({
      buffer= substituteIdentifiers(expr=buffer, sub=c(substVars,substPars,substFuns,
        time="time"), all=TRUE)
    }, error= function(e) {
      stop(paste0("substitution of identifiers in expression for process rate '",
        .self$PROS$name[n],"' failed; details: ",e))
    })
    # In a spatially distributed model, the righthand side of each derivative
    # should contain a reference to a spatially resolved variable or parameter.
    # Reason: In a spatially distributed model (nLevels > 1), the derivative of a
    #   particular state variable must, on evaluation, expand to vectors of
    #   length nLevels (for the sake of computationally efficient R code).
    #   This expansion is (implicitly) caused by the appearance of a spatially
    #   resolved variable of parameter on the righthand side.
    # Note:  It is reasonable to assume that variables/parameters are referenced in the
    #   process rate expressions rather than in the stoichiometry factors.
    # Hint: One can always use a dummy variable/parameter and multiply by zero. 
    if (!(grepl(pattern=paste0(rodeoConst$genIdent["vecVars"],L$eleOpen), x=buffer, fixed=TRUE) || 
         grepl(pattern=paste0(rodeoConst$genIdent["vecPars"],L$eleOpen), x=buffer, fixed=TRUE)))
      stop(paste0("expression for process rate '",.self$PROS$name[n],
        "' does not contain a reference to a state variable or parameter (a dummy reference ",
        "is required at least, e.g. multiplied by a constant of zero)"))
    if (lang == "f95") {
      buffer= fortran.doubleConst(buffer)
      buffer= fortran.breakLine(text=buffer, conti=L$cont, newline=newline)
    }
    code_pros= paste0(code_pros,"      ",buffer,L$cont,newline)   # Add to code
  }
  code_pros=paste0(code_pros,"    ",L$vecClose)


  # Make constructor code for the vector of derivatives
  code_drvs=""
  code_drvs=paste0(code_drvs,rodeoConst$genIdent["vecDrvs"],"0D","= ",L$vecOpen,L$cont,newline)
  STOX= .self$stoichiometryMatrix()
  for (n in 1:ncol(STOX)) {
    if (n > 1) {
      code_drvs=paste0(code_drvs,"    ,",L$cont,newline)
    }
    code_drvs=paste0(code_drvs,"      ",L$com," Variable '",colnames(STOX)[n],"'",newline)
    # Assemble expressions
    buffer=""
    for (k in 1:nrow(.self$PROS)) {
      # Skip terms where stoichiometry factor is exactly zero (e.g. because not set)
      if (grepl(pattern="[^0]", x=STOX[k,n])) {
        if (nchar(buffer) > 0) {
          buffer= paste0(buffer," + ")
        }
        tryCatch({
          buffer=paste0(buffer," ",
            # Process rate (reference to already computed value stored in vector)
            rodeoConst$genIdent["vecPros"],L$eleOpen,
            rodeoConst$genIdent["ilistPros"],L$listElem,names(indexPros)[k],
            L$eleClose,
            # Stoichiometry factor (expression with subtitutes for original identifiers)
            " * (",
            substituteIdentifiers(expr=STOX[k,n], sub=c(substVars,substPars,substFuns,
              time="time"),all=TRUE),
            ")")
        }, error= function(e) {
          stop(paste0("substitution of identifiers in expression failed for",
            " stoichiometry factor of process '",.self$PROS$name[k],
            "', variable '",colnames(STOX)[n],"'; details: ",e))
        })
      }
    }
    # Treat case where all stoichiometry factors are zero. Note: We cannot simply
    # set the derivative to a constant of zero because, in a spatially distributed model,
    # the value of the derivative would not auto-expand to a vector of length nLevels.
    if (nchar(buffer) == 0) {
      stop(paste0("expecting at least one non-zero stoichiometry factor",
        " for state variable '",colnames(STOX)[n],"'"))
    }
    # Specialities of Fortran
    if (lang == "f95") {
      buffer= fortran.doubleConst(buffer)
      buffer= fortran.breakLine(text=buffer, conti=L$cont, newline=newline)
    }
    # Add to code
    code_drvs= paste0(code_drvs,"      ",buffer,L$cont,newline)
  } # End of loop over columns of stoichiometry matrix
  code_drvs=paste0(code_drvs,"    ",L$vecClose) # End of derivatives vector

  ##############################################################################
  # Generate interface in Fortran
  ##############################################################################
  if (lang == "f95") {
    code=paste0("!#################################################",newline)
    code=paste0(code,"!###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###",newline)
    code=paste0(code,"!#################################################",newline)
    code=paste0(code,newline)
    code=paste0(code,"module dimensions_and_indices",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  integer, private:: i",newline)
    code=paste0(code,"  ! Dimension constants",newline)
    code=paste0(code,"  integer, parameter:: ",rodeoConst$genIdent["lenVars"],
      "=",nrow(.self$VARS),newline)
    code=paste0(code,"  integer, parameter:: ",rodeoConst$genIdent["lenPars"],
      "=",nrow(.self$PARS),newline)
    code=paste0(code,"  integer, parameter:: ",rodeoConst$genIdent["lenPros"],
      "=",nrow(.self$PROS),newline)
    code=paste0(code,"  ! Constant index arrays (for OD case or 1st level, respectively)",newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenVars"],
      "), parameter:: ",rodeoConst$genIdent["ivecVars0D"],
      " =(/(i, i=1, ",length(indexVars),")/)",newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenPars"],
      "), parameter:: ",rodeoConst$genIdent["ivecPars0D"],
      " =(/(i, i=1, ",length(indexPars),")/)",newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenPros"],
      "), parameter:: ",rodeoConst$genIdent["ivecPros0D"],
      " =(/(i, i=1, ",length(indexPros),")/)",newline)
    code=paste0(code,"  ! Modifyable index arrays (to be adjusted for each spatial level)",newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenVars"],
      "), target:: ",rodeoConst$genIdent["ivecVars"],newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenPars"],
      "), target:: ",rodeoConst$genIdent["ivecPars"],newline)
    code=paste0(code,"  integer, dimension(",rodeoConst$genIdent["lenPros"],
      "), target:: ",rodeoConst$genIdent["ivecPros"],newline)
    code=paste0(code,newline)
    code=paste0(code,"  ! Lists of pointers to index arrays",newline)
    code=paste0(code,"  type t_var",newline)
    code=paste0(code,paste0("    integer, pointer:: ",names(indexVars)," => ",
      rodeoConst$genIdent["ivecVars"],"(",indexVars,")",newline,collapse=""))
    code=paste0(code,"  end type",newline)
    code=paste0(code,"  type t_par",newline)
    code=paste0(code,paste0("    integer, pointer:: ",names(indexPars)," => ",
      rodeoConst$genIdent["ivecPars"],"(",indexPars,")",newline,collapse=""))
    code=paste0(code,"  end type",newline)
    code=paste0(code,"  type t_pro",newline)
    code=paste0(code,paste0("    integer, pointer:: ",names(indexPros)," => ",
      rodeoConst$genIdent["ivecPros"],"(",indexPros,")",newline,collapse=""))
    code=paste0(code,"  end type",newline)
    code=paste0(code,"  ! Instances of the above lists",newline)
    code=paste0(code,"  type (t_var):: ",rodeoConst$genIdent["ilistVars"],newline)
    code=paste0(code,"  type (t_par):: ",rodeoConst$genIdent["ilistPars"],newline)
    code=paste0(code,"  type (t_pro):: ",rodeoConst$genIdent["ilistPros"],newline)
    code=paste0(code,"end module",newline)
    code=paste0(code,newline)
    code=paste0(code,"subroutine ",name,"(time, ",rodeoConst$genIdent["vecVars"],
      ", ",rodeoConst$genIdent["vecPars"],
      ", ",rodeoConst$genIdent["lenLevels"],", ",rodeoConst$genIdent["vecDrvs"],
      ", ",rodeoConst$genIdent["vecPros"],
      ")",newline)
    code=paste0(code,"  use dimensions_and_indices",newline)
    code=paste0(code,"  ",ifelse(nrow(.self$FUNS) > 0,"","!"),"use functions",newline)
    code=paste0(code,"  implicit none",newline)
    # Arguments of main method
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  double precision, intent(in):: time",newline)
    code=paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent["lenVars"],"*",rodeoConst$genIdent["lenLevels"],
      "), intent(in):: ",rodeoConst$genIdent["vecVars"],newline)
    code=paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent["lenPars"],"*",rodeoConst$genIdent["lenLevels"],
      "), intent(in):: ",rodeoConst$genIdent["vecPars"],newline)
    code=paste0(code,"  integer, intent(in):: ",rodeoConst$genIdent["lenLevels"],newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent["lenVars"],"*",rodeoConst$genIdent["lenLevels"],
      "), intent(out):: ",rodeoConst$genIdent["vecDrvs"],newline)
    code=paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent["lenPros"],"*",rodeoConst$genIdent["lenLevels"],
      "), intent(out):: ",rodeoConst$genIdent["vecPros"],newline)
    # Local variables
    code=paste0(code,"  ! Local variables",newline)
    code=paste0(code,"  integer:: ",rodeoConst$genIdent["levelIndex"],", i",newline)
    # Local constants
    code=paste0(code,newline)
    # Vectors of process rates and derivatives are set here
    code=paste0(code,"  ! Set vector of process rates (all spatial levels)",newline)
    code=paste0(code,"  do ",rodeoConst$genIdent["levelIndex"]," = 1, ",rodeoConst$genIdent["lenLevels"],newline)
    code=paste0(code,"    ",rodeoConst$genIdent["vecPros"],
      "((/(i, i=",rodeoConst$genIdent["levelIndex"],", ((",rodeoConst$genIdent["lenPros"],"-1)*",
      rodeoConst$genIdent["lenLevels"],"+",rodeoConst$genIdent["levelIndex"],"), ",
      rodeoConst$genIdent["lenLevels"],")/))","= ",
      rodeoConst$genIdent["vecPros"],"0D(",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,newline)
    code=paste0(code,"  ! Set vector of derivatives (all spatial levels)",newline)
    code=paste0(code,"  do ",rodeoConst$genIdent["levelIndex"]," = 1, ",rodeoConst$genIdent["lenLevels"],newline)
    code=paste0(code,"    ",rodeoConst$genIdent["vecDrvs"],
      "((/(i, i=",rodeoConst$genIdent["levelIndex"],", ((",rodeoConst$genIdent["lenVars"],"-1)*",
      rodeoConst$genIdent["lenLevels"],"+",rodeoConst$genIdent["levelIndex"],"), ",
      rodeoConst$genIdent["lenLevels"],")/))","= ",
      rodeoConst$genIdent["vecDrvs"],"0D(",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,newline)
    # Internal functions
    code=paste0(code,"  contains  ! Internal functions follow",newline)
    code=paste0(code,newline)
    # Process rates at a particular level
    code=paste0(code,"  ! Process rates at a particular level",newline)
    code=paste0(code,"  function ",rodeoConst$genIdent["vecPros"],
      "0D(",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    implicit none",newline)
    code=paste0(code,"    ! Inputs",newline)
    code=paste0(code,"    integer, intent(in):: ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ! Outputs",newline)
    code=paste0(code,"    double precision, dimension(",
      rodeoConst$genIdent["lenPros"],"):: ",rodeoConst$genIdent["vecPros"],"0D",newline)
    code=paste0(code,"    ! Update indices",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ivecVars"],"= (",
      rodeoConst$genIdent["ivecVars0D"]," - 1) * ",rodeoConst$genIdent["lenLevels"],
      " + ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ivecPars"],"= (",
      rodeoConst$genIdent["ivecPars0D"]," - 1) * ",rodeoConst$genIdent["lenLevels"],
      " + ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ! Set return vector",newline)
    code=paste0(code,"    ",code_pros,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,newline)
    # Derivatives at a particular level
    code=paste0(code,"  ! Derivatives at a particular level",newline)
    code=paste0(code,"  function ",rodeoConst$genIdent["vecDrvs"],
      "0D(",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    implicit none",newline)
    code=paste0(code,"    ! Inputs",newline)
    code=paste0(code,"    integer, intent(in):: ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ! Outputs",newline)
    code=paste0(code,"    double precision, dimension(",
      rodeoConst$genIdent["lenVars"],"):: ",rodeoConst$genIdent["vecDrvs"],"0D",newline)
    code=paste0(code,"    ! Update indices",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ivecVars"],"= (",
      rodeoConst$genIdent["ivecVars0D"]," - 1) * ",rodeoConst$genIdent["lenLevels"],
      " + ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ivecPars"],"= (",
      rodeoConst$genIdent["ivecPars0D"]," - 1) * ",rodeoConst$genIdent["lenLevels"],
      " + ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ivecPros"],"= (",
      rodeoConst$genIdent["ivecPros0D"]," - 1) * ",rodeoConst$genIdent["lenLevels"],
      " + ",rodeoConst$genIdent["levelIndex"],newline)
    code=paste0(code,"    ! Set return vector",newline)
    code=paste0(code,"    ",code_drvs,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,"end subroutine",newline)
    return(code)

  ##############################################################################
  # Generate interface in R
  ##############################################################################
  } else if (lang == "r") {
    code=paste0("#################################################",newline)
    code=paste0(code,"###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###",newline)
    code=paste0(code,"#################################################",newline)
    code=paste0(code,newline)
    code=paste0(code,name," = function (time, ",rodeoConst$genIdent["vecVars"],
      ", ",rodeoConst$genIdent["vecPars"],", ",rodeoConst$genIdent["lenLevels"],
      ", check=TRUE) {",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Check length of arguments",newline)
    code=paste0(code,"  if (check) {",newline)
    code=paste0(code,"    if (length(",rodeoConst$genIdent["vecVars"],
      ") != (",length(indexVars)," * ",rodeoConst$genIdent["lenLevels"],"))",newline)
    code=paste0(code,"      stop(paste0(\"length of argument '",rodeoConst$genIdent["vecVars"],
      "' is \",length(",rodeoConst$genIdent["vecVars"],"),",newline,
      "        \" but it should be \",",length(indexVars)," * ",
      rodeoConst$genIdent["lenLevels"],",",newline,
      "        \" (number of variables * number of levels)\"))",newline)
    code=paste0(code,"    if (length(",rodeoConst$genIdent["vecPars"],
      ") != (",length(indexPars)," * ",rodeoConst$genIdent["lenLevels"],"))",newline)
    code=paste0(code,"      stop(paste0(\"length of argument '",rodeoConst$genIdent["vecPars"],
      "' is \",length(",rodeoConst$genIdent["vecPars"],"),",newline,
      "        \" but it should be \",",length(indexPars)," * ",
      rodeoConst$genIdent["lenLevels"],",",newline,
      "        \" (number of parameters * number of levels)\"))",newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,"  # Lists of array indices",newline)
    code=paste0(code,"  ",rodeoConst$genIdent["ilistVars0D"]," = list(",
      "    ",paste0(names(indexVars),"=",indexVars,collapse=", "),"  )",newline)
    code=paste0(code,"  ",rodeoConst$genIdent["ilistPars0D"]," = list(",
      "    ",paste0(names(indexPars),"=",indexPars,collapse=", "),"  )",newline)
    code=paste0(code,"  ",rodeoConst$genIdent["ilistPros0D"]," = list(",
      "    ",paste0(names(indexPros),"=",indexPros,collapse=", "),"  )",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Function to update indices for particular level(s)",newline)
    code=paste0(code,"  adjIdx= function (x, ",rodeoConst$genIdent["lenLevels"],
      ", ",rodeoConst$genIdent["levelIndex"],") { (x - 1) * ",
      rodeoConst$genIdent["lenLevels"]," + ",rodeoConst$genIdent["levelIndex"]," }",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Internal function: Process rates at a particular level",newline)
    code=paste0(code,"  fun_",rodeoConst$genIdent["vecPros"],"0D = function (",
      rodeoConst$genIdent["levelIndex"],") {",newline)
    code=paste0(code,"    # Update indices",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ilistVars"]," = lapply(",
      rodeoConst$genIdent["ilistVars0D"],", adjIdx ,",
      rodeoConst$genIdent["lenLevels"],"=",rodeoConst$genIdent["lenLevels"],", ",
      rodeoConst$genIdent["levelIndex"],"=",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ilistPars"]," = lapply(",
      rodeoConst$genIdent["ilistPars0D"],", adjIdx ,",
      rodeoConst$genIdent["lenLevels"],"=",rodeoConst$genIdent["lenLevels"],", ",
      rodeoConst$genIdent["levelIndex"],"=",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    # Set return vector",newline)
    code=paste0(code,"    ",code_pros,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Set vector of process rates (all spatial levels)",newline)
    code=paste0(code,"  ",rodeoConst$genIdent["vecPros"]," = unname(fun_",
      rodeoConst$genIdent["vecPros"],"0D(1:",rodeoConst$genIdent["lenLevels"],"))",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Internal function: Derivatives at a particular level",newline)
    code=paste0(code,"  fun_",rodeoConst$genIdent["vecDrvs"],"0D = function (",
      rodeoConst$genIdent["levelIndex"],") {",newline)
    code=paste0(code,"    # Update indices",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ilistVars"]," = lapply(",
      rodeoConst$genIdent["ilistVars0D"],", adjIdx ,",
      rodeoConst$genIdent["lenLevels"],"=",rodeoConst$genIdent["lenLevels"],", ",
      rodeoConst$genIdent["levelIndex"],"=",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ilistPars"]," = lapply(",
      rodeoConst$genIdent["ilistPars0D"],", adjIdx ,",
      rodeoConst$genIdent["lenLevels"],"=",rodeoConst$genIdent["lenLevels"],", ",
      rodeoConst$genIdent["levelIndex"],"=",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    ",rodeoConst$genIdent["ilistPros"]," = lapply(",
      rodeoConst$genIdent["ilistPros0D"],", adjIdx ,",
      rodeoConst$genIdent["lenLevels"],"=",rodeoConst$genIdent["lenLevels"],", ",
      rodeoConst$genIdent["levelIndex"],"=",rodeoConst$genIdent["levelIndex"],")",newline)
    code=paste0(code,"    # Set return vector",newline)
    code=paste0(code,"    ",code_drvs,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,newline)
    code=paste0(code,"  # Set vector of derivatives (all spatial levels)",newline)
    code=paste0(code,"  ",rodeoConst$genIdent["vecDrvs"]," = unname(fun_",
      rodeoConst$genIdent["vecDrvs"],"0D(1:",rodeoConst$genIdent["lenLevels"],"))",newline)
    code=paste0(code,newline)

    code=paste0(code,"  # Return a list",newline)
    code=paste0(code,"  return(list(",rodeoConst$genIdent["vecDrvs"],"=",rodeoConst$genIdent["vecDrvs"],",",rodeoConst$genIdent["vecPros"],"=",rodeoConst$genIdent["vecPros"],"))",newline)
    code=paste0(code,"}",newline)
    return(code)

  } else {
    stop(paste0("language '",lang,"' not supported"))
  }
})

