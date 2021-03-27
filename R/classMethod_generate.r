#' Code Generator
#'
#' This is a low-level method to translate the ODE-model specification into a
#' function that computes process rates and the state variables derivatives
#' (either in R or Fortran). You probably want to use the high-level method
#' \code{\link{compile}} instead, which uses \code{generate} internally.
#'
#' @name generate
#'
#' @param lang Character string to select the language of the generated source
#'   code. Currently either 'f95' (for Fortran) or 'r' (for R).
#' @param name Name for the generated function (character string). It should
#'   start with a letter, optionally followed by letters, numbers, or
#'   underscores.
#'
#' @return The generated source code as a string. Must be written to
#'   disk, e.g. using \code{\link[base]{write}}, prior to compilation.
#'
#' @note Details of this low-level method may change in the future.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso See other methods of the \code{\link{rodeo-class}}, especially
#'   \code{\link{compile}} which internally uses this method.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' fortranCode <- model$generate(lang="f95")
#' \dontrun{
#' write(fortranCode, file="")
#' }

rodeo$set("public", "generate", function(lang, name="derivs") {
  newline="\n"

  # Set language-specific code elements
  L <- codeElem(lang)

  # Check user-defined functions
  funcnames <- private$funsTbl$name
  # name conflicts with variables used in generated code
  conflicts <- funcnames %in% rodeoConst$genIdent
  if (any(conflicts))
    stop(paste0("identifier name(s) in generated code conflict(s) with name(s)",
      " of user-defined item(s); conflicting names(s): '",
      paste(funcnames[which(conflicts)], collapse="', '"),"'"))

  # Define array indices for all items --> these refer to the 0D case
  indexVars <- setNames(1:nrow(private$varsTbl), private$varsTbl$name)
  indexPars <- setNames(1:nrow(private$parsTbl), private$parsTbl$name)
  indexPros <- setNames(1:nrow(private$prosTbl), private$prosTbl$name)

  # Define substitutes for identifiers
  substVars <- setNames(paste0(rodeoConst$genIdent$vec["vars"],L$eleOpen,
    rodeoConst$genIdent$ilist["vars"],L$listElem,names(indexVars),
    L$eleClose), names(indexVars))
  substPars <- setNames(paste0(rodeoConst$genIdent$vec["pars"],L$eleOpen,
    rodeoConst$genIdent$ilist["pars"],L$listElem,names(indexPars),
    L$eleClose), names(indexPars))
  substPros <- setNames(paste0(rodeoConst$genIdent$vec["pros"],L$eleOpen,
    rodeoConst$genIdent$ilist["pros"],L$listElem,names(indexPros),
    L$eleClose), names(indexPros))
  substFuns <- setNames(private$funsTbl$name, private$funsTbl$name) # means no substitution

  # Make constructor code for the vector of process rates
  code_pros <-""
  code_pros <- paste0(code_pros,rodeoConst$genIdent$vec["pros"],"0D","=",L$vecOpen,L$cont,newline)
  for (n in 1:nrow(private$prosTbl)) {
    if (n > 1) code_pros=paste0(code_pros,"    ,",L$cont,newline)
    code_pros <- paste0(code_pros,"      ",L$com," Process rate '",private$prosTbl$name[n],"'",newline)
    buffer <- private$prosTbl$expression[n]
    # Substitute original identifiers by references to vector elements
    tryCatch({
      buffer <- substituteIdentifiers(expr=buffer, sub=c(substVars,substPars,substFuns,
        setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)), all=TRUE)
    }, error= function(e) {
      stop(paste0("substitution of identifiers in expression for process rate '",
        private$prosTbl$name[n],"' failed; details: ",e))
    })
    if (lang == rodeoConst$lang["fortran"]) {
      buffer <- fortran.doubleConst(buffer)
      buffer <- fortran.powerOperator(buffer)
      buffer <- fortran.breakLine(text=buffer, conti=L$cont, newline=newline)
    }
    code_pros <- paste0(code_pros,"      ",buffer,L$cont,newline)   # Add to code
  }
  code_pros <- paste0(code_pros,"    ",L$vecClose)

  # Make constructor code for the vector of derivatives
  code_drvs <- ""
  code_drvs <- paste0(code_drvs,rodeoConst$genIdent$vec["drvs"],"0D","= ",L$vecOpen,L$cont,newline)
  STOX <- self$stoichiometry()
  for (n in 1:ncol(STOX)) {
    if (n > 1) {
      code_drvs <- paste0(code_drvs,"    ,",L$cont,newline)
    }
    code_drvs <- paste0(code_drvs,"      ",L$com," Variable '",colnames(STOX)[n],"'",newline)
    # Assemble expressions
    buffer <- ""
    for (k in 1:nrow(private$prosTbl)) {
      # Skip terms where stoichiometry factor is exactly zero (e.g. because not set)
      if (grepl(pattern="[^0]", x=STOX[k,n])) {
        if (nchar(buffer) > 0) {
          buffer <- paste0(buffer," + ")
        }
        tryCatch({
          buffer <- paste0(buffer," ",
            # Process rate (reference to already computed value stored in vector)
            rodeoConst$genIdent$vec["pros"],L$eleOpen,
            rodeoConst$genIdent$ilist["pros"],L$listElem,names(indexPros)[k],
            L$eleClose,
            # Stoichiometry factor (expression with substitutes for original identifiers)
            " * (",
            substituteIdentifiers(expr=STOX[k,n], sub=c(substVars,substPars,substFuns,
              setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE),
            ")")
        }, error= function(e) {
          stop(paste0("substitution of identifiers in expression failed for",
            " stoichiometry factor of process '",private$prosTbl$name[k],
            "', variable '",colnames(STOX)[n],"'; details: ",e))
        })
      }
    }
    # Treat case where all stoichiometry factors are zero.
    if (nchar(buffer) == 0)
      buffer <- "0"
    # Specialities of Fortran
    if (lang == rodeoConst$lang["fortran"]) {
      buffer <- fortran.doubleConst(buffer)
      buffer <- fortran.powerOperator(buffer)
      buffer <- fortran.breakLine(text=buffer, conti=L$cont, newline=newline)
    }
    # Add to code
    code_drvs <- paste0(code_drvs,"      ",buffer,L$cont,newline)
  } # End of loop over columns of stoichiometry matrix
  code_drvs <- paste0(code_drvs,"    ",L$vecClose) # End of derivatives vector

  ##############################################################################
  # Generate interface in Fortran
  ##############################################################################
  if (lang == rodeoConst$lang["fortran"]) {
    code <- paste0("! GENERATED CODE -- YOU PROBABLY DO NOT WANT TO EDIT THIS",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"module dimensions_and_indices",newline)
    code <- paste0(code,"  implicit none",newline)
    code <- paste0(code,"  integer, private:: i",newline)
    code <- paste0(code,"  ! Dimension constants",newline)
    code <- paste0(code,"  integer, parameter:: ",rodeoConst$genIdent$len["vars"],
      "=",nrow(private$varsTbl),newline)
    code <- paste0(code,"  integer, parameter:: ",rodeoConst$genIdent$len["pars"],
      "=",nrow(private$parsTbl),newline)
    code <- paste0(code,"  integer, parameter:: ",rodeoConst$genIdent$len["pros"],
      "=",nrow(private$prosTbl),newline)
    code <- paste0(code,"  ! Constant index arrays (for OD case or 1st box, respectively)",newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["vars"],
      "), target:: ",rodeoConst$genIdent$ivec0D["vars"],
      " =(/(i, i=1, ",rodeoConst$genIdent$len["vars"],")/)",newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["pars"],
      "), target:: ",rodeoConst$genIdent$ivec0D["pars"],
      " =(/(i, i=1, ",rodeoConst$genIdent$len["pars"],")/)",newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["pros"],
      "), parameter:: ",rodeoConst$genIdent$ivec0D["pros"],
      " =(/(i, i=1, ",rodeoConst$genIdent$len["pros"],")/)",newline)
    code <- paste0(code,"  ! Modifyable index arrays (to be adjusted for each spatial box)",newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["vars"],
      "), target:: ",rodeoConst$genIdent$ivec["vars"],newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["pars"],
      "), target:: ",rodeoConst$genIdent$ivec["pars"],newline)
    code <- paste0(code,"  integer, dimension(",rodeoConst$genIdent$len["pros"],
      "), target:: ",rodeoConst$genIdent$ivec["pros"],newline)
    code <- paste0(code,newline)

    code <- paste0(code,"  ! Lists of pointers to index arrays for first box (0D case)",newline)
    code <- paste0(code,"  ! Note: Only used in conjunction with left() or right()",newline)
    code <- paste0(code,"  type t_var0D",newline)
    code <- paste0(code,paste0("    integer, pointer:: ",names(indexVars)," => ",
      rodeoConst$genIdent$ivec0D["vars"],"(",indexVars,")",newline,collapse=""))
    code <- paste0(code,"  end type",newline)
    code <- paste0(code,"  type t_par0D",newline)
    code <- paste0(code,paste0("    integer, pointer:: ",names(indexPars)," => ",
      rodeoConst$genIdent$ivec0D["pars"],"(",indexPars,")",newline,collapse=""))
    code <- paste0(code,"  end type",newline)
    code <- paste0(code,"  ! Instances of the above lists",newline)
    code <- paste0(code,"  type (t_var0D):: ",rodeoConst$genIdent$ilist0D["vars"],newline)
    code <- paste0(code,"  type (t_par0D):: ",rodeoConst$genIdent$ilist0D["pars"],newline)

    code <- paste0(code,"  ! Lists of pointers to index arrays whose values depend on the box",newline)
    code <- paste0(code,"  type t_var",newline)
    code <- paste0(code,paste0("    integer, pointer:: ",names(indexVars)," => ",
      rodeoConst$genIdent$ivec["vars"],"(",indexVars,")",newline,collapse=""))
    code <- paste0(code,"  end type",newline)
    code <- paste0(code,"  type t_par",newline)
    code <- paste0(code,paste0("    integer, pointer:: ",names(indexPars)," => ",
      rodeoConst$genIdent$ivec["pars"],"(",indexPars,")",newline,collapse=""))
    code <- paste0(code,"  end type",newline)
    code <- paste0(code,"  type t_pro",newline)
    code <- paste0(code,paste0("    integer, pointer:: ",names(indexPros)," => ",
      rodeoConst$genIdent$ivec["pros"],"(",indexPros,")",newline,collapse=""))
    code <- paste0(code,"  end type",newline)
    code <- paste0(code,"  ! Instances of the above lists",newline)
    code <- paste0(code,"  type (t_var):: ",rodeoConst$genIdent$ilist["vars"],newline)
    code <- paste0(code,"  type (t_par):: ",rodeoConst$genIdent$ilist["pars"],newline)
    code <- paste0(code,"  type (t_pro):: ",rodeoConst$genIdent$ilist["pros"],newline)

    code <- paste0(code,"end module",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"subroutine ",name,"(",rodeoConst$reservedNames["time"],", ",rodeoConst$genIdent$vec["vars"],
      ", ",rodeoConst$genIdent$vec["pars"],
      ", ",rodeoConst$genIdent$len["boxes"],", ",rodeoConst$genIdent$vec["drvs"],
      ", ",rodeoConst$genIdent$vec["pros"],
      ")",newline)
    code <- paste0(code,"  use dimensions_and_indices",newline)
    code <- paste0(code,"  ",if (nrow(private$funsTbl) > 0) "" else "!","use functions",newline)
    code <- paste0(code,"  implicit none",newline)
    # Arguments of main method
    code <- paste0(code,"  ! Inputs",newline)
    code <- paste0(code,"  double precision, intent(in):: ",rodeoConst$reservedNames["time"],newline)
    code <- paste0(code,"  integer, intent(in):: ",rodeoConst$genIdent$len["boxes"],newline)
    code <- paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent$len["vars"],"*",rodeoConst$genIdent$len["boxes"],
      "), intent(in):: ",rodeoConst$genIdent$vec["vars"],newline)
    code <- paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent$len["pars"],"*",rodeoConst$genIdent$len["boxes"],
      "), intent(in):: ",rodeoConst$genIdent$vec["pars"],newline)
    code <- paste0(code,"  ! Outputs",newline)
    code <- paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent$len["vars"],"*",rodeoConst$genIdent$len["boxes"],
      "), intent(out):: ",rodeoConst$genIdent$vec["drvs"],newline)
    code <- paste0(code,"  double precision, dimension(",
      rodeoConst$genIdent$len["pros"],"*",rodeoConst$genIdent$len["boxes"],
      "), intent(out):: ",rodeoConst$genIdent$vec["pros"],newline)
    # Local variables
    code <- paste0(code,"  ! Local variables",newline)
    code <- paste0(code,"  integer:: ",rodeoConst$genIdent["iBox"],", i",newline)
    # Local constants
    code <- paste0(code,newline)
    # Vectors of process rates and derivatives are set here
    code <- paste0(code,"  ! Set vector of process rates (all spatial boxes)",newline)
    code <- paste0(code,"  do ",rodeoConst$genIdent["iBox"]," = 1, ",rodeoConst$genIdent$len["boxes"],newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$vec["pros"],
      "((/(i, i=",rodeoConst$genIdent["iBox"],", ((",rodeoConst$genIdent$len["pros"],"-1)*",
      rodeoConst$genIdent$len["boxes"],"+",rodeoConst$genIdent["iBox"],"), ",
      rodeoConst$genIdent$len["boxes"],")/))","= ",
      rodeoConst$genIdent$vec["pros"],"0D(",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"  end do",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"  ! Set vector of derivatives (all spatial boxes)",newline)
    code <- paste0(code,"  do ",rodeoConst$genIdent["iBox"]," = 1, ",rodeoConst$genIdent$len["boxes"],newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$vec["drvs"],
      "((/(i, i=",rodeoConst$genIdent["iBox"],", ((",rodeoConst$genIdent$len["vars"],"-1)*",
      rodeoConst$genIdent$len["boxes"],"+",rodeoConst$genIdent["iBox"],"), ",
      rodeoConst$genIdent$len["boxes"],")/))","= ",
      rodeoConst$genIdent$vec["drvs"],"0D(",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"  end do",newline)
    code <- paste0(code,newline)
    # Internal functions
    code <- paste0(code,"  contains  ! Internal functions follow",newline)
    code <- paste0(code,newline)
    # Process rates in a particular box
    code <- paste0(code,"  ! Process rates in a particular box",newline)
    code <- paste0(code,"  function ",rodeoConst$genIdent$vec["pros"],
      "0D(",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    implicit none",newline)
    code <- paste0(code,"    ! Inputs",newline)
    code <- paste0(code,"    integer, intent(in):: ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ! Outputs",newline)
    code <- paste0(code,"    double precision, dimension(",
      rodeoConst$genIdent$len["pros"],"):: ",rodeoConst$genIdent$vec["pros"],"0D",newline)
    code <- paste0(code,"    ! Update indices",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ivec["vars"],"= (",
      rodeoConst$genIdent$ivec0D["vars"]," - 1) * ",rodeoConst$genIdent$len["boxes"],
      " + ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ivec["pars"],"= (",
      rodeoConst$genIdent$ivec0D["pars"]," - 1) * ",rodeoConst$genIdent$len["boxes"],
      " + ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ! Set return vector",newline)
    code <- paste0(code,"    ",code_pros,newline)
    code <- paste0(code,"  end function",newline)
    code <- paste0(code,newline)
    # Derivatives in a particular box
    code <- paste0(code,"  ! Derivatives in a particular box",newline)
    code <- paste0(code,"  function ",rodeoConst$genIdent$vec["drvs"],
      "0D(",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    implicit none",newline)
    code <- paste0(code,"    ! Inputs",newline)
    code <- paste0(code,"    integer, intent(in):: ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ! Outputs",newline)
    code <- paste0(code,"    double precision, dimension(",
      rodeoConst$genIdent$len["vars"],"):: ",rodeoConst$genIdent$vec["drvs"],"0D",newline)
    code <- paste0(code,"    ! Update indices",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ivec["vars"],"= (",
      rodeoConst$genIdent$ivec0D["vars"]," - 1) * ",rodeoConst$genIdent$len["boxes"],
      " + ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ivec["pars"],"= (",
      rodeoConst$genIdent$ivec0D["pars"]," - 1) * ",rodeoConst$genIdent$len["boxes"],
      " + ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ivec["pros"],"= (",
      rodeoConst$genIdent$ivec0D["pros"]," - 1) * ",rodeoConst$genIdent$len["boxes"],
      " + ",rodeoConst$genIdent["iBox"],newline)
    code <- paste0(code,"    ! Set return vector",newline)
    code <- paste0(code,"    ",code_drvs,newline)
    code <- paste0(code,"  end function",newline)
    code <- paste0(code,"end subroutine",newline)

  ##############################################################################
  # Generate interface in R
  ##############################################################################
  } else if (lang == rodeoConst$lang["r"]) {
    code <- paste0("# GENERATED CODE -- YOU PROBABLY DO NOT WANT TO EDIT THIS",newline)
    code <- paste0(code,newline)
    code <- paste0(code,name," = function (",rodeoConst$reservedNames["time"],", ",rodeoConst$genIdent$vec["vars"],
      ", ",rodeoConst$genIdent$vec["pars"],", check=TRUE) {",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"  # Dimension constants",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$len["boxes"],"=",prod(private$dim),newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$len["vars"],"=",nrow(private$varsTbl),newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$len["pars"],"=",nrow(private$parsTbl),newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$len["pros"],"=",nrow(private$prosTbl),newline)
    code <- paste0(code,newline)
    code <- paste0(code,"  # Check length of arguments",newline)
    code <- paste0(code,"  if (check) {",newline)
    code <- paste0(code,"    if (length(",rodeoConst$genIdent$vec["vars"],
      ") != (",rodeoConst$genIdent$len["vars"]," * ",rodeoConst$genIdent$len["boxes"],"))",newline)
    code <- paste0(code,"      stop(paste0(\"length of argument '",rodeoConst$genIdent$vec["vars"],
      "' is \",length(",rodeoConst$genIdent$vec["vars"],"),",newline,
      "        \" but it should be \",",rodeoConst$genIdent$len["vars"]," * ",
      rodeoConst$genIdent$len["boxes"],",",newline,
      "        \" (number of variables * number of boxes)\"))",newline)
    code <- paste0(code,"    if (length(",rodeoConst$genIdent$vec["pars"],
      ") != (",rodeoConst$genIdent$len["pars"]," * ",rodeoConst$genIdent$len["boxes"],"))",newline)
    code <- paste0(code,"      stop(paste0(\"length of argument '",rodeoConst$genIdent$vec["pars"],
      "' is \",length(",rodeoConst$genIdent$vec["pars"],"),",newline,
      "        \" but it should be \",",rodeoConst$genIdent$len["pars"]," * ",
      rodeoConst$genIdent$len["boxes"],",",newline,
      "        \" (number of parameters * number of boxes)\"))",newline)
    code <- paste0(code,"  }",newline)
    code <- paste0(code,"  # Lists of array indices",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$ilist0D["vars"]," = list(",
      "    ",paste0(names(indexVars),"=",indexVars,collapse=", "),"  )",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$ilist0D["pars"]," = list(",
      "    ",paste0(names(indexPars),"=",indexPars,collapse=", "),"  )",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$ilist0D["pros"]," = list(",
      "    ",paste0(names(indexPros),"=",indexPros,collapse=", "),"  )",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"  # Function to update indices for particular box(es)",newline)
    code <- paste0(code,"  adjIdx= function (x, ",rodeoConst$genIdent$len["boxes"],
      ", ",rodeoConst$genIdent["iBox"],") { (x - 1) * ",
      rodeoConst$genIdent$len["boxes"]," + ",rodeoConst$genIdent["iBox"]," }",newline)
    code <- paste0(code,newline)
    code <- paste0(code,"  # Internal function: Process rates in a particular box",newline)
    code <- paste0(code,"  fun_",rodeoConst$genIdent$vec["pros"],"0D = function (",
      rodeoConst$genIdent["iBox"],") {",newline)
    code <- paste0(code,"    # Update indices",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ilist["vars"]," = lapply(",
      rodeoConst$genIdent$ilist0D["vars"],", adjIdx ,",
      rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],", ",
      rodeoConst$genIdent["iBox"],"=",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ilist["pars"]," = lapply(",
      rodeoConst$genIdent$ilist0D["pars"],", adjIdx ,",
      rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],", ",
      rodeoConst$genIdent["iBox"],"=",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    # Set return vector",newline)
    code <- paste0(code,"    ",code_pros,newline)
    code <- paste0(code,"  }",newline)
    code <- paste0(code,newline)

    code <- paste0(code,"  # Set vector of process rates (all spatial boxes)",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$vec["pros"]," = as.vector(t(vapply(",
      "X= 1:",rodeoConst$genIdent$len["boxes"],", ",
      "FUN= fun_",rodeoConst$genIdent$vec["pros"],"0D, ",newline,
      "    FUN.VALUE= numeric(",rodeoConst$genIdent$len["pros"],"), USE.NAMES=FALSE)))", newline)
#    NOTE: The following vectorized alternative does not work in general, because
#          the result does not necessarily expand to a vector of the proper length.
#          For example, it wouldn't work if the righthandside of an ODE doesn't
#          contain a reference to a spatially resolved variable of parameter
#          (e.g. due to a zero process rate or zero-only stoichiometry factors).
#          Moreover, this code would require the user to write vector-compliant
#          code, e.g. using 'pmin/pmax' instead if 'min/max' or 'ifelse' instead
#          of just 'if'. This is dificult for the normal user and we can hardly
#          check the interior of user-supplied functions automatically. It would
#          also destroy the concept of a generic model code if R-specific
#          constructs like pmin/pmax/ifelse need to be used.
#    # DONT USE
#    code <- paste0(code,"  ",rodeoConst$genIdent$vec["pros"]," = unname(fun_",
#      rodeoConst$genIdent$vec["pros"],"0D(1:",rodeoConst$genIdent$len["boxes"],"))",newline)
#    code <- paste0(code,newline)
#    # END DONT USE
    code <- paste0(code,newline)

    code <- paste0(code,"  # Internal function: Derivatives in a particular box",newline)
    code <- paste0(code,"  fun_",rodeoConst$genIdent$vec["drvs"],"0D = function (",
      rodeoConst$genIdent["iBox"],") {",newline)
    code <- paste0(code,"    # Update indices",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ilist["vars"]," = lapply(",
      rodeoConst$genIdent$ilist0D["vars"],", adjIdx ,",
      rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],", ",
      rodeoConst$genIdent["iBox"],"=",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ilist["pars"]," = lapply(",
      rodeoConst$genIdent$ilist0D["pars"],", adjIdx ,",
      rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],", ",
      rodeoConst$genIdent["iBox"],"=",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    ",rodeoConst$genIdent$ilist["pros"]," = lapply(",
      rodeoConst$genIdent$ilist0D["pros"],", adjIdx ,",
      rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],", ",
      rodeoConst$genIdent["iBox"],"=",rodeoConst$genIdent["iBox"],")",newline)
    code <- paste0(code,"    # Set return vector",newline)
    code <- paste0(code,"    ",code_drvs,newline)
    code <- paste0(code,"  }",newline)
    code <- paste0(code,newline)

    code <- paste0(code,"  # Set vector of derivatives (all spatial boxes)",newline)
    code <- paste0(code,"  ",rodeoConst$genIdent$vec["drvs"]," = as.vector(t(vapply(",
      "X= 1:",rodeoConst$genIdent$len["boxes"],", ",
      "FUN= fun_",rodeoConst$genIdent$vec["drvs"],"0D, ",newline,
      "    FUN.VALUE= numeric(",rodeoConst$genIdent$len["vars"],"), USE.NAMES=FALSE)))", newline)
    code <- paste0(code,newline)
#    NOTE: The following vectorized alternative does not work in general (see
#          comments above for details).
#    # DONT USE
#    code <- paste0(code,"  ",rodeoConst$genIdent$vec["drvs"]," = unname(fun_",
#      rodeoConst$genIdent$vec["drvs"],"0D(1:",rodeoConst$genIdent$len["boxes"],"))",newline)
#    code <- paste0(code,newline)
#    # END DONT USE

    code <- paste0(code,"  # Return a list",newline)
    code <- paste0(code,"  return(list(",
      rodeoConst$genIdent$vec["drvs"],"=",rodeoConst$genIdent$vec["drvs"],",",
      rodeoConst$genIdent$vec["pros"],"=",rodeoConst$genIdent$vec["pros"],"))",newline)
    code <- paste0(code,"}",newline)

  } else {
    stop(paste0("target language '",lang,"' not supported; must be one of: '",
      paste(rodeoConst$lang, collapse="', '"),"'"))
  }

  # Post-process generated code to handle references to neighboring elements
  for (item in c("vars", "pars")) {
    for (fun in rodeoConst$reservedNames[c("left", "right")]) {
      pat <- paste0(fun,"[(]",rodeoConst$genIdent$vec[item],"[",codeElem(lang)$eleOpen,"]",
        rodeoConst$genIdent$ilist[item],"[",codeElem(lang)$listElem,"]",
        "([^",codeElem(lang)$eleClose,"]+)","[",codeElem(lang)$eleClose,"][)]")
      if (fun == rodeoConst$reservedNames["left"]) {
        leftmost <- paste0("(",rodeoConst$genIdent$ilist0D[item],codeElem(lang)$listElem,
          "\\1-1)*",rodeoConst$genIdent$len["boxes"],"+1")
        subst <- paste0(rodeoConst$genIdent$vec[item],codeElem(lang)$eleOpen,
          codeElem(lang)$max,"(",leftmost,",",
          rodeoConst$genIdent$ilist[item],codeElem(lang)$listElem,"\\1-1)", codeElem(lang)$eleClose)
      } else if (fun == rodeoConst$reservedNames["right"]) {
        rightmost <- paste0(rodeoConst$genIdent$ilist0D[item],codeElem(lang)$listElem,
          "\\1*",rodeoConst$genIdent$len["boxes"])
        subst <- paste0(rodeoConst$genIdent$vec[item],codeElem(lang)$eleOpen,
          codeElem(lang)$min,"(",rightmost,",",
          rodeoConst$genIdent$ilist[item],codeElem(lang)$listElem,"\\1+1)", codeElem(lang)$eleClose)
      }
      if (lang == rodeoConst$lang["fortran"]) {
        subst <- paste0("(",L$cont,newline,subst,")")
      }
      code <- gsub(pattern=pat, replacement=subst, x=code, fixed=FALSE)
    }
  }

  return(code)

})

