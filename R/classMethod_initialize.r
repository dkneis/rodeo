#' Initialize a \code{rodeo} Object
#'
#' Initializes an object of the \code{\link{rodeo-class}} with data frames
#' holding the specification of an ODE system.
#'
#' @name initialize
#' @aliases new
#'
#' @param vars Declaration of state variables appearing in the ODE system.
#'   Data frame with mandatory columns 'name', 'unit', 'description'.
#' @param pars Declaration of parameters (i.e. constants) appearing in the ODE
#'   system. Data frame with the same mandatory columns as \code{vars}.
#' @param funs Declaration of functions being referenced in the ODE
#'   system. Data frame with the same mandatory columns as \code{vars}.
#' @param pros Declaration of process rates. Data frame with mandatory columns
#'   'name', 'unit', 'description', 'expression'.
#' @param stoi Declaration of stoichiometric factors. A data frame with
#'   mandatory columns 'variable', 'process', 'expression', if \code{asMatrix}
#'   is \code{FALSE}. The 'expression' colum holds the stoichiometric factors.
#'   If \code{asMatrix} is \code{TRUE}, this must be a matrix of type character
#'   with row names (processes) and colum names (variables). Empty or \code{NA}
#'   matrix elements are interpreted as zero stoichiometry factors.
#' @param asMatrix Logical. Specifies whether stoichiometry information is given
#'   in matrix or data base format.
#' @param dim An integer vector, specifying the number of boxes in each spatial
#'   dimension. Use \code{c(1)} to create a zero-dimensional
#'   (i.e. single-box) model. This is the default.
#'   Use, e.g. \code{c(5)} to create a 1-dimensional model with 5 boxes.
#'   To create, e.g., a 2-dimensional model with 4 x 5 boxes, use \code{c(4,5)}.
#'
#' @return The method is called implicitly for its side effects when a
#'   \code{\link{rodeo}} object is instantiated with \code{\link[methods]{new}}.
#'   It has no accessible return value.
#'
#' @note The mandatory fields of the input data frames should be of type
#'   character. Additional fields may be present in these data frames and the
#'   contents becomes part of the \code{\link{rodeo}} object.
#'   The 'expression' fields of \code{pros} and \code{stoi} (or the contents of
#'   the stoichiometry matrix) should be valid mathematical expressions in R and
#'   Fortran. These can involve the names of declared state variables,
#'   parameters, and functions as well as numeric constants or basic math
#'   operators. Branching or loop constructs are not allowed (but these can
#'   appear inside referenced functions).
#'   There are currently few reserved words that cannot be used as variable,
#'   parameter, function, or process names. The reserved words are 'time',
#'   'left', and 'right'.
#'
#' Initialization does not assign numeric values to state variables or
#'   parameters. Use the decicated methods \code{\link{setVars}} and 
#'   \code{\link{setPars}} for that purpose.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso See the package vignette for examples.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' print(model)

rodeo$set("public", "initialize",
  function(vars, pars, funs, pros, stoi, asMatrix=FALSE, dim=c(1)
) {
  # Set variables ##############################################################
  cn <- c("name","unit","description")
  checkTbl(tbl=vars, tblName="vars", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    vars[,n] <- as.character(vars[,n])
  for (n in c("tex","html"))
    vars[,n] <- if (n %in% names(vars)) as.character(vars[,n]) else vars$name
  private$varsTbl <- as.data.frame(vars, stringsAsFactors=FALSE)
  # Set parameters #############################################################
  cn <- c("name","unit","description")
  checkTbl(tbl=pars, tblName="pars", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    pars[,n] <- as.character(pars[,n])
  for (n in c("tex","html"))
    pars[,n] <- if (n %in% names(pars)) as.character(pars[,n]) else pars$name
  private$parsTbl <- as.data.frame(pars, stringsAsFactors=FALSE)
  # Set functions ##############################################################
  cn <- c("name","unit","description")
  checkTbl(tbl=funs, tblName="funs", colNames=cn, nameCol="name", emptyOK=TRUE)
  for (n in cn)
    funs[,n] <- as.character(funs[,n])
  for (n in c("tex","html"))
    funs[,n] <- if (n %in% names(funs)) as.character(funs[,n]) else funs$name
  private$funsTbl <- as.data.frame(funs, stringsAsFactors=FALSE)
  # Set processes ##############################################################
  # Basic checks
  cn <- c("name","unit","description","expression")
  checkTbl(tbl=pros, tblName="pros", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    pros[,n] <- as.character(pros[,n])
  # Remove newline characters from expressions
  pros$expression <- gsub(pattern="\n", replacement=" ", x=pros$expression)
  # Check for undeclared items in expressions
  for (i in 1:nrow(pros)) {
    bad <- undeclared(pros$expression[i], c(vars$name, pars$name, funs$name,
      rodeoConst$reservedNames))
    if (length(bad) > 0)
      stop(paste0("expression for process '",pros$name[i],
        "' contains undeclared item(s) '",paste(bad,collapse="', '"),"'"))
  }
  # Check for invalid expressions
  for (i in 1:nrow(pros)) {
    tryCatch({
      parse(text=pros$expression[i])
    }, error= function(e) {
      stop(paste0("invalid mathematical expression detected for process rate '",
        pros$name[i],"'; details: ",e))
    })
  }
  # Duplicate name checks over multiple tables
  n <- c(vars$name, pars$name, funs$name, pros$name)
  bad <- unique(n[which(duplicated(n))])
  if (length(bad) > 0)
    stop(paste0("names of variables, parameters, functions, and processes",
      " must be unique; the following ",
      ifelse(length(bad)>1,"names were","name was"),
      " declared more than once: '",paste(bad,collapse="', '"),"'"))
  # Append columns with expressions translated to tex/html
  pros$expression_tex <- pros$expression
  pros$expression_html <- pros$expression
  for (i in 1:nrow(pros)) {
    pros$expression_tex[i] <- substituteIdentifiers(expr=pros$expression_tex[i],
      sub=c(setNames(vars$tex, vars$name), setNames(pars$tex, pars$name),
      setNames(funs$tex, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
    pros$expression_html[i] <- substituteIdentifiers(expr=pros$expression_html[i],
      sub=c(setNames(vars$html, vars$name), setNames(pars$html, pars$name),
      setNames(funs$html, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
  }
  private$prosTbl <- as.data.frame(pros, stringsAsFactors=FALSE)
  # Set stoichiometry ##########################################################
  # Convert matrix to table
  if (asMatrix) {
    if (!is.matrix(stoi))
      stop("stoichiometric factors not in matrix format")
    if (is.null(colnames(stoi)) || any(colnames(stoi) == ""))
      stop("missing column name(s) in matrix of stoichiometric factors")
    if (is.null(rownames(stoi)) || any(rownames(stoi) == ""))
      stop("missing row name(s) in matrix of stoichiometric factors")
    stoi <- data.frame(variable=rep(colnames(stoi), each=nrow(stoi)),
      process=rep(rownames(stoi), ncol(stoi)),
      expression= as.character(stoi), stringsAsFactors=FALSE)
    stoi <- subset(stoi, !(is.na(stoi$expression) | (nchar(stoi$expression)==0)))
  }
  # Basic checks
  cn <- c("variable","process","expression")
  checkTbl(tbl=stoi, tblName="stoi", colNames=cn, nameCol=NULL, emptyOK=FALSE)
  for (n in cn)
    stoi[,n] <- as.character(stoi[,n])
  # Check names of variables
  n <- unique(stoi$variable)
  bad <- n[!(n %in% vars$name)]
  if (length(bad) > 0)
    stop(paste0("stoichiometry factor(s) specified for undeclared variable(s) '",
      paste(bad,collapse="', '"),"'"))
  bad <- vars$name[!(vars$name %in% n)]
  if (length(bad) > 0)
    stop(paste0("missing stoichiometry factor(s) for variable(s) '",
      paste(bad,collapse="', '"),"'"))
  # Check names of processes
  n <- unique(stoi$process)
  bad <- n[!(n %in% pros$name)]
  if (length(bad) > 0)
    stop(paste0("stoichiometry factor(s) specified for undeclared process(es) '",
      paste(bad,collapse="', '"),"'"))
  bad <- pros$name[!(pros$name %in% n)]
  if (length(bad) > 0)
    stop(paste0("missing stoichiometry factor(s) for process(es) '",
      paste(bad,collapse="', '"),"'"))
  # Remove newline characters from expressions
  stoi$expression <- gsub(pattern="\n", replacement=" ", x=stoi$expression)
  # Check for undeclared items in expressions
  for (i in 1:nrow(stoi)) {
    bad <- undeclared(stoi$expression[i], c(vars$name, pars$name, funs$name,
      rodeoConst$reservedNames))
    if (length(bad) > 0)
      stop(paste0("stoichiometry factor for variable '",
        stoi$variable[i],"' and process '",stoi$process[i],
        "' contains undeclared item(s) '",paste(bad,collapse="', '"),"'"))
  }
  # Check for invalid expressions
  for (i in 1:nrow(stoi)) {
    tryCatch({
      parse(text=stoi$expression[i])
    }, error= function(e) {
      stop(paste0("stoichiometry factor for variable '",stoi$variable[i],
      "' and process '",stoi$process[i],
      "' is not a valid mathematical expression; details: ",e))
    })
  }
  # Append columns with expressions translated to tex/html
  stoi$expression_tex <- stoi$expression
  stoi$expression_html <- stoi$expression
  for (i in 1:nrow(stoi)) {
    stoi$expression_tex[i] <- substituteIdentifiers(expr=stoi$expression_tex[i],
      sub=c(setNames(vars$tex, vars$name), setNames(pars$tex, pars$name),
      setNames(funs$tex, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
    stoi$expression_html[i] <- substituteIdentifiers(expr=stoi$expression_html[i],
      sub=c(setNames(vars$html, vars$name), setNames(pars$html, pars$name),
      setNames(funs$html, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
  }
  # Add columns with the variables' symbols
  stoi$variable_tex <- vars$tex[match(stoi$variable, vars$name)]
  stoi$variable_html <- vars$html[match(stoi$variable, vars$name)]
  private$stoiTbl <- as.data.frame(stoi, stringsAsFactors=FALSE)
  # Check tex/html symbols #####################################################
  all_names <- c(vars$name, pars$name, funs$name)
  for (n in c("tex","html")) {
    all_symb <- c(vars[,n], pars[,n], funs[,n])
    # (a) check for duplicates
    bad <- unique(all_symb[which(duplicated(all_symb))])
    if (length(bad) > 0)
      stop(n," symbols of variables, parameters, and functions",
        " must be unique; the following ",
        ifelse(length(bad)>1,"symbols are","symbol is"),
        " used more than once: '",paste(bad,collapse="', '"),"'")
    # (b) check for conflicts with item names (avoids errors when names in
    #     math expressions are replaced by symbols)
    bad <- all_symb[which((all_symb %in% all_names) & (all_symb != all_names))]
    if (length(bad) > 0) {
      stop("the following ",n,ifelse(length(bad)>1," symbols are",
        "symbol is")," cannot be assiged to the respective item (",
        "i.e. variable, parameter, or function) because a different ",
        "item shares the name of the symbol: '",paste(bad,collapse="', '"),"'")
    }
  }
  # Initialize spatial dimensions ##############################################
  dim <- as.integer(dim)
  if ((length(dim) == 0) || any(!is.integer(dim)) || any(dim < 1))
    stop("'dim' must be a vector of positive integers")
  if ((length(dim) > 1) && any(dim < 2))
    stop("all elements of 'dim' must be > 1 when length of 'dim' is > 1")
  private$dim <- dim
  # Initialize numeric data ####################################################
  private$vars <- rep(NA, nrow(vars)*prod(dim))
  private$pars <- rep(NA, nrow(pars)*prod(dim))
  # Register finalizer (unloads stepper libs)
  unld <- function(x) {
    if (length(x$steppers) > 0) {
      for (n in names(x$steppers)) {
        if (file.exists(x$steppers[[n]]$libFile))
          dyn.unload(x$steppers[[n]]$libFile)
      }
    }
  }
  reg.finalizer(self, unld, onexit=TRUE)
})

