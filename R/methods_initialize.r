rodeo$methods(
  initialize = function(
    vars,
    pars,
    funs,
    pros,
    stoi
) {
  "Initializes a rodeo object"
  # Convert table columns to character
    vars= data.frame(lapply(vars, as.character), stringsAsFactors=FALSE)
    pars= data.frame(lapply(pars, as.character), stringsAsFactors=FALSE)
    funs= data.frame(lapply(funs, as.character), stringsAsFactors=FALSE)
    pros= data.frame(lapply(pros, as.character), stringsAsFactors=FALSE)
    stoi= data.frame(lapply(stoi, as.character), stringsAsFactors=FALSE)
  # Set variables ##############################################################
  checkTbl(tbl=vars, tblName="vars",
    colNames=c("name","unit","description"), nameCol="name", emptyOK=FALSE)
  .self$VARS <<- vars
  # Set parameters #############################################################
  checkTbl(tbl=pars, tblName="pars",
    colNames=c("name","unit","description"), nameCol="name", emptyOK=FALSE)
  .self$PARS <<- pars
  # Set functions ##############################################################
  checkTbl(tbl=funs, tblName="funs",
    colNames=c("name","unit","description"), nameCol="name", emptyOK=TRUE)
  .self$FUNS <<- funs
  # Set processes ##############################################################
  # Basic checks
  checkTbl(tbl=pros, tblName="pros", colNames=c("name","unit","description",
    "expression"), nameCol="name", emptyOK=FALSE)
  # Check for undeclared items in expressions
  for (i in 1:nrow(pros)) {
    bad= undeclared(pros$expression[i], c(vars$name, pars$name, funs$name))
    if (length(bad) > 0)
      stop(paste0("expression for process '",pros$name[i],
        "' contains undeclared item(s) '",paste(bad,collapse="', '"),"'"))
  }
  .self$PROS <<- pros
  # Set stoichiometry ##########################################################
  # Basic checks
  checkTbl(tbl=stoi, tblName="stoi", colNames=c("variable","process",
    "expression"), nameCol=NULL, emptyOK=FALSE)
  # Check names of variables
  n= unique(stoi$variable)
  bad= n[!(n %in% vars$name)]
  if (length(bad) > 0)
    stop(paste0("stoichiometry factor(s) specified for undeclared variable(s) '",
      paste(bad,collapse="', '"),"'"))
  bad= vars$name[!(vars$name %in% n)]
  if (length(bad) > 0)
    stop(paste0("missing stoichiometry factor(s) for variable(s) '",
      paste(bad,collapse="', '"),"'"))
  # Check names of processes
  n= unique(stoi$process)
  bad= n[!(n %in% pros$name)]
  if (length(bad) > 0)
    stop(paste0("stoichiometry factor(s) specified for undeclared process(es) '",
      paste(bad,collapse="', '"),"'"))
  bad= pros$name[!(pros$name %in% n)]
  if (length(bad) > 0)
    stop(paste0("missing stoichiometry factor(s) for process(es) '",
      paste(bad,collapse="', '"),"'"))
  # Check for undeclared items in expressions
  for (i in 1:nrow(stoi)) {
    bad= undeclared(stoi$expression[i], c(vars$name, pars$name, funs$name))
    if (length(bad) > 0)
      stop(paste0("stoichiometry factor for variable '",
        stoi$variable[i],"' and process '",stoi$process[i],
        "' contains undeclared item(s) '",paste(bad,collapse="', '"),"'"))
  }
  # Duplicate-name checks over multiple tables
  n= c(vars$name, pars$name, funs$name, pros$name)
  bad= unique(n[which(duplicated(n))])
  if (length(bad) > 0)
    stop(paste0("names of variables, parameters, functions, and processes",
      " must be unique; the following ",
      ifelse(length(bad)>1,"names were","name was"),
      " declared more than once: '",paste(bad,collapse="', '"),"'"))

  .self$STOI <<- stoi
})

