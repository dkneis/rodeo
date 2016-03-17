rodeo$methods(
  initialize = function(vars, pars, funs, pros, stoi, asMatrix=FALSE
) {
  "Initializes a rodeo object"
  # Set variables ##############################################################
  cn= c("name","unit","description")
  checkTbl(tbl=vars, tblName="vars", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    vars[,n]= as.character(vars[,n])
  for (n in c("tex","html"))
    vars[,n]= if (n %in% names(vars)) as.character(vars[,n]) else vars$name
  .self$.vars <<- vars
  # Set parameters #############################################################
  cn= c("name","unit","description")
  checkTbl(tbl=pars, tblName="pars", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    pars[,n]= as.character(pars[,n])
  for (n in c("tex","html"))
    pars[,n]= if (n %in% names(pars)) as.character(pars[,n]) else pars$name
  .self$.pars <<- pars
  # Set functions ##############################################################
  cn= c("name","unit","description")
  checkTbl(tbl=funs, tblName="funs", colNames=cn, nameCol="name", emptyOK=TRUE)
  for (n in cn)
    funs[,n]= as.character(funs[,n])
  for (n in c("tex","html"))
    funs[,n]= if (n %in% names(funs)) as.character(funs[,n]) else funs$name
  .self$.funs <<- funs
  # Set processes ##############################################################
  # Basic checks
  cn= c("name","unit","description","expression")
  checkTbl(tbl=pros, tblName="pros", colNames=cn, nameCol="name", emptyOK=FALSE)
  for (n in cn)
    pros[,n]= as.character(pros[,n])
  # Check for undeclared items in expressions
  for (i in 1:nrow(pros)) {
    bad= undeclared(pros$expression[i], c(vars$name, pars$name, funs$name,
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
  # Append columns with expressions translated to tex/html
  pros$expression_tex= pros$expression
  pros$expression_html= pros$expression
  for (i in 1:nrow(pros)) {
    pros$expression_tex[i]= substituteIdentifiers(expr=pros$expression_tex[i],
      sub=c(setNames(vars$tex, vars$name), setNames(pars$tex, pars$name),
      setNames(funs$tex, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
    pros$expression_html[i]= substituteIdentifiers(expr=pros$expression_html[i],
      sub=c(setNames(vars$html, vars$name), setNames(pars$html, pars$name),
      setNames(funs$html, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
  }
  .self$.pros <<- pros
  # Set stoichiometry ##########################################################
  # Convert matrix to table
  if (asMatrix) {
    stoi= data.frame(lapply(stoi, as.character), stringsAsFactors=FALSE)
    stoi= data.frame(variable=rep(names(stoi)[2:ncol(stoi)], each=nrow(stoi)),
      process=rep(stoi[,1], (ncol(stoi)-1)),
      expression= unlist(stoi[ ,2:ncol(stoi)]), stringsAsFactors=FALSE)
    stoi= subset(stoi, !(is.na(stoi$expression) | (nchar(stoi$expression)==0)))
  }
  # Basic checks
  cn= c("variable","process","expression")
  checkTbl(tbl=stoi, tblName="stoi", colNames=cn, nameCol=NULL, emptyOK=FALSE)
  for (n in cn)
    stoi[,n]= as.character(stoi[,n])
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
    bad= undeclared(stoi$expression[i], c(vars$name, pars$name, funs$name,
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
  stoi$expression_tex= stoi$expression
  stoi$expression_html= stoi$expression
  for (i in 1:nrow(stoi)) {
    stoi$expression_tex[i]= substituteIdentifiers(expr=stoi$expression_tex[i],
      sub=c(setNames(vars$tex, vars$name), setNames(pars$tex, pars$name),
      setNames(funs$tex, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
    stoi$expression_html[i]= substituteIdentifiers(expr=stoi$expression_html[i],
      sub=c(setNames(vars$html, vars$name), setNames(pars$html, pars$name),
      setNames(funs$html, funs$name),
      setNames(rodeoConst$reservedNames,rodeoConst$reservedNames)),all=TRUE)
  }
  # Add columns with the variables' symbols
  stoi$variable_tex= vars$tex[match(stoi$variable, vars$name)]
  stoi$variable_html= vars$html[match(stoi$variable, vars$name)]
  .self$.stoi <<- stoi
  # General checks #############################################################
  # Duplicate checks over multiple tables
  # (1) names
  n= c(vars$name, pars$name, funs$name, pros$name)
  bad= unique(n[which(duplicated(n))])
  if (length(bad) > 0)
    stop(paste0("names of variables, parameters, functions, and processes",
      " must be unique; the following ",
      ifelse(length(bad)>1,"names were","name was"),
      " declared more than once: '",paste(bad,collapse="', '"),"'"))
  # (2) tex symbols
  n= c(vars$tex, pars$tex, funs$tex)
  bad= unique(n[which(duplicated(n))])
  if (length(bad) > 0)
    stop(paste0("tex symbols of variables, parameters, and functions",
      " must be unique; the following ",
      ifelse(length(bad)>1,"symbols are","symbol is"),
      " used more than once: '",paste(bad,collapse="', '"),"'"))
  # (3) html symbols
  n= c(vars$html, pars$html, funs$html)
  bad= unique(n[which(duplicated(n))])
  if (length(bad) > 0)
    stop(paste0("html symbols of variables, parameters, and functions",
      " must be unique; the following ",
      ifelse(length(bad)>1,"symbols are","symbol is"),
      " used more than once: '",paste(bad,collapse="', '"),"'"))
})

