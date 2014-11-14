
breakline= function(text, conti="&", newline="\n") {
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


rodeo$methods( generate = function(name="derivs", size=1, lang="R") {
    "Generate code to compute the variables' derivatives with respect to time.
    \\bold{Arguments:} \\code{name}: A string giving the name for the generated
    function; \\code{size}: An integer (default 1) specifying the number of
    instances of each state variable, e.g. in spatially distributed models.
    \\code{lang}: The language of generated code (currently 'R' or 'F').
    \\bold{Returns:} The generated function as a character string.
    "
  # Constants
  newline= ifelse(.Platform$OS.type=="windows","\r\n","\n")
  nameVecDrvs= "dydt"
  nameVecProc= "proc"

  # Set language-specific constants
  lang= substr(tolower(lang),1,1)
  if (lang == "r") {
    L= list(
      funOpen= paste0(name," = function(time, vars, pars) {"),
      argDecl="",
      funClose= "}",
      vecOpen= "unname(c(",
      vecClose="))",
      eleOpen= "[",
      eleClose="]",
      lstOpen="list(",
      lstClose=")",
      lstAppend=",",
      cont="",
      com= "#"
    )
  } else if (lang == "f") {
    L= list(
      funOpen= paste0("subroutine ",name,"(time, vars, pars, ",nameVecDrvs,", ",nameVecProc,")"),
      funClose= "end subroutine",
      argDecl=paste0(ifelse(length(funs) > 0,paste0("  use functions",newline),""),
                     "  implicit none",newline,
                     "  double precision, intent(in):: time",newline,
                     "  double precision, dimension(",length(vars)*size,"), intent(in):: vars",newline,
                     "  double precision, dimension(",length(pars),"), intent(in):: pars",newline,
                     "  double precision, dimension(",length(vars)*size,"), intent(out):: ",nameVecDrvs,newline,
                     "  double precision, dimension(",length(proc)*size,"), intent(out):: ",nameVecProc,newline),
      vecOpen= "(/",
      vecClose="/)",
      eleOpen= "(",
      eleClose=")",
      lstOpen="",
      lstClose="",
      lstAppend="",
      cont="&",
      com= "!"
    )
  } else {
    stop("requested language not supported")
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

  # Replace aux. expressions in other aux. expressions in a forward manner, i.e. assuming
  # that an aux. expression is defined BEFORE it is referenced.
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
      stop(paste0("auxiliary expression '",names(AUXX)[i],"' is referenced before it is defined"))
    }
  }

  # Substitute aux. expression names by their values
  for (i in 1:length(AUXX)) {
    patt= paste0(beforeName,names(AUXX)[i],afterName)
    repl= paste0("\\1","(",AUXX[i],")","\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (p in 1:ncol(STOX)) {
      STOX[,p]= gsub(pattern=patt, replacement=repl, x=STOX[,p])
    }
  }

  # Turn names of variables into references to vector elements
  # Here, the index is set for the case of a 0D-model - it is reset later
  for (i in 1:ncol(STOX)) {
    patt= paste0(beforeName,names(STOX)[i],afterName)
    repl= paste0("\\1","vars",L$eleOpen,i,L$eleClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }

  # Turn names of parameters into references to vector elements
  # Here, the index is set for the case of a 0D-model - it is reset later
  for (i in 1:length(pars)) {
    patt= paste0(beforeName,names(pars)[i],afterName)
    repl= paste0("\\1","pars",L$eleOpen,i,L$eleClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }  

  # Create code
  code=paste0(L$com," THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
  code=paste0(code,newline)  
  code=paste0(code,L$funOpen,newline)
  code=paste0(code,L$argDecl,newline)
  code=paste0(code,L$lstOpen,newline)

  ##############################################################################
  # Derivatives of state variables
  ##############################################################################
  code=paste0(code,"  ",nameVecDrvs,"= ",L$vecOpen,L$cont,newline)
  for (n in 1:ncol(STOX)) {
    if (n > 1) {
      code=paste0(code,"    ,",L$cont,newline)
    }
    code=paste0(code,"    ",L$com," Variable '",names(STOX)[n],"' with ",size," level",ifelse(size>1,"s",""),newline)
    for (iunit in 1:size) {
      if (iunit > 1) {
        code=paste0(code,"      ,",L$cont,newline)
      }
      # Update index of variables, accounting for the spatial unit
      # Note: A dummy character '@' is inserted on value replacement in order to
      #   avoid continued substitution of already updated indices. The dummy
      #   character is removed after all substitutions have been performed.
      local_PROC= PROC
      local_STOX= STOX
      for (q in 1:ncol(STOX)) {
        local_PROC= gsub(pattern=paste0("vars",L$eleOpen,q,L$eleClose),
          replacement=paste0("vars@",L$eleOpen,(q-1)*size+iunit,L$eleClose),
          x=local_PROC, fixed=TRUE)
        for (p in 1:ncol(STOX)) {
          local_STOX[,p]= gsub(pattern=paste0("vars",L$eleOpen,q,L$eleClose),
            replacement=paste0("vars@",L$eleOpen,(q-1)*size+iunit,L$eleClose),
            x=local_STOX[,p], fixed=TRUE)
        }
      }
      # Remove dummy character
      local_PROC= gsub(pattern=paste0("vars@",L$eleOpen),replacement=paste0("vars",L$eleOpen), x=local_PROC, fixed=TRUE)
      for (p in 1:ncol(STOX)) {
        local_STOX[,p]= gsub(pattern=paste0("vars@",L$eleOpen),replacement=paste0("vars",L$eleOpen), x=local_STOX[,p], fixed=TRUE)
      }
      # Assemble expressions
      buffer=""
      for (k in 1:length(local_PROC)) {
        if (grepl(pattern="[^0]", x=local_STOX[k,n])) { # Suppress terms where a stoichiometry factor is zero
          if (nchar(buffer) > 0) {
            buffer= paste0(buffer," + ")
          }
          buffer=paste0(buffer,"      (",local_PROC[k],") * (",local_STOX[k,n],")")
        }
      }
      if (nchar(buffer) == 0) {
        buffer= "      0"  # treat case where all stoichiometry factors are zero
      }

      # Break fortran lines
      if (lang == "f") {
        buffer= breakline(text=buffer, conti=L$cont, newline=newline)
      }

      # Add to code
      code= paste0(code,buffer,L$cont,newline)

    }
  }
  code=paste0(code,"  ",L$vecClose,newline) # End of derivatives vector

  ##############################################################################
  # Process rates
  ##############################################################################
  code=paste0(code,"  ",L$lstAppend,nameVecProc,"=",L$vecOpen,L$cont,newline)
  for (n in 1:length(PROC)) {
    if (n > 1) {
      code=paste0(code,"    ,",L$cont,newline)
    }
    code=paste0(code,"    ",L$com," Process rate '",names(proc)[n],"' with ",size," level",ifelse(size>1,"s",""),newline)
    for (iunit in 1:size) {
      if (iunit > 1) {
        code=paste0(code,"      ,",L$cont,newline)
      }
      # Update index of variables, accounting for the spatial unit
      # Note: See comments for the same procedure in the section dealing with derivatives
      local_PROC_n= PROC[n]
      for (q in 1:ncol(STOX)) {
        local_PROC_n= gsub(pattern=paste0("vars",L$eleOpen,q,L$eleClose),
          replacement=paste0("vars@",L$eleOpen,(q-1)*size+iunit,L$eleClose),
          x=local_PROC_n, fixed=TRUE)
      }
      local_PROC_n= gsub(pattern=paste0("vars@",L$eleOpen),replacement=paste0("vars",L$eleOpen), x=local_PROC_n, fixed=TRUE)

      # Break fortran lines
      if (lang == "f") {
        local_PROC_n= breakline(text=local_PROC_n, conti=L$cont, newline=newline)
      }

      # Add to code
      code= paste0(code,"      ",local_PROC_n,L$cont,newline)
    }
  }
  code=paste0(code,"  ",L$vecClose,newline)
  code=paste0(code,L$lstClose,newline)
  code=paste0(code,L$funClose,newline)
  return(code)
})

