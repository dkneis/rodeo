

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


rodeo$methods( generate = function(name="derivs", size=1, return_proc=TRUE) {
    "Generate code to compute the variables' derivatives with respect to time.
    \\bold{Arguments:} \\code{name}: A string giving the name for the generated
    function; \\code{size}: An integer (default 1) specifying the number of
    instances of each state variable, e.g. in spatially distributed models.
    \\code{return_proc}: If TRUE, the generated function also returns the values of process rates.
    \\bold{Returns:} The generated function as a character string.
    "

  # We work with local copies here!
  AUXX= auxx
  PROC= proc
  STOX= stox

  # Constants
  rangeOpen="["
  rangeClose="]"

  # Replace aux. expressions in other aux. expressions in a forward manner, i.e. assuming
  # that an aux. expression is defined BEFORE it is referenced.
  for (i in 1:length(AUXX)) {
    if (i < length(AUXX)) {
      AUXX[(i+1):length(AUXX)]= gsub(pattern=names(AUXX)[i],
        replacement=paste0("(",AUXX[i],")"), x=AUXX[(i+1):length(AUXX)], fixed=TRUE)
    }
  }
  # Check whether the above assumption was actually fulfilled.
  for (i in 1:length(AUXX)) {
    if (any(grepl(pattern=names(AUXX)[i], x=AUXX, fixed=TRUE))) {
      stop(paste0("auxiliary expression '",names(AUXX)[i],"' is referenced before it is defined"))
    }
  }

  # Substitute aux. expression names by their values
  for (i in 1:length(AUXX)) {
    PROC= gsub(pattern=names(AUXX)[i], replacement=paste0("(",AUXX[i],")"), x=PROC, fixed=TRUE)
    for (p in 1:ncol(STOX)) {
      STOX[,p]= gsub(pattern=names(AUXX)[i], replacement=paste0("(",AUXX[i],")"), x=STOX[,p], fixed=TRUE)
    }
  }

  # Turn names of variables into references to vector elements
  # Here, the index is set for the case of a 0D-model - it is reset later
  for (i in 1:ncol(STOX)) {
    patt= paste0("(^|[^a-zA-Z0-9_])",names(STOX)[i],"([^a-zA-Z0-9_]|$)")
    repl= paste0("\\1","vars",rangeOpen,i,rangeClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }

  # Turn names of parameters into references to vector elements
  # Here, the index is set for the case of a 0D-model - it is reset later
  for (i in 1:length(pars)) {
    patt= paste0("(^|[^a-zA-Z0-9_])",names(pars)[i],"([^a-zA-Z0-9_]|$)")
    repl= paste0("\\1","pars",rangeOpen,i,rangeClose,"\\2")
    PROC= gsub(pattern=patt, replacement=repl, x=PROC)
    for (n in 1:ncol(STOX)) {
      STOX[,n]= gsub(pattern=patt, replacement=repl, x=STOX[,n])
    }
  }  

  # Create code
  newline= ifelse(.Platform$OS.type=="windows","\r\n","\n")
  code=paste0("# THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
  code=paste0(code,newline)
  code=paste0(code,"# This function returns the state variables' derivatives with resp. to time.",newline)
  code=paste0(code,"# Does it also return the values of process rates? : ",ifelse(return_proc,"Yes","No"),newline)
  code=paste0(code,newline)  
  code=paste0(code,name," = function(time,vars,pars) {",newline)
  code=paste0(code,"list(",newline) # return values is wrapped into a list

  ##############################################################################
  # Derivatives of state variables
  ##############################################################################
  code=paste0(code,"  dytd= unname(c(",newline)
  for (n in 1:ncol(STOX)) {
    if (n > 1) {
      code=paste0(code,"    ,",newline)
    }
    code=paste0(code,"    # Variable '",names(STOX)[n],"' with ",size," level",ifelse(size>1,"s",""),newline)
    for (iunit in 1:size) {
      if (iunit > 1) {
        code=paste0(code,"      ,",newline)
      }
      # Update index of variables, accounting for the spatial unit
      # Note: A dummy character '@' is inserted on value replacement in order to
      #   avoid continued substitution of already updated indices. The dummy
      #   character is removed after all substitutions have been performed.
      local_PROC= PROC
      local_STOX= STOX
      for (q in 1:ncol(STOX)) {
        local_PROC= gsub(pattern=paste0("vars[",q,"]"),replacement=paste0("vars@[",(q-1)*size+iunit,"]"),
          x=local_PROC, fixed=TRUE)
        for (p in 1:ncol(STOX)) {
          local_STOX[,p]= gsub(pattern=paste0("vars[",q,"]"),replacement=paste0("vars@[",(q-1)*size+iunit,"]"),
            x=local_STOX[,p], fixed=TRUE)
        }
      }
      # Remove dummy character
      local_PROC= gsub(pattern=paste0("vars@["),replacement=paste0("vars["), x=local_PROC, fixed=TRUE)
      for (p in 1:ncol(STOX)) {
        local_STOX[,p]= gsub(pattern=paste0("vars@["),replacement=paste0("vars["), x=local_STOX[,p], fixed=TRUE)
      }
      # Assemble expressions
      buffer=""
      for (k in 1:length(local_PROC)) {
        if (grepl(pattern="[^0]", x=local_STOX[k,n])) { # Suppress terms where a stoichiometry factor is zero
          if (nchar(buffer) > 0) {
            buffer= paste0(buffer," +",newline)
          }
          buffer=paste0(buffer,"      (",local_PROC[k],") * (",local_STOX[k,n],")")
        }
      }
      if (nchar(buffer) == 0) {
        buffer= "      0"  # treat case where all stoichiometry factors are zero
      }
      code= paste0(code,buffer,newline)

    }
  }
  code=paste0(code,"  ))",newline) # End of derivatives vector

  ##############################################################################
  # Process rates (optional output)
  ##############################################################################
  if (return_proc) {
    code=paste0(code,"  ,proc= unname(c(",newline)
    for (n in 1:length(PROC)) {
      if (n > 1) {
        code=paste0(code,"    ,",newline)
      }
      code=paste0(code,"    # Process rate '",names(proc)[n],"' with ",size," level",ifelse(size>1,"s",""),newline)
      for (iunit in 1:size) {
        if (iunit > 1) {
          code=paste0(code,"      ,",newline)
        }
        # Update index of variables, accounting for the spatial unit
        # Note: See comments for the same procedure in the section dealing with derivatives
        local_PROC_n= PROC[n]
        for (q in 1:ncol(STOX)) {
          local_PROC_n= gsub(pattern=paste0("vars[",q,"]"),replacement=paste0("vars@[",(q-1)*size+iunit,"]"),
            x=local_PROC_n, fixed=TRUE)
        }
        local_PROC_n= gsub(pattern=paste0("vars@["),replacement=paste0("vars["), x=local_PROC_n, fixed=TRUE)
        # Add to code
        code= paste0(code,local_PROC_n,newline)
      }
    }
    code=paste0(code,"  ))",newline) # End of process rates vector
  }

  code=paste0(code,")",newline) # End of list
  code=paste0(code,"}",newline)
  return(code)
})

