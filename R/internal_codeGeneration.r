
create_code= function(name, nameVecDrvs, nameVecProc, nameVecVars, nameVecPars,
  nameLevelIndex, namesVars, namesPars, 
  code_drvs, code_proc, nameLenVars, nameLenPars, nameLenProc, nProc,
  nameLenLevels, nLevels,
  importFuns, newline, lang
) {

  if (lang == "f") {

    # Create code
    code=paste0("! THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
    code=paste0(code,"subroutine ",name,"(time, ",nameVecVars,", ",nameVecPars,", ",nameVecDrvs,", ",nameVecProc,")",newline)
    code=paste0(code,"  ",ifelse(importFuns,"","!"),"use functions",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Dimension constants",newline)
    code=paste0(code,"  integer, parameter:: ",nameLenVars,"=",length(namesVars),newline)
    code=paste0(code,"  integer, parameter:: ",nameLenPars,"=",length(namesPars),newline)
    code=paste0(code,"  integer, parameter:: ",nameLenProc,"=",nProc,newline)
    code=paste0(code,"  integer, parameter:: ",nameLenLevels,"=",nLevels,newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  double precision, intent(in):: time",newline)
    code=paste0(code,"  double precision, dimension(",nameLenVars,"*",nameLenLevels,"), intent(in):: ",nameVecVars,newline)
    code=paste0(code,"  double precision, dimension(",nameLenPars,"), intent(in):: ",nameVecPars,newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",nameLenVars,"*",nameLenLevels,"), intent(out):: ",nameVecDrvs,newline)
    code=paste0(code,"  double precision, dimension(",nameLenProc,"*",nameLenLevels,"), intent(out):: ",nameVecProc,newline)
    code=paste0(code,"  ! Local",newline)
    code=paste0(code,"  integer:: spatial_level, i",newline)
    code=paste0(code,"  ! Index constants for variables",newline)
    code=paste0(code,paste0("  integer, parameter:: ",namesVars,"=",1:length(namesVars),newline,collapse=""))
    code=paste0(code,"  ! Index constants for parameters",newline)
    code=paste0(code,paste0("  integer, parameter:: ",namesPars,"=",1:length(namesPars),newline,collapse=""))
    code=paste0(code,"",newline)
    code=paste0(code,"  ! Set vector of derivatives (all spatial levels)",newline)
    code=paste0(code,"  do spatial_level = 1, ",nameLenLevels,newline)
    code=paste0(code,"    ",nameVecDrvs,"((/(i, i=spatial_level, ((",nameLenVars,"-1)*",
      nameLenLevels,"+spatial_level), ",nameLenLevels,")/))","= ",nameVecDrvs,"0D(spatial_level)",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Set vector of process rates (all spatial levels)",newline)
    code=paste0(code,"  do spatial_level = 1, ",nameLenLevels,newline)
    code=paste0(code,"    ",nameVecProc,"((/(i, i=spatial_level, ((",nameLenProc,"-1)*",
      nameLenLevels,"+spatial_level), ",nameLenLevels,")/))","= ",nameVecProc,"0D(spatial_level)",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  contains  ! Internal functions follow",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Function to compute the derivatives at a particular level",newline)
    code=paste0(code,"  function ",nameVecDrvs,"0D(",nameLevelIndex,")",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  integer, intent(in):: ",nameLevelIndex,newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",nameLenVars,"):: ",nameVecDrvs,"0D",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ",code_drvs,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Function to compute the process rates at a particular level",newline)
    code=paste0(code,"  function ",nameVecProc,"0D(",nameLevelIndex,")",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  integer, intent(in):: ",nameLevelIndex,newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",nameLenProc,"):: ",nameVecProc,"0D",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ",code_proc,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,"end subroutine",newline)
    return(code)

  } else if (lang == "r") {

    # Create code
    code=paste0("# THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
    code=paste0(code,name," = function(time, ",nameVecVars,", ",nameVecPars,") {",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Dimension constants",newline)
    code=paste0(code,"  ",nameLenLevels,"=",nLevels,newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Index constants for variables",newline)
    code=paste0(code,paste0("  ",namesVars,"=",1:length(namesVars),newline,collapse=""))
    code=paste0(code,"  # Index constants for parameters",newline)
    code=paste0(code,paste0("  ",namesPars,"=",1:length(namesPars),newline,collapse=""))
    code=paste0(code,"",newline)
    code=paste0(code,"  # Internal function to compute the derivatives at a particular level",newline)
    code=paste0(code,"  fun_",nameVecDrvs,"0D = function(",nameLevelIndex,") {",newline)
    code=paste0(code,"  ",code_drvs,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  # Internal function to compute the process rates at a particular level",newline)
    code=paste0(code,"  fun_",nameVecProc,"0D = function(",nameLevelIndex,") {",newline)
    code=paste0(code,"  ",code_proc,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,"  # Set vector of derivatives (all spatial levels)",newline)
    code=paste0(code,"  ",nameVecDrvs," = unname(fun_",nameVecDrvs,"0D(1:",nameLenLevels,"))",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  # Set vector of process rates (all spatial levels)",newline)
    code=paste0(code,"  ",nameVecProc," = unname(fun_",nameVecProc,"0D(1:",nameLenLevels,"))",newline)
    code=paste0(code,"  # Return a list",newline)
    code=paste0(code,"  return(list(",nameVecDrvs,"=",nameVecDrvs,",",nameVecProc,"=",nameVecProc,"))",newline)
    code=paste0(code,"}",newline)
    return(code)

  } else {
      stop("requested language not supported")
  }
}

