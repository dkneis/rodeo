
generate_f= function(name, nameVecDrvs, nameVecProc, nameSpatialLevelIndex, nLevels, code_drvs, code_proc, nVars, nPars, nFuns, nProc, newline) {
  # Create code
  code=paste0("! THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
  code=paste0(code,"subroutine ",name,"(time, vars, pars, ",nameVecDrvs,", ",nameVecProc,")",newline)
  code=paste0(code,ifelse(nFuns > 0,paste0("  use functions",newline),""))
  code=paste0(code,"  implicit none",newline)
  code=paste0(code,"  ! Inputs",newline)
  code=paste0(code,"  double precision, intent(in):: time",newline)
  code=paste0(code,"  double precision, dimension(",nVars*nLevels,"), intent(in):: vars",newline)
  code=paste0(code,"  double precision, dimension(",nPars,"), intent(in):: pars",newline)
  code=paste0(code,"  ! Outputs",newline)
  code=paste0(code,"  double precision, dimension(",nVars*nLevels,"), intent(out):: ",nameVecDrvs,newline)
  code=paste0(code,"  double precision, dimension(",nProc*nLevels,"), intent(out):: ",nameVecProc,newline)
  code=paste0(code,"  ! Local",newline)
  code=paste0(code,"  integer:: spatial_level, i",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ! Set vector of derivatives (all spatial levels)",newline)
  code=paste0(code,"  do spatial_level = 1, ",nLevels,newline)
  code=paste0(code,"    ",nameVecDrvs,"((/(i, i=spatial_level, (",(nVars-1)*nLevels,"+spatial_level), ",nLevels,")/))","= ",nameVecDrvs,"0D(spatial_level)",newline)
  code=paste0(code,"  end do",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ! Set vector of process rates (all spatial levels)",newline)
  code=paste0(code,"  do spatial_level = 1, ",nLevels,newline)
  code=paste0(code,"    ",nameVecProc,"((/(i, i=spatial_level, (",(nProc-1)*nLevels,"+spatial_level), ",nLevels,")/))","= ",nameVecProc,"0D(spatial_level)",newline)
  code=paste0(code,"  end do",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  contains  ! Internal functions follow",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ! Function to compute the derivatives at a particular level",newline)
  code=paste0(code,"  function ",nameVecDrvs,"0D(",nameSpatialLevelIndex,")",newline)
  code=paste0(code,"  implicit none",newline)
  code=paste0(code,"  ! Inputs",newline)
  code=paste0(code,"  integer, intent(in):: ",nameSpatialLevelIndex,newline)
  code=paste0(code,"  ! Outputs",newline)
  code=paste0(code,"  double precision, dimension(",nVars,"):: ",nameVecDrvs,"0D",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ",code_drvs,newline)
  code=paste0(code,"  end function",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ! Function to compute the process rates at a particular level",newline)
  code=paste0(code,"  function ",nameVecProc,"0D(",nameSpatialLevelIndex,")",newline)
  code=paste0(code,"  implicit none",newline)
  code=paste0(code,"  ! Inputs",newline)
  code=paste0(code,"  integer, intent(in):: ",nameSpatialLevelIndex,newline)
  code=paste0(code,"  ! Outputs",newline)
  code=paste0(code,"  double precision, dimension(",nProc,"):: ",nameVecProc,"0D",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  ",code_proc,newline)
  code=paste0(code,"  end function",newline)
  code=paste0(code,"end subroutine",newline)
  return(code)
}

generate_r= function(name, nameVecDrvs, nameVecProc, nameSpatialLevelIndex, nLevels, code_drvs, code_proc, nVars, nProc, newline) {
  # Create code
  code=paste0("# THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
  code=paste0(code,name," = function(time, vars, pars) {",newline)
  code=paste0(code,"  # Internal function to compute the derivatives at a particular level",newline)
  code=paste0(code,"  fun_",nameVecDrvs,"0D = function(",nameSpatialLevelIndex,") {",newline)
  code=paste0(code,"  ",code_drvs,newline)
  code=paste0(code,"  }",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  # Internal function to compute the process rates at a particular level",newline)
  code=paste0(code,"  fun_",nameVecProc,"0D = function(",nameSpatialLevelIndex,") {",newline)
  code=paste0(code,"  ",code_proc,newline)
  code=paste0(code,"  }",newline)
  code=paste0(code,"  # Set vector of derivatives (all spatial levels)",newline)
  code=paste0(code,"  #",nameVecDrvs,"= vector('numeric',",nVars*nLevels,")",newline)
  code=paste0(code,"  #for (spatial_level in 1:",nLevels,") {",newline)
  code=paste0(code,"  #  ",nameVecDrvs,"[seq(from=spatial_level, by=",nLevels,
    ", length.out=",nVars,")]","= fun_",nameVecDrvs,"0D(spatial_level)",newline)
  code=paste0(code,"  #}",newline)
  code=paste0(code,"  # NOTE: Next line is a replacement for the above loop code",newline)
  code=paste0(code,"  ",nameVecDrvs," = c(t(apply(X=matrix(1:",nLevels,", ncol=1), MARGIN=1, FUN=fun_",nameVecDrvs,"0D)))",newline)
  code=paste0(code,"  # We can't use the simpler statement",newline)
  code=paste0(code,"  #",nameVecDrvs," = unname(fun_",nameVecDrvs,"0D(1:",nLevels,"))",newline)
  code=paste0(code,"  # because level-independent derivatives would not be replicated.",newline)
  code=paste0(code,"  ",newline)
  code=paste0(code,"  # Set vector of process rates (all spatial levels)",newline)
  code=paste0(code,"  #",nameVecProc,"= vector('numeric',",nProc*nLevels,")",newline)
  code=paste0(code,"  #for (spatial_level in 1:",nLevels,") {",newline)
  code=paste0(code,"  #  ",nameVecProc,"[seq(from=spatial_level, by=",nLevels,
    ", length.out=",nProc,")]","= fun_",nameVecProc,"0D(spatial_level)",newline)
  code=paste0(code,"  #}",newline)
  code=paste0(code,"  # NOTE: Next line is a replacement for the above loop code",newline)
  code=paste0(code,"  ",nameVecProc," = c(t(apply(X=matrix(1:",nLevels,", ncol=1), MARGIN=1, FUN=fun_",nameVecProc,"0D)))",newline)
  code=paste0(code,"  # We can't use the simpler statement",newline)
  code=paste0(code,"  #",nameVecProc," = unname(fun_",nameVecProc,"0D(1:",nLevels,"))",newline)
  code=paste0(code,"  # because level-independent process rates would not be replicated.",newline)
  code=paste0(code,"  # Return a list",newline)
  code=paste0(code,"  return(list(",nameVecDrvs,"=",nameVecDrvs,",",nameVecProc,"=",nameVecProc,"))",newline)
  code=paste0(code,"}",newline)
  return(code)
}

