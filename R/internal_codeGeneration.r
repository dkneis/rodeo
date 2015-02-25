
create_code= function(name, namesVars, namesPars, namesPros, ident,
  code_drvs, code_pros, nPros, importFuns, newline, lang
) {

  if (lang == "f95") {

    # Create code
    code=paste0("! THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
    code=paste0(code,"subroutine ",name,"(time, ",ident["vecVars"],", ",ident["vecPars"],
      ", ",ident["lenLevels"],", ",ident["vecDrvs"],", ",ident["vecPros"],")",newline)
    code=paste0(code,"  ",ifelse(importFuns,"","!"),"use functions",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Dimension constants",newline)
    code=paste0(code,"  integer, parameter:: ",ident["lenVars"],"=",length(namesVars),newline)
    code=paste0(code,"  integer, parameter:: ",ident["lenPars"],"=",length(namesPars),newline)
    code=paste0(code,"  integer, parameter:: ",ident["lenPros"],"=",nPros,newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  double precision, intent(in):: time",newline)
    code=paste0(code,"  double precision, dimension(",ident["lenVars"],"*",ident["lenLevels"],"), intent(in):: ",ident["vecVars"],newline)
    code=paste0(code,"  double precision, dimension(",ident["lenPars"],"), intent(in):: ",ident["vecPars"],newline)
    code=paste0(code,"  integer, intent(in):: ",ident["lenLevels"],newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",ident["lenVars"],"*",ident["lenLevels"],"), intent(out):: ",ident["vecDrvs"],newline)
    code=paste0(code,"  double precision, dimension(",ident["lenPros"],"*",ident["lenLevels"],"), intent(out):: ",ident["vecPros"],newline)
    code=paste0(code,"  ! Local",newline)
    code=paste0(code,"  integer:: spatial_level, i",newline)
    code=paste0(code,"  ! Index constants for variables",newline)
    code=paste0(code,paste0("  integer, parameter:: ",namesVars,"=",1:length(namesVars),newline,collapse=""))
    code=paste0(code,"  ! Index constants for parameters",newline)
    code=paste0(code,paste0("  integer, parameter:: ",namesPars,"=",1:length(namesPars),newline,collapse=""))
    code=paste0(code,"  ! Index constants for processes",newline)
    code=paste0(code,paste0("  integer, parameter:: ",namesPros,"=",1:length(namesPros),newline,collapse=""))
    code=paste0(code,"",newline)
    code=paste0(code,"  ! Set vector of process rates (all spatial levels)",newline)
    # Note: A 'forall' statement doesn't make this loop faster. Furthermore, use
    #       of openMP 'parallel do' led to a significant loss of performance
    #       (comp. time increased by factor 2.5) in a typical example. Tested: 2015-02-10
    code=paste0(code,"  do spatial_level = 1, ",ident["lenLevels"],newline)
    code=paste0(code,"    ",ident["vecPros"],"((/(i, i=spatial_level, ((",ident["lenPros"],"-1)*",
      ident["lenLevels"],"+spatial_level), ",ident["lenLevels"],")/))","= ",ident["vecPros"],"0D(spatial_level)",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Set vector of derivatives (all spatial levels)",newline)
    # Note: A 'forall' statement doesn't make this loop faster. Furthermore, use
    #       of openMP 'parallel do' led to a significant loss of performance
    #       (comp. time increased by factor 2.5) in a typical example. Tested: 2015-02-10
    code=paste0(code,"  do spatial_level = 1, ",ident["lenLevels"],newline)
    code=paste0(code,"    ",ident["vecDrvs"],"((/(i, i=spatial_level, ((",ident["lenVars"],"-1)*",
      ident["lenLevels"],"+spatial_level), ",ident["lenLevels"],")/))","= ",ident["vecDrvs"],"0D(spatial_level)",newline)
    code=paste0(code,"  end do",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  contains  ! Internal functions follow",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Function to compute the process rates at a particular level",newline)
    code=paste0(code,"  function ",ident["vecPros"],"0D(",ident["levelIndex"],")",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  integer, intent(in):: ",ident["levelIndex"],newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",ident["lenPros"],"):: ",ident["vecPros"],"0D",newline)
    code=paste0(code,"  ! Local",newline)
    code=paste0(code,"  integer, parameter:: ",ident["constOne"],"=1",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ",code_pros,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ! Function to compute the derivatives at a particular level",newline)
    code=paste0(code,"  function ",ident["vecDrvs"],"0D(",ident["levelIndex"],")",newline)
    code=paste0(code,"  implicit none",newline)
    code=paste0(code,"  ! Inputs",newline)
    code=paste0(code,"  integer, intent(in):: ",ident["levelIndex"],newline)
    code=paste0(code,"  ! Outputs",newline)
    code=paste0(code,"  double precision, dimension(",ident["lenVars"],"):: ",ident["vecDrvs"],"0D",newline)
    code=paste0(code,"  ! Local",newline)
    code=paste0(code,"  integer, parameter:: ",ident["constOne"],"=1",newline)
    code=paste0(code,"  ",newline)
    code=paste0(code,"  ",code_drvs,newline)
    code=paste0(code,"  end function",newline)
    code=paste0(code,"end subroutine",newline)
    return(code)

  } else if (lang == "r") {

    # Create code
    code=paste0("# THIS IS A GENERATED FILE - EDITING DOESN'T MAKE SENSE",newline)
    code=paste0(code,name," = function(time, ",ident["vecVars"],", ",ident["vecPars"],", ",ident["lenLevels"],") {",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Index constants for variables",newline)
    code=paste0(code,paste0("  ",namesVars,"=",1:length(namesVars),newline,collapse=""))
    code=paste0(code,"  # Index constants for parameters",newline)
    code=paste0(code,paste0("  ",namesPars,"=",1:length(namesPars),newline,collapse=""))
    code=paste0(code,"  # Index constants for processes",newline)
    code=paste0(code,paste0("  ",namesPros,"=",1:length(namesPros),newline,collapse=""))
    code=paste0(code,"  # Other constants",newline)
    code=paste0(code,"  ",ident["constOne"],"=1",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Internal function to compute the process rates at a particular level",newline)
    code=paste0(code,"  fun_",ident["vecPros"],"0D = function(",ident["levelIndex"],") {",newline)
    code=paste0(code,"  ",code_pros,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Set vector of process rates (all spatial levels)",newline)
    code=paste0(code,"  ",ident["vecPros"]," = unname(fun_",ident["vecPros"],"0D(1:",ident["lenLevels"],"))",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Internal function to compute the derivatives at a particular level",newline)
    code=paste0(code,"  fun_",ident["vecDrvs"],"0D = function(",ident["levelIndex"],") {",newline)
    code=paste0(code,"  ",code_drvs,newline)
    code=paste0(code,"  }",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Set vector of derivatives (all spatial levels)",newline)
    code=paste0(code,"  ",ident["vecDrvs"]," = unname(fun_",ident["vecDrvs"],"0D(1:",ident["lenLevels"],"))",newline)
    code=paste0(code,"",newline)
    code=paste0(code,"  # Return a list",newline)
    code=paste0(code,"  return(list(",ident["vecDrvs"],"=",ident["vecDrvs"],",",ident["vecPros"],"=",ident["vecPros"],"))",newline)
    code=paste0(code,"}",newline)
    return(code)

  } else {
      stop("requested language not supported")
  }
}

