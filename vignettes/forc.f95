! THIS IS A GENERATED FILE 

include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.3/rodeo/fortran/forcingsGenericMethods.f95'
module forcings
use forcings_generic
implicit none
private TSeries, readTS, interpol
contains

  function temp (time) result (res)
  double precision, intent(in):: time
  character(len=256), parameter:: file='meteo.txt'
  character(len=256), parameter:: col='temp'
  integer, parameter:: lweight= -1
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  double precision, parameter:: NA= huge(0d0)
  character(len=512):: errmsg
  double precision:: res
  include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.3/rodeo/fortran/forcingsInclude.f95'
end function
  function humid (time) result (res)
  double precision, intent(in):: time
  character(len=256), parameter:: file='meteo.txt'
  character(len=256), parameter:: col='humid'
  integer, parameter:: lweight= -1
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  double precision, parameter:: NA= huge(0d0)
  character(len=512):: errmsg
  double precision:: res
  include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.3/rodeo/fortran/forcingsInclude.f95'
end function
end module
