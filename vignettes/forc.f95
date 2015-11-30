! THIS IS A GENERATED FILE 

    include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.2/rodeo/fortran/forcingsGenericMethods.f95'
    module forcings
    use forcings_generic
    implicit none
    private TSeries, readTS, interpol
    contains
      function temp (time) result (res)
        character(len=256), parameter:: file='meteo.txt'
        character(len=256), parameter:: col='temp'
        integer, parameter:: lweight= -1
        include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.2/rodeo/fortran/forcingsInclude.f95'
      end function
      function humid (time) result (res)
        character(len=256), parameter:: file='meteo.txt'
        character(len=256), parameter:: col='humid'
        integer, parameter:: lweight= -1
        include '/home/dkneis/R/x86_64-pc-linux-gnu-library/3.2/rodeo/fortran/forcingsInclude.f95'
      end function
    end module
