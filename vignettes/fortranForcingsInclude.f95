
  ! BEGIN OF INCLUDE CODE
  double precision, intent(in):: time
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  double precision, parameter:: NA= huge(0d0)
  character(len=512):: errmsg
  double precision:: res
  ! init
  if (firstCall) then
    firstCall= .FALSE.
    if (.not. readTS(file, nskip, x)) then
      write(errmsg,'(3(a))')"initialization of forcing data from file '", &
        trim(adjustl(file)),"' failed"
      call rexit(trim(adjustl(errmsg)))
    end if
  end if
  ! interpolate
  if (allocated(x%times)) then
    res= interpol(time=time, x=x, latest=latest, lweight=lweight, na=NA)
    if (res .eq. NA) then
      write(errmsg,'(3(a),f0.5)')"failed to retrieve forcing data from file '", &
        trim(adjustl(file)),"' for query time ",time
      call rexit(trim(adjustl(errmsg)))
    end if
  else
    call rexit("forcing data not allocated")
  end if
  ! END OF INCLUDE CODE

