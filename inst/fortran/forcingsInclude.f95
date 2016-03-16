  ! BEGIN OF INCLUDE CODE
  if (interpolate) then
    ! init
    if (firstCall) then
      firstCall= .FALSE.
      if (.not. readTS(file, col, x)) then
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
  else
    res= dflt
  end if
  ! END OF INCLUDED CODE

