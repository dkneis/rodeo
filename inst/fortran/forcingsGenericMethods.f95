!###############################################################################
! Module providing routines for reading/interpolation of forcing data.
!###############################################################################

module forcings_generic
implicit none

public TSeries, readTS, interpol
private pos

!###############################################################################
! A dedicated data type for time series
type TSeries
  double precision, dimension(:), allocatable:: times, values
end type TSeries

contains

!###############################################################################
! Finds the index of a column name in a header line. The very first column name
! is not analyzed (first column is reserved for time data anyway). On error
! the function returns zero.
integer function pos(string, x)
  ! args
  character(len=*), intent(in):: string, x
  ! local
  character(len=len(string)):: garbage, value
  character(len=512):: errmsg
  integer:: i, nskip, ioresult
  ! algorithm
  value= ""
  nskip= 0
  do
    nskip= nskip + 1
    read(unit=string, fmt=*, iostat=ioresult, end=1000)(garbage, i=1, nskip), value
    if (ioresult .ne. 0) then
      write(errmsg,'(3(a))')"failed to analyze header string '",trim(adjustl(string)),"'"
      call rwarn(errmsg)
      exit
    end if
    if (trim(adjustl(value)) .eq. trim(adjustl(x))) then
      pos= nskip + 1
      return
    end if
  end do
  1000 continue
  pos= 0
end function


!###############################################################################
! Function to read time series data from an ASCII text file. It is called when a
! forcing item is queried for the first time.
logical function readTS(file, col, x)
  ! args
  character(len=*), intent(in):: file    ! name of data file
  character(len=*), intent(in):: col     ! name of column with data
  type(TSeries), intent(out):: x         ! object of type TSeries
  ! const
  integer, parameter:: un=10
  integer, parameter:: n_ini=1, n_fac=2
  double precision, parameter:: ZERO=0d0
  ! locals
  type(TSeries):: tmp
  integer:: line, ioresult, n, skipcols
  character(len=512):: errmsg
  character(len=256):: string
  character(len=1), parameter:: comment="#"
  double precision, dimension(:), allocatable:: garbage
  !Open file
  open(unit=un, file=file, status="old", action="read", iostat=ioresult)
  if (ioresult .ne. 0) then
    write(errmsg,'(3(a))')"cannot open file '",trim(adjustl(file)),"'"
    call rwarn(errmsg)
    goto 1
  end if
  !Initial allocations
  allocate(tmp%times(n_ini))
  allocate(tmp%values(n_ini))
  !Read data
  n= 0
  line= 1
  skipcols= -1
  do
    read(unit=un, fmt='(a)', iostat=ioresult, end=1000) string
    if (ioresult .ne. 0) then
      write(errmsg,'(a,i0,3(a))')"read error at line ",line," of file '", &
        trim(adjustl(file)),"'"
      call rwarn(errmsg)
      goto 1
    end if
    string= adjustl(string)
    ! Skip blank lines and comments
    if ((len_trim(string) .eq. 0) .or. (scan(string(1:1), comment) .ne. 0)) then
      continue
    ! Column headers
    else if (skipcols .lt. 0) then
      skipcols= pos(string, col) - 2
      if (skipcols .lt. 0) then
        write(errmsg,'(5(a))')"expecting to find column name '", &
          trim(adjustl(col)),"' in file '", &
          trim(adjustl(file)),"' at position 2 or higher"
        call rwarn(errmsg)
        goto 1
      end if
      allocate(garbage(skipcols))
    ! Numeric records
    else
      ! Increase array lengths, if necessary
      if ((n + 1) .gt. size(tmp%times)) then
        allocate(x%times(n))
        allocate(x%values(n))
        x%times= tmp%times
        x%values= tmp%values
        deallocate(tmp%times)
        deallocate(tmp%values)
        allocate(tmp%times(n*n_fac))
        allocate(tmp%values(n*n_fac))
        tmp%times(1:n)= x%times
        tmp%values(1:n)= x%values
        deallocate(x%times)
        deallocate(x%values)
      end if
      ! Extract the numbers
      n= n + 1
      read(unit=string, fmt=*, iostat=ioresult) tmp%times(n), garbage, tmp%values(n)
      if (ioresult .ne. 0) then
        write(errmsg,'(a,i0,3(a))')"read error at line ",line," of file '", &
          trim(adjustl(file)),"'"
        call rwarn(errmsg)
        goto 1
      end if
    end if
    ! Update line counter
    line= line + 1
  end do
  1000 continue
  if (n .lt. 2) then
    write(errmsg,'(3(a))')"found less than two records in file '", &
      trim(adjustl(file)),"'"
    call rwarn(errmsg)
    goto 1
  end if
  close(un)
  ! Set output data
  allocate(x%times(n))
  allocate(x%values(n))
  x%times= tmp%times(1:n)
  x%values= tmp%values(1:n)
  deallocate(tmp%times)
  deallocate(tmp%values)
  ! Check data
  if (any(x%times(2:n)-x%times(1:(n-1)) .le. ZERO)) then
    write(errmsg,'(3(a))')"times not strictly ascending in file '", &
      trim(adjustl(file)),"'"
    call rwarn(errmsg)
    goto 1
  end if
  ! Return
  readTS= .TRUE.
  return
  1 readTS= .FALSE.
  if (allocated(x%times)) deallocate(x%times)
  if (allocated(x%values)) deallocate(x%values)
  if (allocated(garbage)) deallocate(garbage)
end function

!###############################################################################
! Function to perform interpolation in a time series
! Argument 'lweight' is used as follows:
!  1: constant interpolation, full weight given to value at begin of interval
!  0: constant interpolation, full weight given to value at end of interval
!  <0 or >1: linear interpolation, weights set automatically

double precision function interpol(time, x, latest, lweight, na)
  ! args
  double precision, intent(in):: time ! query time
  type(TSeries), intent(in):: x       ! object of type TSeries
  integer, intent(inout):: latest     ! index of latest access (inout)
  integer, intent(in):: lweight       ! weight for begin of time interval
  double precision, intent(in):: na   ! return value on failure
  ! locals
  character(len=256):: errmsg
  integer:: i
  logical:: ok
  ! interpolation
  if (time .lt. x%times(1)) then
    call rwarn("interpolation failed: query time < time stamp of first record")
    interpol= na
  else if (time .gt. x%times(size(x%times))) then
    call rwarn("interpolation failed: query time > time stamp of final record")
    interpol= na
  else
    ok= .FALSE.
    ! Forward search
    if (time .ge. x%times(latest)) then
      do i=min(max(1,latest),size(x%times)-1), (size(x%times)-1)
        if ((time .ge. x%times(i)) .and. (time .le. x%times(i+1))) then
          if ((lweight .lt. 0) .or. (lweight .gt. 1)) then
            interpol= x%values(i) * (x%times(i+1) - time) / &
              (x%times(i+1) - x%times(i)) + &
              x%values(i+1) * (time - x%times(i)) / &
              (x%times(i+1) - x%times(i))
          else
            interpol= x%values(i) * dble(lweight) + &
              x%values(i+1) * dble(1 - lweight)
          end if
          latest= i
          ok= .TRUE.
          exit
        end if
      end do
    ! Backward search
    else
      do i=max(min(latest,size(x%times)), 2), 2, -1
        if ((time .ge. x%times(i-1)) .and. (time .le. x%times(i))) then
          if ((lweight .lt. 0) .or. (lweight .gt. 1)) then
            interpol= x%values(i-1) * (x%times(i) - time) / &
              (x%times(i) - x%times(i-1)) + &
              x%values(i) * (time - x%times(i-1)) / &
              (x%times(i) - x%times(i-1))
          else
            interpol= x%values(i-1) * dble(lweight) + &
              x%values(i) * dble(1 - lweight)
          end if
          latest= i
          ok= .TRUE.
          exit
        end if
      end do
    end if
    if (.not. ok) then
      call rwarn("interpolation failed: corrupted time series")
      interpol= na
    end if
  end if
end function

end module

