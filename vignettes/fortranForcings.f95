module functions
implicit none

!###############################################################################
! A dedicated data type for time series
type TSeries
  double precision, dimension(:), allocatable:: times, values
end type TSeries

contains

!###############################################################################
! Function to read time series data from an ASCII text file. It is called when a
! forcing item is queried for the first time.
! Notes:
! - data must be in two columns; column 1: time, column 2: value
! - times must be real numbers (e.g. unix time of offset from user datum)
! - columns are expected to be separated by space or tabulator 
! - a header line (column names) must NOT be present
logical function readTS(file, x)
  implicit none
  ! args
  character(len=*), intent(in):: file    ! name of data file
  type(TSeries), intent(out):: x         ! object of type TSeries
  ! const
  integer, parameter:: un=10
  integer, parameter:: n_ini=1, n_fac=2
  double precision, parameter:: ZERO=0d0
  ! locals
  type(TSeries):: tmp
  integer:: line, ioresult, n
  character(len=256):: string

  !Open file
  open(unit=un, file=file, status="old", action="read", iostat=ioresult)
  if (ioresult .ne. 0) then
    write(*,*)"cannot open file '",trim(file),"'"
    goto 1
  end if
  !Initial allocations
  allocate(tmp%times(n_ini))
  allocate(tmp%values(n_ini))
  !Read data
  n= 0
  line= 1
  do
    read(unit=un, fmt='(a)', iostat=ioresult, end=1000) string
    if (ioresult .ne. 0) then
      write(*,*)"read error at line ",line," of file '",trim(file),"'"
      goto 1
    end if      
    string= adjustl(string)
    ! Increase array lengths, if necessary
    if (len_trim(string) .gt. 0) then
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
      read(unit=string, fmt=*, iostat=ioresult) tmp%times(n), tmp%values(n)
      if (ioresult .ne. 0) then
        write(*,*)"read error at line ",line," of file '",trim(file),"'"
        goto 1
      end if
    end if
    line= line + 1
  end do
  1000 continue
  if (n .lt. 2) then
    write(*,*)"found less than two records in file '",trim(file),"'"
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
    write(*,*)"times not strictly ascending in file '",trim(file),"'"
    goto 1
  end if
  ! Return
  readTS= .TRUE.
  return
  1 readTS= .FALSE.
  deallocate(x%times)
  deallocate(x%values)
end function

!###############################################################################
! Function to perform linear interpolation in a time series
double precision function linInt(time, x, latest, na)
  implicit none
  ! args
  double precision, intent(in):: time    ! query time
  type(TSeries), intent(in):: x          ! object of type TSeries
  integer, intent(inout):: latest        ! index of latest access (modifyable)
  double precision, intent(in):: na      ! return value on failure
  ! const
  double precision, parameter:: SMALL=1d-20
  ! locals
  double precision:: interval
  integer:: i
  logical:: ok
  ! interpolation
  if (time .lt. x%times(1)) then
    write(*,*)"interpolation failed: query time < time stamp of first record"
    linInt= na
  else if (time .gt. x%times(size(x%times))) then
    write(*,*)"interpolation failed: query time > time stamp of final record"
    linInt= na
  else
    ok= .FALSE.
    do i=min(max(1,latest),size(x%times)-1), (size(x%times)-1)
      if ((time .ge. x%times(i)) .and. (time .le. x%times(i+1))) then
        interval= x%times(i+1) - x%times(i)
        linInt= x%values(i) * (x%times(i+1) - time) / interval + &
                x%values(i+1) * (time - x%times(i)) / interval
        latest= i
        ok= .TRUE.
        exit
      end if
    end do
    if (.not. ok) then
      write(*,*)"interpolation failed: corrupted time series"
      linInt= na
    end if
  end if
end function

!###############################################################################
! Function to return perform linear interpolation in a time series
function forcing (time, na) result (value)
  ! args
  double precision, intent(in):: time    ! query time
  double precision, intent(in):: na      ! return value on failure
  ! constants
  character(len=256), parameter:: file="exampleTimeSeries.txt" ! to be adjusted
  ! local
  double precision:: value
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  ! init
  if (firstCall) then
    firstCall= .FALSE.
    if (.not. readTS(file, x)) then
      write(*,*) "initialization of forcing data failed"
    end if
  end if
  ! interpolate
  if (allocated(x%times)) then
    value= linInt(time=time, x=x, latest=latest, na=na)
  else
    write(*,*) "no forcing data to interpolate"
    value= na
  end if
end function

end module

!###############################################################################
! A test program for external use - uncomment this, if necessary
!program test
!use functions
!implicit none
!  write(*,*) forcing(time=2.4d0, na=-9999d0)
!  write(*,*) forcing(time=2.6d0, na=-9999d0)
!  write(*,*) forcing(time=2.8d0, na=-9999d0)
!end program

