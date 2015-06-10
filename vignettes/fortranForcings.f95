!###############################################################################
!# PART 1: GENERIC CODE NOT REQUIRING CHANGES BY USER
!###############################################################################

module forcings
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
logical function readTS(file, nskip, x)
  implicit none
  ! args
  character(len=*), intent(in):: file    ! name of data file
  integer, intent(in):: nskip            ! number of lines to skip in file
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
    write(*,*)"cannot open file '",trim(adjustl(file)),"'"
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
      write(*,*)"read error at line ",line," of file '",trim(adjustl(file)),"'"
      goto 1
    end if
    string= adjustl(string)
    if ((len_trim(string) .gt. 0) .and. (line .gt. nskip)) then
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
      read(unit=string, fmt=*, iostat=ioresult) tmp%times(n), tmp%values(n)
      if (ioresult .ne. 0) then
        write(*,*)"read error at line ",line," of file '",trim(adjustl(file)),"'"
        goto 1
      end if
    end if
    line= line + 1
  end do
  1000 continue
  if (n .lt. 2) then
    write(*,*)"found less than two records in file '",trim(adjustl(file)),"'"
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
    write(*,*)"times not strictly ascending in file '",trim(adjustl(file)),"'"
    goto 1
  end if
  ! Return
  readTS= .TRUE.
  return
  1 readTS= .FALSE.
  if (allocated(x%times)) deallocate(x%times)
  if (allocated(x%values)) deallocate(x%values)
end function

!###############################################################################
! Function to perform interpolation in a time series
! Argument 'lweight' is used as follows:
!  1: constant interpolation, full weight given to value at begin of interval
!  0: constant interpolation, full weight given to value at end of interval
!  <0 or >1: linear interpolation, weights set automatically

double precision function interpol(time, x, latest, lweight, na)
  implicit none
  ! args
  double precision, intent(in):: time ! query time
  type(TSeries), intent(in):: x       ! object of type TSeries
  integer, intent(inout):: latest     ! index of latest access (inout)
  integer, intent(in):: lweight       ! weight for begin of time interval
  double precision, intent(in):: na   ! return value on failure
  ! locals
  integer:: i
  logical:: ok
  ! interpolation
  if (time .lt. x%times(1)) then
    write(*,*) "interpolation failed: query time < time stamp of first record"
    interpol= na
  else if (time .gt. x%times(size(x%times))) then
    write(*,*) "interpolation failed: query time > time stamp of final record"
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
      write(*,*) "interpolation failed: corrupted time series"
      interpol= na
    end if
  end if
end function

!###############################################################################
! Function to return the value of a forcing variable
double precision function forcing (time, lweight, na, file, nskip)
  ! args
  double precision, intent(in):: time    ! query time
  integer, intent(in):: lweight          ! weight for begin of time interval
  double precision, intent(in):: na      ! return value on failure
  character(len=*), intent(in):: file    ! text file with time series data
  integer, intent(in):: nskip            ! number of lines to skip in file
  ! constants
  ! local
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  ! init
  if (firstCall) then
    firstCall= .FALSE.
    if (.not. readTS(file, nskip, x)) then
      write(*,*) "initialization of forcing data from file '", &
        trim(adjustl(file)),"' failed"
      forcing= na
    end if
  end if
  ! interpolate
  if (allocated(x%times)) then
    forcing= interpol(time=time, x=x, latest=latest, lweight=lweight, na=na)
  else
    write(*,*) "forcing data not allocated"
    forcing= na
  end if
end function

end module


!###############################################################################
!# PART 2: MODULE WITH USER DEFINED FUNCTIONS
!###############################################################################

module functions
! Imports generic code for handling of forcings
use forcings

implicit none

contains

! Example of a user-defined forcing
! You need to define a function like this for every time-variable forcing. You
! must adjust:
!   1) The name of the function. A unique name is required for each forcing.
!   2) The value of 'lweight' (integer). Use 0 for constant interpolation with
!      full weight given to the value at the end of a time interval. Use 1 for
!      constant interpolation with full weight given to the value at the begin
!      of a time interval. Any other values (< 0 or > 1) result in linear
!      interpolation with weights being set automatically.
!   3) The name of the data file. Conventions for file contents:
!      - There must be two numeric columns; column 1: time, column 2: value.
!      - Times must be real numbers (e.g. unix time of offset from user datum).
!      - Columns are expected to be separated by comma, blank, or tabulator.
!      - Numeric data start in the first row unless nskip is set > 0.
!      - The file may contain blank lines, comment lines are NOT supported.
!   4) The value of 'nskip' (integer). This is the number of lines to skip
!      before reading numeric data from the file. Typical values are 0 or 1,
!      depending on whether column names are present.

function temperature (time) result (x)
  double precision, intent(in):: time
  ! ---- BEGIN OF ADJUSTABLE SETTINGS ----
  character(len=256), parameter:: file= "temperature.csv"
  integer, parameter:: nskip= 1
  integer, parameter:: lweight= -1
  ! ---- END OF ADJUSTABLE SETTINGS ----
  double precision, parameter:: NA= huge(0d0)
  character(len=512):: errmsg
  double precision:: x
  x= forcing(time=time, lweight=lweight, na=NA, file=file, nskip=nskip)
  if (x .eq. NA) then
    write(errmsg,'(3(a),f0.5)')"failed to retrieve forcing data from file '", &
      trim(adjustl(file)),"' for query time ",time
    call rexit(trim(adjustl(errmsg)))
  end if
end function

end module



