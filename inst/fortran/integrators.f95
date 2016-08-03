MODULE integrators

!This module contains routines for integration of a set of ODEs.
!All routines are adapted from "numerical recipes", vol 1 & 2 (fortran 77 & 90)

!Public entities
PUBLIC  rk5

!Private entities
PRIVATE rk5Step_variable
PRIVATE rk5Step_fixed

CONTAINS

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@ Given values for n variables y and their derivatives dydx known at x,
!@ use the fifth order Cash-Karp Runge-Kutta method to advance the solution over
!@ an interval h and return the incremented variables as yout. Also return an
!@ estimate of the local truncation error in yout using the embedded fourth order 
!@ method. The user supplies the subroutine, which returns derivatives dydx at x.
!@
!@ - This Routine is intended to be called by the stepper routine
!@   "rk5Step_variable". It is not intended for independed use!
!@ - Routine calls "derivs" that computes the righthand side of the ODEs
!@
!@ @@@@@ Changes to original version from numerical recipes @@@@@
!@ - The function 'derivs' that computes the righthand sides of the derivatives uses an additional
!@   input array which can be used to pass values for parameters which appear in the derivatives.
!@ - Routines from module "nrutil" have been replaced by normal statements or (in case
!@   of the error report routine) by an own routine
!@ @@@@@ Input @@@@@
!@ - y:     vector of n dependent variables y at x
!@ - dydx:  derivatives of y at x
!@ - x:     x-value, where y(x) and dydx(x) are known
!@ - h:     size of a step in x-direction
!@ - par:   vector with values of constants appearing in the derivatives
!@ - derivs: The dummy routine that evaluates the derivatives
!@ @@@@@ Output @@@@@
!@ - error: is set to false if an error occures in the routine
!@ - yout:  estimated values of y at (x+h)
!@ - yerr:  estimated error in yout
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE rk5Step_fixed(error,y,dydx,x,h,yout,yerr,par,derivs)
IMPLICIT NONE
!INTENT(IN)
real(8),dimension(:),intent(in) :: y,dydx,par
real(8),intent(in) :: x,h
INTERFACE
  SUBROUTINE derivs(t,u,par,dudt,error)
  implicit none
  real(8),intent(in):: t                 !IN: independend variable
  real(8),dimension(:),intent(in):: u    !IN: vector of dependend variables
  real(8),dimension(:),intent(in):: par  !IN: vector of constants
    logical,intent(out):: error              !OUT: error indicator
    real(8),dimension(:),intent(out):: dudt  !OUT: derivatives of dependend variables
  END SUBROUTINE derivs
END INTERFACE
!INTENT(OUT)
logical,intent(out):: error
real(8),dimension(:),intent(out) :: yout,yerr
!LOCAL
character(len=256):: errmsg=""
real(8),dimension(size(y)) :: ak2,ak3,ak4,ak5,ak6,ytemp
!Parameters for the Runge-Kutta method by Cash-Karp 
real(8),parameter:: A2=0.2_8,A3=0.3_8,A4=0.6_8,A5=1.0_8,&
  A6=0.875_8,B21=0.2_8,B31=3.0_8/40.0_8,B32=9.0_8/40.0_8,&
  B41=0.3_8,B42=-0.9_8,B43=1.2_8,B51=-11.0_8/54.0_8,&
  B52=2.5_8,B53=-70.0_8/27.0_8,B54=35.0_8/27.0_8,&
  B61=1631.0_8/55296.0_8,B62=175.0_8/512.0_8,&
  B63=575.0_8/13824.0_8,B64=44275.0_8/110592.0_8,&
  B65=253.0_8/4096.0_8,C1=37.0_8/378.0_8,&
  C3=250.0_8/621.0_8,C4=125.0_8/594.0_8,&
  C6=512.0_8/1771.0_8,DC1=C1-2825.0_8/27648.0_8,&
  DC3=C3-18575.0_8/48384.0_8,DC4=C4-13525.0_8/55296.0_8,&
  DC5=-277.0_8/14336.0_8,DC6=C6-0.25_8
!CODE
!Check argument sizes
if ((size(dydx).ne.size(y)).or.(size(yout).ne.size(y)).or.(size(yerr).ne.size(y))) then
  errmsg= "Dummy argument arrays not of equal size."
  goto 1
end if
errmsg= "Calculation of derivatives by external routine failed."
!First step
ytemp(:)= y(:)+B21*h*dydx(:)
!Second step
CALL derivs(x+A2*h,ytemp(:),par(:),ak2(:),error); if (error) goto 1
ytemp(:)= y(:)+h*(B31*dydx(:)+B32*ak2(:))
!Third step
CALL derivs(x+A3*h,ytemp(:),par(:),ak3(:),error); if (error) goto 1
ytemp(:)= y(:)+h*(B41*dydx(:)+B42*ak2(:)+B43*ak3(:))
!Fourth step
CALL derivs(x+A4*h,ytemp(:),par(:),ak4(:),error); if (error) goto 1
ytemp(:)= y(:)+h*(B51*dydx(:)+B52*ak2(:)+B53*ak3(:)+B54*ak4(:))
!Fifth step
CALL derivs(x+A5*h,ytemp(:),par(:),ak5(:),error); if (error) goto 1
ytemp(:)= y(:)+h*(B61*dydx(:)+B62*ak2(:)+B63*ak3(:)+B64*ak4(:)+B65*ak5(:))
!Sixth step
CALL derivs(x+A6*h,ytemp(:),par(:),ak6(:),error); if (error) goto 1
!Accumulate increments with proper weights
yout(:)= y(:)+h*(C1*dydx(:)+C3*ak3(:)+C4*ak4(:)+C6*ak6(:))
!Estimate error as difference between fourth and fifth order methods
yerr(:)= h*(DC1*dydx(:)+DC3*ak3(:)+DC4*ak4(:)+DC5*ak5(:)+DC6*ak6(:))
!Error handling
error= .false.
RETURN
1 error= .true.
CALL rwarn("rk5Step_fixed:" // trim(adjustl(errmsg)))
END SUBROUTINE rk5Step_fixed


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@ Fifth order Runge-Kutta step with monitoring of local truncation error to
!@ ensure accuracy and adjust stepsize (actual stepsize is determined automaticly).
!@
!@ - Routine is intended to be called by driver routine "rk5".
!@   It is not intended for independed use!
!@ - Routine calls "rk5Step_fixed".
!@
!@ @@@@@ Changes to original version from numerical recipes @@@@@
!@ - Routines from module "nrutil" have been replaced by normal statements or (in case
!@   of the error report routine) by an own routine
!@ @@@@@ Input @@@@@
!@ - y:     vector of dependent variable at starting value of x
!@ - dydx:  derivatives of y at starting value of x
!@ - x:     starting value of the independent variable x
!@ - htry:  stepsize to be attempted
!@ - eps:   required accuracy
!@ - yscal: vector against which the error is scaled
!@ - par:   vector with values of constants appearing in the derivatives
!@ - derivs: The dummy routine that evaluates the derivatives
!@ @@@@@ Output @@@@@
!@ - error: is set to false if an error occures in the routine
!@ - vector y is replaced by the new values
!@ - x is replaced by new value (x + hdid)
!@ - hdid: stepsize that was actually accomplished
!@ - hnext: estimated stepsize for the next step
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE rk5Step_variable &
             (error,y,dydx,x,htry,eps,yscal,hdid,hnext,par,derivs)
IMPLICIT NONE
!INTENT(IN)
double precision,dimension(:),intent(in) :: dydx,yscal,par
double precision,intent(in) :: htry,eps
INTERFACE
  SUBROUTINE derivs(t,u,par,dudt,error)
  implicit none
  double precision,intent(in):: t                 !IN: independend variable
  double precision,dimension(:),intent(in):: u    !IN: vector of dependend variables
  double precision,dimension(:),intent(in):: par  !IN: vector of constants
    logical,intent(out):: error              !OUT: error indicator
    double precision,dimension(:),intent(out):: dudt  !OUT: derivatives of dependend variables
  END SUBROUTINE derivs
END INTERFACE
!INTENT(INOUT)
double precision,dimension(:),intent(inout) :: y
double precision,intent(inout) :: x
!INTENT(OUT)
logical,intent(out):: error
double precision,intent(out) :: hdid,hnext
!LOCAL
character(len=256):: errmsg=""
double precision:: errmax,h,htemp,xnew
double precision,dimension(size(y)) :: yerr,ytemp
double precision,parameter:: SAFETY=0.9_8,PGROW=-0.2_8,PSHRNK=-0.25_8
double precision,parameter:: ERRCON=1.89e-4_8 !ERRCON equals (5/SAFETY)**(1/PGROW)
!CODE
!Check input arguments
if ((size(y) .ne. size(dydx)) .or. (size(y) .ne. size(yscal))) then
  errmsg= "Dummy argument arrays not of equal size."
  goto 1
end if
!Set stepsize to the initial trial value.
h=htry
do
  !Take a step using 5th-order runge-kutta-cash-karp.
  CALL rk5Step_fixed(error,y(:),dydx(:),x,h,ytemp(:),yerr(:),par(:),derivs)
  if (error) then
    errmsg= "Fixed stepsize integration returned invalid results."
    goto 1
  end if
  !Evaluate accuracy.
  errmax=maxval(abs(yerr(:)/yscal(:)))/eps
  !Exit loop if step succeeded.
  if (errmax .le. 1.0_8) then
    exit
  end if
  !Truncation error too large, reduce stepsize.
  htemp=SAFETY*h*(errmax**PSHRNK)
  !No more than a factor of 10.
  h=sign(max(abs(htemp),0.1_8*abs(h)),h)
  xnew=x+h
  if (xnew .eq. x) then
    errmsg= "Autocontrolled stepsize is zero (stepsize underflow)."
    goto 1
  end if
  !Go back for another try.
end do
!Compute size for the next step.
if (errmax .gt. ERRCON) then
  hnext=SAFETY*h*(errmax**PGROW)
else
  !No more than a factor of 5 increase.
  hnext=5.0_8*h
end if
hdid=h
x=x+h
y(:)= ytemp(:)
!Error handling
error= .false.
RETURN
1 error= .true.
CALL rwarn("rk5Step_variable:" // trim(adjustl(errmsg)))
END SUBROUTINE rk5Step_variable


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@ Runge-Kutta driver with adaptive stepsize control. Integrate the array of
!@ starting values ystart from x1 to x2 with accuracy eps.
!@
!@ - Routine calls the stepper routine "rk5Step_variable".
!@ - Routine calls "derivs" that computes the righthand side of the ODEs
!@
!@ @@@@@ Changes to original version from numerical recipes @@@@@
!!@ - The function 'derivs' that computes the righthand sides of the derivatives uses an additional
!@   input array which can be used to pass values for parameters which appear in the derivatives.
!@ - Routines from module "nrutil" have been replaced by normal statements or (in case
!@   of the error report routine) by an own routine
!@ - No storage of intermediate results in a separate module (part of code deleted).
!@
!@ @@@@@ Input @@@@@
!@ - ystart:  vector of dependent variable y at starting value of x=x1
!@ - x1:      starting value of x
!@ - x2:      target value of x
!@ - eps:     required accuracy
!@ - h1:      guessed first stepsize
!@ - hmin:    minimum allowed stepsize (can be zero)
!@ - par:     vector with values of constants appearing in the derivatives
!@ - derivs: The dummy routine that evaluates the derivatives
!@ @@@@@ Output @@@@@
!@ - error: is set to false if an error occures in the routine
!@ - values in ystart are replaced by values at the end of the integration interval
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE rk5(error,ystart,x1,x2,eps,h1,hmin,nmax,par,derivs)
IMPLICIT NONE
!INTENT(IN)
double precision,intent(in):: x1,x2,eps,h1,hmin
double precision,dimension(:),intent(in):: par
integer, intent(in):: nmax
INTERFACE
  SUBROUTINE derivs(t,u,par,dudt,error)
  implicit none
  double precision,intent(in):: t                 !IN: independend variable
  double precision,dimension(:),intent(in):: u    !IN: vector of dependend variables
  double precision,dimension(:),intent(in):: par  !IN: vector of constants
    logical,intent(out):: error              !OUT: error indicator
    double precision,dimension(:),intent(out):: dudt  !OUT: derivatives of dependend variables
  END SUBROUTINE derivs
END INTERFACE
!INTENT(INOUT)
double precision,dimension(:),intent(inout):: ystart
!INTENT(OUT)
logical,intent(out):: error
!LOCAL
double precision,parameter:: TINY=1.0e-30_8
integer:: nstp
double precision:: h,hdid,hnext,x
double precision,dimension(size(ystart)):: dydx,y,yscal
character(len=256):: errmsg=""
integer:: i
!CODE
!Initializations
x= x1
h= sign(h1,x2-x1)
y(:)= ystart(:)
!Take at most nmax steps.
do nstp=1,nmax  
  !Compute derivatives at the start of the step
  CALL derivs(x,y(:),par(:),dydx(:),error)
  if (error) then
    errmsg= "Calculation of derivatives by external routine failed."
    goto 1
  end if
  !Scaling used to monitor accuracy. This general purpose choice can be modified.
  yscal(:)=abs(y(:))+abs(h*dydx(:))+TINY
  !If stepsize can overshoot, decrease.
  if ((x+h-x2)*(x+h-x1) .gt. 0.0_8) then
    h=x2-x
  end if  
  !Make a step with error dependend stepsize
  CALL rk5Step_variable(error,y,dydx,x,h,eps,yscal,hdid,hnext,par(:),derivs)
  if (error) then
    errmsg= "Error dependend stepsize integration returned invalid results."
    goto 1
  end if
  !Are we done?
  if ((x-x2)*(x2-x1) .ge. 0.0_8) then
    ystart(:)= y(:)
    !Normal exit.
    goto 1000
  end if
  if (abs(hnext) .lt. hmin) then
    errmsg= "Stepsize falls below lower limit specified in 'hmin'."
    goto 1
  end if
  h=hnext
end do
errmsg= "Number of iterations exceeds the limit specified in 'nmax'."
goto 1
1000 continue
!Error handling
error= .false.
RETURN
1 error= .true.
CALL rwarn("rk5:" // trim(adjustl(errmsg)))
END SUBROUTINE rk5

END MODULE integrators
