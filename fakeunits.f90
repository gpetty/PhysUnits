!---------------------------------------------------------------------------
! Fortran 90 Module 'FAKEUNITS'
! Copyright (C) 2002  Grant W. Petty
!
! Written by Grant W. Petty and Michael A. Walters
!
! This module is a dummy stand-in for PHYSUNITS, to be used to help users 
! determine the amount of computational overhead associated with that
! module
! __________________________________________________________________________


MODULE fakeunits

  IMPLICIT NONE
!!$  INTEGER, PRIVATE, PARAMETER :: Wprec = Selected_Real_Kind(P=12) ! Double precision
  INTEGER, PRIVATE, PARAMETER :: Wprec = SELECTED_REAL_KIND(P=6)
  INTEGER, PRIVATE, PARAMETER :: NUM_BASE_UNITS = 8

  TYPE preal
     REAL(Wprec) :: value
  END TYPE preal

  CHARACTER (len=3) :: unit_labels(NUM_BASE_UNITS)

!------------------------------------------------------------------------------------------------
! INTERFACES FOR ALGEBRAIC OPERATORS
!------------------------------------------------------------------------------------------------
  PRIVATE :: pplus1, pplus2, pplus3, pplus4, pplus5, pplus6, pplus7, pplus8
  PRIVATE :: pminus1, pminus2, pminus3, pminus4, pminus5, pminus6, pminus7, pminus8
  PRIVATE :: ptimes1, ptimes2, ptimes3, ptimes4, ptimes5, ptimes6, ptimes7
  PRIVATE :: pdiv1, pdiv2, pdiv3, pdiv4, pdiv5, pdiv6, pdiv7
  PRIVATE :: pexp1, pexp2, pexp3, pexp4, pexp5, pexp6, pexp7
  PRIVATE :: pgt1, pgt2, pgt3, pgt4, pgt5, pgt6, pgt7
  PRIVATE :: pge1, pge2, pge3, pge4, pge5, pge6, pge7
  PRIVATE :: peq1, peq2, peq3, peq4, peq5, peq6, peq7
  PRIVATE :: ple1, ple2, ple3, ple4, ple5, ple6, ple7
  PRIVATE :: plt1, plt2, plt3, plt4, plt5, plt6, plt7
  PRIVATE :: pne1, pne2, pne3, pne4, pne5, pne6, pne7
  PRIVATE :: passign1, passign2, passign3, passign4, passign5, passign6, passign7
  PRIVATE :: pabs, psqrt
  PRIVATE :: prealtoint, prealtonint, prealtoaint, prealtoanint, prealtoreal, prealtodble
  PRIVATE :: prealout1, prealout2, prealout3, prealout4
  PRIVATE :: pout1, pout2, pout3, pout4, pout5, pout6, pout7, pout8, pout9, pout10, &
             pout11, pout12
  
  INTERFACE OPERATOR (+)
     MODULE PROCEDURE pplus1
     MODULE PROCEDURE pplus2
     MODULE PROCEDURE pplus3
     MODULE PROCEDURE pplus4
     MODULE PROCEDURE pplus5
     MODULE PROCEDURE pplus6
     MODULE PROCEDURE pplus7
     MODULE PROCEDURE pplus8
  END INTERFACE

  INTERFACE OPERATOR (-)
     MODULE PROCEDURE pminus1
     MODULE PROCEDURE pminus2
     MODULE PROCEDURE pminus3
     MODULE PROCEDURE pminus4
     MODULE PROCEDURE pminus5
     MODULE PROCEDURE pminus6
     MODULE PROCEDURE pminus7
     MODULE PROCEDURE pminus8
  END INTERFACE

  INTERFACE OPERATOR (*)
     MODULE PROCEDURE ptimes1
     MODULE PROCEDURE ptimes2
     MODULE PROCEDURE ptimes3
     MODULE PROCEDURE ptimes4
     MODULE PROCEDURE ptimes5
     MODULE PROCEDURE ptimes6
     MODULE PROCEDURE ptimes7
  END INTERFACE

  INTERFACE OPERATOR (/)
     MODULE PROCEDURE pdiv1
     MODULE PROCEDURE pdiv2
     MODULE PROCEDURE pdiv3
     MODULE PROCEDURE pdiv4
     MODULE PROCEDURE pdiv5
     MODULE PROCEDURE pdiv6
     MODULE PROCEDURE pdiv7
  END INTERFACE

  INTERFACE OPERATOR (**)
     MODULE PROCEDURE pexp1
     MODULE PROCEDURE pexp2
     MODULE PROCEDURE pexp3
     MODULE PROCEDURE pexp4
     MODULE PROCEDURE pexp5
     MODULE PROCEDURE pexp6
     MODULE PROCEDURE pexp7
  END INTERFACE

  INTERFACE OPERATOR (.GT.)
     MODULE PROCEDURE pgt1
     MODULE PROCEDURE pgt2
     MODULE PROCEDURE pgt3
     MODULE PROCEDURE pgt4
     MODULE PROCEDURE pgt5
     MODULE PROCEDURE pgt6
     MODULE PROCEDURE pgt7
  END INTERFACE

  INTERFACE OPERATOR (.GE.)
     MODULE PROCEDURE pge1
     MODULE PROCEDURE pge2
     MODULE PROCEDURE pge3
     MODULE PROCEDURE pge4
     MODULE PROCEDURE pge5
     MODULE PROCEDURE pge6
     MODULE PROCEDURE pge7
  END INTERFACE

  INTERFACE OPERATOR (.EQ.)
     MODULE PROCEDURE peq1
     MODULE PROCEDURE peq2
     MODULE PROCEDURE peq3
     MODULE PROCEDURE peq4
     MODULE PROCEDURE peq5
     MODULE PROCEDURE peq6
     MODULE PROCEDURE peq7
  END INTERFACE

  INTERFACE OPERATOR (.LE.)
     MODULE PROCEDURE ple1
     MODULE PROCEDURE ple2
     MODULE PROCEDURE ple3
     MODULE PROCEDURE ple4
     MODULE PROCEDURE ple5
     MODULE PROCEDURE ple6
     MODULE PROCEDURE ple7
  END INTERFACE

  INTERFACE OPERATOR (.LT.)
     MODULE PROCEDURE plt1
     MODULE PROCEDURE plt2
     MODULE PROCEDURE plt3
     MODULE PROCEDURE plt4
     MODULE PROCEDURE plt5
     MODULE PROCEDURE plt6
     MODULE PROCEDURE plt7
  END INTERFACE

  INTERFACE OPERATOR (.NE.)
     MODULE PROCEDURE pne1
     MODULE PROCEDURE pne2
     MODULE PROCEDURE pne3
     MODULE PROCEDURE pne4
     MODULE PROCEDURE pne5
     MODULE PROCEDURE pne6
     MODULE PROCEDURE pne7
  END INTERFACE

  INTERFACE ASSIGNMENT (=)
     MODULE PROCEDURE passign1
     MODULE PROCEDURE passign2
     MODULE PROCEDURE passign3
     MODULE PROCEDURE passign4
     MODULE PROCEDURE passign5
     MODULE PROCEDURE passign6
     MODULE PROCEDURE passign7
  END INTERFACE

  INTERFACE ABS
     MODULE PROCEDURE pabs
  END INTERFACE

  INTERFACE SQRT
     MODULE PROCEDURE psqrt
  END INTERFACE

  INTERFACE PINT
     MODULE PROCEDURE paint
  END INTERFACE

  INTERFACE PNINT
     MODULE PROCEDURE panint
  END INTERFACE

  INTERFACE INT
     MODULE PROCEDURE prealtoint
  END INTERFACE

  INTERFACE NINT
     MODULE PROCEDURE prealtonint
  END INTERFACE

  INTERFACE AINT
     MODULE PROCEDURE prealtoaint
  END INTERFACE

  INTERFACE ANINT
     MODULE PROCEDURE prealtoanint
  END INTERFACE

  INTERFACE REAL
     MODULE PROCEDURE prealtoreal
  END INTERFACE

  INTERFACE DBLE
     MODULE PROCEDURE prealtodble
  END INTERFACE

  INTERFACE nounits
     MODULE PROCEDURE prealtoreal
  END INTERFACE

  INTERFACE nondim
     MODULE PROCEDURE prealtoreal
  END INTERFACE

  INTERFACE prealout
     MODULE PROCEDURE prealout1
     MODULE PROCEDURE prealout2
     MODULE PROCEDURE prealout3
     MODULE PROCEDURE prealout4
  END INTERFACE

  INTERFACE pout
     MODULE PROCEDURE pout1
     MODULE PROCEDURE pout2
     MODULE PROCEDURE pout3
     MODULE PROCEDURE pout4
     MODULE PROCEDURE pout5
     MODULE PROCEDURE pout6
     MODULE PROCEDURE pout7
     MODULE PROCEDURE pout8
     MODULE PROCEDURE pout9
     MODULE PROCEDURE pout10
     MODULE PROCEDURE pout11
     MODULE PROCEDURE pout12
  END INTERFACE

CONTAINS

!---------------------------------------------------
! Implementation of '+' operator
!--------------------------------------------------

! Case:  PREAL + INTEGER = REAL
  FUNCTION pplus1(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    REAL :: sum
    sum = x1%value + x2
  END FUNCTION pplus1

! Case:  INTEGER + PREAL = REAL
  FUNCTION pplus2(x1,x2) RESULT(sum)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: sum
    sum = x1 + x2%value
  END FUNCTION pplus2

! Case:  PREAL + REAL = REAL
  FUNCTION pplus3(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    REAL :: sum
    sum = x1%value + x2
  END FUNCTION pplus3

! Case:  REAL + PREAL = REAL
  FUNCTION pplus4(x1,x2) RESULT(sum)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: sum
    sum = x1 + x2%value
  END FUNCTION pplus4

! Case:  PREAL + DOUBLE = DOUBLE
  FUNCTION pplus5(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    DOUBLE PRECISION :: sum
    sum = x1%value + x2
  END FUNCTION pplus5

! Case:  DOUBLE + PREAL = DOUBLE
  FUNCTION pplus6(x1,x2) RESULT(sum)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    DOUBLE PRECISION :: sum
    sum = x1 + x2%value
  END FUNCTION pplus6

! Case:  PREAL + PREAL = PREAL
  FUNCTION pplus7(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: sum
    sum = preal(x1%value + x2%value)
  END FUNCTION pplus7

! Case: + PREAL
  FUNCTION pplus8(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2 = preal(x1%value)
  END FUNCTION pplus8


!---------------------------------------------------
! Implementation of '-' operator
!--------------------------------------------------

! Case:  PREAL - INTEGER = REAL
  FUNCTION pminus1(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    REAL :: diff
    diff = x1%value - x2
  END FUNCTION pminus1

! Case:  INTEGER - PREAL = REAL
  FUNCTION pminus2(x1,x2) RESULT (diff)
    INTEGER,INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: diff
    diff = x1 - x2%value
  END FUNCTION pminus2

! Case:  PREAL - REAL = REAL
  FUNCTION pminus3(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    REAL :: diff
    diff = x1%value - x2
  END FUNCTION pminus3

! Case:  REAL - PREAL = REAL
  FUNCTION pminus4(x1,x2) RESULT(diff)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: diff
    diff = x1 - x2%value
  END FUNCTION pminus4

! Case:  PREAL - DOUBLE = DOUBLE 
  FUNCTION pminus5(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    DOUBLE PRECISION :: diff
    diff = x1%value - x2
  END FUNCTION pminus5

! Case:  DOUBLE - PREAL = DOUBLE
  FUNCTION pminus6(x1,x2) RESULT(diff)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    DOUBLE PRECISION :: diff
    diff = x1 - x2%value
  END FUNCTION pminus6

! Case:  PREAL - PREAL = PREAL
  FUNCTION pminus7(x1,x2) RESULT (diff)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: diff
    diff = preal(x1%value - x2%value)
  END FUNCTION pminus7

! Case: - PREAL (negation)
  FUNCTION pminus8(x) RESULT(neg)
    TYPE(preal), INTENT(in) :: x
    TYPE(preal) :: neg
    neg = preal(- x%value)
  END FUNCTION pminus8


!---------------------------------------------------
! Implementation of '*' operator
!--------------------------------------------------

! Case:  PREAL * REAL = PREAL
  FUNCTION ptimes1(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    TYPE(preal) :: product
    product%value = x1%value * x2
  END FUNCTION ptimes1

! Case:  REAL * PREAL = PREAL
  FUNCTION ptimes2(x1,x2) RESULT(product)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
  END FUNCTION ptimes2

! Case:  PREAL * DOUBLE = PREAL
  FUNCTION ptimes3(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: product
    product%value = x1%value * x2
  END FUNCTION ptimes3

! Case:  DOUBLE * PREAL = PREAL
  FUNCTION ptimes4(x1,x2) RESULT(product)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
  END FUNCTION ptimes4

! Case:  PREAL * INTEGER = PREAL
  FUNCTION ptimes5(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    TYPE(preal) :: product
    product%value = x1%value * x2
  END FUNCTION ptimes5

! Case:  INTEGER * PREAL = PREAL
  FUNCTION ptimes6(x1,x2) RESULT(product)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
  END FUNCTION ptimes6

! Case:  PREAL * PREAL = PREAL
  FUNCTION ptimes7(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: product
    product%value = x1%value * x2%value
  END FUNCTION ptimes7


!---------------------------------------------------
! Implementation of '/' operator
!--------------------------------------------------

! Case:  PREAL / REAL = PREAL
  FUNCTION pdiv1(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2
  END FUNCTION pdiv1

! Case:  REAL / PREAL = PREAL
  FUNCTION pdiv2(x1,x2) RESULT(quotient)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
  END FUNCTION pdiv2

! Case:  PREAL / DOUBLE = PREAL
  FUNCTION pdiv3(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2
  END FUNCTION pdiv3

! Case:  DOUBLE / PREAL = PREAL
  FUNCTION pdiv4(x1,x2) RESULT(quotient)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
  END FUNCTION pdiv4

! Case:  PREAL / INTEGER = PREAL
  FUNCTION pdiv5(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2
  END FUNCTION pdiv5

! Case:  INTEGER / PREAL = PREAL
  FUNCTION pdiv6(x1,x2) RESULT(quotient)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
  END FUNCTION pdiv6

! Case:  PREAL / PREAL = PREAL
  FUNCTION pdiv7(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2%value
  END FUNCTION pdiv7


!---------------------------------------------------
! Implementation of '**' operator
!--------------------------------------------------

! Case:  PREAL ** INTEGER = PREAL
  FUNCTION pexp1(x1,i2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: i2
    TYPE(preal) :: power
    power%value = (x1%value)**i2
  END FUNCTION pexp1
  
! Case:  INTEGER ** PREAL = REAL (conditioned on PREAL = non-dimensional)
  FUNCTION pexp2(i1,x2) RESULT(power)
    INTEGER, INTENT(in) :: i1
    TYPE(preal), INTENT(in) ::  x2
    REAL :: power
    power = i1**x2%value
  END FUNCTION pexp2
  
! Case:  PREAL ** REAL = PREAL 
  FUNCTION pexp3(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    TYPE(preal) :: power
    power%value = x1%value ** x2
  END FUNCTION pexp3
  
! Case:  REAL ** PREAL = REAL (conditioned on PREAL = non-dimensional)
  FUNCTION pexp4(x1,x2) RESULT(power)
    REAL, INTENT(in) :: x1
    TYPE (preal), INTENT(in) :: x2
    REAL :: power
    power = x1**x2%value
  END FUNCTION pexp4
  
! Case:  PREAL ** DOUBLE = PREAL 
  FUNCTION pexp5(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: power
    power%value = x1%value ** x2
  END FUNCTION pexp5
  
! Case:  DOUBLE ** PREAL = DOUBLE (conditioned on PREAL = non-dimensional)

  FUNCTION pexp6(x1,x2) RESULT(power)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE (preal), INTENT(in) :: x2
    DOUBLE PRECISION :: power
    power = x1**x2%value
  END FUNCTION pexp6
   
! Case:  PREAL ** PREAL = PREAL 
! Note - second argument must be non-dimensional.
  FUNCTION pexp7(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: power
    power%value = x1%value ** x2%value
  END FUNCTION pexp7
  

!---------------------------------------------------
! Implementation of assignment operator '=' 
!--------------------------------------------------

! Case:  REAL = PREAL
  SUBROUTINE passign1(x1,x2)
    REAL, INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    x1 = x2%value
  END SUBROUTINE passign1

! Case:  PREAL = REAL
  SUBROUTINE passign2(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    REAL, INTENT(in) :: x2
    x1%value = x2
  END SUBROUTINE passign2

! Case:  PREAL = PREAL
  SUBROUTINE passign3(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    x1%value = x2%value
  END SUBROUTINE passign3

! Case:  PREAL = INTEGER
  SUBROUTINE passign4(x1,i2)
    TYPE(preal), INTENT(out) :: x1
    INTEGER, INTENT(in) :: i2
    x1%value = i2
  END SUBROUTINE passign4

! Case:  INTEGER = PREAL
  SUBROUTINE passign5(i1,x2)
    INTEGER, INTENT(out) :: i1
    TYPE(preal), INTENT(in) :: x2
    i1 = x2%value
  END SUBROUTINE passign5

! Case:  DOUBLE = PREAL
  SUBROUTINE passign6(x1,x2)
    DOUBLE PRECISION, INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    x1 = x2%value
  END SUBROUTINE passign6

! Case:  PREAL = DOUBLE
  SUBROUTINE passign7(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    x1%value = x2
  END SUBROUTINE passign7


!---------------------------------------------------
! Implementation of '.gt.' operator
!--------------------------------------------------

! Case:  PREAL .gt. INTEGER ?
  FUNCTION pgt1(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: greater
    greater = (x1%value .GT. x2)
  END FUNCTION pgt1

! Case:  INTEGER .gt. PREAL ?
  FUNCTION pgt2(x1,x2) RESULT(greater)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: greater
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt2

! Case:  PREAL .gt. REAL ?
  FUNCTION pgt3(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: greater
    greater = (x1%value .GT. x2)
  END FUNCTION pgt3

! Case:  REAL .gt. PREAL ?
  FUNCTION pgt4(x1,x2) RESULT(greater)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: greater
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt4

! Case:  PREAL .gt. DOUBLE ?
  FUNCTION pgt5(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: greater
    greater = (x1%value .GT. x2)
  END FUNCTION pgt5

! Case:  DOUBLE .gt. PREAL ?
  FUNCTION pgt6(x1,x2) RESULT(greater)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: greater
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt6

! Case:  PREAL .gt. PREAL ?
  FUNCTION pgt7(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: greater
    greater = (x1%value .GT. x2%value)
  END FUNCTION pgt7


!---------------------------------------------------
! Implementation of '.ge.' operator
!--------------------------------------------------

! Case:  PREAL .ge. INTEGER ?
  FUNCTION pge1(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: g_or_e
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge1

! Case:  INTEGER .ge. PREAL ?
  FUNCTION pge2(x1,x2) RESULT(g_or_e)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge2

! Case:  PREAL .ge. REAL ?
  FUNCTION pge3(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: g_or_e
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge3

! Case:  REAL .ge. PREAL ?
  FUNCTION pge4(x1,x2) RESULT(g_or_e)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge4

! Case:  PREAL .ge. DOUBLE ?
  FUNCTION pge5(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: g_or_e
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge5

! Case:  DOUBLE PRECISION .ge. PREAL ?
  FUNCTION pge6(x1,x2) RESULT(g_or_e)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge6

! Case:  PREAL .ge. PREAL ?
  FUNCTION pge7(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: g_or_e
    g_or_e = (x1%value .GE. x2%value)
  END FUNCTION pge7


!---------------------------------------------------
! Implementation of '.eq.' operator
!--------------------------------------------------

! Case:  PREAL .eq. INTEGER ?
  FUNCTION peq1(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1%value .EQ. x2)
  END FUNCTION peq1

! Case:  INTEGER .eq. PREAL ?
  FUNCTION peq2(x1,x2) RESULT(equal)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq2

! Case:  PREAL .eq. REAL ?
  FUNCTION peq3(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1%value .EQ. x2)
  END FUNCTION peq3

! Case:  REAL .eq. PREAL ?
  FUNCTION peq4(x1,x2) RESULT(equal)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq4

! Case:  PREAL .eq. DOUBLE ?
  FUNCTION peq5(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1%value .EQ. x2)
  END FUNCTION peq5

! Case:  DOUBLE .eq. PREAL ?
  FUNCTION peq6(x1,x2) RESULT(equal)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq6

! Case:  PREAL .eq. PREAL ?
  FUNCTION peq7(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: equal
    equal = (x1%value .EQ. x2%value)
  END FUNCTION peq7


!---------------------------------------------------
! Implementation of '.le.' operator
!--------------------------------------------------

! Case:  PREAL .le. INTEGER ?
  FUNCTION ple1(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: l_or_e
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple1

! Case:  INTEGER .le. PREAL ?
  FUNCTION ple2(x1,x2) RESULT(l_or_e)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple2

! Case:  PREAL .le. REAL ?
  FUNCTION ple3(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: l_or_e
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple3

! Case:  REAL .le. PREAL ?
  FUNCTION ple4(x1,x2) RESULT(l_or_e)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple4

! Case:  PREAL .le. DOUBLE ?
  FUNCTION ple5(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: l_or_e
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple5

! Case:  DOUBLE .le. PREAL ?
  FUNCTION ple6(x1,x2) RESULT(l_or_e)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple6

! Case:  PREAL .le. PREAL ?
  FUNCTION ple7(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: l_or_e
    l_or_e = (x1%value .LE. x2%value)
  END FUNCTION ple7


!---------------------------------------------------
! Implementation of '.lt.' operator
!--------------------------------------------------

! Case:  PREAL .lt. INTEGER ?
  FUNCTION plt1(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: less
    less = (x1%value .LT. x2)
  END FUNCTION plt1

! Case:  INTEGER .lt. PREAL ?
  FUNCTION plt2(x1,x2) RESULT(less)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: less
    less = (x1 .LT. x2%value)
  END FUNCTION plt2

! Case:  PREAL .lt. REAL ?
  FUNCTION plt3(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: less
    less = (x1%value .LT. x2)
  END FUNCTION plt3

! Case:  REAL .lt. PREAL ?
  FUNCTION plt4(x1,x2) RESULT(less)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: less
    less = (x1 .LT. x2%value)
  END FUNCTION plt4

! Case:  PREAL .lt. DOUBLE ?
  FUNCTION plt5(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: less
    less = (x1%value .LT. x2)
  END FUNCTION plt5

! Case:  DOUBLE .lt. PREAL ?
  FUNCTION plt6(x1,x2) RESULT(less)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: less
    less = (x1 .LT. x2%value)
  END FUNCTION plt6

! Case:  PREAL .lt. PREAL ?
  FUNCTION plt7(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: less
    less = (x1%value .LT. x2%value)
  END FUNCTION plt7


!---------------------------------------------------
! Implementation of '.ne.' operator
!--------------------------------------------------

! Case:  PREAL .ne. INTEGER ?
  FUNCTION pne1(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: notequal
    notequal = (x1%value .NE. x2)
  END FUNCTION pne1

! Case:  INTEGER .ne. PREAL ?
  FUNCTION pne2(x1,x2) RESULT(notequal)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne2

! Case:  PREAL .ne. REAL ?
  FUNCTION pne3(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: notequal
    notequal = (x1%value .NE. x2)
  END FUNCTION pne3

! Case:  REAL .ne. PREAL ?
  FUNCTION pne4(x1,x2) RESULT(notequal)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne4

! Case:  PREAL .ne. DOUBLE ?
  FUNCTION pne5(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: notequal
    notequal = (x1%value .NE. x2)
  END FUNCTION pne5

! Case:  DOUBLE .ne. PREAL ?
  FUNCTION pne6(x1,x2) RESULT(notequal)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne6

! Case:  PREAL .ne. PREAL ?
  FUNCTION pne7(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: notequal
    notequal = (x1%value .NE. x2%value)
  END FUNCTION pne7


!---------------------------------------------------------------------------
! Interface operator for intrinsic function ABS.
!---------------------------------------------------------------------------

  FUNCTION pabs(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = ABS(x1%value)
  END FUNCTION pabs

!---------------------------------------------------------------------------
! Interface operator for intrinsic function SQRT.
!---------------------------------------------------------------------------

  FUNCTION psqrt(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = SQRT(x1%value)
  END FUNCTION psqrt

!---------------------------------------------------------------------------
! Operator for functions PINT and PAINT.
!---------------------------------------------------------------------------

  FUNCTION paint(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = AINT(x1%value)
  END FUNCTION paint

!---------------------------------------------------------------------------
! Operator for functions PNINT and PANINT.
!---------------------------------------------------------------------------

  FUNCTION panint(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = ANINT(x1%value)
  END FUNCTION panint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function INT.
!---------------------------------------------------------------------------

  FUNCTION prealtoint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    INTEGER :: y
    y = INT(x%value)
  END FUNCTION prealtoint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function NINT.
!---------------------------------------------------------------------------

  FUNCTION prealtonint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    INTEGER :: y
    y = NINT(x%value)
  END FUNCTION prealtonint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function AINT.
!---------------------------------------------------------------------------

  FUNCTION prealtoaint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y
    y = AINT(x%value)
  END FUNCTION prealtoaint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function ANINT.
!---------------------------------------------------------------------------

  FUNCTION prealtoanint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y
    y = ANINT(x%value)
  END FUNCTION prealtoanint

!--------------------------------------------------------------------------
! Function to convert PREAL to REAL  -- used to convert non-dimensional 
! arguments of type PREAL to something that can be passed to transcendental
! functions and dimension-unaware subroutines
!--------------------------------------------------------------------------

  FUNCTION prealtoreal(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y
    y = real(x%value)
  END FUNCTION prealtoreal

!---------------------------------------------------------------------------
! Interface operator for intrinsic function DBLE.
!---------------------------------------------------------------------------

  FUNCTION prealtodble(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    DOUBLE PRECISION :: y
    y = dble(x%value)
  END FUNCTION prealtodble

!---------------------------------------------------------------------------
! Function to convert PREAL to an ordinary array of real numbers for output.
!---------------------------------------------------------------------------

  FUNCTION prealout1(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y(1)
    y(1) = x%value
  END FUNCTION prealout1

  FUNCTION prealout2(x) RESULT(y)
    DOUBLE PRECISION, INTENT(in) :: x
    REAL :: y(1)
    y(1) = x
  END FUNCTION prealout2

  FUNCTION prealout3(x) RESULT(y)
    REAL, INTENT(in) :: x
    REAL :: y(1)
    y(1) = x
  END FUNCTION prealout3

  FUNCTION prealout4(x) RESULT(y)
    INTEGER, INTENT(in) :: x
    REAL :: y(1)
    y(1) = REAL(x)
  END FUNCTION prealout4

!---------------------------------------------------------------------------
! Subroutine for labeled output.
!---------------------------------------------------------------------------

  SUBROUTINE pout1(x)
    TYPE(preal), INTENT(in) :: x
    CHARACTER (len=120) :: string = ''
    WRITE (string, *) x%value
    string = ADJUSTL(string)
    WRITE (*,*) TRIM(string)
  END SUBROUTINE pout1

  SUBROUTINE pout2(unit, x)
    INTEGER, INTENT(in) :: unit
    TYPE(preal), INTENT(in) :: x
    CHARACTER (len=120) :: string = ''
    WRITE (string, *) x%value
    string = ADJUSTL(string)
    WRITE (unit,*) TRIM(string)
  END SUBROUTINE pout2

  SUBROUTINE pout3(string, x)
    CHARACTER (len=*), INTENT(out) :: string
    TYPE(preal), INTENT(in) :: x
    string = ''
    WRITE (string, *) x%value
    string = ADJUSTL(string)
  END SUBROUTINE pout3

  SUBROUTINE pout4(x)
    DOUBLE PRECISION, INTENT(in) :: x
    WRITE (*,*) x
  END SUBROUTINE pout4

  SUBROUTINE pout5(unit, x)
    INTEGER, INTENT(in) :: unit
    DOUBLE PRECISION, INTENT(in) :: x
    WRITE (unit,*) x
  END SUBROUTINE pout5

  SUBROUTINE pout6(string, x)
    CHARACTER (len=*), INTENT(out) :: string
    DOUBLE PRECISION, INTENT(in) :: x
    string = ''
    WRITE (string,*) x
  END SUBROUTINE pout6

  SUBROUTINE pout7(x)
    REAL, INTENT(in) :: x
    WRITE (*,*) x
  END SUBROUTINE pout7

  SUBROUTINE pout8(unit, x)
    INTEGER, INTENT(in) :: unit
    REAL, INTENT(in) :: x
    WRITE (unit,*) x
  END SUBROUTINE pout8

  SUBROUTINE pout9(string, x)
    CHARACTER (len=*), INTENT(out) :: string
    REAL, INTENT(in) :: x
    string = ''
    WRITE (string,*) x
  END SUBROUTINE pout9

  SUBROUTINE pout10(x)
    INTEGER, INTENT(in) :: x
    WRITE (*,*) x
  END SUBROUTINE pout10

  SUBROUTINE pout11(unit, x)
    INTEGER, INTENT(in) :: unit
    INTEGER, INTENT(in) :: x
    WRITE (unit,*) x
  END SUBROUTINE pout11

  SUBROUTINE pout12(string, x)
    CHARACTER (len=*), INTENT(out) :: string
    INTEGER, INTENT(in) :: x
    string = ''
    WRITE (string,*) x
  END SUBROUTINE pout12

END MODULE fakeunits
