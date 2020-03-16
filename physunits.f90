!--------------------------------------------------------------------------------
!PHYSUNITS package - automated computation of dimensions and units in scientific programs
!Copyright (C) 2002  Grant W. Petty
!
!This library is free software; you can redistribute it and/or
!modify it under the terms of the GNU Lesser General Public
!License as published by the Free Software Foundation; either
!version 2.1 of the License, or (at your option) any later version.
!
!This library is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!Lesser General Public License for more details.
!
!You should have received a copy of the GNU Lesser General Public
!License along with this library; if not, write to the Free Software
!Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
!Author contact:  Prof. Grant W. Petty
!                 Atmospheric and Oceanic Sciences
!                 University of Wisconsin-Madison
!                 1225 W. Dayton St.
!                 Madison, WI 53711
!                 gpetty@aos.wisc.edu
!--------------------------------------------------------------------------
!
! Fortran 90 Module 'PHYSUNITS'    Version 1.10
!
! Written by Grant W. Petty and Michael A. Walters
!    with suggestions by Howard W. Ludwig 
!         correction  by Michael Janeschitz-Kriegl
!
! The purpose of this module is to confer awareness of physical
! dimensions and units on scientific programs written in Fortran 90.
! It defines the basic data types and operators, as well as initializing 
! the values of SI and common non-SI units and physical constants.  
!
! The motivation behind this module is explained in the following paper:
!
! Petty, G.W., 2001: Automated computation and consistency checking of
! physical dimensions and units in scientific programs.  Software -
! Practice and Experience (Volume 31, Issue 11, 19 June 2001) 
! 
! ------------------------------------------------------------------------
!  
! You are encouraged to submit improvements and bug fixes to the author for 
! possible incorporation into the standard distribution of this module. See 
! examples of desired improvements below.
!
! --------------------------------------------------------------------------
!  REVISION HISTORY
!  Version 1.10 (10/31/05)     Corrected error in constant definition u_sqkm
!
!  Version 1.9 (7/31/02)       Expanded POUT routines to allow writing to character
!                                 variables.
!                              Added interfaces for REAL and DBLE, and changed
!                                 interfaces for INT, NINT, AINT, and ANINT.  These
!                                 functions can now be used to convert between type 
!                                 PREAL and other variable types.  For rounding
!                                 type PREAL, use PINT, PNINT, PAINT, and PANINT.
!                                 These functions return type PREAL; their names are
!                                 formed by putting P in front of INT, NINT, etc.
!                                 (Previously, they were named INT, NINT, etc.)
!                                 Because of these changes, functions like NOUNITS
!                                 (or NONDIM) are no longer necessary. - MW
!
!  Version 1.8 (6/26/02)       No significant changes to this file.
!                              Added fakeunits.f90 and dummy_units.f90 to package 
!                                 for testing overhead.
!                              Fixed bug in physunits_test.f90.  Also added check of
!                                 some constants, so physunits_test.out has changed.
!                              Fixed underflow problems and several typos in SI_units
!                              Added some viscosity units suggested by Edouard Canot.
!
!  Version 1.7 (6/17/02)       Split module into two parts: PHYSUNITS handles the
!                                 algebra and SI_UNITS defines the units and 
!                                 constants.  Note that SI_UNITS "uses" PHYSUNITS.
!                                 This division makes it easier for users to
!                                 substitute a different set of units.
!                              Added POUT subroutines for improved output. - MW
!
!  Version 1.6 (6/13/02)       Physical constants now start with "c_" instead of 
!                                 "u_". Changed 0_s to PARAMETER "zero" to make
!                                 module compatible with Intel compilers. - MW
!
!  Version 1.5 (6/11/02)       Added more constants of nature.
!                              Added numerous abbreviations for more common units.
!                              Since Fortran is usually case insensitive, it's not
!                                 possible to have an abbreviation for every unit. 
!                                 - MW
!
!  Version 1.4 (7/27/01)       Added support for using operators with DOUBLE 
!                                 PRECISION variables. - MW
!
!  Version 1.3beta  (5/7/01)   Added SAVE attribute to units not initialized in
!                                 module; this probably won't matter for most 
!                                 systems, but it's there just in case.  Removed 
!                                 testdim subroutines because they were no longer 
!                                 needed.
!                              Added u_solar_mass. - MW
!
!  Version 1.2beta  (5/7/01)   Made operator functions private. Reorganized for
!                                 easier reading. - MW
!
!  Version 1.1beta  (5/2/01)   Added support for intrinsic functions sqrt, int,
!                                 nint, aint, and anint.  Moved definitions of
!                              derived units back to module. - MW
!
!  Version 1.0beta  (4/24/01)  Removed definitions of derived units to separate
!                                 routine initialized at runtime.  
!                              Also made several internal variables private 
!                                 to avoid name conflicts.
!
!  Version 0.3 (4/24/01)  -   Merged in Michael Walters' changes, including
!
!                                Added support for non-integer
!                                dimensional power, while maintaining
!                                efficiency of integer representations
!
!                                Added support include .eq., .gt., .lt., etc.
!
!  Version 0.2  (3/27/01)  -  Revised internal TESTDIM code for portability - GWP
!
!  Version 0.1  (2/17/01)  -  Revised declaration of type PREAL for greater portability and 
!                             flexibility, following recommendations of an anonymous reviewer.
!
!  Version 0.0  (7/17/00)  -  Prototype implementation by Grant W. Petty
!
! --------------------------------------------------------------------------
! DESIRED IN FUTURE VERSIONS (Please contribute!)
! 
! Support for efficient storage of arrays of type PREAL
! Enhancements to improve numerical efficiency
! Add-on modules with discipline-specific units and physical constants
! Improved run-time error handling 
! Anything improving the adherence of this module to standard Fortran 90
!    programming practice.
! __________________________________________________________________________


MODULE physunits

  IMPLICIT NONE
!!$  INTEGER, PRIVATE, PARAMETER :: Wprec = Selected_Real_Kind(P=12) ! Double precision
  INTEGER, PRIVATE, PARAMETER :: Wprec = SELECTED_REAL_KIND(P=6)
  INTEGER, PRIVATE, PARAMETER :: Short = SELECTED_INT_KIND(R=4)
  INTEGER, PRIVATE, PARAMETER :: NUM_BASE_UNITS = 8
  INTEGER(Short), PARAMETER :: zero = 0

  TYPE preal
     REAL(Wprec) :: value
     INTEGER(Short) :: units(NUM_BASE_UNITS)
  END TYPE preal

! The following value of base = 1*2*3*4*5*6 is chosen to be evenly
! divisible by many small integers so as to minimize roundoff errors
! associated with common non-integer powers of base dimensions

  INTEGER(Short), PARAMETER :: base = 1*2*3*4*5*6  ! Base denominator for fractional dimensions
  INTEGER(Short), PRIVATE, PARAMETER :: tol = 10   ! Tolerance for round off error in units of 
!                                                       base fraction
!!$! Use only integer dimensions
!!$  INTEGER(Short), PRIVATE, PARAMETER :: base = 1   ! Base denominator for fractional dimensions
!!$  INTEGER(Short), PRIVATE, PARAMETER :: tol = 0    ! Tolerance for round off error in units of 
!!$!                                                       base fraction

  LOGICAL, PRIVATE, PARAMETER :: unitcheck = .true. ! Check units of arguements to INT, REAL, etc.

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

!!$  INTERFACE PAINT
!!$     MODULE PROCEDURE paint
!!$  END INTERFACE
!!$
!!$  INTERFACE PANINT
!!$     MODULE PROCEDURE panint
!!$  END INTERFACE

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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to INTEGER'  
    ENDIF
    sum = x1%value + x2
  END FUNCTION pplus1

! Case:  INTEGER + PREAL = REAL
  FUNCTION pplus2(x1,x2) RESULT(sum)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: sum
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to INTEGER'  
    ENDIF
    sum = x1 + x2%value
  END FUNCTION pplus2

! Case:  PREAL + REAL = REAL
  FUNCTION pplus3(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    REAL :: sum
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to REAL'  
    ENDIF
    sum = x1%value + x2
  END FUNCTION pplus3

! Case:  REAL + PREAL = REAL
  FUNCTION pplus4(x1,x2) RESULT(sum)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: sum
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to REAL'  
    ENDIF
    sum = x1 + x2%value
  END FUNCTION pplus4

! Case:  PREAL + DOUBLE = DOUBLE
  FUNCTION pplus5(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    DOUBLE PRECISION :: sum
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to DOUBLE PRECISION'  
    ENDIF
    sum = x1%value + x2
  END FUNCTION pplus5

! Case:  DOUBLE + PREAL = DOUBLE
  FUNCTION pplus6(x1,x2) RESULT(sum)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    DOUBLE PRECISION :: sum
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to add dimensioned PREAL to DOUBLE PRECISION'  
    ENDIF
    sum = x1 + x2%value
  END FUNCTION pplus6

! Case:  PREAL + PREAL = PREAL
  FUNCTION pplus7(x1,x2) RESULT(sum)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: sum
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units   ! This was needed to prevent a segmentation fault
                                   ! on our system, but I don't know why.
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to add dimensionally inconsistent values of type PREAL'  
    ENDIF
    sum = preal( x1%value + x2%value, x1%units)
!    sum = preal( x1%value + x2%value, x1%units - deltad/2)  ! Alternate formula that
                                                             ! averages dimensions
  END FUNCTION pplus7

! Case: + PREAL
  FUNCTION pplus8(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2 = preal(x1%value, x1%units)
  END FUNCTION pplus8


!---------------------------------------------------
! Implementation of '-' operator
!--------------------------------------------------

! Case:  PREAL - INTEGER = REAL
  FUNCTION pminus1(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    REAL :: diff
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to subtract INTEGER from dimensioned PREAL'  
    ENDIF
    diff = x1%value - x2
  END FUNCTION pminus1

! Case:  INTEGER - PREAL = REAL
  FUNCTION pminus2(x1,x2) RESULT (diff)
    INTEGER,INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: diff
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to subtract dimensioned PREAL from INTEGER'  
    ENDIF
    diff = x1 - x2%value
  END FUNCTION pminus2

! Case:  PREAL - REAL = REAL
  FUNCTION pminus3(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    REAL :: diff
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to subtract REAL from dimensioned PREAL'  
    ENDIF
    diff = x1%value - x2
  END FUNCTION pminus3

! Case:  REAL - PREAL = REAL
  FUNCTION pminus4(x1,x2) RESULT(diff)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    REAL :: diff
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to subtract dimensioned PREAL from REAL'  
    ENDIF
    diff = x1 - x2%value
  END FUNCTION pminus4

! Case:  PREAL - DOUBLE = DOUBLE 
  FUNCTION pminus5(x1,x2) RESULT(diff)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    DOUBLE PRECISION :: diff
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to subtract DOUBLE PRECISION from dimensioned PREAL'  
    ENDIF
    diff = x1%value - x2
  END FUNCTION pminus5

! Case:  DOUBLE - PREAL = DOUBLE
  FUNCTION pminus6(x1,x2) RESULT(diff)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    DOUBLE PRECISION :: diff
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to subtract dimensioned PREAL from DOUBLE PRECISION'  
    ENDIF
    diff = x1 - x2%value
  END FUNCTION pminus6

! Case:  PREAL - PREAL = PREAL
  FUNCTION pminus7(x1,x2) RESULT (diff)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: diff
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units    ! This was needed to prevent a segmentation fault
                                    ! on our system, but I don't know why.
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to subtract dimensionally inconsistent values of type PREAL'  
    ENDIF
    diff = preal(x1%value - x2%value, x1%units)
!    diff = preal(x1%value - x2%value, x1%units -  deltad/2)  ! Alternate formula that averages dimensions
  END FUNCTION pminus7

! Case: - PREAL (negation)
  FUNCTION pminus8(x) RESULT(neg)
    TYPE(preal), INTENT(in) :: x
    TYPE(preal) :: neg
    neg = preal(- x%value, x%units)
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
    product%units = x1%units
  END FUNCTION ptimes1

! Case:  REAL * PREAL = PREAL
  FUNCTION ptimes2(x1,x2) RESULT(product)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
    product%units = x2%units
  END FUNCTION ptimes2

! Case:  PREAL * DOUBLE = PREAL
  FUNCTION ptimes3(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: product
    product%value = x1%value * x2
    product%units = x1%units
  END FUNCTION ptimes3

! Case:  DOUBLE * PREAL = PREAL
  FUNCTION ptimes4(x1,x2) RESULT(product)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
    product%units = x2%units
  END FUNCTION ptimes4

! Case:  PREAL * INTEGER = PREAL
  FUNCTION ptimes5(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    TYPE(preal) :: product
    product%value = x1%value * x2
    product%units = x1%units
  END FUNCTION ptimes5

! Case:  INTEGER * PREAL = PREAL
  FUNCTION ptimes6(x1,x2) RESULT(product)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    TYPE(preal) :: product
    product%value = x1 * x2%value
    product%units = x2%units
  END FUNCTION ptimes6

! Case:  PREAL * PREAL = PREAL
  FUNCTION ptimes7(x1,x2) RESULT(product)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: product
    product%value = x1%value * x2%value
    product%units = x1%units + x2%units
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
    quotient%units = x1%units
  END FUNCTION pdiv1

! Case:  REAL / PREAL = PREAL
  FUNCTION pdiv2(x1,x2) RESULT(quotient)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
    quotient%units = - x2%units
  END FUNCTION pdiv2

! Case:  PREAL / DOUBLE = PREAL
  FUNCTION pdiv3(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2
    quotient%units = x1%units
  END FUNCTION pdiv3

! Case:  DOUBLE / PREAL = PREAL
  FUNCTION pdiv4(x1,x2) RESULT(quotient)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
    quotient%units = -x2%units
  END FUNCTION pdiv4

! Case:  PREAL / INTEGER = PREAL
  FUNCTION pdiv5(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2
    quotient%units = x1%units
  END FUNCTION pdiv5

! Case:  INTEGER / PREAL = PREAL
  FUNCTION pdiv6(x1,x2) RESULT(quotient)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    TYPE(preal) :: quotient
    quotient%value = x1 / x2%value
    quotient%units = -x2%units
  END FUNCTION pdiv6

! Case:  PREAL / PREAL = PREAL
  FUNCTION pdiv7(x1,x2) RESULT(quotient)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: quotient
    quotient%value = x1%value / x2%value
    quotient%units = x1%units - x2%units
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
    power%units = i2*x1%units
  END FUNCTION pexp1
  
! Case:  INTEGER ** PREAL = REAL (conditioned on PREAL = non-dimensional)
  FUNCTION pexp2(i1,x2) RESULT(power)
    INTEGER, INTENT(in) :: i1
    TYPE(preal), INTENT(in) ::  x2
    REAL :: power
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Exponent must be non-dimensional number'  
    ENDIF
    power = i1**x2%value
  END FUNCTION pexp2
  
! Case:  PREAL ** REAL = PREAL 
  FUNCTION pexp3(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    TYPE(preal) :: power
    REAL :: num(NUM_BASE_UNITS)
    num = REAL(x1%units) * x2
    power%units = NINT(num,kind=Short)
    power%value = x1%value ** x2
  END FUNCTION pexp3
  
! Case:  REAL ** PREAL = REAL (conditioned on PREAL = non-dimensional)
  FUNCTION pexp4(x1,x2) RESULT(power)
    REAL, INTENT(in) :: x1
    TYPE (preal), INTENT(in) :: x2
    REAL :: power
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Exponent must be non-dimensional number'  
    ENDIF
    power = x1**x2%value
  END FUNCTION pexp4
  
! Case:  PREAL ** DOUBLE = PREAL 
  FUNCTION pexp5(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    TYPE(preal) :: power
    DOUBLE PRECISION :: num(NUM_BASE_UNITS)
    num = DBLE(x1%units) * x2
    power%units = NINT(num,kind=Short)
    power%value = x1%value ** x2
  END FUNCTION pexp5
  
! Case:  DOUBLE ** PREAL = DOUBLE (conditioned on PREAL = non-dimensional)

  FUNCTION pexp6(x1,x2) RESULT(power)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE (preal), INTENT(in) :: x2
    DOUBLE PRECISION :: power
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Exponent must be non-dimensional number'  
    ENDIF
    power = x1**x2%value
  END FUNCTION pexp6
   
! Case:  PREAL ** PREAL = PREAL 
! Note - second argument must be non-dimensional.
  FUNCTION pexp7(x1,x2) RESULT(power)
    TYPE(preal), INTENT(in) :: x1, x2
    TYPE(preal) :: power
    REAL :: num(NUM_BASE_UNITS)
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Exponent must be non-dimensional number'  
    ENDIF
    num = REAL(x1%units) * x2%value
    power%units = NINT(num,kind=Short)
    power%value = x1%value ** x2%value
  END FUNCTION pexp7
  

!---------------------------------------------------
! Implementation of assignment operator '=' 
!--------------------------------------------------

! Case:  REAL = PREAL
  SUBROUTINE passign1(x1,x2)
    REAL, INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to assign dimensioned PREAL value to type REAL'  
    ENDIF
    x1 = x2%value
  END SUBROUTINE passign1

! Case:  PREAL = REAL
  SUBROUTINE passign2(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    REAL, INTENT(in) :: x2
    x1%value = x2
    x1%units = (/ zero, zero, zero, zero, zero, zero, zero, zero /)
  END SUBROUTINE passign2

! Case:  PREAL = PREAL
  SUBROUTINE passign3(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    x1%value = x2%value
    x1%units = x2%units
  END SUBROUTINE passign3

! Case:  PREAL = INTEGER
  SUBROUTINE passign4(x1,i2)
    TYPE(preal), INTENT(out) :: x1
    INTEGER, INTENT(in) :: i2
    x1%value = i2
    x1%units = (/ zero, zero, zero, zero, zero, zero, zero, zero /)
  END SUBROUTINE passign4

! Case:  INTEGER = PREAL
  SUBROUTINE passign5(i1,x2)
    INTEGER, INTENT(out) :: i1
    TYPE(preal), INTENT(in) :: x2
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to assign dimensioned PREAL value to type INTEGER'  
    ENDIF
    i1 = x2%value
  END SUBROUTINE passign5

! Case:  DOUBLE = PREAL
  SUBROUTINE passign6(x1,x2)
    DOUBLE PRECISION, INTENT(out) :: x1
    TYPE(preal), INTENT(in) :: x2
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to assign dimensioned PREAL value to type DOUBLE PRECISION'  
    ENDIF
    x1 = x2%value
  END SUBROUTINE passign6

! Case:  PREAL = DOUBLE
  SUBROUTINE passign7(x1,x2)
    TYPE(preal), INTENT(out) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    x1%value = x2
    x1%units = (/ zero, zero, zero, zero, zero, zero, zero, zero /)
  END SUBROUTINE passign7


!---------------------------------------------------
! Implementation of '.gt.' operator
!--------------------------------------------------

! Case:  PREAL .gt. INTEGER ?
  FUNCTION pgt1(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    INTEGER, INTENT(in) :: x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    greater = (x1%value .GT. x2)
  END FUNCTION pgt1

! Case:  INTEGER .gt. PREAL ?
  FUNCTION pgt2(x1,x2) RESULT(greater)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt2

! Case:  PREAL .gt. REAL ?
  FUNCTION pgt3(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    greater = (x1%value .GT. x2)
  END FUNCTION pgt3

! Case:  REAL .gt. PREAL ?
  FUNCTION pgt4(x1,x2) RESULT(greater)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt4

! Case:  PREAL .gt. DOUBLE ?
  FUNCTION pgt5(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    greater = (x1%value .GT. x2)
  END FUNCTION pgt5

! Case:  DOUBLE .gt. PREAL ?
  FUNCTION pgt6(x1,x2) RESULT(greater)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: greater
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    greater = (x1 .GT. x2%value)
  END FUNCTION pgt6

! Case:  PREAL .gt. PREAL ?
  FUNCTION pgt7(x1,x2) RESULT(greater)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: greater
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge1

! Case:  INTEGER .ge. PREAL ?
  FUNCTION pge2(x1,x2) RESULT(g_or_e)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge2

! Case:  PREAL .ge. REAL ?
  FUNCTION pge3(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: g_or_e
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge3

! Case:  REAL .ge. PREAL ?
  FUNCTION pge4(x1,x2) RESULT(g_or_e)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge4

! Case:  PREAL .ge. DOUBLE ?
  FUNCTION pge5(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: g_or_e
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    g_or_e = (x1%value .GE. x2)
  END FUNCTION pge5

! Case:  DOUBLE PRECISION .ge. PREAL ?
  FUNCTION pge6(x1,x2) RESULT(g_or_e)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: g_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    g_or_e = (x1 .GE. x2%value)
  END FUNCTION pge6

! Case:  PREAL .ge. PREAL ?
  FUNCTION pge7(x1,x2) RESULT(g_or_e)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: g_or_e
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    equal = (x1%value .EQ. x2)
  END FUNCTION peq1

! Case:  INTEGER .eq. PREAL ?
  FUNCTION peq2(x1,x2) RESULT(equal)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq2

! Case:  PREAL .eq. REAL ?
  FUNCTION peq3(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: equal
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    equal = (x1%value .EQ. x2)
  END FUNCTION peq3

! Case:  REAL .eq. PREAL ?
  FUNCTION peq4(x1,x2) RESULT(equal)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq4

! Case:  PREAL .eq. DOUBLE ?
  FUNCTION peq5(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: equal
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    equal = (x1%value .EQ. x2)
  END FUNCTION peq5

! Case:  DOUBLE .eq. PREAL ?
  FUNCTION peq6(x1,x2) RESULT(equal)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: equal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    equal = (x1 .EQ. x2%value)
  END FUNCTION peq6

! Case:  PREAL .eq. PREAL ?
  FUNCTION peq7(x1,x2) RESULT(equal)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: equal
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple1

! Case:  INTEGER .le. PREAL ?
  FUNCTION ple2(x1,x2) RESULT(l_or_e)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple2

! Case:  PREAL .le. REAL ?
  FUNCTION ple3(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: l_or_e
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple3

! Case:  REAL .le. PREAL ?
  FUNCTION ple4(x1,x2) RESULT(l_or_e)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple4

! Case:  PREAL .le. DOUBLE ?
  FUNCTION ple5(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: l_or_e
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    l_or_e = (x1%value .LE. x2)
  END FUNCTION ple5

! Case:  DOUBLE .le. PREAL ?
  FUNCTION ple6(x1,x2) RESULT(l_or_e)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: l_or_e
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    l_or_e = (x1 .LE. x2%value)
  END FUNCTION ple6

! Case:  PREAL .le. PREAL ?
  FUNCTION ple7(x1,x2) RESULT(l_or_e)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: l_or_e
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    less = (x1%value .LT. x2)
  END FUNCTION plt1

! Case:  INTEGER .lt. PREAL ?
  FUNCTION plt2(x1,x2) RESULT(less)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) :: x2
    LOGICAL :: less
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    less = (x1 .LT. x2%value)
  END FUNCTION plt2

! Case:  PREAL .lt. REAL ?
  FUNCTION plt3(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: less
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    less = (x1%value .LT. x2)
  END FUNCTION plt3

! Case:  REAL .lt. PREAL ?
  FUNCTION plt4(x1,x2) RESULT(less)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: less
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    less = (x1 .LT. x2%value)
  END FUNCTION plt4

! Case:  PREAL .lt. DOUBLE ?
  FUNCTION plt5(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: less
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    less = (x1%value .LT. x2)
  END FUNCTION plt5

! Case:  DOUBLE .lt. PREAL ?
  FUNCTION plt6(x1,x2) RESULT(less)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: less
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    less = (x1 .LT. x2%value)
  END FUNCTION plt6

! Case:  PREAL .lt. PREAL ?
  FUNCTION plt7(x1,x2) RESULT(less)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: less
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
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
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    notequal = (x1%value .NE. x2)
  END FUNCTION pne1

! Case:  INTEGER .ne. PREAL ?
  FUNCTION pne2(x1,x2) RESULT(notequal)
    INTEGER, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and INTEGER'  
    ENDIF
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne2

! Case:  PREAL .ne. REAL ?
  FUNCTION pne3(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1
    REAL, INTENT(in) :: x2
    LOGICAL :: notequal
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    notequal = (x1%value .NE. x2)
  END FUNCTION pne3

! Case:  REAL .ne. PREAL ?
  FUNCTION pne4(x1,x2) RESULT(notequal)
    REAL, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and REAL'  
    ENDIF
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne4

! Case:  PREAL .ne. DOUBLE ?
  FUNCTION pne5(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1
    DOUBLE PRECISION, INTENT(in) :: x2
    LOGICAL :: notequal
    IF (MAXVAL(ABS(x1%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    notequal = (x1%value .NE. x2)
  END FUNCTION pne5

! Case:  DOUBLE .ne. PREAL ?
  FUNCTION pne6(x1,x2) RESULT(notequal)
    DOUBLE PRECISION, INTENT(in) :: x1
    TYPE(preal), INTENT(in) ::  x2
    LOGICAL :: notequal
    IF (MAXVAL(ABS(x2%units)) .GT. tol) THEN
       STOP 'Attempt to compare dimensioned PREAL and DOUBLE PRECISION'  
    ENDIF
    notequal = (x1 .NE. x2%value)
  END FUNCTION pne6

! Case:  PREAL .ne. PREAL ?
  FUNCTION pne7(x1,x2) RESULT(notequal)
    TYPE(preal), INTENT(in) :: x1, x2
    LOGICAL :: notequal
    INTEGER(Short) :: deltad(NUM_BASE_UNITS)
    deltad = x1%units - x2%units
    IF (MAXVAL(ABS(deltad)) .GT. tol) THEN
       STOP 'Attempt to compare dimensionally inconsistent values of type PREAL'  
    ENDIF
    notequal = (x1%value .NE. x2%value)
  END FUNCTION pne7


!---------------------------------------------------------------------------
! Interface operator for intrinsic function ABS.
!---------------------------------------------------------------------------

  FUNCTION pabs(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = ABS(x1%value)
    x2%units = x1%units
  END FUNCTION pabs

!---------------------------------------------------------------------------
! Interface operator for intrinsic function SQRT.
!---------------------------------------------------------------------------

  FUNCTION psqrt(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = SQRT(x1%value)
    x2%units = x1%units / 2.0
  END FUNCTION psqrt

!---------------------------------------------------------------------------
! Operator for functions PINT and PAINT.
!---------------------------------------------------------------------------

  FUNCTION paint(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = AINT(x1%value)
    x2%units = x1%units
  END FUNCTION paint

!---------------------------------------------------------------------------
! Operator for functions PNINT and PANINT.
!---------------------------------------------------------------------------

  FUNCTION panint(x1) RESULT(x2)
    TYPE(preal), INTENT(in) :: x1
    TYPE(preal) :: x2
    x2%value = ANINT(x1%value)
    x2%units = x1%units
  END FUNCTION panint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function INT.
!---------------------------------------------------------------------------

  FUNCTION prealtoint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    INTEGER :: y
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to INT should be non-dimensional' 
    y = INT(x%value)
  END FUNCTION prealtoint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function NINT.
!---------------------------------------------------------------------------

  FUNCTION prealtonint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    INTEGER :: y
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to NINT should be non-dimensional' 
    y = NINT(x%value)
  END FUNCTION prealtonint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function AINT.
!---------------------------------------------------------------------------

  FUNCTION prealtoaint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to AINT should be non-dimensional' 
    y = AINT(x%value)
  END FUNCTION prealtoaint

!---------------------------------------------------------------------------
! Interface operator for intrinsic function ANINT.
!---------------------------------------------------------------------------

  FUNCTION prealtoanint(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to ANINT should be non-dimensional' 
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
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to REAL should be non-dimensional'
    y = real(x%value)
  END FUNCTION prealtoreal

!---------------------------------------------------------------------------
! Interface operator for intrinsic function DBLE.
!---------------------------------------------------------------------------

  FUNCTION prealtodble(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    DOUBLE PRECISION :: y
    IF (unitcheck .AND. (MAXVAL(ABS(x%units)) .GT. tol)) &
         STOP 'PREAL argument to DBLE should be non-dimensional'
    y = dble(x%value)
  END FUNCTION prealtodble

!---------------------------------------------------------------------------
! Function to convert PREAL to an ordinary array of real numbers for output.
!---------------------------------------------------------------------------

  FUNCTION prealout1(x) RESULT(y)
    TYPE(preal), INTENT(in) :: x
    REAL :: y(NUM_BASE_UNITS + 1)
    y(1) = x%value
    y(2:(NUM_BASE_UNITS + 1)) = REAL(x%units) / REAL(base)
  END FUNCTION prealout1

  FUNCTION prealout2(x) RESULT(y)
    DOUBLE PRECISION, INTENT(in) :: x
    REAL :: y(NUM_BASE_UNITS + 1)
    y(1) = x
    y(2:(NUM_BASE_UNITS + 1)) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
  END FUNCTION prealout2

  FUNCTION prealout3(x) RESULT(y)
    REAL, INTENT(in) :: x
    REAL :: y(NUM_BASE_UNITS + 1)
    y(1) = x
    y(2:(NUM_BASE_UNITS + 1)) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
  END FUNCTION prealout3

  FUNCTION prealout4(x) RESULT(y)
    INTEGER, INTENT(in) :: x
    REAL :: y(NUM_BASE_UNITS + 1)
    y(1) = REAL(x)
    y(2:(NUM_BASE_UNITS + 1)) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
  END FUNCTION prealout4

!---------------------------------------------------------------------------
! Subroutine for labeled output.
!---------------------------------------------------------------------------

  SUBROUTINE pout1(x)
    TYPE(preal), INTENT(in) :: x
    REAL :: dims(NUM_BASE_UNITS)
    CHARACTER (len=8) :: units = ''
    CHARACTER (len=120) :: string = ''
    INTEGER :: i
    WRITE (string, *) x%value
    string = ADJUSTL(string)
    dims = REAL(x%units) / REAL(base)
    DO i = 1, NUM_BASE_UNITS
       IF (ABS(dims(i)) .LT. REAL(tol)/REAL(base)) CYCLE
       IF (dims(i) .EQ. 1.0) THEN
          string = TRIM(string) // ' ' // unit_labels(i)
       ELSE
          WRITE (units, '(F8.3)') dims(i)
          string = TRIM(string) // ' ' // TRIM(unit_labels(i)) // '^' &
                // TRIM(ADJUSTL(units))
       ENDIF
    ENDDO
    WRITE (*,*) TRIM(string)
  END SUBROUTINE pout1

  SUBROUTINE pout2(unit, x)
    INTEGER, INTENT(in) :: unit
    TYPE(preal), INTENT(in) :: x
    REAL :: dims(NUM_BASE_UNITS)
    CHARACTER (len=8) :: units = ''
    CHARACTER (len=120) :: string = ''
    INTEGER :: i
    WRITE (string, *) x%value
    string = ADJUSTL(string)
    dims = REAL(x%units) / REAL(base)
    DO i = 1, NUM_BASE_UNITS
       IF (ABS(dims(i)) .LT. REAL(tol)/REAL(base)) CYCLE
       IF (dims(i) .EQ. 1.0) THEN
          string = TRIM(string) // ' ' // unit_labels(i)
       ELSE
          WRITE (units, '(F8.3)') dims(i)
          string = TRIM(string) // ' ' // TRIM(unit_labels(i)) // '^' &
                // TRIM(ADJUSTL(units))
       ENDIF
    ENDDO
    WRITE (unit,*) TRIM(string)
  END SUBROUTINE pout2

  SUBROUTINE pout3(string, x)
    CHARACTER (len=*), INTENT(out) :: string
    TYPE(preal), INTENT(in) :: x
    REAL :: dims(NUM_BASE_UNITS)
    CHARACTER (len=8) :: units = ''
    INTEGER :: i
    string = ''
    WRITE (string, *) x%value
    string = ADJUSTL(string)
    dims = REAL(x%units) / REAL(base)
    DO i = 1, NUM_BASE_UNITS
       IF (ABS(dims(i)) .LT. REAL(tol)/REAL(base)) CYCLE
       IF (dims(i) .EQ. 1.0) THEN
          string = TRIM(string) // ' ' // unit_labels(i)
       ELSE
          WRITE (units, '(F8.3)') dims(i)
          string = TRIM(string) // ' ' // TRIM(unit_labels(i)) // '^' &
                // TRIM(ADJUSTL(units))
       ENDIF
    ENDDO
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

END MODULE physunits
