! This program is designed to test the correct functioning of the basic
! algebraic operator interfaces defined in the modules PHYSUNITS and 
! SI_UNITS.  The compilation should go something like this:
!
!    f90 -c physunits.f90
!    f90 -c SI_units.f90
!    f90 physunits_test.f90 physunits.f90 SI_units.f90
!
! where "f90" is your Fortran 90 compiler and "-c" is the flag for 
! compiling without linking.  Multiple commands are used (instead of a
! single line) because most (all?) Fortran 90 compilers require modules 
! to be compiled before they are used.  Next, run the program:
!
!    a.out > test.out
!
! Compare the output file 'test.out' with the provided file 
! 'physunits_test.out'.  They should be the same (except perhaps for 
! some white space).
! 
! The next step is then to uncomment each line in turn that is supposed
! to generate an error and verify that it actually does.  These errors
! could show up at run-time or compile-time, depending on your compiler.

PROGRAM physunits_test

  USE SI_units

  INTEGER :: i1, i2 = 2
  REAL :: r1, r2 = 2.0
  DOUBLE PRECISION :: d1, d2 = 2.0d0
  TYPE(preal) :: p0, p1, p2, p3
  CHARACTER (len=40) :: string

  CALL physunits_init

  CALL pout(c_first_radiation)
  CALL pout(c_second_radiation)
  CALL pout(c_stefan_boltzmann)
  CALL pout(c_fine_structure)
  CALL pout(c_bohr_magneton)
  CALL pout(string,c_nuclear_magneton)
  WRITE (*,*) string
  WRITE (*,*)

!----------------------------------------
! Test interface assignment (=)
!----------------------------------------
  p0 = 1
  CALL pout(p0)
  p1 = 1.0
  CALL pout(p1)
  p2 = 2.0d0
  CALL pout(p2)
  p3 = u_meter
  CALL pout(p3)

  i1 = p0
!!$  i2 = p3   ! Error
  WRITE (*,*) i1
  r1 = p0
!!$  r2 = p3   ! Error
  WRITE (*,*) r1
  d1 = p0
!!$  d2 = p3   ! Error
  WRITE (*,*) d1

!----------------------------------------
! Test interface operator (+)
!----------------------------------------
  CALL pout(p0 + i1)
  CALL pout(i1 + p0)
!!$  call pout(p3 + i2)  ! Error
!!$  call pout(i2 + p3)  ! Error

  CALL pout(p0 + r1)
  CALL pout(r1 + p0)
!!$  call pout(p3 + r2)  ! Error
!!$  call pout(r2 + p3)  ! Error

  CALL pout(p0 + d1)
  CALL pout(d1 + p0)
!!$  call pout(p3 + d2)  ! Error
!!$  call pout(d2 + p3)  ! Error

  CALL pout(u_centimeter + u_millimeter)
!!$  call pout(u_meter + u_second)  ! Error

  CALL pout(+p3)

!----------------------------------------
! Test interface operator (-)
!----------------------------------------
  p0 = 3.0
  CALL pout(p0 - i1)
  CALL pout(i1 - p0)
!!$  call pout(p3 - i2)  ! Error
!!$  call pout(i2 - p3)  ! Error

  CALL pout(p0 - r1)
  CALL pout(r1 - p0)
!!$  call pout(p3 - r2)  ! Error
!!$  call pout(r2 - p3)  ! Error

  CALL pout(p0 - d1)
  CALL pout(d1 - p0)
!!$  call pout(p3 - d2)  ! Error
!!$  call pout(d2 - p3)  ! Error

  CALL pout(u_hour - u_minute)
!!$  call pout(u_kilogram - u_mole)  ! Error

  CALL pout(+p3)

!----------------------------------------
! Test interface operator (*)
!----------------------------------------
  CALL pout(p1 * r1)
  CALL pout(r2 * p3)
  CALL pout(p2 * i2)
  CALL pout(i1 * p0)
  CALL pout(p3 * d1)
  CALL pout(d2 * p2)
  CALL pout(p1 * p3)

!----------------------------------------
! Test interface operator (/)
!----------------------------------------
  CALL pout(p1 / r1)
  CALL pout(r2 / p3)
  CALL pout(p2 / i2)
  CALL pout(i1 / p0)
  CALL pout(p3 / d1)
  CALL pout(d2 / p2)
  CALL pout(p1 / p3)

!----------------------------------------
! Test interface operator (**)
!----------------------------------------
  CALL pout(p3 ** i2)
  CALL pout(i2 ** p0)
!!$  call pout(i2 ** p3)  ! Error

  CALL pout(p0 ** r1)
  CALL pout(r2 ** p1)
!!$  call pout(r1 ** p3)  ! Error

  CALL pout(p1 ** d1)
  CALL pout(d2 ** p2)
!!$  call pout(d1 ** p3)  ! Error

  CALL pout(p0 ** p0)
!!$  call pout(p1 ** p3)  ! Error

!----------------------------------------
! Test interface operator (.eq.)
!----------------------------------------

  WRITE (*,*) (p0 .EQ. i1)  ! .FALSE. 
  WRITE (*,*) (i1 .EQ. p0)  ! .FALSE.
!!$  write(*,*) (p3 .eq. i1)  ! Error
!!$  write(*,*) (i1 .eq. p3)  ! Error

  WRITE (*,*) (p0 .EQ. r1)  ! .FALSE.
  WRITE (*,*) (r1 .EQ. p0)  ! .FALSE.
!!$  write (*,*) (p3 .eq. r1)  ! Error
!!$  write (*,*) (r1 .eq. p3)  ! Error

  WRITE (*,*) (p0 .EQ. d1)  ! .FALSE.
  WRITE (*,*) (d1 .EQ. p0)  ! .FALSE.
!!$  write (*,*) (p3 .eq. d1)  ! Error
!!$  write (*,*) (d1 .eq. p3)  ! Error

  WRITE(*,*) (p1 .EQ. p2)  ! .FALSE.
!!$  write(*,*) (p1 .eq. p3)   ! Error

!----------------------------------------
! Test interface operator (.ne.)
!----------------------------------------
  WRITE (*,*) (p0 .NE. i1)  ! .TRUE.
  WRITE (*,*) (i1 .NE. p0)  ! .TRUE.
!!$  write(*,*) (p3 .ne. i1)  ! Error
!!$  write(*,*) (i1 .ne. p3)  ! Error

  WRITE (*,*) (p0 .NE. r1)  ! .TRUE.
  WRITE (*,*) (r1 .NE. p0)  ! .TRUE.
!!$  write (*,*) (p3 .ne. r1)  ! Error
!!$  write (*,*) (r1 .ne. p3)  ! Error

  WRITE (*,*) (p0 .NE. d1)  ! .TRUE.
  WRITE (*,*) (d1 .NE. p0)  ! .TRUE.
!!$  write (*,*) (p3 .ne. d1)  ! Error
!!$  write (*,*) (d1 .ne. p3)  ! Error

  WRITE(*,*) (u_kilometer .NE. p3)  ! .TRUE.
!!$  write(*,*) (p1 .ne. p3)   ! Error
  
!----------------------------------------
! Test interface operator (.gt.)
!----------------------------------------
  WRITE (*,*) (p0 .GT. i2)  ! .TRUE.
  WRITE (*,*) (i2 .GT. p0)  ! .FALSE.
!!$  write(*,*) (p3 .gt. i1)  ! Error
!!$  write(*,*) (i1 .gt. p3)  ! Error

  WRITE (*,*) (p0 .GT. r1)  ! .TRUE.
  WRITE (*,*) (r1 .GT. p0)  ! .FALSE.
!!$  write (*,*) (p3 .gt. r1)  ! Error
!!$  write (*,*) (r1 .gt. p3)  ! Error

  WRITE (*,*) (p0 .GT. d2)  ! .TRUE.
  WRITE (*,*) (d2 .GT. p0)  ! .FALSE.
!!$  write (*,*) (p3 .gt. d1)  ! Error
!!$  write (*,*) (d1 .gt. p3)  ! Error

  WRITE(*,*) (u_day .GT. u_hour)  ! .TRUE.
!!$  write(*,*) (p1 .gt. p3)   ! Error

!----------------------------------------
! Test interface operator (.ge.)
!----------------------------------------
  WRITE (*,*) (p0 .GE. i2)  ! .TRUE.
  WRITE (*,*) (i2 .GE. p0)  ! .FALSE.
!!$  write(*,*) (p3 .ge. i1)  ! Error
!!$  write(*,*) (i1 .ge. p3)  ! Error

  WRITE (*,*) (p0 .GE. r1)  ! .TRUE.
  WRITE (*,*) (r1 .GE. p0)  ! .FALSE.
!!$  write (*,*) (p3 .ge. r1)  ! Error
!!$  write (*,*) (r1 .ge. p3)  ! Error

  WRITE (*,*) (p0 .GE. d2)  ! .TRUE.
  WRITE (*,*) (d2 .GE. p0)  ! .FALSE.
!!$  write (*,*) (p3 .ge. d1)  ! Error
!!$  write (*,*) (d1 .ge. p3)  ! Error

  WRITE (*,*) (u_atomic_mass_unit .GE. u_kg)  ! .FALSE.
!!$  write(*,*) (p1 .ge. p3)   ! Error

!----------------------------------------
! Test interface operator (.le.)
!----------------------------------------
  WRITE (*,*) (p0 .LE. i1)  ! .FALSE.
  WRITE (*,*) (i1 .LE. p0)  ! .TRUE.
!!$  write(*,*) (p3 .le. i1)  ! Error
!!$  write(*,*) (i1 .le. p3)  ! Error

  WRITE (*,*) (p0 .LE. r2)  ! .FALSE.
  WRITE (*,*) (r2 .LE. p0)  ! .TRUE.
!!$  write (*,*) (p3 .le. r1)  ! Error
!!$  write (*,*) (r1 .le. p3)  ! Error

  WRITE (*,*) (p0 .LE. d1)  ! .FALSE.
  WRITE (*,*) (d1 .LE. p0)  ! .TRUE.
!!$  write (*,*) (p3 .le. d1)  ! Error
!!$  write (*,*) (d1 .le. p3)  ! Error

  WRITE(*,*) (p3 .LE. u_kilometer)  ! .TRUE.
!!$  write(*,*) (p1 .le. p3)   ! Error

!----------------------------------------
! Test interface operator (.lt.)
!----------------------------------------
  WRITE (*,*) (p1 .LT. i1)  ! .FALSE.
  WRITE (*,*) (i1 .LT. p1)  ! .FALSE.
!!$  write(*,*) (p3 .lt. i1)  ! Error
!!$  write(*,*) (i1 .lt. p3)  ! Error

  WRITE (*,*) (p0 .LT. r2)  ! .FALSE.
  WRITE (*,*) (r2 .LT. p0)  ! .TRUE.
!!$  write (*,*) (p3 .lt. r1)  ! Error
!!$  write (*,*) (r1 .lt. p3)  ! Error

  WRITE (*,*) (p0 .LT. d1)  ! .FALSE.
  WRITE (*,*) (d1 .LT. p0)  ! .TRUE.
!!$  write (*,*) (p3 .lt. d1)  ! Error
!!$  write (*,*) (d1 .lt. p3)  ! Error

  WRITE(*,*) (p1 .LT. p2)  ! .TRUE.
!!$  write(*,*) (p1 .lt. p3)   ! Error

!----------------------------------------
! Test interfaces REAL
!----------------------------------------
  WRITE (*,*) REAL(p0)
!!$  write (*,*) REAL(p3)  ! Error
     
!----------------------------------------
! Test interface sqrt
!----------------------------------------
  CALL pout(SQRT(p3))

END PROGRAM physunits_test
