!------------------------------------------------------------------------
! Compile and run under Unix with any standard F90 compiler as follows:
!
!   f90 -c physunits.f90
!   f90 -c SI_units.f90
!   f90 physunits_demo.f90 physunits.f90 SI_units.f90
!   a.out 
!
! Example of a simple program to compute the total mass of the
! atmosphere, given the radius of the earth and the average atmospheric
! surface pressure.  It is deliberately written to use Fortran 77-style
! syntax wherever possible in order to highlight the relative ease of
! converting legacy code so as to utilize the Fortran 90 PHYSUNITS
! module.
! 
! The key point to recognize in this example is that the programmer is
! able to use completely arbitrary units to specify the values of
! various constants used in the calculation and let the computer do all
! of the internal conversions to a consistent set of units.  The
! programmer can then choose an arbitrary set of units for displaying
! the results of the computation.  Also, there is run-time checking for
! illegal operations involving physically dimensioned quantities.
!------------------------------------------------------------------------

      program physunits_demo
        
      use SI_units
      implicit none

      type(preal) radius,grav,sfcpres,sfcarea,atmos_mass
      real acres1,acres2
      character*80 string

! DO NOT FORGET this step!!!  If you do, most units will default to 0.
      call physunits_init

! Note:  By convention, all variable names with prefix 'u_' identify 
! physical units or constants predefined in module SI_UNITS 

! Radius of the earth specified in kilometers
      radius = 6370.*u_kilometer

! Acceleration due to gravity specified in feet per second squared
      grav = 32.2*u_foot/(u_second**2)

! Average surface air pressure specified as one standard atmosphere
      sfcpres = 1.0*u_atmosphere

! Calculate earth's surface area using generic formula
      sfcarea = 4.*pi*radius**2

! Print out the Earth's surface area in units of acres.  
! Because 'acres1' is  an ordinary scalar REAL variable, this
! assignment would generate a run-time error if the ratio
! sfcarea/u_acre were for some reason found not to be dimensionless 
! due to a previous programming error.

      acres1 = sfcarea/u_acre
      write(*,*)' Earth surface area = ',acres1,' Acres'
      
! Calculate mass of the atmosphere using generic formula based on 
! hydrostatic balance

      atmos_mass = sfcarea*sfcpres/grav 

! Print out the mass of the atmosphere in units of kilograms.  
! Function 'nounits' casts variable of type PREAL to REAL, assuming that
! it is non-dimensional; otherwise generates run-time error.
 
      write(*,*)' Atmospheric mass = ', real(atmos_mass/u_kilogram), ' kg'

! An attempt to print out the mass of the atmosphere in inappropriate
! units.  If not commented out, this line would generate an error because 
! the ratio is not dimensionless and therefore cannot be assigned to 'acres2', 
! which is of standard type REAL.
!
!      acres2 = atmos_mass/u_acre
!      write(*,*)'Atmospheric mass = ',acres2,' Acres'

      end
