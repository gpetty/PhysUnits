
Software is licensed under the GNU General Public License v3:
https://www.gnu.org/licenses/gpl-3.0.en.html




This distribution contains the following files:

physunits.pdf       PDF preprint version of a paper discussing the use 
		    of dimensions and units in scientific programming. 

physunits.dvi       DVI version of the above document

physunits.f90       External module conferring 'dimension-awareness' to
                    Fortran 90 programs

physunits.fpp       Same as physunits.f90 but containing preprocessor 
		    commands for conditional compilation.  Depending on 
		    your compiler, you may need to use a different
		    extention for preprocessing, such as .F or .F90 .

SI_units.f90        Module defining the SI system of units; USEs physunits.f90

SI_units.fpp        Same as SI_units.f90 but with a single preprocessor command

physunits_test.f90  Test program to verify correct behavior on new platforms

physunits_test.out  Correct output of test program

physunits_demo.f90  Simple demonstration program utilizing PHYSUNITS module.

fakeunits.f90       Substitute for physunits.f90 when ascertaining computational
		    overhead associated with units.     

dummy_units.f90     Substitute for SI_units.f90 when ascertaining overhead
                    associated with units.  USEs fakeunits.f90.  To use, just
                    change "USE SI_units" to "USE dummy_units" in the program. 

lesser.txt          The GNU Lesser General Public License

README              This file


