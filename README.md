# PhysUnits
A Fortran 90/95 library that adds support for physical dimensions and units in scientific programs

The software package described on this page is an outgrowth of a paper that we published in the journal  Software – Practice and Experience (https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.401).   Legacy Fortran-77 code can be easily converted to utilize the system with only minor recoding.

The module is most likely to be useful in scientific programs, such as geophysical models and satellite retrieval algorithms, that are forced to deal with heterogeneous systems of units and/or complex formulas involving physically dimensioned constants and variables. For example, some old canned cloud physics subroutines in our own software library expect water densities in g/cm3; others expect kg/m3 . Many similar examples exist for drop sizes, fall speeds, scattering cross-sections, extinction coefficients, and the like.

We were motivated to develop this system by the realization that we were spending way too many hours debugging and painstakingly validating otherwise simple programs because of these inconsistencies in units.  Why should it be necessary for a programmer to manually figure out how to convert a viscosity from cgs to mks units, when the capability exists for a programming language to perform these conversions automatically and transparently?  Also, why shouldn’t a dimensionally invalid operation, such as the addition of a length to an area, trigger the same kind of fatal run-time error as division by zero?  The PHYSUNITS module addresses both problems and allows the programmer to code up physical relationships simply and generically by eliminating the need for embedded conversion factors.

This  facility was designed not for computational efficiency but rather programming/debugging efficiency.   Depending on the application, the modest increase in computational overhead might be an acceptable price to pay for a reduced likelihood of  subtle errors in unit conversion or in the coding of a physical formula.  (Note that once a program has been thoroughly tested, it is possible to virtually eliminate the extra overhead without rewriting the code, simply by substituting “dummy” definitions of the new data types and operators.)


Software is licensed under the GNU General Public License v3:
https://www.gnu.org/licenses/gpl-3.0.en.html


This distribution contains the following files:

physunits.pdf: PDF preprint version of a paper discussing the use of dimensions and units in scientific programming. 

physunits.dvi:       DVI version of the above document

physunits.f90:       External module conferring 'dimension-awareness' to Fortran 90 programs

physunits.fpp:      Same as physunits.f90 but containing preprocessor commands for conditional compilation.  Depending on your compiler, you may need to use a different extention for preprocessing, such as .F or .F90 .

SI_units.f90:        Module defining the SI system of units; USEs physunits.f90

SI_units.fpp:        Same as SI_units.f90 but with a single preprocessor command

physunits_test.f90:  Test program to verify correct behavior on new platforms

physunits_test.out:  Correct output of test program

physunits_demo.f90:  Simple demonstration program utilizing PHYSUNITS module.

fakeunits.f90:       Substitute for physunits.f90 when ascertaining computational overhead associated with units.     

dummy_units.f90:     Substitute for SI_units.f90 when ascertaining overhead associated with units.  USEs fakeunits.f90.  To use, just change "USE SI_units" to "USE dummy_units" in the program.
