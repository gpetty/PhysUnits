!------------------------------------------------------------------------------------------------
! Fortran 90 Module 'SI_UNITS'
! Copyright (C) 2002  Grant W. Petty
!
! Written by Grant W. Petty and Michael A. Walters
!    with suggestions by Howard W. Ludwig
!
! This module is indended for use with the module 'PHYSUNITS'.  See PHYSUNITS for more
! information.
!
! This file differs from SI_units.f90 in that it contains preprocessor commands for
! conditional compilation.  To compile with radians as a unit, the command line would 
! look something like this:
!
!    f90 -c -DRADUNIT SI_units.fpp
!
!------------------------------------------------------------------------------------------------


MODULE SI_units

  USE physunits

  PUBLIC

  LOGICAL, PRIVATE, SAVE :: initialized = .FALSE.

  PRIVATE :: base, zero

!------------------------------------------------------------------------------------------------
! DEFINE UNITS AND CONSTANTS
!------------------------------------------------------------------------------------------------

! Define base units
  TYPE(preal), PARAMETER :: u_meter    = preal(1.0e+0, (/base,zero,zero,zero,zero,zero,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_kilogram = preal(1.0e+0, (/zero,base,zero,zero,zero,zero,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_second   = preal(1.0e+0, (/zero,zero,base,zero,zero,zero,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_kelvin   = preal(1.0e+0, (/zero,zero,zero,base,zero,zero,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_ampere   = preal(1.0e+0, (/zero,zero,zero,zero,base,zero,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_mole     = preal(1.0e+0, (/zero,zero,zero,zero,zero,base,zero,zero/)) 
  TYPE(preal), PARAMETER :: u_candela  = preal(1.0e+0, (/zero,zero,zero,zero,zero,zero,base,zero/))
#ifdef RADUNIT
! NOTE: Radians are best treated as dimensionless units, otherwise you have problems with
! angles computed as a ratio of lengths.  If this is not a problem for you and you
! want to keep track of radians (and steradians), then use this definition:
  TYPE(preal), PARAMETER  :: u_radian  = preal(1.0e+0, (/zero,zero,zero,zero,zero,zero,zero,base/))
#else
  TYPE(preal), PARAMETER :: u_radian   = preal(1.0e+0, (/zero,zero,zero,zero,zero,zero,zero,zero/)) 
#endif

! Abbreviations
  TYPE(preal), SAVE :: u_m   != u_meter
  TYPE(preal), SAVE :: u_kg  != u_kilogram
  TYPE(preal), SAVE :: u_s   != u_second
  TYPE(preal), SAVE :: u_K   != u_kelvin
  TYPE(preal), SAVE :: u_A   != u_ampere
  TYPE(preal), SAVE :: u_mol != u_mole
  TYPE(preal), SAVE :: u_cd  != u_candela
!!$  TYPE(preal), SAVE :: u_rad = u_radian  ! Could be confused with unit of radiation

!!$  PARAMETER (unit_labels = (/'m  ', 'kg ', 's  ', 'K  ', 'A  ', 'mol', 'cd ', 'rad'/))

! Define non-dimensional multipliers
  REAL, PARAMETER :: u_yotta = 1.0e+24
  REAL, PARAMETER :: u_zetta = 1.0e+21
  REAL, PARAMETER :: u_exa   = 1.0e+18
  REAL, PARAMETER :: u_peta  = 1.0e+15
  REAL, PARAMETER :: u_tera  = 1.0e+12
  REAL, PARAMETER :: u_giga  = 1.0e+09
  REAL, PARAMETER :: u_mega  = 1.0e+06
  REAL, PARAMETER :: u_kilo  = 1.0e+03
  REAL, PARAMETER :: u_hecto = 1.0e+02
  REAL, PARAMETER :: u_deka  = 1.0e+01,  u_deca = 1.0e+01
  REAL, PARAMETER :: u_deci  = 1.0e-01
  REAL, PARAMETER :: u_centi = 1.0e-02
  REAL, PARAMETER :: u_milli = 1.0e-03
  REAL, PARAMETER :: u_micro = 1.0e-06
  REAL, PARAMETER :: u_nano  = 1.0e-09
  REAL, PARAMETER :: u_pico  = 1.0e-12
  REAL, PARAMETER :: u_femto = 1.0e-15
  REAL, PARAMETER :: u_atto  = 1.0e-18
  REAL, PARAMETER :: u_zepto = 1.0e-21
  REAL, PARAMETER :: u_zocto = 1.0e-24

! Constants of nature
  TYPE(preal), SAVE :: c_speed_of_light
  TYPE(preal), SAVE :: c_magnetic
  TYPE(preal), SAVE :: c_electric
  TYPE(preal), SAVE :: c_planck
  TYPE(preal), SAVE :: c_h_bar

  TYPE(preal), SAVE :: c_avogadro
  TYPE(preal), SAVE :: c_universal_gas, c_molar_gas
  TYPE(preal), SAVE :: c_standard_molar_volume
  TYPE(preal), SAVE :: c_boltzmann

  TYPE(preal), SAVE :: c_electron_charge, c_elementary_charge, c_e
  TYPE(preal), SAVE :: c_faraday
 
  TYPE(preal), SAVE :: c_first_radiation
  TYPE(preal), SAVE :: c_second_radiation
  TYPE(preal), SAVE :: c_stefan_boltzmann
  TYPE(preal), SAVE :: c_wiens_radiation

  TYPE(preal), SAVE :: c_electron_rest_mass
  TYPE(preal), SAVE :: c_proton_rest_mass
  TYPE(preal), SAVE :: c_fine_structure

  TYPE(preal), SAVE :: c_bohr_magneton
  TYPE(preal), SAVE :: c_nuclear_magneton

  TYPE(preal), SAVE :: c_gravity_accel, c_g_force
  TYPE(preal), SAVE :: c_gravity

  TYPE(preal), SAVE :: c_ice_point
  TYPE(preal), SAVE :: c_water_triple_point

  DOUBLE PRECISION, PARAMETER :: pi = 3.141592653589793d0

! Accepted non-SI units
  TYPE(preal), SAVE :: u_minute, u_min
  TYPE(preal), SAVE :: u_hour, u_hr
  TYPE(preal), SAVE :: u_day, u_d
  TYPE(preal), SAVE :: u_degree, u_deg
  TYPE(preal), SAVE :: u_arc_minute
  TYPE(preal), SAVE :: u_arc_second
  TYPE(preal), SAVE :: u_liter, u_litre, u_L
  TYPE(preal), SAVE :: u_electronvolt, u_eV
  TYPE(preal), SAVE :: u_atomic_mass_unit, u_amu, u_u
  TYPE(preal), SAVE :: u_astronomical_unit, u_au, u_ua

  TYPE(preal), SAVE :: u_angstrom
  TYPE(preal), SAVE :: u_nautical_mile
  TYPE(preal), SAVE :: u_knot
  TYPE(preal), SAVE :: u_barn!, u_b  ! Could be confused with u_bar
  TYPE(preal), SAVE :: u_hectare, u_ha
  TYPE(preal), SAVE :: u_bar
  TYPE(preal), SAVE :: u_curie, u_Ci
  TYPE(preal), SAVE :: u_roentgen, u_R
  TYPE(preal), SAVE :: u_rem

! SI derived units
  TYPE(preal), SAVE :: u_steradian, u_sr
  TYPE(preal), SAVE :: u_hertz, u_Hz
  TYPE(preal), SAVE :: u_newton, u_N
  TYPE(preal), SAVE :: u_pascal, u_Pa
  TYPE(preal), SAVE :: u_joule, u_J
  TYPE(preal), SAVE :: u_watt, u_W
  TYPE(preal), SAVE :: u_coulomb, u_C 
  TYPE(preal), SAVE :: u_volt, u_V
  TYPE(preal), SAVE :: u_farad, u_F 
  TYPE(preal), SAVE :: u_ohm       
  TYPE(preal), SAVE :: u_siemens   
  TYPE(preal), SAVE :: u_weber, u_Wb
  TYPE(preal), SAVE :: u_tesla, u_T
  TYPE(preal), SAVE :: u_henry!, u_H
  TYPE(preal), SAVE :: u_lumen, u_lm
  TYPE(preal), SAVE :: u_lux, u_lx
  TYPE(preal), SAVE :: u_becquerel, u_Bq
  TYPE(preal), SAVE :: u_gray, u_Gy
  TYPE(preal), SAVE :: u_sievert, u_Sv

  TYPE(preal), SAVE :: u_micrometer, u_micron
  TYPE(preal), SAVE :: u_millimeter, u_mm
  TYPE(preal), SAVE :: u_centimeter, u_cm
  TYPE(preal), SAVE :: u_kilometer, u_km

  TYPE(preal), SAVE :: u_square_millimeter, u_sqmm
  TYPE(preal), SAVE :: u_square_centimeter, u_sqcm
  TYPE(preal), SAVE :: u_square_meter, u_sqm
  TYPE(preal), SAVE :: u_square_kilometer, u_sqkm

  TYPE(preal), SAVE :: u_cubic_centimeter, u_cc
  TYPE(preal), SAVE :: u_milliliter, u_mL

  TYPE(preal), SAVE :: u_kilometer_per_hour, u_kph

  TYPE(preal), SAVE :: u_cycles_per_second
  TYPE(preal), SAVE :: u_rotations_per_minute, u_rpm

  TYPE(preal), SAVE :: u_gram, u_g
  TYPE(preal), SAVE :: u_gramme
  TYPE(preal), SAVE :: u_milligram, u_mg
  TYPE(preal), SAVE :: u_tonne, u_metric_ton

  TYPE(preal), SAVE :: u_dyne

  TYPE(preal), SAVE :: u_millibar, u_mbar, u_mb
  TYPE(preal), SAVE :: u_hectopascal

  TYPE(preal), SAVE :: u_calorie
  TYPE(preal), SAVE :: u_kilocalorie
  TYPE(preal), SAVE :: u_erg

  TYPE(preal), SAVE :: u_celsius_degree

  TYPE(preal), SAVE :: u_poiseuille, u_pl
  TYPE(preal), SAVE :: u_poise
  TYPE(preal), SAVE :: u_stokes, u_sk

! Other (non-SI) derived units
  TYPE(preal), SAVE :: u_foot, u_ft
  TYPE(preal), SAVE :: u_inch, u_in
  TYPE(preal), SAVE :: u_light_year
  TYPE(preal), SAVE :: u_mil
  TYPE(preal), SAVE :: u_statute_mile, u_mile
  TYPE(preal), SAVE :: u_yard, u_yd
  TYPE(preal), SAVE :: u_parsec, u_pc

  TYPE(preal), SAVE :: u_square_inch, u_sqin
  TYPE(preal), SAVE :: u_square_foot, u_sqft
  TYPE(preal), SAVE :: u_square_yard, u_sqyd
  TYPE(preal), SAVE :: u_square_mile, u_sqmi
  TYPE(preal), SAVE :: u_acre

  TYPE(preal), SAVE :: u_cubic_inch
  TYPE(preal), SAVE :: u_cubic_foot
  TYPE(preal), SAVE :: u_imperial_gallon_uk
  TYPE(preal), SAVE :: u_dry_gallon_usa
  TYPE(preal), SAVE :: u_liquid_gallon_usa
  TYPE(preal), SAVE :: u_quart
  TYPE(preal), SAVE :: u_pint
  TYPE(preal), SAVE :: u_cup
  TYPE(preal), SAVE :: u_ounce_volume
  TYPE(preal), SAVE :: u_tablespoon
  TYPE(preal), SAVE :: u_teaspoon

  TYPE(preal), SAVE :: u_year, u_yr, u_y
  TYPE(preal), SAVE :: u_year_sidereal
  TYPE(preal), SAVE :: u_year_tropical

  TYPE(preal), SAVE :: u_mile_per_hour, u_mph

  TYPE(preal), SAVE :: u_pound_mass
  TYPE(preal), SAVE :: u_slug
  TYPE(preal), SAVE :: u_ton
  TYPE(preal), SAVE :: u_solar_mass

  TYPE(preal), SAVE :: u_pound_force, u_lb

  TYPE(preal), SAVE :: u_atmosphere, u_atm
  TYPE(preal), SAVE :: u_inchH2O, u_inH2O
  TYPE(preal), SAVE :: u_inchHg, u_inHg
  TYPE(preal), SAVE :: u_millimeterHg, u_mmHg
  TYPE(preal), SAVE :: u_torricelli, u_torr
  TYPE(preal), SAVE :: u_psi
  
  TYPE(preal), SAVE :: u_btu
  TYPE(preal), SAVE :: u_therm

  TYPE(preal), SAVE :: u_horsepower_electric
  TYPE(preal), SAVE :: u_horsepower_imperial
  TYPE(preal), SAVE :: u_horsepower_metric
  TYPE(preal), SAVE :: u_horsepower_water

  TYPE(preal), SAVE :: u_fahrenheit_degree
  TYPE(preal), SAVE :: u_rankine

CONTAINS

!----------------------------------------------------------------------------
! Subroutine to initialize values of derived units and physical constants.
! MUST be called at beginning of the main program using the PHYSUNITS module.
!----------------------------------------------------------------------------

  SUBROUTINE physunits_init

! Only execute this subroutine one time
    IF (initialized) RETURN

    unit_labels = (/'m  ', 'kg ', 's  ', 'K  ', 'A  ', 'mol', 'cd ', 'rad'/)

!------------------------------------------------------------------------------------------------
! Abbreviations
!------------------------------------------------------------------------------------------------
    u_m   = u_meter
    u_kg  = u_kilogram
    u_s   = u_second
    u_K   = u_kelvin
    u_A   = u_ampere
    u_mol = u_mole
    u_cd  = u_candela
!!$    u_rad = u_radian

!------------------------------------------------------------------------------------------------
! Compute values of SI derived units
!------------------------------------------------------------------------------------------------
    u_steradian = u_radian ** 2
    u_degree    = u_radian * pi / 180.0
    u_hertz     = 2.0*pi * u_radian / u_second
    u_newton    = u_kilogram * u_meter / u_second ** 2
    u_pascal    = u_newton / u_meter ** 2
    u_joule     = u_newton * u_meter
    u_watt      = u_joule / u_second
    u_coulomb   = u_ampere * u_second  ! = u_watt / u_ampere
    u_volt      = u_joule / u_coulomb
    u_farad     = u_coulomb / u_volt
    u_ohm       = u_volt / u_ampere
    u_siemens   = u_ampere / u_volt
    u_weber     = u_volt * u_second
    u_tesla     = u_weber / u_meter ** 2
    u_henry     = u_weber / u_ampere
    u_lumen     = u_candela * u_steradian
    u_lux       = u_lumen / u_meter ** 2

! Abbreviations
    u_sr  = u_steradian
    u_deg = u_degree
    u_Hz  = u_hertz
    u_N   = u_newton
    u_Pa  = u_pascal
    u_J   = u_joule
    u_W   = u_watt
    u_C   = u_coulomb
    u_V   = u_volt
    u_F   = u_farad
    u_Wb  = u_weber
    u_T   = u_tesla
!!$    u_H   = u_henry
    u_lm  = u_lumen
    u_lx  = u_lux

!------------------------------------------------------------------------------------------------
! Definitions of common physical constants (from NIST - http://physics.nist.gov/cuu/index.html)
!------------------------------------------------------------------------------------------------
    c_speed_of_light        = 2.99792458e+8 * u_meter / u_second
    c_magnetic              = 4.0*pi * 1.0e-7 * u_newton / u_ampere**2
!                           = 1.2566370614e-6 * u_newton / u_ampere**2
    c_electric              = 1.0 / (c_magnetic * c_speed_of_light**2)
!                           = 8.854187817e-12 * u_farad / u_meter
    c_planck                = 6.62606876e-34 * u_joule * u_second
    c_h_bar                 = c_planck / (2.0*pi)
   

    c_avogadro              = 6.02214199e+23 / u_mole
    c_universal_gas         = 8.314472 * u_joule / (u_mole * u_kelvin)
    c_molar_gas             = c_universal_gas      
    c_standard_molar_volume = 2.2413996e-2 * u_meter**3 / u_mole
    c_boltzmann             = c_molar_gas / c_avogadro   
!                           = 1.3806503e-23 * u_joule / u_kelvin
    
    c_electron_charge       = 1.602176462e-19 * u_coulomb
!                           = 1.602176462e-19 * u_ampere * u_second
    c_elementary_charge     = c_electron_charge
    c_e                     = c_electron_charge
    c_faraday               = c_avogadro * c_electron_charge
!                           = 9.64853415e+4 * u_coulomb / u_mole     
!                           = 9.64853415e+4 * u_ampere * u_second / u_mole

    c_first_radiation       = 2.0*pi * c_planck * c_speed_of_light**2
!                           = 3.74177107e-16 * u_watt / u_meter**2
    c_second_radiation      = c_planck * c_speed_of_light / c_boltzmann
!                           = 1.4387752e-2 * u_meter * u_kelvin
    c_stefan_boltzmann      = (pi**2 / 60.0) * c_boltzmann * (c_boltzmann / c_h_bar) &
                              * (c_boltzmann / (c_h_bar * c_speed_of_light))**2
!                           = 5.670400e-8 * u_watt / (u_meter**2 * u_kelvin**4)
    c_wiens_radiation       = 2.8977686e-3 * u_meter * u_kelvin
 
    c_electron_rest_mass    = 9.10938188e-31 * u_kilogram
    c_proton_rest_mass      = 1.67262158e-27 * u_kilogram
    c_fine_structure        = c_electron_charge**2  &
                              / (2.0 * c_electric * c_speed_of_light * c_planck)

    c_bohr_magneton         = c_electron_charge * (c_h_bar / (2.0 * c_electron_rest_mass))
    c_nuclear_magneton      = c_electron_charge * (c_h_bar / (2.0 * c_proton_rest_mass))

    c_gravity               = 6.673e-11 * u_meter**3 / (u_kilogram * u_second**2)
    c_gravity_accel         = 9.80665e+0 * u_meter / u_second**2
    c_g_force               = c_gravity_accel
    
    c_ice_point             = 273.15 * u_kelvin
    c_water_triple_point    = 273.16 * u_kelvin

!------------------------------------------------------------------------------------------------
! Derived and non-SI units
!------------------------------------------------------------------------------------------------

! Selected unit of angle
    u_arc_minute = u_degree / 60.0
    u_arc_second = u_arc_minute / 60.0

! Selected units of length
    u_angstrom          = 1.0e-10 * u_meter
    u_micrometer        = u_micro * u_meter
    u_micron            = u_micrometer
    u_millimeter        = u_milli * u_meter
    u_mm                = u_millimeter
    u_centimeter        = u_centi * u_meter
    u_cm                = u_centimeter
    u_kilometer         = u_kilo * u_meter
    u_km                = u_kilometer
    u_nautical_mile     = 1.852e+3 * u_meter
    u_astronomical_unit = 1.495979e+11 * u_meter
    u_au                = u_astronomical_unit
    u_ua                = u_astronomical_unit
  
    u_mil               = 2.54e-5 * u_meter
    u_inch              = 2.54 * u_centimeter
    u_in                = u_inch
    u_foot              = 12.0 * u_inch
    u_ft                = u_foot
    u_yard              = 3.0 * u_foot
    u_yd                = u_yard
    u_statute_mile      = 5280.0 * u_foot
    u_mile              = u_statute_mile
    u_light_year        = c_speed_of_light * u_year
    u_parsec            = 3.085678e+15 * u_meter
    u_pc                = u_parsec

! Selected units of area
    u_square_millimeter = u_millimeter ** 2
    u_sqmm              = u_square_millimeter
    u_square_centimeter = u_centimeter ** 2
    u_sqcm              = u_square_centimeter
    u_square_meter      = u_meter ** 2
    u_sqm               = u_square_meter
    u_square_kilometer  = u_kilometer ** 2
    u_sqkm              = u_sqkm
    u_barn              = 1.0e-28 * u_square_meter
!!$    u_b                 = u_barn
    u_hectare           = 1.0e+4 * u_square_meter
    u_ha                = u_hectare

    u_square_inch       = u_inch ** 2
    u_sqin              = u_square_inch
    u_square_foot       = u_foot ** 2
    u_sqft              = u_square_foot
    u_square_yard       = u_yard ** 2
    u_sqyd              = u_square_yard
    u_square_mile       = u_mile ** 2
    u_sqmi              = u_square_mile
    u_acre              = 43560.0 * u_square_foot

! Selected units of volume
    u_liter              = 1.0e-3 * u_meter ** 3
    u_litre              = u_liter
    u_L                  = u_liter
    u_cubic_centimeter   = u_centimeter ** 3
    u_cc                 = u_cubic_centimeter
    u_milliliter         = u_cubic_centimeter
    u_mL                 = u_milliliter

    u_cubic_inch         = u_inch ** 3
    u_cubic_foot         = u_foot ** 3
    u_imperial_gallon_uk = 4.54609 * u_liter
    u_dry_gallon_usa     = 4.404884 * u_liter
    u_liquid_gallon_usa  = 3.785412 * u_liter            ! = 231.0 * u_cubicinch
    u_quart              = u_liquid_gallon_usa / 4.0
    u_pint               = u_liquid_gallon_usa / 8.0     ! = u_quart / 2.0
    u_cup                = u_liquid_gallon_usa / 16.0    ! = u_pint / 2.0
    u_ounce_volume       = u_liquid_gallon_usa / 128.0   ! = u_cup / 8.0
    u_tablespoon         = u_liquid_gallon_usa / 256.0   ! = u_ounce_volume / 2.0
    u_teaspoon           = u_liquid_gallon_usa / 768.0   ! = u_tablespoon / 3.0

! Selected units of time
    u_minute        = 60.0 * u_second
    u_min           = u_minute
    u_hour          = 60.0 * u_minute
    u_hr            = u_hour
    u_day           = 24.0 * u_hour
    u_d             = u_day

    u_year          = 365.0 * u_day
    u_yr            = u_year
    u_y             = u_year
    u_year_sidereal = 3.155815e+15 * u_second
    u_year_tropical = 3.155693e+15 * u_second

! Selected units of linear velocity
    u_kilometer_per_hour = u_kilometer / u_hour
    u_kph                = u_kilometer_per_hour
    u_knot               = u_nautical_mile / u_hour

    u_mile_per_hour      = u_statute_mile / u_hour
    u_mph                = u_mile_per_hour
 
! Selected units of angular velocity
    u_cycles_per_second    = 2.0*pi * u_radian / u_second
    u_rotations_per_minute = 2.0*pi * u_radian / u_minute
    u_rpm                  = u_rotations_per_minute

! Selected units of mass
    u_gram             = u_kilogram / u_kilo
    u_gramme           = u_gram
    u_g                = u_gram
    u_milligram        = 1.0e-6 * u_kilogram
    u_mg               = u_milligram
    u_tonne            = 1.0e+3 * u_kilogram
    u_metric_ton       = 1.0e+3 * u_kilogram
    u_atomic_mass_unit = 1.0e-3 * u_kilogram / (c_avogadro * u_mole)
    u_amu              = u_atomic_mass_unit
    u_u                = u_atomic_mass_unit

    u_pound_mass       = 4.535924e-1 * u_kilogram
    u_slug             = 1.459390e+1 * u_kilogram
    u_ton              = 2000.0 * u_pound_mass
    u_solar_mass       = 1.989e+30 * u_kilogram

! Selected units of force
    u_dyne        = 1.0e-5 * u_newton

    u_pound_force = u_pound_mass * c_gravity_accel
    u_lb          = u_pound_force
  
! Selected non-SI units of pressure
    u_hectopascal  = u_hecto * u_pascal
    u_bar          = 1.0e+5 * u_pascal
    u_millibar     = u_milli * u_bar
    u_mbar         = u_millibar
    u_mb           = u_millibar

    u_atmosphere   = 1.01325e+5 * u_pascal
    u_atm          = u_atmosphere
    u_inchH2O      = 2.490889e+2 * u_pascal
    u_inH2O        = u_inchH2O
    u_inchHg       = 3.386389e+3 * u_pascal
    u_inHg         = u_inchHg
    u_millimeterHg = 1.333224e+2 * u_pascal
    u_mmHg         = u_millimeterHg
    u_torricelli   = u_millimeterHg
    u_torr         = u_torricelli
    u_psi          = u_pound_force / u_square_inch

! Selected units of energy
    u_electronvolt = c_electron_charge * u_volt
    u_eV           = u_electronvolt
    u_erg          = 1.0e-7 * u_joule

    u_btu          = 1.05505585262e+3 * u_joule  ! International Table value
    u_calorie      = 4.1868 * u_joule            ! International Table value
    u_kilocalorie  = u_kilo * u_calorie
    u_therm        = 1.054804e+8 * u_joule       ! U.S.
!    u_therm        = 1.05506e+8 * u_Joule        ! EC

! Selected non-SI units of power
    u_horsepower_electric = 7.46e+2 * u_watt
    u_horsepower_imperial = 7.4570e+2 * u_watt
    u_horsepower_metric   = 7.354988e+2 * u_watt
    u_horsepower_water    = 7.46043e+2 * u_watt

! Selected units of temperature DIFFERENCE
! Note actual temperatures in Celsius or Fahrenheit cannot be expressed as a simple scaling of 
! degrees Kelvin, because an offset is required as well.  Therefore, the following units
! should only be used to convert temperature DIFFERENCES, unless care is taken to supply
! the correct offset in the program performing the conversion.
    u_celsius_degree    = u_kelvin

    u_fahrenheit_degree = u_kelvin / 1.8
    u_rankine           = u_kelvin / 1.8

! Selected units of radioactivity and radiation
    u_curie     = 3.7e+10 / u_second
    u_Ci        = u_curie
    u_becquerel = 1.0 / u_second
    u_Bq        = u_becquerel
    u_gray      = u_joule / u_kilogram
    u_Gy        = u_gray
    u_sievert   = u_joule / u_kilogram
    u_Sv        = u_sievert
    u_rem       = 1.0e-2 * u_sievert
    u_roentgen  = 2.58e-4 * u_coulomb / u_kilogram
    u_R         = u_roentgen

! Selected units of viscosity
    u_poiseuille = u_kilogram / (u_meter * u_second)
    u_pl         = u_poiseuille
    u_poise      = 1.0e-1 * u_poiseuille
    u_stokes     = 1.0e-4 * u_meter**2 / u_second
    u_sk         = u_stokes

! Set flag to indicate that this routine has been executed
    initialized = .TRUE.

  END SUBROUTINE physunits_init

END MODULE SI_units
