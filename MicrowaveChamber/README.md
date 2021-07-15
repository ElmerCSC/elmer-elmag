# MicrowaveChamber

Case on microwave modelling presented on Elmer webinar series - 20th of May, 2021.

**"Industrial applications oriented, microwave modelling in ELMER"**

Webinar video available [here](https://youtu.be/aGf4sC5q0QE)

Presentation file available [here](https://www.nic.funet.fi/pub/sci/physics/elmer/webinar/)

## File data

First published: **19th of May, 2021**

File list:
- GEO. file: geometry model - run by Netgen software
- SIF. file: solver intput file - run by ELMER FEM software

## Description

Project of the microwave chamber with a waveguide. It allowed study on the resonance phenomenon in microwave chamber modelled with MES software.

## 3D model

Device consists of two parts: cylindrical waveguide and cylindrical chamber. They are joined axisymmetricly. Electromagnetic wave propagated through a waveguide is of TE 01 mode, and it is generating TE 011 mode in a chamber.

![Model 3D - geometry](/MicrowaveChamber/img/Org_3D_geo.png)

For given dimensions, cylindrical chamber's shape and knowing wave mods TE 011 with its identifiers (*m*=0, *n*=1, *p*=1), resonance frequency can be calculated from a formula:
![Formula for resonance frequency for a case](/MicrowaveChamber/eq/fres_eq.png)
where:
- fres - resonance frequency,
- c - speed of light,
- u'mn - n-th root of the derivative of m-th order Bessel function,
- R - chamber's radius,
- L - chamber's length,
- m, n, p - wave mode identifiers.

## Model's mathematical description - Elmer FEM

### Material

Model's interior is filled with air - its parameters are approximated with parameters of vacuum in *Material* section.

`Relative Permittivity = Real 1`

### Solvers

Model uses *VectorHelmholtz* module for solving Helmholtz equation describing electromagnetic wave propagation. It is provided in *Solver 1* section. Moreover *Solver 2* provides solver, which allows calculating results from *VectorHelmholtz* module. *Solver 3* allows saving results describing chamber's area in YZ plane to an external data file.

### Constants

To simplify formulas used in a program, *Constants* section was introduced.

*beta* - phase constant:

![Phase constant equation](/MicrowaveChamber/eq/phase_const_eq.png)

![Main phase constant equation](/MicrowaveChamber/eq/phase_eq.png)

- wres - angular resonance frequency,
- e0, u0 - electric permittivity and magnetic permeability of chamber's interior - here vacuum where er=1 and ur=1.
Note! Phase constant needs to be a real value!

*beta_lim* - limit phase constant:

![Limit phase constant equation](/MicrowaveChamber/eq/phase_lim_eq.png)

- u'mn - n-th root of the derivative of m-th order Bessel function - here *m*=0, *n*=1,
- r - waveguide's radius.

*const* - custom constant introduced to simplify Helmholtz equation:

![Const equation](/MicrowaveChamber/eq/const_eq.png)

- r - waveguide's radius,
- u'mn - n-th root of the derivative of m-th order Bessel function - here *m*=0, *n*=1.

*w* - angular resonance frequency:

![Angular frequency equation](/MicrowaveChamber/eq/wres_eq.png)

*l* - custom constant introduced to simplify equation for Leontovich boundary condition:

![l equation](/MicrowaveChamber/eq/l_eq.png)

- u0 - magnetic permeability for chamber's interior - here vacuum where ur=1,
- wres - angular resonance frequency,
- sigmaCu - relative conductivity for chamber's walls - here copper,
- uCu - relative magnetic permeability for chamber's walls - here copper.

### Boundary conditions

Model is described with two boundary conditions.

![Model's boundary conditions](/MicrowaveChamber/img/Org_b_conditions.png)

### Inport

Inport is a source of electromagnetic wave. It is described with specific Helmholtz wave equation for the case of cylindrical waveguide with TE type:

![Helmholtz equation for the case](/MicrowaveChamber/eq/Hz_eq.png)

where:
- Hz - wave identifying vector - EM wave propagating in Z direction,
- Hz(0) - magnetic field strength amplitude in *t*=0 in point *(0,0,0)*,
- Jm - m-th order Bessel function,
- u'mn - n-th root of the derivative of m-th order Bessel function,
- r - waveguide's radius,
- beta - phase constant,
- ro, phi, z - cylindrical coordinates,
- m, n - wave mode identifiers - for TE 01 *m*=0, *n*=1.

To provide the equation in form of Robin boundary condition, two variables need to be used:

![Electric Robin coefficient - inport](/MicrowaveChamber/eq/ER_inport_eq.png)

![g - inport](/MicrowaveChamber/eq/g_inport_short_eq.png)

![g - inport - full equation](/MicrowaveChamber/eq/g_inport_full_eq.png)

*g* function introduced simplified Bessel function with conversion to Cartesian coordinates.

### Walls

Energy absorption by copper walls are described with Leontovich impedance boundary condition. Presented in form of Robin boundary condition:

![Electric Robin coefficient - walls](/MicrowaveChamber/eq/ER_walls_eq.png)

![g - walls](/MicrowaveChamber/eq/g_walls_eq.png)

## Usage

Project was implemented for educational purposes.

## Gallery

Visualisation of the electromagnetic field distribution inside a model given in YZ plane.

> ### Electric field E
>
>![Electric field vector distribution, real part](/MicrowaveChamber/img/Org_E_field_re_YZ.png)
>
> ![Electric field vector distribution, imaginary part](/MicrowaveChamber/img/Org_E_field_im_YZ.png)

> ### Magnetic field strength H
>
> ![Magnetic field strength distribution, real part](/MicrowaveChamber/img/Org_M_field_re_YZ.png)
>
> ![Magnetic field strength distribution, imaginary part](/MicrowaveChamber/img/Org_M_field_im_YZ.png)
