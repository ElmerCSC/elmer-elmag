# MicrowaveHeating

This case can be found on Elmer Discussion Forum. A contrast with the results of the Comsol can also be found here.

[Heat source for microwave heating - Elmer Discussion Forum (elmerfem.org)](http://www.elmerfem.org/forum/viewtopic.php?t=7749)

## Description

Heat a potato in a rectangular waveguide. Port input power *P*in can be adjusted. Use electric field or divergence of  Poynting vector as heat source. This case has two heat sources, one is the reaction heat, assumed to stay open, the other is the microwave heating, on / off according to the temperature of a point.

## Port input power $P_{in}$

The flow of energy is described by the complex Poynting vector $S_c$:

$$
S_c = \frac{1}{2}(E \times H^*) \tag{1}
$$
whose real part gives the time-averaged flux of energy $S_{av}$:
$$
S_{av} = ReS_c \tag{2}
$$
To evaluate the total power flow *P* we integrate $S_{av}$ over the cross-sectional area *A*:
$$
P_{in} = P = \frac{1}{2}Re\displaystyle \int_A(E \times H^*) · dA = \frac{1}{2Z}\displaystyle \int^a_0\int^b_0|E_t|^2 · dxdy \tag{3}
$$
where:

- $E_t$ - the tangential component of $E$
- $Z$ - wave impedance of this wave type

The primary wave of the rectangular waveguide is *TE*10 mode, so the corresponding transmission power $P_{in}$:
$$
P_{in} = \frac{1}{2Z_{TE10}}\displaystyle \int^a_0\int^b_0|E^2_msin^2(\fracπa)|^2 · dxdy = \frac{ab}{4Z_{TE10}}E^2_m \tag{4}
$$
According to the TE10 wave field component:
$$
E_y(x,y) = - \frac{j\omega\mu a}{\pi}H_msin(\frac\pi ax)e^{-j\beta_{10}z} \tag{5}
$$
$E_m = \frac{\omega\mu a}{\pi}H_m$ is  the amplitude value of the component $E_y$ at the center of the wide edge of the waveguide.

Let's calculate  $Z_{TE10}$:

cut-off frequency $f_{c10}$:
$$
f_{c10} = \frac{1}{2a\sqrt{\mu\epsilon}} \tag{6}
$$
wave impedance $Z_{TE10}$:
$$
Z_{TE10} = \frac{\eta_0}{\sqrt{1 - (\frac{f_c10}{f})^2}} \tag{7}
$$
where:

- Intrinsic Impedance of air $\eta_0 = \sqrt(\frac{\mu_0}{\epsilon_0})$ = 120$\pi$ *Ω*

phase velocity $\nu_{p10}$:
$$
\nu_{p10} = \frac{\nu_{p0}}{\sqrt{1 - (\frac{f_c10}{f})^2}} \tag{8}
$$
where:

- $\nu_{p0}$ - phase velocity in vacuum, 3e8 m/s
- *f* - frequency

guide wavelength $\lambda_{g10}$:
$$
\lambda_{g10} = \frac{\nu_{p10}}{f} \tag{9}
$$

## Inport 

```
-2*beta0*Em*sin(kc*(tx))
```

## Heat source

divergence of  Poynting vector:
$$
- \bigtriangledown ·\frac{1}{2}(E \times H^*) = j\omega\frac{1}{2}\mu_c(H \times H^*) - j\omega\frac{1}{2}\epsilon^*_c(E \times E^*) + \frac{1}{2}\sigma(E \times E^*) \tag{10}
$$
where:

- complex permeability $\mu_c = \mu' - j\mu''$
- complex dielectric constant $\epsilon_c = \epsilon' - j\epsilon''$, so $\epsilon^*_c = \epsilon' + j\epsilon''$

Integrate the above equation over the volume V, using the divergence theorem, available Complex Poynting Theorem:
$$
-\frac{1}{2}\displaystyle \int_A(E \times H^*) · dA = \displaystyle \int_V(\frac{1}{2}\omega\mu''H · H^* - \frac{1}{2}\omega\epsilon''E · E^* + \frac{1}{4}\sigma E · E^*) dV + j2\omega\displaystyle \int_V(\frac{1}{4}\mu'H · H^* - \frac{1}{4}\epsilon'E · E^*) dV \\= \displaystyle \int_V(p_{eav} + p_{mav} + p_{jav})dV + j2\omega\displaystyle \int_V(w_{mav} - m_{eav})dV \tag{11}
$$
whose real part is active power, the imaginary part is reactive power.

where:

- $p_{eav}$ - average polarization loss power per unit volume
- $p_{mav}$ - average magnetization loss power per unit volume
- $p_{jav}$ - average Joule Loss Power per Unit Volume

Divergence of Poynting vector can be the heat source:

```
Heat Source = Variable "Div Poynting Vector re e"
Real MATC "abs(tx)/density"
```

For general substances, the electromagnetic power loss, Qε, can be obtained from the computed electric field:
$$
Q_{\epsilon} = \frac12\omega\epsilon_0\epsilon_r''|E|^2 \tag{12}
$$

```
Heat Source = Variable Electric field E
Real MATC "......(tx(0)^2)"+tx(1)^2+tx(2)^2+tx(3)^2+tx(4)^2+tx(5)^2)/density"
```

## HeatControlExplicit

Microwave heating heat source on / off according to the temperature of a point. Use HeatControlExplicit to implement it.

[elmerfem/fem/tests/HeatControlExplicit at devel · ElmerCSC/elmerfem (github.com)](https://github.com/ElmerCSC/elmerfem/tree/devel/fem/tests/HeatControlExplicit)

```
Apply Explicit Control = Logical True   
Number Of Controls = Integer 1    
Control Node Coordinates(1,3) = Real 0.04 0.0225 0.102
```

```
Heat Source = Variable "cpar, Div Poynting Vector re e"
Real Procedure "AlternatingSource" "AlternatingSource"
```

