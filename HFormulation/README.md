# HFormulation

Implementation of the **_H_** formulation of the Maxwell equations in 3D. The original solver was the "Electromagnetic Waves" one that served as template to solve the magnetic field **_H_** intead of the electric field **_E_**.

The **_H_** formulation is expressed as:
```math
\nabla \times \rho \nabla \times \mathbf{h} + \partial_t \mathbf{b} = 0
```
Its weak formulation, assuming the natural condition that the tangent of the electric field is zero at the boundary, is then:
```math
\int_\Omega \rho \nabla  \times \mathbf{h} \cdot \nabla \times \mathbf{h}^\star \mathrm{d}\Omega
+
\int_\Omega \partial_t \left(\mu \mathbf{h}\right) \cdot \mathbf{h}^\star \mathrm{d}\Omega 
= 
0
```
with $`\mathbf{h}^\star`$, the test function. $`\Omega`$ is the full domain made of a conductive domain $`\Omega_c`$ and a non-conductive domain $`\Omega_{cc}`$ ($`\Omega = \Omega_c \cup \Omega_{cc}`$). The boundary of the domain $`\Omega`$: $`\partial\Omega = \Gamma`$.

The current density $`\mathbf{j}`$ is computed from the magnetic field from: $`\mathbf{j} = \nabla \times \mathbf{h}`$.

Using Whitney elements (edge elements), the non-conductive domains $`\Omega_{cc}`$ should have a fictitious electrical resistivity. Here, we used $`\rho =1~\Omega.`$m in the non-conductive domain. We assumed that the electrical material has a resistivity several orders of magnitude lower.

## Case study

The case study is the induction of current in a conductive bulk. At the boundary of the universe, the magnetic field $`\mathbf{h}`$ is given by an impressed external field $`\mathbf{h}_{ext}`$ so that:
```math
\mathbf{h}|_{\Gamma} = \mathbf{h}_{ext}\left(t\right)
```



