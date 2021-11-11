# What is the CircuitBuilder?
The Elmer CircuitBuilder is a Python library with components such as ideal sources(V,I), resistors(R), capacitors (C), and inductors (L) 
to streamline the creation of circuit networks and enable circuit-field simulations by building the stiffness and damping matrices needed.
The available Finite Element coil models in Elmer's component block are: massive, stranded and foil, in 2D and 3D. 

In this directory you'll find basic examples on how to model coils in Magnetodynamics applications using ElmerFem. 
Global sources (e.g., Ideal Voltage and Current Sources) can be added using electrical networks with the help of the elmer_circuitbuilder Python module. 

![3D Stranded Closed Coil](TEAM7_current_source.png)

# Coil Models: Massive, Stranded, and Foil



## References
For information about massive, stranded and foil coil types

@article{dular2002spatially,
  title={Spatially dependent global quantities associated with 2-D and 3-D magnetic vector potential formulations for foil winding modeling},
  author={Dular, Patrick and Geuzaine, Christophe},
  journal={IEEE transactions on magnetics},
  volume={38},
  number={2},
  pages={633--636},
  year={2002},
  publisher={IEEE}
}

More references can be found on page 128 of Elmer's Documentation: [Elmer Documentation](http://www.nic.funet.fi/pub/sci/physics/elmer/doc/ElmerModelsManual.pdf)


# How to Download the CircuitBuilder
To use and take advantage of the models in this directory go to Terminal and enter:

```
$ pip install elmer-circuitbuilder
```

# How does Elmer CircuitBuilder work?
After importing all the functions within the library into a fresh Python file (see main_template.py or main.py in each model), you can develop a circuit network 
using a common two-terminal component approach. 

The steps are simple. The idea is that following the main.py template on the examples in this directory you'd be able to create
your own circuit to model coils with ElmerFEM. The steps are summarized below.

Instructions: \
               1) Import the circuit builder library (from elmer_circuitbuilder import *) \
               2) Set output file name as a string (e.g output_file = "string_circuit.definitions") \
               3) Set number of circuits with number_of_circuits(n) (e.g c = number_of_circuits(1)) \
               4) Set your ground/reference node in the current circuit c[1].ref_node = pin_number \
               5) Select and configure electrical component \
                     Resistors (R), Inductors (L), Capacitors (C), Voltage Source (V), Current Source (I)
                     or FEM Component (ElmerComponent) 
                     Arguments needed for R, L, C, V, I is the same. ElmerComponent takes additional arguments\
               6) Add circuit components to circuit c[n].components.append([R1, V1, ElmerFemCoil, ...etc]) \
               7) Write circuits generate_elmer_circuits(c, output_file) \
               8) Output file must be included in .sif file 



