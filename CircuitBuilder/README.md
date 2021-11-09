# What is the CircuitBuilder?
The Elmer CircuitBuilder is a Python library with components such as ideal sources(V,I), resistors(R), capacitors (C), and inductors (L) to streamline the creation of circuit networks and enable circuit-field simulations. 
In this directory you'll find basic examples on how to model coils in Magnetodynamics applications using ElmerFem. 
Global sources (e.g., Ideal Voltage and Current Sources) can be added using electrical networks with the help of elmer_circuitbuilder.py. 


# How to Download the CircuitBuilder
To use and take advantage of the models in this directory, please download Elmer CircuitBuilder: 
$ pip install elmer-circuitbuilder

# How does Elmer CircuitBuilder work?
After importing all the functions within the library into a fresh Python file, you can develop a circuit network 
using a common two-terminal component approach. Where each component has two nodes that are connected to their
"high" and "low" potentials. 

The steps are simple: \

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

The CircuitBuilder builds the stiffness and damping matrices coupled to your FEM coil model of choice: massive, stranded, and foil in 2D and 3D.

![3D Stranded Closed Coil](TEAM7_current_source.pdf)
