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



Elmer CircuitAndDynamics Module Documentation:
<A HREF="http://www.nic.funet.fi/pub/sci/physics/elmer/doc/ElmerModelsManual.pdf#page=128">

If you use the CircuitAndDynamics module or the CircuitBuilder, please make sure to cite the following reference:

[9] Eelis Takala, Evren Yurtesen, Jan Westerholm, Juha Ruokolainen, and Peter Råback. Parallel simula- tions of inductive components with elmer finite element software in cluster environments. Electromag- netics, 36(3):167–185, April 2016.


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


The steps can be easily spotted on the following template

```
# Imported Libraries:
import sys
import os
from elmer_circuitbuilder import *                                       # STEP 1
# -----------------------------------------------------------------------------------------------------

def main(argv=None):

    # name output file - do not remove                                   # STEP 2
    output_file = ""

    # initialize circuits: number of circuits - do not remove            # STEP 3
    c = number_of_circuits(1)

    # reference/ground node needed - do not remove.                      # STEP 4
    c[1].ref_node = 1

    # ------------- Electrical Network Definition -------------          # STEP 5

    # Components

    # Define coil type: massive, stranded, foil

    # Define dimension related features if needed (closed, open)

    # store components in array components = [comp1, comp2,...] - do not remove
    c[1].components.append([])

    # --------------------------------------------------

    # generate elmer circuit.definitions - do not remove / do not edit   # STEP 6
    generate_elmer_circuits(c, output_file)                              # STEP 7

    return 0


if __name__ == "__main__":
    sys.exit(main() or 0)

```

Note that STEP 8 is not part of the template but rather on the .sif file itself as an include of the .definition file created by the CircuitBuilder

```
Include "<name_of_circuit>.definition"
```

