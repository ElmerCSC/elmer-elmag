
# Voltage Source

This model shows how to set up an ideal DC voltage source driving a 2D massive FEM coil. 

The model is solved in transient and time-harmonic cases and each case is stored in their respective directory. 


<p align="center">
  <img src=voltage_source_sch.png width="400" height="400">
</p>


# How to create the circuit?
The circuit is created using the CircuitBuilder and it's stored in the main.py file. To create or modify the circuit run the following command in terminal
```
$ python3 main.py

```
This will output a file with the extension .definition. This file contains the circuit definition, Body Force 1 and Component required by Elmer's .sif file. In these models the file is included in the .sif file as 

```
Include "<name_of_circuit>.definition"

```

# How to run Elmer's model?

Elmer can be run in serial or in parallel. Depending on the model you're running the <name_of_model>.sif will be either harmonic_current_wire.sif or transient_current_wire.sif


## Serial run

```
$ ElmerSolver <name_of_model>.sif

```

## Parallel run with MPI
Before attempting to run it in parallel:
* Make sure to have an ElmerFEM MPI-enabled build.
* Your mesh needs to be in ElmerSolver format

If you comply with the two requirements above:
* Partition the mesh according to the number of MPI processes. 

For example if you are using 2 processes, partition your mesh using ElmerGrid as follows

```
ElmerGrid 2 2 <mesh_directory> -partdual -metiskway 2
```
This will create a partitioning directory within the mesh directory. There are other ways to partition your mesh. For more information you can always look into ElmerGrid's documentation.

To run your model in parallel using 2 MPI processes

```
$ mpirun -np 2 ElmerSolver_mpi <name_of_model>.sif

```

