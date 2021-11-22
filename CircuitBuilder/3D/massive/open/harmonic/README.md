# 3D Open Massive Coil - Harmonic


## How to create the circuit?
The circuit is created using the CircuitBuilder and it's stored in the main.py file. To create or modify the circuit run the following command in terminal
```
$ python3 main.py
```

This will output a file with the extension .definition. This file contains the circuit definition, Body Force 1 and Component required by Elmer's .sif file. In these models the file is included in the .sif file as 

```
Include "harmonic_open3Dmassive_circuit.definition"
```

# How to run the model?

## Serial run

```
$ ElmerSolver harmonic_open_wire.sif
```

## Parallel run with MPI
Before attempting to run it in parallel:
* Make sure to have an ElmerFEM MPI-enabled build.
* Your mesh needs to be in ElmerSolver format

If you comply with the two requirements above:
* Partition the mesh according to the number of MPI processes. 

For example if you are using 3 processes, partition your mesh using ElmerGrid as follows

```
ElmerGrid 2 2 wire -partdual -metiskway 3
```
This will create a partitioning directory within the mesh directory. There are other ways to partition your mesh. For more information you can always look into ElmerGrid's documentation.

To run your model in parallel using 3 MPI processes

```
$ mpirun -np 3 ElmerSolver_mpi harmonic_open_wire.sif
```

