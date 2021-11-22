# 3D Closed Massive Coil - Transient


## How to create the circuit?
The circuit is created using the CircuitBuilder and it's stored in the main.py file. To create or modify the circuit run the following command in terminal
```
$ python3 main.py
```

This will output a file with the extension .definition. This file contains the circuit definition, Body Force 1 and Component required by Elmer's .sif file. In these models the file is included in the .sif file as 

```
Include "transient_massive3D_circuit.definition"
```

## Transient Source

The main setup outputs a DC source by default. 

```
Body Force 1
  V1_Source = Variable "time" 
  	 Real MATC "V1"
End

```

 Adding time-dependencies to the excitation source requires modifying the .definition file's Body Force 1. 
To add a sinusoidal excitation 

```
Body Force 1
  V1_Source = Variable "time" 
  	 Real MATC "V1*sin(omega*tx)"
End

```

# How to the model?

## Serial run

```
$ ElmerSolver helix_coil_massive.sif.sif
```

## Parallel run with MPI
Before attempting to run it in parallel:
* Make sure to have an ElmerFEM MPI-enabled build.
* Your mesh needs to be in ElmerSolver format

If you comply with the two requirements above:
* Partition the mesh according to the number of MPI processes. 

For example if you are using 3 processes, partition your mesh using ElmerGrid as follows

```
ElmerGrid 2 2 helix_closed_coil -partdual -metiskway 3
```
This will create a partitioning directory within the mesh directory. There are other ways to partition your mesh. For more information you can always look into ElmerGrid's documentation.

To run your model in parallel using 3 MPI processes

```
$ mpirun -np 3 ElmerSolver_mpi helix_coil_massive.sif.sif
```

