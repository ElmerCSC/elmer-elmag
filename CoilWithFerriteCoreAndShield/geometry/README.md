# Edit Geometry with FreeCAD

To edit the geometry, you need to install [FreeCAD](https://www.freecad.org/). Once FreeCAD is present on your
path, you can open the CAD file from the command line:

```
> freecad coil.FCStd
```

Once you're satisfied with your changes, export each object into a separate `.brep` file.


# Generate Mesh with gmsh

This example uses [GMSH](https://gmsh.info/)'s python API to produce a mesh from the exported CAD files.
To generate the mesh you need to have GMSH installed on your machine and present on your path.

This directory contains a `prepare.sh` file that will create a python virtual environment and install
gmsh into it.

```
> source ./prepare.py
```

You can then start mesh conversion:

```
> ./generate-mesh.py
```

You can change the `generate-mesh.py` python file to import your own CAD files. Based on the example script it
shouldn't be difficult to change the geometry.

Obs.: The `generate-mesh.py` script requires your local python virtual environment to be activated. See
`prepare.sh` for details.

To deactivate your virtual environment type:

```
> deactivate
```

To re-activate your virtual environment:

```
> .venv/bin/activate
```

To uninstall it:

```
> rm -rf .venv
```


# Debug Mesh with ElmerGrid

Execute the `debug-geometry.sh` script to pass the generated mesh through ElmerGrid and check presence,
mesh density and numbering of the physical volumes and surfaces as seen by Elmer. You can hide/show
entities via GMSH's visibility tool (Press Shift-Ctrl-V to display it).

Take note of the numbering of the physical volumes as well as ElmerGrid's log output which contains
the mapping from Mesh numbering to final physical volume/surface numbering which you need to reference
in your Elmer `.sif` file.
