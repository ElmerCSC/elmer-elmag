# This script will install the necessary requirements
# to generate a mesh from BREP CAD export files. It
# requires python3 to be present on your path.

# Execute this script within your current shell:

# > source ./prepare.sh

# I.e. do not execute it in a subshell otherwise
# the python virtual environment will not be activated
# in your current shell.

python3 -m venv .venv
. .venv/bin/activate
pip --require-virtualenv install gmsh
