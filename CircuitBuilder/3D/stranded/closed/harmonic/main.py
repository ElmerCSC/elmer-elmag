
# -----------------------------------------------------------------------------------------------------
# Elmer circuit builder main:
#
# Description:
#                This is a tool to write circuits in Elmer format using pin-connection convention.
#
# Instructions:
#                 1) Import the circuit builder library (from circuit_builder import *)
#                 2) Set output file name as a string (e.g output_file = "string_circuit.definitions")
#                 3) Set number of circuits with number_of_circuits(n) (e.g c = number_of_circuits(1))
#                 4) Set your ground/reference node in the current circuit c[1].ref_node = pin_number
#                 5) Select and configure electrical component
#                     Resistors (R), Inductors (L), Capacitors (C), Voltage Source (V), Current Source (I)
#                     or FEM Component (ElmerComponent)
#                     Arguments needed for R, L, C, V, I are the same. ElmerComponent takes additional arguments
#                 6) Add circuit components to circuit c[n].components.append([R1, V1, ElmerFemCoil, ...etc])
#                 7) Write circuits generate_elmer_circuits(c, output_file)
#                 8) Output file must be included in .sif file
#
# ------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------
# Imported Libraries:
from elmer_circuitbuilder import *

# -----------------------------------------------------------------------------------------------------

def main(argv=None):

    # name output file
    output_file = "harmonic_TEAM7_circuit.definition"

    # initialize circuits: number of circuits - do not remove
    c = number_of_circuits(1)

    # ------------------------------------------Circuit 1 ------------------------------------------------
    # ---------------(Current Source - Steady State /  Modify Body Force 1 in file for Transient )---------------------

    # reference/ground node needed - do not remove.
    c[1].ref_node = 1

    # Components
    Is = I("Is", 1, 2, 2742+1j*0)
    FEM_Component1 = ElmerComponent("Coil1", 2, 1, 1, ["Coil"])

    # Define coil parameters in 3D
    FEM_Component1.is3D()
    FEM_Component1.stranded(1, 0)
    FEM_Component1.isClosed()

    # store components in array components = [comp1, comp2,...] - do not remove
    c[1].components.append([Is, FEM_Component1])

    # --------------------------------------------------

    # generate elmer circuit.definitions - do not remove / do not edit
    generate_elmer_circuits(c, output_file)

    return 0


if __name__ == "__main__":
    sys.exit(main() or 0)

