import os
from pathlib import Path

import pydot
from elmer_circuitbuilder import (
    Circuit,
    ElmerComponent,
    I,
    generate_elmer_circuits,
    number_of_circuits,
)


def create_circuit_delta(dir_out: Path):
    """Creates circuit definition file for Elmer.

    Args:
        dir_out: Directory the circuit definition file is written to

    Returns:
        Filename of the circuit definition file.
    """
    # initialize circuits: number of circuits - do not remove
    c = number_of_circuits(3)
    # reference/ground node needed - do not remove.
    c[1].ref_node = 1
    c[2].ref_node = 1
    c[3].ref_node = 1

    # Components
    peak_current = 6.0
    current_0 = I("I_0", 1, 2, peak_current)
    current_1 = I("I_1", 1, 2, peak_current)
    current_2 = I("I_2", 1, 2, peak_current)

    phase_0 = ElmerComponent("Phase_0", 2, 1, 1, [2, 3], 1)
    phase_1 = ElmerComponent("Phase_1", 2, 1, 2, [6, 7], 1)
    phase_2 = ElmerComponent("Phase_2", 2, 1, 3, [4, 5], 1)
    phases = [phase_0, phase_1, phase_2]
    for phase in phases:
        n_turns = 35
        resistance = 0.8 / float(n_turns)
        phase.stranded(n_turns, resistance)

    # store components in array components = [comp1, comp2,...] - do not remove
    c[1].components.append([current_0, phase_0])
    c[2].components.append([current_1, phase_1])
    c[3].components.append([current_2, phase_2])

    fname_out = "circuit.definition"
    path_out = dir_out / fname_out
    if os.path.exists(path_out):
        os.remove(path_out)

    # generate elmer circuit.definitions - do not remove / do not edit
    generate_elmer_circuits(c, path_out)

    return c


def plot_circuit(circuits: dict[int, Circuit], fname: Path):
    """Plots the circuit."""
    # TODO: Show reference Node
    graph = pydot.Dot(graph_type="digraph")
    pins = []

    # Add components as graph nodes
    for key, circuit in circuits.items():
        for comp in circuit.components[0]:
            pins.append(f"{key}_{comp.pin1}")
            pins.append(f"{key}_{comp.pin2}")
            graph.add_node(
                pydot.Node(
                    comp.name, style="filled", fillcolor="turquoise", shape="box"
                )
            )

    # Add nodes/pins as graph nodes
    pins = list(set(pins))
    for pin in pins:
        graph.add_node(
            pydot.Node(f"{pin}", style="filled", fillcolor="red", shape="circle")
        )

    # Connect components and nodes/pins
    for key, circuit in circuits.items():
        for comp in circuit.components[0]:
            edge = pydot.Edge(f"{key}_{comp.pin1}", comp.name)
            graph.add_edge(edge)
            edge = pydot.Edge(comp.name, f"{key}_{comp.pin2}")
            graph.add_edge(edge)

    save = getattr(graph, f"write_{fname.suffix.replace('.', '')}")
    save(fname)


if __name__ == "__main__":
    # Change path to your case directory
    path = Path(__file__).parent
    circuits = create_circuit_delta(Path(path))
    plot_circuit(circuits, path / "circuit.png")
