# ------------------------------------------------------------
# Elmer Circuit Builder Library
# Description: This library enables automatic build of circuit
#              matrices in ElmerFEM format.
# Electrical Components Available: Voltage and Current Sources,
#                           Resistors, Inductors and Capacitors
# Author: Jonathan Velasco, CSC - IT Center for Science Ltd.
# Email: jvelasco@csc.fi
# Date: August 2021
# ------------------------------------------------------------

import os
import sys
import numpy as np
from datetime import date
import cmath


class Component:
    def __init__(self, name, pin1, pin2, value=None):
        self.name = name
        self.pin1 = pin1
        self.pin2 = pin2
        self.value = value

class R(Component):
    pass

class V(Component):
    pass

class I(Component):
    pass

class L(Component):
    pass

class C(Component):
    pass

class ElmerComponent(Component):
    def __init__(self, name, pin1, pin2, resistance_value, component_number, master_body_list, coiltype, number_of_turns=1, dimension="2D",sector=1, length=1, ):
        Component.__init__(self, name, pin1, pin2, resistance_value)
        self.component_number = component_number
        self.master_bodies = master_body_list
        self.coil_type = coiltype
        self.sector = sector
        self.length = length
        self.number_turns = number_of_turns
        self.dimension = dimension


class Circuit:
    def __init__(self, index):
        self.index = index
        self.components = []
        self.ref_node = None


def number_of_circuits(ncircuits):
    c = {}
    for i in range(1, ncircuits+1):
        c[i] = Circuit(i)

    return c


def get_component_information(components):
    cnode = []  # [n1,n2] # component node
    cmptype = []  # component type
    cval = []  # component value

    for cmp in components:
        cnode.append([cmp.pin1, cmp.pin2])
        cmptype.append(cmp)
        cval.append([cmp.value])

    return cnode, cmptype, cval



def get_num_nodes(components):
    # check for unique nodes
    unique_nodes = []

    for component in components:
        if component.pin1 not in unique_nodes:
            unique_nodes.append(component.pin1)
        if component.pin2 not in unique_nodes:
            unique_nodes.append(component.pin2)

    return len(unique_nodes)


def get_num_edges(components):
    return len(components)


def get_incidence_matrix(components, numnodes, numedges, n_ref):

    # use netlist node numbering as indices (n -1 so we can start indexing from 0)
    n1_index = [components[i].pin1 - 1 for i in range(0, numedges)]
    n2_index = [components[i].pin2 - 1 for i in range(0, numedges)]

    # dummy vector for indexing in A matrix
    plus_terminal = [i for i in range(0, numedges)]
    minus_terminal = [i for i in range(0, numedges)]

    # initialize A matrix
    Amat_plus = np.zeros(shape=(numnodes, numedges))
    Amat_minus = np.zeros(shape=(numnodes, numedges))

    for i, j in zip(n1_index, plus_terminal):
        Amat_plus[i][j] = 1

    for i, j in zip(n2_index, minus_terminal):
        Amat_minus[i][j] = -1

    # Incident matrix by adding the negative and positive nodes
    Amat_comp = Amat_plus + Amat_minus

    # Remove row of Amat_comp (Linear Independent Matrix)
    '''
    if n_ref: # FIXME?
        Amat = np.delete(Amat_comp, n_ref - 1, 0)  # remove GND/Ref node row
    else:
        Amat = Amat_comp[:-1, :]
    '''
    Amat = np.delete(Amat_comp, n_ref - 1, 0)  # remove GND/Ref node row
    #Amat = Amat_comp[:-1, :]


    return Amat

def get_incidence_matrix_str(components, numnodes, numedges, n_ref):

    # use netlist node numbering as indices (n -1 so we can start indexing from 0)
    n1_index = [components[i].pin1 - 1 for i in range(0, numedges)]
    n2_index = [components[i].pin2 - 1 for i in range(0, numedges)]

    # dummy vector for indexing in A matrix
    plus_terminal = [i for i in range(0, numedges)]
    minus_terminal = [i for i in range(0, numedges)]

    # initialize string matrices
    Amat_plus_str = np.chararray((numnodes, numedges))
    Amat_plus_str = np.chararray(Amat_plus_str.shape, itemsize=3)
    Amat_minus_str = np.chararray((numnodes, numedges))
    Amat_minus_str = np.chararray(Amat_minus_str.shape, itemsize=3)

    # initialize zero char in all matrices

    for i in range(0, numnodes):
        for j in range(0, numedges):
            Amat_plus_str[i][j] = ''
            Amat_minus_str[i][j] = ''

    for i, j in zip(n1_index, plus_terminal):
        Amat_plus_str[i][j] = str(1)

    for i, j in zip(n2_index, minus_terminal):
        Amat_minus_str[i][j] = str(-1)

    # Incident matrix by adding the negative and positive nodes
    Amat_comp_str = Amat_plus_str + Amat_minus_str

    Amat_str = np.delete(Amat_comp_str, n_ref - 1, 0)  # remove GND/Ref node row
    #Amat_str = Amat_comp_str[:-1, :]

    return Amat_str

def get_resistance_matrix(components, nedges, indr, indi, indcap):
    # initialize R matrix. R = R_r + R_i + R_cap
    # (resistor, current generators, capacitors)
    Rmat_r = np.zeros(shape=(nedges, nedges))
    Rmat_i = np.zeros(shape=(nedges, nedges))
    Rmat_cap = np.zeros(shape=(nedges, nedges))

    for i in indr:
        Rmat_r[i][i] = components[i].value

    for i in indi:
        Rmat_i[i][i] = 1

    for i in indcap:
        Rmat_cap[i][i] = 1

    # Add all R contributions
    Rmat = Rmat_r + Rmat_i + Rmat_cap

    return Rmat

def get_resistance_matrix_str(components, nedges, indr, indi, indcap, indcelm):

    # initialize R matrix. R = R_r + R_i + R_cap
    # (resistor, current generators, capacitors)
    Rmat_r_str = np.chararray((nedges, nedges))
    Rmat_r_str = np.chararray(Rmat_r_str.shape, itemsize=5)
    Rmat_i_str = np.chararray((nedges, nedges))
    Rmat_i_str = np.chararray(Rmat_i_str.shape, itemsize=5)
    Rmat_cap_str = np.chararray((nedges, nedges))
    Rmat_cap_str = np.chararray(Rmat_cap_str.shape, itemsize=5)

    # initialize zero char in all matrices

    for i in range(0, nedges):
        for j in range(0, nedges):
            Rmat_r_str[i][j] = ''
            Rmat_i_str[i][j] = ''
            Rmat_cap_str[i][j] = ''

    for i in indr:
        Rmat_r_str[i][i] = components[i].name

    for i in indi:
        Rmat_i_str[i][i] = str(1)

    for i in indcap:
        Rmat_cap_str[i][i] = str(1)


    # Add all R contributions
    Rmat_str = Rmat_r_str + Rmat_i_str + Rmat_cap_str

    return Rmat_str

def get_conductance_matrix(nedges, indr, indv, indInd):
    # initialize G matrix. G = G_r + G_v + G_ind
    # (resistor, voltage generators, inductors)
    Gmat_r = np.zeros(shape=(nedges, nedges))
    Gmat_v = np.zeros(shape=(nedges, nedges))
    Gmat_ind = np.zeros(shape=(nedges, nedges))
    Gmat_v_re = np.zeros(shape=(nedges, nedges))
    Gmat_v_im = np.zeros(shape=(nedges, nedges))

    for i in indr:
        Gmat_r[i][i] = -1

    for i in indv:
        Gmat_v[i][i] = 1

    for i in indInd:
        Gmat_ind[i][i] = 1

    # Add all G contributions
    Gmat = Gmat_r + Gmat_v + Gmat_ind

    return Gmat


def get_conductance_matrix_str(nedges, indr, indv, indInd):

    # initialize G matrix. G = G_r + G_v + G_ind
    # (resistor, voltage generators, inductors)
    Gmat_r_str = np.chararray((nedges, nedges))
    Gmat_r_str = np.chararray(Gmat_r_str.shape, itemsize=5)
    Gmat_v_str = np.chararray((nedges, nedges))
    Gmat_v_str = np.chararray(Gmat_v_str.shape, itemsize=5)
    Gmat_ind_str = np.chararray((nedges, nedges))
    Gmat_ind_str = np.chararray(Gmat_ind_str.shape, itemsize=5)

    # initialize zero char in all matrices
    for i in range(0, nedges):
        for j in range(0, nedges):
            Gmat_r_str[i][j] = ''
            Gmat_v_str[i][j] = ''
            Gmat_ind_str[i][j] = ''

    for i in indr:
        Gmat_r_str[i][i] = str(-1)

    for i in indv:
        Gmat_v_str[i][i] = str(1)

    for i in indInd:
        Gmat_ind_str[i][i] = str(1)


    # Add all R contributions
    Gmat_str = Gmat_r_str + Gmat_v_str + Gmat_ind_str

    return Gmat_str


def get_inductance_matrix(components, nedges, indInd):
    # initialize L matrix.
    Lmat = np.zeros(shape=(nedges, nedges))

    for i in indInd:
        Lmat[i][i] = -components[i].value

    return Lmat

def get_inductance_matrix_str(components, nedges, indInd):
    # initialize L matrix.
    Lmat_str = np.chararray((nedges, nedges))
    Lmat_str = np.chararray(Lmat_str.shape, itemsize=5)

    # initialize zero char in all matrices
    for i in range(0, nedges):
        for j in range(0, nedges):
            Lmat_str[i][j] = ''

    for i in indInd:
        Lmat_str[i][j] = "-" + components[i].name

    return Lmat_str


def get_capacitance_matrix(components, nedges, indcap):
    # initialize C matrix.
    Cmat = np.zeros(shape=(nedges, nedges))

    for i in indcap:
        Cmat[i][i] = -components[i].value

    return Cmat


def get_capacitance_matrix_str(components, nedges, indcap):
    # initialize L matrix.
    Cmat_str = np.chararray((nedges, nedges))
    Cmat_str = np.chararray(Cmat_str.shape, itemsize=5)

    # initialize zero char in all matrices
    for i in range(0, nedges):
        for j in range(0, nedges):
            Cmat_str[i][j] = ''

    for i in indcap:
        Cmat_str[i][j] = "-" + components[i].name

    return Cmat_str


def get_rhs(components, nedges, indi, indv):
    # initialize RHS vector. RHS = RHS_i + RHS_v
    # (current source, voltage source)
    rhs_v = np.zeros(shape=(nedges, 1))
    rhs_i = np.zeros(shape=(nedges, 1))

    complex_switch = True # used to create separate real and imag rhs

    for i in indi:
        if isinstance(components[i].value, complex):

            if(complex_switch):
                rhs_i_re = np.zeros(shape=(nedges, 1))
                rhs_i_im = np.zeros(shape=(nedges, 1))
                complex_switch = False

            rhs_i_re[i] = np.real(components[i].value)
            rhs_i_im[i] = np.imag(components[i].value)

            rhs_i = rhs_i_re + 1j*rhs_i_im

        else:
            rhs_i[i] = components[i].value

    complex_switch = True  # used to create separate real and imag rhs
    for i in indv:
        if isinstance(components[i].value, complex):
            if(complex_switch):
                rhs_v_re = np.zeros(shape=(nedges, 1))
                rhs_v_im = np.zeros(shape=(nedges, 1))
                complex_switch = False

            rhs_v_re[i] = -np.real(components[i].value)
            rhs_v_im[i] = -np.imag(components[i].value)

            rhs_v = rhs_v_re + 1j*rhs_v_im
        else:
            rhs_v[i] = -components[i].value

    rhs = rhs_v + rhs_i

    return rhs

def get_rhs_str(components, nedges, indi, indv):


    # initialize RHS vector. RHS = RHS_i + RHS_v
    # (current source, voltage source)
    rhs_v_str = np.chararray((nedges, 1))
    rhs_v_str = np.chararray(rhs_v_str.shape, itemsize=5)

    rhs_i_str = np.chararray((nedges, 1))
    rhs_i_str = np.chararray(rhs_i_str.shape, itemsize=5)

    # initialize zero char in all matrices
    for i in range(0, nedges):
        rhs_v_str[i] = ''
        rhs_i_str[i] = ''

    complex_switch = True # used to create separate real and imag rhs

    for i in indi:
        rhs_i_str[i] = components[i].name

    for i in indv:
        rhs_v_str[i] = '-' + components[i].name # -value

    rhs_str = rhs_v_str + rhs_i_str

    return rhs_str



def get_indices(components):
    # create indices per component
    indr = []
    indv = []
    indi = []
    indInd = []
    indcap = []
    indcelm = []

    for i, comp in enumerate(components):
        if isinstance(comp, R):
            indr.append(i)
        if isinstance(comp, V):
            indv.append(i)
        if isinstance(comp, I):
            indi.append(i)
        if isinstance(comp, L):
            indInd.append(i)
        if isinstance(comp, C):
            indcap.append(i)
        if isinstance(comp, ElmerComponent):
            indcelm.append(i)

    return indr, indv, indi, indInd, indcap, indcelm


def get_tableau_matrix(Amat, Rmat, Gmat, Lmat, Cmat, fvec, numnodes, numedges):
    M_kcl = np.block([Amat, np.zeros(shape=(numnodes - 1, numedges)), np.zeros(shape=(numnodes - 1, numnodes - 1))])
    M_kvl = np.block([np.zeros(shape=(numedges, numedges)), -np.eye(numedges, numedges), np.transpose(Amat)])
    M_comp = np.block([Rmat, Gmat, np.zeros(shape=(numedges, numnodes - 1))])

    # B matrix in Elmer
    Mmat1 = np.block([[M_kcl], [M_kvl], [M_comp]])

    # Source term
    bvec = np.block([[np.zeros(shape=(numnodes - 1, 1))], [np.zeros(shape=(numedges, 1))], [fvec]])

    # A matrix in Elmer
    Mmat2 = np.block([[np.zeros(shape=(numedges+(numnodes-1), 2*numedges+(numnodes-1)))],[Lmat, Cmat, np.zeros(shape=(numedges,numnodes - 1))]])

    return Mmat1, Mmat2, bvec

def get_tableau_matrix_str(Amat_str, Rmat_str, Gmat_str, Lmat_str, Cmat_str, fvec_str, numnodes, numedges):

    M_kcl_str = np.block([Amat_str, np.zeros(shape=(numnodes - 1, numedges)), np.zeros(shape=(numnodes - 1, numnodes - 1))])

    '''
    Amat_str_minus_transposed = np.transpose(Amat_str.copy())
    rows1, cols1 = Amat_str_minus_transposed.shape
    for i in range(0, rows1):
        for j in range(0, cols1):
            try:
                if Amat_str_minus_transposed[i][j].decode() == "-1" or Amat_str_minus_transposed[i][j].decode() == "-1.0":
                    Amat_str_minus_transposed[i][j] = str(1)
                elif Amat_str_minus_transposed[i][j].decode() == "1" or Amat_str_minus_transposed[i][j].decode() == "1.0":
                    Amat_str_minus_transposed[i][j] = str(-1)
            except:
                pass
    '''
    #print(np.transpose(Amat_str), Amat_str_minus_transposed)

    #M_kvl_str = np.block([np.zeros(shape=(numedges, numedges)), np.eye(numedges, numedges), Amat_str_minus_transposed])
    #print(M_kvl_str)

    M_kvl_str = np.block([np.zeros(shape=(numedges, numedges)), -np.eye(numedges, numedges), np.transpose(Amat_str.copy())])
    M_comp_str = np.block([Rmat_str, Gmat_str, np.zeros(shape=(numedges, numnodes - 1))])


    # B matrix in Elmer
    Mmat1_str = np.block([[M_kcl_str], [M_kvl_str], [M_comp_str]])

    # Source term
    for i in range(len(fvec_str)):
        if fvec_str[i].decode() == '' or fvec_str[i].decode() == '0.0' or fvec_str[i].decode() == '-0.0':
            fvec_str[i] = str(0)
    bvec_str = np.block([[np.zeros(shape=(numnodes - 1, 1))], [np.zeros(shape=(numedges, 1))], [fvec_str]])

    # redundant cleanup for int format looks
    for i in range(len(bvec_str)):
        if bvec_str[i][0].decode() == '' or bvec_str[i][0].decode() == '0.0' or bvec_str[i][0].decode() == '-0.0':
            bvec_str[i][0] = str(0)

    # A matrix in Elmer
    Mmat2_str = np.block([[np.zeros(shape=(numedges+(numnodes-1), 2*numedges+(numnodes-1)))],[Lmat_str, Cmat_str, np.zeros(shape=(numedges,numnodes - 1))]])


    rows, cols = Mmat1_str.shape
    for i in range(0, rows):
        for j in range(0, cols):
            if Mmat1_str[i][j].decode() == '' or Mmat1_str[i][j].decode() == '-0.0' or Mmat1_str[i][j].decode() == '0.0':
                Mmat1_str[i][j] = str(0)
            if Mmat1_str[i][j].decode() == '-1.0':
                Mmat1_str[i][j] = str(-1)
            if Mmat1_str[i][j].decode() == '1.0':
                Mmat1_str[i][j] = str(1)

    rows, cols = Mmat2_str.shape
    for i in range(0, rows):
        for j in range(0, cols):
            if Mmat2_str[i][j].decode() == '' or Mmat2_str[i][j].decode() == '-0.0' or Mmat2_str[i][j].decode() == '0.0':
                Mmat2_str[i][j] = str(0)
            if Mmat2_str[i][j].decode() == '-1.0':
                Mmat2_str[i][j] = str(-1)
            if Mmat2_str[i][j].decode() == '1.0':
                Mmat2_str[i][j] = str(1)

    return Mmat1_str, Mmat2_str, bvec_str


def solve_system(M1, M2, b, freq=50):

    iw = 1j * 2 * np.pi * freq

    if np.all((M2 == 0)):
        lhs = M1
    else:
        lhs = M1 + iw * M2

    rhs = b

    return np.linalg.solve(lhs, rhs)

def elmer_format_matrix(M1_str, M2_str, b_str, vcomp_rows, zero_rows):

    # elmer matrices don't allow entries in v_component(n) rows

    # ----------------------------------------------------------------------------
    #   Swap rows in M matrix to comply with vcomp rows = 0 in B matrix in Elmer
    # ----------------------------------------------------------------------------

    elmer_Bmat = np.copy(M1_str)
    elmer_source = np.copy(b_str)
    elmer_Amat = np.copy(M2_str)


    # swap zero rows to Vcomp rows
    for zrow, vcomprow in zip(zero_rows, vcomp_rows):
        elmer_Bmat[[zrow, vcomprow]] = elmer_Bmat[[vcomprow, zrow]]
        elmer_Amat[[zrow, vcomprow]] = elmer_Amat[[vcomprow, zrow]]
        elmer_source[[zrow, vcomprow]] = elmer_source[[vcomprow, zrow]]

    return elmer_Amat, elmer_Bmat, elmer_source

def create_unknown_name(components, ref_node, circuit_number):

    v_comp_rows = []
    unknown_names = []
    unknown_nodes = []

    # only include pins that are unknown (remove reference)
    for component in components:
        if (component.pin1 not in unknown_nodes) and (component.pin1 != ref_node):
            unknown_nodes.append(component.pin1)
        if (component.pin2 not in unknown_nodes) and (component.pin2 != ref_node):
            unknown_nodes.append(component.pin2)

    # create current I entries
    for i, component in enumerate(components):
        if isinstance(component, ElmerComponent):
            current_string = "\"i_component(" + str(component.component_number) + ")\""
        else:
            current_string = "\"i_" + component.name + "\""
        unknown_names.append(current_string)

    # create current V entries
    for i, component in enumerate(components):
        if isinstance(component, ElmerComponent):
            voltage_string = "\"v_component(" + str(component.component_number) + ")\""
        else:
            voltage_string = "\"v_" + component.name + "\""
        unknown_names.append(voltage_string)

    # create current potential entries
    for i, node_name in enumerate(unknown_nodes):
        node_string = "\"u_" + str(node_name) + "_circuit_" + str(circuit_number) + "\""
        unknown_names.append(node_string)

    # v_comp rows
    for i, name in enumerate(unknown_names):
        if "v_component(" in name:
            v_comp_rows.append(i)

    return unknown_names, v_comp_rows

def get_zero_rows(M1, M2, b):

    zero_rows_matrix = np.copy(M1) + np.copy(M2) + np.copy(b)

    check_zeros = np.where(~zero_rows_matrix.any(axis=1))[0]

    # changing format to list for uniformity
    zero_row_index = [zr for zr in check_zeros]

    return zero_row_index


def get_zero_rows_str(M1_str, M2_str, b_str):

    #zero_rows_matrix = np.copy(M1) + np.copy(M2) + np.copy(b)
    rows, cols = M1_str.shape
    zero_row_index = []
    zero_counter = 0
    for i in range(rows):
        for j in range(cols):
            m1_ = M1_str[i][j].decode().strip("-")
            m2_ = M2_str[i][j].decode().strip("-")

            zero_condition1 = (m1_ == str(0.0) or m1_ == str(0)) and (m2_ == str(0.0) or m2_ == str(0))

            if zero_condition1:
                zero_counter += 1

        b_ = b_str[i][0].decode().strip("-")

        zero_condition2 = (b_ == str(0.0) or b_ == str(0))

        if (zero_counter == cols) and zero_condition2:
            zero_row_index.append(i)

        zero_counter = 0

    return zero_row_index

def write_file_header(circuit, ofile):

    for i in range(1, len(circuit)+1):

        # loop over all circuits
        c = circuit[i]
        components = c.components[0]

        # only run script if there are no elmer components
        check_elmer_instance = [isinstance(components[i], ElmerComponent) for i in range(len(components))]
        check_component_values = [(component.value is None) for component in components]

        # condition that no elmer components in circuit
        isElmerComponent = True in check_elmer_instance
        isValueNone = True in check_component_values

        # if there are elmer components or there's no value component break the loop
        if not isElmerComponent:
            return 0

    # Remove file from previous matrix generation
    if os.path.isfile(ofile) is True:
        os.system('rm -r ' + ofile)

    elmer_file = open(ofile, 'w')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! ElmerFEM Circuit Generated: " + str(date.today().strftime("%B %d, %Y")), file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Number of Circuits in Model", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("$ Circuits = " + str(len(circuit)), file=elmer_file)
    print("", file=elmer_file)
    elmer_file.close()

def write_matrix_initialization(c, num_variables, ofile):

    # Write matrices in Elmer Format
    elmer_file = open(ofile, 'a')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Matrix Size Declaration and Matrix Initialization", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("$ C." + str(c.index) + ".variables = " + str(num_variables), file=elmer_file)
    print("$ C." + str(c.index) + ".perm = zeros(" + "C." + str(c.index) + ".variables" + ")", file=elmer_file)
    print("$ C." + str(c.index) + ".A = zeros(" + "C." + str(c.index) + ".variables," + "C." + str(c.index) +
          ".variables" + ")", file=elmer_file)
    print("$ C." + str(c.index) + ".B = zeros(" + "C." + str(c.index) + ".variables," + "C." + str(c.index) +
          ".variables" + ")", file=elmer_file)
    print("", file=elmer_file)
    elmer_file.close()

def write_unknown_vector(c, unknown_names, ofile):

    # Write matrices in Elmer Format
    elmer_file = open(ofile, 'a')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Dof/Unknown Vector Definition", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)

    for i, name in enumerate(unknown_names):
        print("$ C." + str(c.index) + ".name." + str(i+1) + " = " + name , file=elmer_file)

    print("", file=elmer_file)
    elmer_file.close()

def write_source_vector(c, source_vector, ofile):
    elmer_file = open(ofile, 'a')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Source Vector Definition", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    for i, source_name in enumerate(source_vector):
        if(source_name[0].decode() != str(0.0)) and (source_name[0].decode() != str(0)):
            print("$ C." + str(c.index) + ".source." + str(i+1) + " = \"" + source_name[0].decode().strip("-")+"_Source\"", file=elmer_file)
    print("", file=elmer_file)
    elmer_file.close()

def write_kcl_equations(c, num_nodes, num_variables, elmer_Amat, elmer_Bmat, ofile):

    elmer_file = open(ofile, 'a')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! KCL Equations", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)

    for i in range(num_nodes - 1):
        for j in range(num_variables):
            if (elmer_Bmat[i][j].decode() != str(0)) and (elmer_Bmat[i][j].decode() != str(0.0)) :
                print("$ C." + str(c.index) + ".B(" + str(i) + "," + str(j) + ")" + " = " + str(elmer_Bmat[i][j].decode()),
                      file=elmer_file)

    for i in range(num_nodes - 1):
        for j in range(num_variables):
            if (elmer_Amat[i][j].decode() != str(0)) and (elmer_Amat[i][j].decode() != str(0.0)):
                print("$ C." + str(c.index) + ".A(" + str(i) + "," + str(j) + ")" + " = " + str(elmer_Amat[i][j].decode()),
                      file=elmer_file)

    print("", file=elmer_file)
    elmer_file.close()

def write_kvl_equations(c, num_nodes, num_edges, num_variables, elmer_Amat, elmer_Bmat, unknown_names, ofile):

    range_init = num_nodes - 1

    # this trick switches all source voltage signs
    # to comply with Elmer's convention
    source_names = []
    components = c.components[0]
    for component in components:
        if(type(component) == V or type(component) == I):
          source_names.append(component.name)

    source_sign_index = []
    for i,name in enumerate(unknown_names):
        if (name.strip('"').strip("v_") in source_names or name.strip('"').strip("i_") in source_names):
            source_sign_index.append(i)
        else:
            source_sign_index.append(None)

    elmer_file = open(ofile, 'a')
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! KVL Equations", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)

    for i in range(range_init, num_edges + range_init):
        for j in range(num_variables):
            if (elmer_Bmat[i][j].decode().strip("-") != str(0)) and (elmer_Bmat[i][j].decode().strip("-") != str(0.0)):
                kvl_without_decimal = elmer_Bmat[i][j].decode().split(".")[0]
                if(j == source_sign_index[j]):
                    if("-" in kvl_without_decimal):
                        print("$ C." + str(c.index) + ".B(" + str(i) + "," + str(j) + ")" + " = " + str(kvl_without_decimal.strip("-")),
                              file=elmer_file)
                    else:
                        print("$ C." + str(c.index) + ".B(" + str(i) + "," + str(j) + ")" + " = -" + str(kvl_without_decimal.strip("-")),
                              file=elmer_file)
                else:
                    print("$ C." + str(c.index) + ".B(" + str(i) + "," + str(j) + ")" + " = " + str(kvl_without_decimal),
                          file=elmer_file)


    for i in range(range_init, num_edges + range_init):
        for j in range(num_variables):
            if (elmer_Amat[i][j].decode().strip("-") != str(0)) and (elmer_Amat[i][j].decode().strip("-") != str(0.0)):
                print("$ C." + str(c.index) + ".A(" + str(i) + "," + str(j) + ")" + " = " + str(elmer_Amat[i][j].decode()),
                      file=elmer_file)
    print("", file=elmer_file)

    elmer_file.close()


def write_component_equations(c, num_nodes, num_edges, num_variables, elmer_Amat, elmer_Bmat, ofile):

    range_init = num_nodes - 1 + num_edges

    elmer_file = open(ofile, 'a')

    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Component Equations", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)


    for i in range(range_init, num_edges + range_init):
        for j in range(num_variables):
            if (elmer_Bmat[i][j].decode().strip("-") != str(0)) and (elmer_Bmat[i][j].decode().strip("-") != str(0.0)):
                print("$ C." + str(c.index) + ".B(" + str(i) + "," + str(j) + ")" + " = " + str(elmer_Bmat[i][j].decode()),
                      file=elmer_file)

    print("", file=elmer_file)

    for i in range(range_init, num_edges + range_init):
        for j in range(num_variables):
            if (elmer_Amat[i][j].decode().strip("-") != str(0)) and  (elmer_Amat[i][j].decode().strip("-") != str(0.0)):
                print("$ C." + str(c.index) + ".A(" + str(i) + "," + str(j) + ")" + " = " + str(elmer_Amat[i][j].decode()),
                      file=elmer_file)

    print("", file=elmer_file)

    elmer_file.close()

def write_sif_additions(c, source_vector, ofile):

    components = c.components[0]

    # split and store components and sources
    source_components = []
    elmer_components = []
    for i, component in enumerate(components):
        if (isinstance(component, I) or isinstance(component, V)) and \
                component not in source_components:
            source_components.append(component)
        if (isinstance(component, ElmerComponent)) and \
                component not in elmer_components:
            elmer_components.append(component)

    # store source parameter value
    source_str_values = []
    for source_val in source_vector:
        if source_val[0].decode() != str(0.0) and source_val[0].decode() != str(0) \
                and source_val[0].decode() not in source_str_values:
            source_str_values.append(source_val[0].decode())


    elmer_file = open(ofile, 'a')

    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Additions in SIF file", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    if len(elmer_components) > 0:
        for ecomp in elmer_components:

            print("Component " + str(ecomp.component_number), file=elmer_file)
            print("  Name = \"" + str( ecomp.name) + "\"", file=elmer_file)

            # split integer and string list members: master bodies, and master bodies name
            str_mbody = []
            str_mb_count = 0
            int_mbody = []
            int_mb_count = 0
            for mbody in ecomp.master_bodies:
                if( type(mbody) == str):
                    str_mbody.append(mbody)
                    str_mb_count += 1
                elif(type(mbody) == int):
                    int_mbody.append(str(mbody))
                    int_mb_count += 1

            if(str_mbody):
                joined_str_master_names = ", ".join(str_mbody)
                print("  Master Bodies Name = " + str(joined_str_master_names), file=elmer_file)
            if(int_mbody):
                joined_str_master_bodies = ", ".join(int_mbody)
                print("  Master Bodies(" + str(int_mb_count) + ") = " +
                      str(joined_str_master_bodies) , file=elmer_file)
            # ------------------------------------------------------------------------------
            print("  Coil Type = \"" + str(ecomp.coil_type) + "\"", file=elmer_file)
            if ecomp.coil_type == "Stranded":
                print("  Number of Turns = Real $ N_" + str(ecomp.name), file=elmer_file)
                print("  Resistance = Real $ R_" + str(ecomp.name), file=elmer_file)
            if(ecomp.dimension == "3D" or ecomp.dimension == "3d" or ecomp.dimension == "3"):
                print("  ! Additions for 3D Coil", file=elmer_file)
                print("  Coil Use W Vector = Logical True", file=elmer_file)
                print("  W Vector Variable Name = String "'CoilCurrent e'"", file=elmer_file)
                print("  Electrode Area = Real $ Ae_" + str(ecomp.name) , file=elmer_file)
            else:
                print("  Coil Thickness = Real $ L_" + str(ecomp.name), file=elmer_file)
                print("  Symmetry Coefficient = Real $ 1/(Ns_" + str(ecomp.name) + ")", file=elmer_file)
            print("End \n", file=elmer_file)

    # store body forces per circuit to print later
    body_force_list = []
    for component, str_val in zip(source_components, source_str_values):
        name = component.name
        value = component.value

        val_sign = ""
        if "-" in str_val:
            val_sign = "-"
        if isinstance(value, complex):
            body_force_list.append("  " + name + "_Source re = Real $ "+ val_sign + "re_" + str_val.strip("-") + "*cos(phase_"+name+")")
            body_force_list.append("  " + name + "_Source im = Real $ "+ val_sign + "im_" + str_val.strip("-") + "*sin(phase_"+ name+")")
        else:
            body_force_list.append("  " + name + "_Source = Real $ " + str_val.strip("-"))

    elmer_file.close()

    return body_force_list

def write_parameters(c,ofile):

    components = c.components[0]
    elmer_file = open(ofile, 'a')


    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Parameters", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("", file=elmer_file)

    print("! General Parameters " , file=elmer_file)
    for component in components:
        if not isinstance(component, ElmerComponent):
            if(isinstance(component.value, complex)):
                print("! " + component.name + " = re_" + component.name + "+ j im_" + component.name + ", phase_"
                      + component.name + " = " + str(np.degrees(cmath.phase(component.value))) + "(Deg)", file=elmer_file)
                print("$ re_" + component.name + " = " + str(abs(np.real(component.value))), file=elmer_file)
                print("$ im_" + component.name + " = " + str(abs(np.imag(component.value))), file=elmer_file)
                print("$ phase_" + component.name + " = " + str(cmath.phase(component.value)), file=elmer_file)
            else:
                print("$ " + component.name + " = " + str(component.value), file=elmer_file)
    print("", file=elmer_file)

    for component in components:
        if isinstance(component, ElmerComponent):
            print("! Parameters in Component " + str(component.component_number) + ": " + str(component.name) , file=elmer_file)
            if(component.coil_type == "Stranded"):
                print("$ N_" + component.name + " = " + str(component.number_turns) + "\t ! Number of Turns", file=elmer_file)
            print("$ L_" + component.name + " = " + str(component.length) + "\t ! Component Length", file=elmer_file)
            print("$ Ns_" + component.name + " = " + str(component.sector) + "\t ! Sector/Symmetry Coefficient (e.g. 4 is 1/4 of the domain)", file=elmer_file)
            print("$ R_" + component.name + " = " + str(component.value) + "\t ! Resistance connected in Series (pin1-Resistance-" +str(component.name) +"-pin2)", file=elmer_file)
            print("$ Ae_" + component.name + " = 0.0025 " + "\t ! Electrode Area (dummy for now change as required)", file=elmer_file)
        print("", file=elmer_file)
    print("", file=elmer_file)

    elmer_file.close


def write_elmer_circuit_file(c, elmerA, elmerB, elmersource, unknown_names, num_nodes, num_edges, ofile):


    components = c.components[0]

    # only run script if there are no elmer components
    check_elmer_instance = [isinstance(components[i], ElmerComponent) for i in range(len(components))]
    check_component_values = [(component.value is None) for component in components]

    # condition that no elmer components in circuit
    isElmerComponent = True in check_elmer_instance

    # if there are elmer components or there's no value component break the loop
    if isElmerComponent:

        num_variables = len(unknown_names)
        write_parameters(c, ofile)
        write_matrix_initialization(c, num_variables, ofile)
        write_unknown_vector(c, unknown_names, ofile)
        write_source_vector(c, elmersource, ofile)
        write_kcl_equations(c, num_nodes, num_variables, elmerA, elmerB, ofile)
        write_kvl_equations(c, num_nodes, num_edges, num_variables, elmerA, elmerB, unknown_names, ofile)
        write_component_equations(c, num_nodes, num_edges, num_variables, elmerA, elmerB, ofile)
        body_forces=write_sif_additions(c, elmersource, ofile)

        print("Circuit model will be written in:", ofile)

        return body_forces


    else:
        print("No ElmerComponents found in Model Description")

def write_body_forces(body_force_def, ofile):

    elmer_file = open(ofile, 'a')

    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! Sources in SIF ", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("", file=elmer_file)

    print("Body Force 1", file=elmer_file)

    for ckt_body_force in body_force_def:
        if ckt_body_force is not None:
            for body_force in ckt_body_force:
                print(body_force , file=elmer_file)

    print("End", file=elmer_file)

    print("", file=elmer_file)

    print("! -----------------------------------------------------------------------------", file=elmer_file)
    print("! End of Circuit", file=elmer_file)
    print("! -----------------------------------------------------------------------------", file=elmer_file)

    elmer_file.close()


def solve_circuit(circuit):

    # loop over all circuits
    source_components = []  # store sources separately for Body Force 1
    for i in range(1, len(circuit) + 1):

        # loop over all circuits
        c = circuit[i]
        components = c.components[0]
        ref_node = c.ref_node

        # only run script if there are no elmer components
        check_elmer_instance = [isinstance(components[i], ElmerComponent) for i in range(len(components))]
        check_component_values = [(component.value is None) for component in components]

        # condition that no elmer components in circuit
        isElmerComponent = True in check_elmer_instance
        isValueNone = True in check_component_values

        # if there are elmer components or there's no value component break the loop
        if isElmerComponent or isValueNone:
            print("This model will not be solved externally due to:")
            print("\t Elmer Circuit Model:", isElmerComponent)
            print("\t Component Values not Defined:", isValueNone)

            if isElmerComponent:
                print("Include circuit file in .sif file to be run with ElmerSolver")
            break

        # number of nodes and edges in our network
        num_nodes = get_num_nodes(components)
        num_edges = get_num_edges(components)

        # indices numbered based on component type
        # ind resistor, voltage, current, inductor, capacitor, elmer comp
        indr, indv, indi, indInd, indcap, indcelm = get_indices(components)

        # incidence/connectivity matrix for KCL and KVL
        A = get_incidence_matrix(components, num_nodes, num_edges, ref_node)

        # R matrix including current generators
        R = get_resistance_matrix(components, num_edges, indr, indi, indcap)

        # G matrix including voltage generators
        G = get_conductance_matrix(num_edges, indr, indv, indInd)

        # The following matrices are only needed in time/harmonic cases

        # L matrix including
        L = get_inductance_matrix(components, num_edges, indInd)

        # C matrix including
        C = get_capacitance_matrix(components, num_edges, indcap)

        # RHS = source vector f
        f = get_rhs(components, num_edges, indi, indv)

        # M Matrix and b full source vector RHS (M1x + M2x' = b)
        M1, M2, b = get_tableau_matrix(A, R, G, L, C, f, num_nodes, num_edges)

        # get/create unknown vector name and the v_comp index and source names/index
        unknown_names, vcomp_rows = create_unknown_name(components, ref_node, i)

        # Solve Mx = b if no elmer components
        print("This is NOT an Elmer Circuit model")
        print("Solution: ")
        x = solve_system(M1, M2, b)
        for var, val in zip(unknown_names, x):
            print(var, val)




def generate_elmer_circuits(circuit, ofile):

    # create list to store all body forces from each circuit def
    all_body_forces = []

    # write elmer file header
    write_file_header(circuit, ofile)

    # loop over all circuits
    source_components = []     # store sources separately for Body Force 1
    for i in range(1, len(circuit)+1):

        # loop over all circuits
        c = circuit[i]
        components = c.components[0]
        ref_node = c.ref_node

        # number of nodes and edges in our network
        num_nodes = get_num_nodes(components)
        num_edges = get_num_edges(components)

        # indices numbered based on component type
        # ind resistor, voltage, current, inductor, capacitor, elmer comp
        indr, indv, indi, indInd, indcap, indcelm = get_indices(components)

        # incidence/connectivity matrix for KCL and KVL
        A_str = get_incidence_matrix_str(components, num_nodes, num_edges, ref_node)

        # R matrix including current generators
        R_str = get_resistance_matrix_str(components, num_edges, indr, indi, indcap, indcelm)

        # G matrix including voltage generators
        G_str = get_conductance_matrix_str(num_edges, indr, indv, indInd)

        # The following matrices are only needed in time/harmonic cases

        # L matrix including
        L_str = get_inductance_matrix_str(components, num_edges, indInd)

        # C matrix including
        C_str = get_capacitance_matrix_str(components, num_edges, indcap)

        # RHS = source vector f
        f_str = get_rhs_str(components, num_edges, indi, indv)

        # M Matrix and b full source vector RHS (M1x + M2x' = b)
        M1_str, M2_str, b_str = get_tableau_matrix_str(A_str, R_str, G_str, L_str, C_str, f_str, num_nodes, num_edges)

        # get/create unknown vector name and the v_comp index and source names/index
        unknown_names, vcomp_rows = create_unknown_name(components, ref_node, i)

        # get rows filled with zeros
        zero_rows_str = get_zero_rows_str(M1_str, M2_str, b_str)

        # create elmer matrices
        elmerA, elmerB, elmersource = elmer_format_matrix(M1_str, M2_str, b_str, vcomp_rows, zero_rows_str)

        # create elmer circuits file
        body_forces = write_elmer_circuit_file(c, elmerA, elmerB, elmersource, unknown_names, num_nodes, num_edges, ofile)
        all_body_forces.append(body_forces)

        # just for debugging. valued matrices and solution solve if no elmer components
        solve_circuit(circuit)

    write_body_forces(all_body_forces, ofile)
