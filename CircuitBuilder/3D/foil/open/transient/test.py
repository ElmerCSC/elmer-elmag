import sys
import numpy as np

def len_columns_rows(array):
    return len(array), len(array[0])

def sparse_matrix_multiplication(matrix_a, matrix_b):
    nrows_a, ncol_a = len_columns_rows(matrix_a)
    nrows_b, ncol_b = len_columns_rows(matrix_b)

    if ncol_a != nrows_b:
        return [[]]

    mat_a_nz = remove_mat_zeros(matrix_a)
    mat_b_nz = remove_mat_zeros(matrix_b)
    solution = [[0]*ncol_a for _ in range(nrows_b-1)]
    #print(solution)

    for i, k in mat_a_nz.keys():
        for j in range(nrows_b):
            if (k, j) in mat_b_nz.keys():
                solution[i][j] += mat_a_nz[(i, k)] * mat_b_nz[(k,j)]

    return solution

def remove_mat_zeros(mat):
    non_zero_dict = {}
    nrows = len(mat)
    ncols = len(mat[0])
    for i in range(0, nrows):
        for j in range(0, ncols):
            if mat[i][j] != 0:
                non_zero_dict[(i,j)] = mat[i][j]

    return non_zero_dict

def main(argv=None):

    matrix_a = [[0, 2, 0],
                [0, -3, 5]]
    matrix_b = [[0, 10, 0],
                [0, 0, 0],
                [0, 0, 4]]

    solution = sparse_matrix_multiplication(matrix_a, matrix_b)
    #print(solution)

    #mylist = ["a", "b", "a", "c", "c"]
    #mylist = list(dict.fromkeys(mylist))
    #print(mylist)

    mylist = [1,2,3,4]
    #mylist = list(dict.fromkeys(mylist))

    mode = max(set(mylist), key=mylist.count)
    print('mylist', mode)

    for _ in range(3):
        print(np.random.rand(4))

    print(" ")

    for _ in range(3):
        np.random.seed(2021)
        print(np.random.rand(4))




    return 0

if __name__ == "__main__":
    sys.exit(main() or 0)

