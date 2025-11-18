"""
numpy - Numerical computing
Install: pip install numpy

Fundamental package for numerical computing with arrays and linear algebra
"""

import numpy as np


def array_creation():
    """Create arrays in various ways"""
    # From list
    array_1d = np.array([1, 2, 3, 4, 5])
    array_2d = np.array([[1, 2, 3], [4, 5, 6]])

    # Using built-in functions
    zeros = np.zeros((3, 4))  # 3x4 array of zeros
    ones = np.ones((2, 3))  # 2x3 array of ones
    empty = np.empty((2, 2))  # Uninitialized array
    eye = np.eye(4)  # 4x4 identity matrix

    # Ranges
    arange = np.arange(0, 10, 2)  # [0, 2, 4, 6, 8]
    linspace = np.linspace(0, 1, 5)  # 5 evenly spaced values from 0 to 1

    # Random arrays
    random_array = np.random.rand(3, 3)  # Random values [0, 1)
    random_int = np.random.randint(0, 100, size=(3, 3))  # Random integers
    random_normal = np.random.randn(3, 3)  # Standard normal distribution

    print(f"1D Array: {array_1d}")
    print(f"2D Array:\n{array_2d}")
    print(f"Zeros:\n{zeros}")
    print(f"Arange: {arange}")


def array_operations():
    """Basic array operations"""
    array1 = np.array([1, 2, 3, 4])
    array2 = np.array([5, 6, 7, 8])

    # Element-wise operations
    add = array1 + array2
    subtract = array1 - array2
    multiply = array1 * array2
    divide = array1 / array2
    power = array1 ** 2

    # Scalar operations
    scalar_mult = array1 * 3
    scalar_add = array1 + 10

    print(f"Add: {add}")
    print(f"Multiply: {multiply}")
    print(f"Power: {power}")


def array_indexing_slicing():
    """Array indexing and slicing"""
    array = np.array([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]])

    # Basic indexing
    element = array[0, 1]  # Row 0, Column 1 = 2
    row = array[1]  # Second row = [5, 6, 7, 8]
    column = array[:, 2]  # Third column = [3, 7, 11]

    # Slicing
    sub_array = array[0:2, 1:3]  # Rows 0-1, Columns 1-2

    # Boolean indexing
    mask = array > 5
    filtered = array[mask]  # All elements > 5

    print(f"Element at [0,1]: {element}")
    print(f"Second row: {row}")
    print(f"Third column: {column}")
    print(f"Filtered (> 5): {filtered}")


def array_reshaping():
    """Reshape and manipulate array dimensions"""
    array = np.arange(12)  # [0, 1, 2, ..., 11]

    # Reshape
    reshaped_3x4 = array.reshape(3, 4)
    reshaped_2x6 = array.reshape(2, 6)

    # Transpose
    transposed = reshaped_3x4.T

    # Flatten
    flattened = reshaped_3x4.flatten()
    raveled = reshaped_3x4.ravel()  # Flattened view (not copy)

    print(f"Original: {array}")
    print(f"Reshaped (3x4):\n{reshaped_3x4}")
    print(f"Transposed:\n{transposed}")


def array_statistics():
    """Statistical operations"""
    array = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

    # Basic stats
    mean_value = np.mean(array)
    median_value = np.median(array)
    std_dev = np.std(array)
    variance = np.var(array)

    # Along axis
    mean_rows = np.mean(array, axis=0)  # Mean of each column
    mean_cols = np.mean(array, axis=1)  # Mean of each row

    # Min/Max
    min_value = np.min(array)
    max_value = np.max(array)
    argmin = np.argmin(array)  # Index of minimum
    argmax = np.argmax(array)  # Index of maximum

    # Sum and product
    total_sum = np.sum(array)
    cumsum = np.cumsum(array)  # Cumulative sum
    product = np.prod(array)

    print(f"Mean: {mean_value}")
    print(f"Std Dev: {std_dev}")
    print(f"Mean per column: {mean_rows}")


def linear_algebra():
    """Linear algebra operations"""
    matrix1 = np.array([[1, 2], [3, 4]])
    matrix2 = np.array([[5, 6], [7, 8]])
    vector = np.array([1, 2])

    # Matrix multiplication
    mat_mult = np.dot(matrix1, matrix2)
    mat_mult_alt = matrix1 @ matrix2  # Alternative syntax

    # Matrix-vector multiplication
    mat_vec = np.dot(matrix1, vector)

    # Determinant
    det = np.linalg.det(matrix1)

    # Inverse
    inverse = np.linalg.inv(matrix1)

    # Eigenvalues and eigenvectors
    eigenvalues, eigenvectors = np.linalg.eig(matrix1)

    # Solve linear system Ax = b
    A = np.array([[3, 1], [1, 2]])
    b = np.array([9, 8])
    x = np.linalg.solve(A, b)

    print(f"Matrix multiplication:\n{mat_mult}")
    print(f"Determinant: {det}")
    print(f"Eigenvalues: {eigenvalues}")
    print(f"Solution to Ax=b: {x}")


def broadcasting():
    """Broadcasting demonstration"""
    # Add scalar to array (broadcasting)
    array = np.array([[1, 2, 3], [4, 5, 6]])
    broadcasted = array + 10  # Adds 10 to each element

    # Add 1D array to 2D array
    row_vector = np.array([1, 2, 3])
    result = array + row_vector  # Adds row_vector to each row

    col_vector = np.array([[1], [2]])
    result2 = array + col_vector  # Adds col_vector to each column

    print(f"Original:\n{array}")
    print(f"Add row vector:\n{result}")
    print(f"Add col vector:\n{result2}")


def advanced_indexing():
    """Advanced indexing techniques"""
    array = np.arange(20).reshape(4, 5)

    # Fancy indexing
    rows = np.array([0, 2, 3])
    cols = np.array([1, 3, 4])
    elements = array[rows, cols]  # Elements at (0,1), (2,3), (3,4)

    # Boolean mask
    mask = (array > 5) & (array < 15)
    filtered = array[mask]

    # np.where
    result = np.where(array > 10, array, 0)  # Replace values <=10 with 0

    print(f"Fancy indexing: {elements}")
    print(f"Boolean filtered: {filtered}")
    print(f"np.where result:\n{result}")


def useful_functions():
    """Commonly used NumPy functions"""
    array = np.array([1, 2, 3, 4, 5])

    # Sorting
    unsorted = np.array([3, 1, 4, 1, 5, 9, 2, 6])
    sorted_array = np.sort(unsorted)
    argsort = np.argsort(unsorted)  # Indices that would sort the array

    # Unique values
    unique_values = np.unique(unsorted)

    # Concatenate
    array1 = np.array([[1, 2], [3, 4]])
    array2 = np.array([[5, 6], [7, 8]])
    vstack = np.vstack((array1, array2))  # Vertical stack
    hstack = np.hstack((array1, array2))  # Horizontal stack

    # Split
    array_to_split = np.arange(9)
    split_arrays = np.split(array_to_split, 3)  # Split into 3 equal parts

    # Clip (limit values)
    clipped = np.clip(unsorted, 2, 7)  # Values outside [2,7] are clipped

    print(f"Sorted: {sorted_array}")
    print(f"Unique: {unique_values}")
    print(f"VStack:\n{vstack}")
    print(f"Clipped: {clipped}")


if __name__ == "__main__":
    print("=== NumPy Examples ===\n")

    print("1. Array Creation")
    array_creation()

    print("\n2. Array Operations")
    array_operations()

    print("\n3. Indexing and Slicing")
    array_indexing_slicing()

    print("\n4. Reshaping")
    array_reshaping()

    print("\n5. Statistics")
    array_statistics()

    print("\n6. Linear Algebra")
    linear_algebra()

    print("\n7. Broadcasting")
    broadcasting()

    print("\n8. Advanced Indexing")
    advanced_indexing()

    print("\n9. Useful Functions")
    useful_functions()
