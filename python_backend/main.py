from fastapi import FastAPI
import numpy as np
import euler

app = FastAPI(title="Euler Fortran API", version="1.0")

@app.get("/even_fib")
def even_fib(limit: int):
    """
    Returns the sum of even Fibonacci numbers up to `limit`.
    """
    return {"result": int(euler.sum_even_fibs(limit))}


@app.get("/largest_prime")
def largest_prime(n: int):
    """
    Returns the largest prime factor of `n`.
    """
    return {"result": int(euler.compute_largest_prime(n))}


@app.get("/multiples_sum")
def multiples_sum(limit: int):
    """
    Returns the sum of all multiples of 3 or 5 up to `limit`.
    """
    return {"result": int(euler.multiples_sum(limit))}


@app.get("/smallest_multiple")
def smallest_multiple(max_div: int = 20):
    """
    Returns the smallest number divisible by all numbers from 1 to `max_div`.
    """
    return {"result": int(euler.smallest_multiple(max_div))}


@app.get("/largest_palindrome")
def largest_palindrome():
    """
    Returns the largest palindrome made from the product of two 3-digit numbers.
    """
    # Create mutable numpy arrays to allow the Fortran subroutine to modify them
    best_i = np.array([0], dtype=np.int32)
    best_j = np.array([0], dtype=np.int32)
    largest = np.array([0], dtype=np.int32)

    # Call the Fortran subroutine
    euler.find_largest_pal(best_i, best_j, largest)

    return {
            "value1": int(best_i[0]),
            "value2": int(best_j[0]),
            "largest_palindrome": int(largest[0])
            }

