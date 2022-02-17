## Put comments here that give an overall description of what your
## functions do

# If you have a large squared matrices  it may take some time to calculate the 
# inverse repeatedly. So it may be a good idea to cache the inverse and then get 
# the inverse from the cache when you need it. 


## Write a short comment describing this function

# The function makeCacheMatrix creates  a special matrix object (saved as a list)  
# that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  mat_s <- NULL
  set <- function(y) {
    x <<- y
    mat_s <<- NULL
  }
  get <- function() x
  setsolve_m <- function(solve_m) mat_s <<- solve_m
  getsolve_m <- function() mat_s
  list(set = set, get = get,
       setsolve = setsolve_m,
       getsolve = getsolve_m)  
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatirx function. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve should
# retrieve the inverse from the cache. (se my testing below).

# Computing the inverse of matrix is done with the solve function. 
# If X is a square invertible matrix, then solve(X) returns the inverse of X.

# The cacheSolve() function is using the solve() to calculate the inverse.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  mat_s <- x$getsolve()
  if(!is.null(mat_s)) {
    message("getting cached data")
    return(mat_s)
  }
  matrix_to_solve <- x$get()
  mat_s <- solve(matrix_to_solve, ...)
  x$setsolve(mat_s)
  mat_s
}

# testing
# 

# set.seed(16)
# matrix_test <- matrix(sample(100,16),4,4)
# matrix_test
# solve(matrix_test)
# 
# mat_a <- makeCacheMatrix(matrix_test)
# cacheSolve(mat_a)        # cache the inverse of matrix
# 
# mat_a$get()              # retrieve the matrix
# mat_a$getsolve()         # get the cached inverse of matrix
# cacheSolve(mat_a)        # getting cached data
# 
# mat_a$set(matrix(sample(100,16),4,4)) # reset value with a new matrix

