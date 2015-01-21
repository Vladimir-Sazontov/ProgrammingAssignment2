## Here is a pair of functions that cache the inverse of a matrix.
## Details are described below.


## The output of the function makeCacheMatrix is a list of 4 functions 
## 1) to get a matrix 
## 2) to set a matrix 
## 3) to get inverse of the matrix
## 4) to set inverse of the matrix.
## These functions should cache variables defined 
## in makeCachematrix using lexical scoping.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Setting values of matrix by lexical scoping
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  
  ## Getting values of cached matrix
  get <- function() x 
  
  ## Setting values of inverse matrix by lexical scoping
  setinverse <- function(solve) inv <<- solve 
  
  ## Getting values of inverse matrix
  getinverse <- function() inv
  
  ## Below is the output list of 4 functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The next function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
## Else the function makes a direct calculation 
## for the inverse current matrix x_current.


cacheSolve <- function(x, x_current, ...) {
  
  ## Take a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Take the original matrix
  mat <- x$get()
  
  ## If cache of the inverse matrix is empty or
  ## the original matrix was changed make a direct calculation
  ## of the inverse matrix. Else return cache.
  
  ## For element-wise comparison of matrices the function matequal() is used.
  ## This function return TRUE for two input arguments 
  ## if they have the same class "matrix", equal dimensions 
  ## and equal elements in the same positions. 
  ## (The only base function all() could work incorrect without 
  ## explicit conditions for classes and dimensions.)
  
  matequal <- function(x, y){
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) 
  }
    
  if(!is.null(inv) && matequal(mat, x_current)) {
    message("getting cached data")
    return(inv)
    }
    data <- x_current
    inv <- solve(data, ...)
    x$setinverse(inv)
    message("getting direct calculation")
    
  inv
}