## Create a cached matrix to solve
## the inverse of the marix, return the inverse of the cached matrix
##
## Method to call function:
##  M <- matrix(c(3, 4, 5, 6), nrow=2, ncol=2)
##  cache.matrix <- makeCacheMatrix(M)
##  cacheSolve(cache.matrix)

## Create an object for an inverted matrix
makeCacheMatrix <- function(original.matrix = matrix()) {
  
  cached_inverse <- NULL
  
  set <- function(y) {
    
    original.matrix <<- y
    cached_inverse <<- NULL
  }

  get <- function() original.matrix
  
  # Inversing the matrix using build in inverse() function in R
  setInverse <- function(inverse) cached_inverse <<- inverse
  getInverse <- function() cached_inverse
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
  }


## Return the inverse of a cached.matrix object
cacheSolve <- function(cached.matrix, ...) {
  
  ## Return a cached matrix: using the solve function 
  ## to return the inverse of 'cached.matrix'
  
  inverse_matrix <- cached.matrix$getInverse()
  
  if(!is.null(inverse_matrix)) {
    message("getting cached matrix")
    return(inverse_matrix)
  }
  
  data <- cached.matrix$get()
  inverse_matrix <- solve(data, ...)
  
  # Function to inverse cached matrix
  cached.matrix$setInverse(inverse_matrix)
  inverse_matrix
  
  }
