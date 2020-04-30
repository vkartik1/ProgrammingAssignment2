## Put comments here that give an overall description of what your
## functions do
## Cache the inverse matrix and return it when asked for same data
## Write a short comment describing this function

## Create the special matrix and set the lists of functions to
## get matrix and inverse matrix data
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix = NULL
  setMatrix=function(y) {
    x<<-y
    inverseMatrix<<-NULL
  }
  
  getMatrix=function() x
  
  setInverseMatrix = function(inverse) inverseMatrix <<- inverse
  getInverseMatrix = function() inverseMatrix
  
  list(set=setMatrix, get=getMatrix,
       setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}

## Write a short comment describing this function
## Cache the inverse matrix if not present and if present,
## get the cached inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix=x$getInverseMatrix()
  
  if(!is.null(inverseMatrix)) {
    print("Cached Inverse Matrix")
    # return the cached inverseMatrix
  }
  else {
    # Compute the new inverseMatrix and return it
    mat.data=x$get()
    inverseMatrix=solve(mat.data)
    # Now cache it
    x$setInverseMatrix(inverseMatrix)
  }
  
  # return the cached or the newly computed inverseMatrix
  return(inverseMatrix)
}



mat1=makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(mat1)
cacheSolve(mat1)

