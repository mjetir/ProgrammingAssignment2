## The functions below enable caching the inverse of a matrix in order to avoid 
## solving the inverse repeatedly. The first function creates a special "matrix" and
## the second function computes the inverse of a "matrix" created by the first function. 

#This function creates a special "matrix" object that can cache
#its inverse.
makeCacheMatrix <- function(x = numeric()) {
  #Initialize the inverse of the matrix x to be null in the
  #current environment.
  i <- NULL
  #This function sets the matrix x to be y and the
  #inverse of x to be null in the current environment.
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  #This function returns the matrix x.
  getMatrix <- function() x
  #This function sets the inverse of the matrix x to be inv.
  setMatrixInverse <- function(inv) i <<- inv
  #This functions returns the inverse of the matrix x.
  getMatrixInverse <- function() i
  #Return the list of the functions created above.
  list(set = setMatrix, get = getMatrix,
       setinverse = setMatrixInverse,
       getinverse = getMatrixInverse)
}


#This function returns the inverse of the special "matrix" (created 
#by the makeCacheMatrix function). The function computes the inverse of 
#the "matrix" or, if the inverse has already been calculated for this 
#"matrix", the inverse from the cache is returned.
cacheSolve <- function(x) {
  #Set i to be the inverse from the cache.
  i <- x$getinverse()
  #If the inverse is not null i.e. the inverse has already been computed,
  #print the message below and return the inverse from the cache.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #Get the actual matrix from the cache.
  z <- x$get()
  #Solve the inverse of this matrix.
  i <- solve(z)
  #Set the inverse of this matrix to be i in the cache.
  x$setinverse(i)
  #Return the inverse i.
  i
}
