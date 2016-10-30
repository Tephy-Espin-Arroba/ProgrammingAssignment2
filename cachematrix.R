makeCacheMatrix <- function(x = matrix()) {
  #Fist, the funtion will generate a cuadratic matrix
  #then it will store the original and the inverse matrix in variables
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  #this funtion will store the inverse result
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  #The function will receive the matrix, then, show the inverse if it is already obtained
  #or it will pass to the solve function
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  #the function solve the inverse from the variable m, defined from the x variable in the   
  #previous function
  x$setInverse(inv)
  inv
}

#sea