#Assignment: Caching the Inverse of a Matrix

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function uses the solve() function to compute the inverse of the special
#"matrix" returned by the function makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then it retrieves
#the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

#This code was inspired by numerous contributors on the forums and the DSS
#Community Site.Reading these helped me to understand the assignment and how
#to adapt the example to the challenge of a matrix.