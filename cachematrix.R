## Reducing the time-consuming computation about get the inverse of a Matrix
## the function cache the value of the inverse of a matrix

## The first function create a special list containing a function to:
## set and get the value of the matrix, and set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The next function verifies if the inverse has been calculated and get it from the cache, if not, 
## it calculate the inverse of the matrix from the special list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)){
                  message("getting cached data")
                  return(inv)
          }
          mtrx <- x$get()
          inv <- solve(mtrx, ...)
          x$setinverse(inv)
          inv
}
