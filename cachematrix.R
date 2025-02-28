## This is code for a pair of functions that cache the inverse of a matrix

## The first function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL #initiatlize the inverse matrix as null
 #function to set a new matrix
 set <- function(y){  
   x <<- y #update the matrix in the parent environment
   m <<- NULL #Reset the cached inverse
 }
 get <- function() x #function to get the current matrix
 setinverse <- function(inverse)m <<- inverse #function to set the inverse matrix
 getinverse <- function() m #function to get the inverse matrix
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## The second function computes the inverse of the special matrix returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
