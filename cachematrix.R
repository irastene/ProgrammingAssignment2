## Below are two functions that are used to create a special object 
## that stores a invertible matrix and cache's the inverse of that matrix.


## This function creates a special "matrix" object that can cache its inverse. 
## List containing four function (set matrix, get matrix, set inverse of the 
## matrix, get inverse of the matrix) is returned.
makeCacheMatrix <- function(A = matrix()) {
    inv <- NULL
    set <- function(B) {
        A <<- B
        inv <<- NULL
    }
    get <- function() A
    setinverse <- function(invB) inv <<- invB
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieve the inverse 
## from the cache.The assumpton that the matrix supplied is always invertible 
## is done.
cacheSolve <- function(A) {
    inv <- A$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } else {
        data <- A$get()
        inv <- solve(data)
        A$setinverse(inv)
        return(inv)
    }
}


## Example
# a <- matrix(c(1,0,1,2,4,0,3,5,6), 3, 3)
# b <- matrix(c(3,0,2,2,4,0,3,6,6), 3, 3)
#
# mya <- makeCacheMatrix(a)
# cacheSolve(mya)
# cacheSolve(mya)
#
# myb <- makeCacheMatrix(b)
# cacheSolve(myb)
# cacheSolve(myb)
#
# myb$set(a)
# cacheSolve(myb)

