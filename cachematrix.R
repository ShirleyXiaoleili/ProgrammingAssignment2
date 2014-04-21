####Two functions: makeCacheMatrix and cacheSolve. 
####Function makeCacheMatrix creates a special matrix object that can cache its inverse.
####Function cacheSolve calculates the inverse of the special matrix 
#######from function makeCacheMatrix. If the matrix has not been changed, 
#######and its inverse is calculated, the function will retrieve its inverse from the cache. 

## Function makeCacheMatrix takes an argument of matrix x and returns a list of 4 functions.
## The four functions have the following functions:
## 1) sets the values of matrix x
## 2) gets the values of matrix x
## 3) sets the values of the inverse of matrix x 
## 4) gets the values of the inverse of matrix s

makeCacheMatrix <- function(x = matrix()) { ##function makeCacheMatrix takes an argument of x which is a matrix
        inv <- NULL ##Create empty cache to store inverse of matrix x
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } ##Set the value of matrix x
        
        get <- function() x ##Gets the value of matrix x
        
        setinv <- function(inverse) inv <<- inverse ##Lets the inverse of matrix x
        
        getinv <- function() inv ##Gets the inverse of matrix x
        
        list(set=set, get=get, setinv=setinv, getinv=getinv) ##returns list of functions specified 

}

## Function cacheSolve takes as an argument the special matrix created from makeCacheMatrix
## It outputs either the inverse matrix stored in cache or computes the inverse matrix and stores it in cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv() ##Checks whether the inverse is already in matrix xs cache
        
        if(!is.null(inv)) { ##If it is in the cache, prints out message and returns stored inverse matrix
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get() ##If it is not in the cache
        
        inv <- solve(data, ...) ##Computes the inverse of matrix x
        
        x$setinv(inv) ##Stores it in the cache
        
        inv ## Return a matrix that is the inverse of 'x'
}
