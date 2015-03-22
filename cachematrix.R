## The functions "makeCacheMatrix" and "cacheSolve" work in combintation
## to take an invertable matrix and solve for it's inverse. In order to 
## make this process more efficient values of the inverse are cached
## so that they do not require recalculation.


## The "makeCacheMatrix" takes a matrix argument and a creates a variable 
## for the inverse saving them both to cache. It also creates a named list 
## of functions which can be called by the "cacheSolve" function to resolve
## the inverse. 
makeCacheMatrix <- function(x = matrix()) {
        
## the "i" variable is created to hold the inverse matrix
        i <- NULL
        
## the set function allows a new matrix to be set as X and cached as well 
## as a NULL value for the inverse
        set <- function(y) {

## A new matrix is set to x variable and stored in cache            
                x <<- y
                
## A NULL value is set to i variable and cached to indicate this is a new 
## inverse calculation                
                i<<- NULL

        }
## Allows the initial matrix to be extracted for use in further functions        
        get <- function() x

## Allows a new value to be set for the inverse and cached      
        setinv <- function(inverse) i <<- inverse

## Allows the inverse variable to extracted for use in further functions        
        getinv <- function() i

## A list created to hold all of the get/set functions and allow them
## to be called by name
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The "cacheSolve" function takes the matrix variable from "makeCacheMatrix"
## and determines whether an inverse has already been calculated. If it hasn't
## then this function calculates it.

cacheSolve <- function(x, ...) {

## The variable "i" created to hold the inverse matrix is extracted from the cache       
        i <- x$getinv()

## The "i" variable is tested to see whether an inverse calculation has already been
## cached. If it has already been calculated the value of "i" will not be NULL and the 
## cached value will be returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
## If a value for the inverse does not exist in the cache then it is calculated

## The matrix to be inverted is extracted from the cache
        data <- x$get()

## The solve function is used to calculate the inverse and store it in the "i"
## variable.
        i<- solve(data, ...)

## The local "i" variable is set to the cache via the makeCacheMatrix set function.
        x$setinv(i)

## The inverted matrix is returned
        i
}