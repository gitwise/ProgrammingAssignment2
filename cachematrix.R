## special matrix which has a cache of its inverse.
## the return type is a list containing
## set and get accessor functions for the matrix
## set and get accessor functions for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # variable which stores the cached inverse
        inv <- NULL

        #assign a new matrix. since a new matrix is assigned
        #we need to clear the cached inverse 
        set <- function(y){
                x <<-y
                inv <<- NULL
        }
        #get the matrix x from this data structure
        get <- function()x

        #set the inverse of the matrix. i.e. set the cache
        #trust the setters are not bogus
        setinv <- function(mat_inverse) inv <<- mat_inverse

        #return the cached inverse value of matrix x
        getinv <- function() inv

        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## given a makeCacheMatrix called x, check to see if the inverse
## needs to be calculated, if so use the solve() function,
## otherwise, return the cached inverse stored within the
## makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ##if inv is not NULL, then it is cached,
        ##so return the cache. 
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }

        # inv is NULL and therefore the inverse needs to be
        # recalculated  
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
