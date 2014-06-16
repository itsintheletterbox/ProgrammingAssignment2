## Calculate the inverse of a matrix and store the result for future reference.

## Create a list object with the required properties
## Reset the cached inverse value when created
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        set <- function(x){
                        x <<- y
                        inv <<- NULL
                }
                
        get <- function() x
        
        setinv <- function(x) inv <<- x
        
        getinv <- function() inv
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Returns the invese of the matrix
## Returns cached value if exists, else calculates inverse a
## and stores the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        else {
                data <- x$get()
                inv <- solve(data,...)
                x$setinv(inv)
                inv
        }
                
}
