## makeCacheMatrix function : it will cache the value of the inverse of the matrix
## so we can just call the inverse from the cache (memory) rather than repeat the whole computational process.

makeCacheMatrix <- function(x = matrix()) {          # create matrix = x
        inv <- NULL                    # set the inverse values to NULL (represents null objects)
        set <- function(y){            # set the value of the matrix bu using set function.
                x <<- y                ## the double arrow (super assignment) operator is useful in conjunction with a function closure (apparent environment)
                inv <<- NULL
        }
        get <- function() {x}                               # get the value of matrix
        setInverse <- function(inverse) {inv <<- inverse}   ## set the value of inverse of the matrix
        getInverse <- function() {inv}                      ### get the value of inverse of the matrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Return a matrix that is the inverse of matrix 'x' from the cached data.
## Otherwise it will compute it by the solve function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()   # get the inverse from the cache and skip repeated computation
        if(!is.null(inv)) {
                message("get the inverse from the cache")
                return(inv)
        }
        mat <- x$get()          ## else, compute the inverse of the matrix by solve function
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
