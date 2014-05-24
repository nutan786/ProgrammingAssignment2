#This function takes a square matrix as an input and 
#creates a matrix with the following properties 
#       set - sets the matrix
#       get - returns the set matrix 
#       setinv - sets the inverse of the matrix 
#       getinv - returns the inverse of the matrix 
#The function returns a matrix as an output 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

#This function takes a matrix as an input 
#and returns the returns the inverse of a matrix and caches it  
#
#If the inverse is not calculated then its calculated and cached  
#If the inverse exists in the cache then that is returned instead of calculating 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
