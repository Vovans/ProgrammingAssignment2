#Our aim here is to compute matrix inverse and cache it for next computations,
#for example. 

#There are two functions here.

#Function makeCacheMatrix get a matrix (which we want to inverse) as argument
#and creates a list of four another functions. 
#These 4 function can set the value of the matrix, get the value of
#the matrix, set the inverse of the matrix and get the inverse of the matrix.
#The inverse of matrix is set to variable m. There we can cache that value.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setsolve <- function(solve) {
                m <<- solve
        }
        getsolve <- function() {
                m
        }
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#Function cacheSolve computes the inverse of matrix returned by makeCacheMatrix
#If the inverse has already been calculated then the cacheSolve 
#get the inverse from the cache (actually just get the value m from makeCacheMatrix's
# environment). 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
