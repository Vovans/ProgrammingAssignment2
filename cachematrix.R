#Our aim here to compute matrix inverse and cache it for next computations,
#for example. 

#There are two functions here. Function makeCacheMatrix creates a list of four another
#functions. These 4 function can set the value of the matrix, get the value of
#the matrix, set the inverse of the matrix and get the inverse of the matrix.

#first we define function with argument x which is matrix
makeCacheMatrix <- function(x = matrix()) {
        #m is the inverse of the matrix. We set it for NULL. We must defined
        #this variable here because in the opposite case the function set set m in
        #the global environment, not in makeCacheMatrix's environment.
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


## Write a short comment describing this function

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
