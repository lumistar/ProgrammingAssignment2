## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the value of the inverse
        setinv <- function(inverse) inv <<- inverse
        
        ## Get the value of the inverse
        getinv <- function() inv
        
        ## Return a list of the above functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function, cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. Do the following:
## Check to see if the inverse has already been calculated.
## If so, gets the inverse from the cache.
## If not, calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check to see if the inverse has already been calculated
        ## If so, gets the inverse from the cache
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If not cached, get the matrix into data
        data <- x$get()
        
        ## Compute the inverse
        inv <- solve(data, ...)
        
        ## Cache the inverse
        x$setinv(inv)
        
        ## Return the inverse
        inv
}
