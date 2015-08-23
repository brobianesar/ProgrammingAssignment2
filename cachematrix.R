## Matrix inversion is usually costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following functions cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a list containing a function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse 
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function return an inverse of matrix above
## However it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise it calculate the inverse of the matrix and set the value of the inverse
## to the cache via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
