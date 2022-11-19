## Pretty much a copy of makeVector except that of course we're dealing with 
## the inverse of a matrix. So, we still define setters and getters because 
## we're creating a "makeCacheMatrix" object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y #sets x in the parent environment
                inv <<- NULL #effectively, clears the cache.
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse #caches the inverse
        getinv <- function() inv # returns inverse from cache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of the matrix created with/for the 
## makeCacheMatrix function. However, it first checks to see if
## the inverse has already been calculated. If so it returns the 
## value already calculated and cached.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) #calculating the inverse.
        x$setinv(inv)
        inv
}
