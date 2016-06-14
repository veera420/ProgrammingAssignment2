makeCacheMatrix <- function(x = matrix()) {

## creates a matrix in cache that will return a list
## for getting and setting matrix and its inverse
## which will be used as a list for solving matrix - Cachesolve
        
        m <- NULL
        set <- function(y) {

## initializing matrix

                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m<<- solve
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
## returns the inverse of the original matrix to cache

        m <- x$getinv()

## Has the inverse been calculated?

         if (!is.null(m)){

## gather from cache to avoid computation 

                message("getting cached data")
                
        return(m)
        }

## calculating inverse

        data <- x$get()
        m <- solve(data,...)

## setting inverse in cache

        x$setinv(m)
        m
}

