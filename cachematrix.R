
#Overall Purpose:
    #The Following Two Functions functions cache the inverse of a matrix.
    #Matrix Inversion is a complex procedure that can be potentially costly
    #The following two functions cache the inverse of a matrix rather than
    #having the computer repeat the procedure

## The function below has 4 components:
    #1. Setting the Value of a Matrix (set)
    #2. Getting the value of a Matrix (get)
    #3. Setting the value of the Inverse (setinverse)
    #4. Getting the value of the Inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set= set, get= get, setinverse= setinverse, 
        getinverse= getinverse)
}



## The function below returns the inverse of a given matrix, using the functions generated above.
    ##First, the function checks to see if the inverse is already stored in the cache
    ##If not, the setinverse function generates the inverse and then returns it
    ##If the function is returning a cached inverse, the label "getting cached data" appears 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


#Test Run:

#> x <- rbind(c(4, 3), c(3, 2))

#> x
#[,1] [,2]
#[1,]    4    3
#[2,]    3    2

#> y <- makeCacheMatrix(x1)

#> y$get()
#[,1] [,2]
#[1,]    4    3
#[2,]    3    2

#> cacheSolve(y)
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4

#> cacheSolve(y)
#getting cached data.
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4

