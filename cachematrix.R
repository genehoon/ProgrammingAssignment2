## The object of below functions are to compute a matrix and the inversed matrix, and cache the value to avoid repeatitive calculation.  


## 1. Create a special matrix based on user's input that will be inversed and cached. (The matrix input has to be a square)

makeCacheMatrix <- function(x = matrix()){
     Inver <- NULL
     set <- function(y) {
           x <<- y
           Inver <<- NULL
     }
       
     get <- function () x
     setInver <- function(a) Inver <<- a
     getInver <- function() Inver
     list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## 2. Calculates the inverse of the output from above function. 
## If the inversed matrix was already calculated, it will return the value with the message of "getting cached data". 

cacheSolve <- function(x, ...) {
     Inver <- x$getInver()
     if(!is.null(Inver)) {
           message("getting cached data")
           return(Inver)
       }
     Matri <- x$get()
     Inver <- solve(Matri, ...)
     x$setInver(Inver)
     Inver
}
