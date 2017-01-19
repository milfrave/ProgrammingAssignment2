# These functions calculate the inverse of a matrix
# makin use of cache memory

# This first function receive de original matrix and
# create additional functions to pass them to cacheSolve(),
# where the inverse matrix is calculated in put in cache memory.

#  Based in software: "Caching the Mean of a Vector" published by
# Prof. Peng on GitHub

makeCacheMatrix <- function(x = matrix()) {
    matriz = matrix()
    z = matrix()
    matriz <- NULL
    setmat <- function(z) {
        x <<- z
        matriz <<- NULL
    }
    getmat <- function() x
    setmatinv <- function(solve) matriz <<- solve
    getmatinv <- function() matriz
    list(setmat = setmat, getmat = getmat, setmatinv = setmatinv, getmatinv = getmatinv)
    
    
}


# cacheSolve calculate the inverse of the matrix if it doesn't exist and
# put it in cache memory and return it. If the inverse matrix exist in cache 
# memory, just read it and return it

cacheSolve <- function(x, ...) {
    matdata = matrix()
    matriz <- x$getmatinv()
    if (!is.null(matriz)) {
        message("There's an inverse matrix in cache memory. Getting data")
        return(matriz)
    }
    matdata <- x$getmat()
    matriz <- solve(matdata, ...)
    x$setmatinv(matriz)
    return(matriz)
    
# After apply solve() function, cacheSolve() return the inverse of matrix,
# having calculated it or, if it exist, reading it from cache memory.    

}

# Run test. A 10x10 random matrix is created and functions are ran. In
# order to check the answer appears the result of calculate A x inv[A].
# if everything is ok, the result should be I (identity matrix)

set.seed(250)
x <- matrix(rnorm(100, 5, 2), nrow = 10, ncol =10)
d <- x
matriz <- makeCacheMatrix(x)
matriz$getmat()
cacheSolve(matriz)
cacheSolve(matriz)
print(round(d %*% cacheSolve(matriz),0))

