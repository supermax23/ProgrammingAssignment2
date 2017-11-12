## The makeCacheMatrix function takes a matrix and outputs a 
##l ist for the retrieval of the matrix and the its inverse

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      setMatrix <- function(y) {
            x <<- y
            mat <- NULL
            
      }
      getMatrix <- function() {
            x
      }
      setInvMatrix <- function(invMat) {
            mat <<- invMat
      }
      getInvMatrix <- function() {
            mat
      }
      list(
            setMatrix = setMatrix,
            getMatrix = getMatrix,
            setInvMatrix = setInvMatrix,
            getInvMatrix = getInvMatrix
      )
}

## The cacheSolve function takes the output of the makeCacheMatrix function (list)
## and sets the value of the inverse matrix in the setInvMatrix function.
cacheSolve <- function(x,...) {
      mat <- x$getInvMatrix()
      if(!is.null(mat)) {
            return(mat)
      }
      val <- x$getMatrix()
      mat <- solve(val)
      x$setInvMatrix(mat)
      mat
}
