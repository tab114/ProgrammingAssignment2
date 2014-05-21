## Put comments here that give an overall description of what your
## functions do

# The Functions below inverse a square matrix and cache it's content. 
# If the same matrix is "asked" to be inverse, the functions instead of re-calculating 
# the inverse matrix they will retrieve it from the cache.


## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
# Comments describing the function are added within the function.


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL  #cache (solved matrix) set to NULL 
  set <- function(y) {   #Here we are passing a square matrix e.g. "b$set(matrix(1:4),2,2) "
    x <<- y      	#it takes the matrix and assigns it to a variable named x in the parent
    #environment - this is the makeCacheMatrix's environment 
    s <<- NULL   #if set() gets a new matrix, the previous cache "s" is cleared 
  }
  get <- function() x     #get() subfunction grabs the matrix stored in x and returns it.
  setinv <- function(solve) s <<- solve  #the inverse matrix is passed  into it (solve)
  #and stored in the cache variable "s" in the parent environment.
  getinv <- function() s   #returns the cache.
  list(set = set, get = get,  #a list with all subfunctions.
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Comments describing the function are added within the function.


cacheSolve <- function(x, ...) {    #x is the object to which we assign makeCacheMatrix() 
  ss <- x$getinv()   #we start by calling makeCacheMatrix's getinv() subfunction, which 
  #returns the content of cache "s" and stores it in a new variable "ss". 
  if(!is.null(ss)) {  #checks if the returned cache has anything in it (!is.null(ss).
    message("getting cached Inverse Matrix") #If so, prints "getting cached Inverse Matrix" 
    return(ss)   #and the content of cache is returned.
  }
  matrix <- x$get()   #else we get the matrix passed with get()
  inv_matrix <- solve(matrix, ...)  #and we inverse it using the solve() function.
  x$setinv(inv_matrix) #we pass the inverse matrix to setinv() which stores it to "s" (cache).
  inv_matrix  #inverse matrix is returned.
}


## Verify

b <- makeCacheMatrix()
b
b$set(matrix(c(4,2,4,8),2,2))
cacheSolve(b)
cacheSolve(b)

cacheSolve(b)%*%matrix(c(4,2,4,8),2,2)

b$set(matrix(1:4,2,2))
cacheSolve(b)
cacheSolve(b)

cacheSolve(b)%*%matrix((1:4),2,2)


#################################