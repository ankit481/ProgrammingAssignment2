## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function accepts a matrix and returns a list of functions, further passed as an argument to cacheSolve function calculate the inverse of the passed matrix

makeCacheMatrix <- function(x = matrix()) {
	
	## Object initialised with a null value. The object will hold the cached inverse value of the matrix and will be accessed by cacheSolve function.
	inv <- NULL
	
	## The function will be assign with the value of matrix passed as argument during the call to makeCacheMatrix.
	## Eg. a <- matrix(1:4, 2,2)
	## mat <- makeCacheMatrix(a)
	set <- function(y){
		
		x <<- y
		inv <<- NULL
	}
	
	## Function to get the value of matrix
	get <- function() x
	
	## Function to set inverse value of matrix
	setinverse <- function(solve) inv <<- solve
	
	## Function to get the inverse value of matrix
	getinverse <- function() inv
	
	## List of getter and setter functions returned during call to makeCacheMatrix function. The list will be passed as argument to cacheSolve function.
	## Eg. inverseValue <- cacheSolve(mat)
	list (set = set, get = get, setinverse =  setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## The function will accept a list of getter and setter methods and will compute the value the inverse of matrix passed to makeCacheMatrix function. The function will first check for the cached inverse value. If not found, the function will retrieve the matrix and compute inverse

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	
	## Retrieve the cached inverse value from makeCacheMatrix function
	inv <- x$getinverse()
	
	## Checks if cached value exists, then returns the same.
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	## If the cached inverse value does not exists, the code will retrieve matrix value and compute the inverse value of the matrix.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
		
}
