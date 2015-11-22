
#Function for storing a cached matrix to memory 
makeCacheMatrix <- function(x = matrix()) {
  
  ##When ,aking the Cache, set m = NULL 
  m <- NULL
  
  #function set sets x = y and m = NULL in a differnet enviroment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get funciton stores the Cache inverse and gives it back 
  #to the user user if called
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Fuction to solve the inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  #Checks to see if m = Null
  #if m = Null it returns the inverse and 
  #stores the answer in Cache
  #If m !NULL then return the Cached Matrix
  if(!is.null(m)) {
    
    #Tells the User the answer is stored in Cache 
    #and being returned
    message("getting cached data")
    return(m) #Return the Cached Matrix
  }
  
  data <- x$get()
  m <- solve(data, ...) #use solve function to inverse the matrix
  x$setinverse(m) #store the inverse matrix in Cache
  
  m #return the inversed Matrix
}