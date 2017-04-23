##makeCachematrix fuctions consists of four functions inside the body set,get,getmt and setmt
##set and get function is used to assign and retrieve the input matrix for which the inverse is to be computed
##setinv and getinv functions is used to assign and retrieve the computed inverse matrix
##<<-operator is used to assign the values so that it can be retrieved from the parent frame functions
##the whole function is pushed into the vector n

makeCacheMatrix <- function(x = matrix()) {

mt<-NULL
set<-function(k){
	x<<-k
	mt<<-NULL

}
get<-function()x
setinv<-function(m)mt<<-m
getinv<-function()mt
list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
n<-makeCacheMatrix()

## The cacheSolve function is used to get the stored data in the variable mt if the computation has already been done and thus avoiding the more computations
##If there is no cached data then it get the value from the makecachematrix get function and calculates the inverse of the matrix and assign that value to mt variable
##If there is cached data then it prints "getting cached data" and returns the value holding in the variable mt

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
mt<-n$getinv()
if(!is.null(mt)){
print("getting cached data")
return(mt)
}
i<-n$get()
mt<-solve(i)
n$setinv(mt)
mt
}
