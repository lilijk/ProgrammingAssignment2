## Put comments here that give an overall description of what your
## functions do

## The function below creates a matrix of special kind, in fact it's a function  

makeMatrix <-function(x=matrix()) {
	inv<-NULL
	set <-function(y) {
		x<<-y
		inv<<-NULL
		}
	get<-function() x
	setinver <- function(inver) {inv <<-inver}
	getinver <- function() inv
	list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## The following function solves the inverse of a matrix and places 
#this solution to the relevant special matrix above.

cacheInver <- function(x,...) {
	inv<-x$getinver()
	if(!is.null(inv)) 
		{message("getting cached data");return(inv)}
	data<-x$get()
	inv<-solve(data,...)
	x$setinver(inv)
	inv
	}
