## Pair of functions to store and retrieve matrices and
## their inverses
## Example of use:
##     testdata<-c(1,-1,-1,-1,2,3,1,1,4)
##     test_matrix<-matrix(testdata,nrow=3,ncol=3,byrow=T)
##     smart_matrix<-makeCacheMatrix(test_matrix)
##     cacheSolve(smart_matrix)#first time running (creates)
##     cacheSolve(smart_matrix)#second time running (retrieves)


## Smart extension to atomic type matrix
## - stores the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m_inv<-NULL
        #set function to store the original matrix
        set<-function(y){
                x<<-y
                m_inv<<-NULL#Reset the matrix inverse
        }#end set function
        
        #get function to retrieve the original matrix
        get<-function() { x }
        
        #setter function for the matrix inverse
        # - externally calculated
        setinv<-function(matrix_inv) { m_inv<<-matrix_inv }
        
        #getter function for the matrix inverse
        # - stored here
        getinv<-function() { m_inv }
        
        #return a list of all smart functions
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Smart extension to the "solve" function, retrieves a
## previously calculated matrix inverse
##  - if stored as from makeCachedMatrix(x <- matrix())
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv<-x$getinv()
        
        ##Check if we have executed "solve" previously
        if(!is.null(m_inv)) {
                message("Retrieving cached matrix inverse")
                return (m_inv)
        }#end of if (!.is.null(m_inv))
        
        #Retrieve matrix from smart matrix
        this_matrix<-x$get()
        
        #"solve" for the inverse and set in smart matrix
        m_inv<-solve(this_matrix,...)
        x$setinv(m_inv)
        
        #return this found value
        m_inv
}
