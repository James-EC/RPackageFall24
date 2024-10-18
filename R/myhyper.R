#' @title myhyper
#'
#' @param iter number of iteration, higher produces more accurate results
#' @param N total number of items in the collection
#' @param r total number of "win" conditions in the collection
#' @param n number of items in the sample
#'
#' @return a barplot showing the distribution of the hypergeometric sim and a table of the probabilities
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#' @importFrom graphics curve
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' myhyper(iter=1000,n=19, N=20,r=12)
myhyper=function(iter=100,N=20,r=12,n=5){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}
