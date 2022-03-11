#' Create a barplot showing a binomial simulation iterated a given number of times
#'
#' @param iter Number of iteration samples
#' @param n size of sample
#' @param p probability of success
#' @return A barplot showing the generated binomial simulation
#' @examples
#' \dontrun{
#' simbin(20, 20, 0.8)
#' simbin(1000, 16, 0.3)
#' }

simbin <- function(iter=100,n=10, p=0.5){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()

  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
