
#' myboot2
#'
#' @param iter Number of bootstrap iterations (default 10000).
#' @param xNumeric vector of data values.
#' @param fun Statistic to compute (default "mean").
#' @param alpha Significance level for the confidence interval (default 0.05).
#' @param cx Text size for labels in the plot.
#' @param ... Additional graphical parameters passed to hist().
#'\describe{
#'   \item{ci}{The bootstrap confidence interval.}
#'   \item{fun}{The statistic used.}
#'   \item{x}{The input data.}
#' }
#' @returns A list containing the CI, the function name, and the data.
#' @export
myboot2 <- function(iter = 1000, x = x, fun = "mean")
#'
#' @examples
#' x <- rnorm(20)
#' myboot2(1000, x)
myboot2 <- function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))#

  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha = ",alpha," iter = ", iter, sep = ""),
            ...)

  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  pte = apply(mat,2,fun)
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1],0,ci[2],0, lwd = 4)
  text(ci[1],0,paste("(",round(ci[1],2),sep = ""), col = "Red", cex = cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep = ""), col = "Red", cex = cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}
