#' My first function
#'
#' @param x a numeric vector
#'
#' @returns a list of components 'x' and 'y', where 'y' is the square of 'x'.
#' @export
#'
#' @examples
#' myfirstfunstion(1:10)
myfirstfunction <- function(x) {
  y = x^2
  plot(y ~ x)
  list(x = x, y = y)
}
