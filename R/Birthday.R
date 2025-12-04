#' Birthday
#'
#' @param k A numeric vector representing the number of people in the room.
#'
#' @returns The probability that at least two people share a birthday.
#' @export
#'
#' @examples
#' birthday(k = 20:25)
birthday <- function(k) {
    1-exp(lchoose(365,k) + lfactorial(k) - k*log(365))
}
