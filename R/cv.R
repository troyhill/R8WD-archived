#' Calculate Coefficient of Variation
#'
#' @param x a vector of two or more values
#'
#' @return a single numeric value
#' @export
#'
#' @examples
#' cv(c(10, 20))
#' cv(c(10:30))
cv <- function(x) {
  val <- sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
  if (length(na.omit(x)) < 2) {
    val <- NA # has no meaning for n = 1
  }
  return(as.numeric(val))
}
