#' Split a vector into a list with equal sized elements
#' @param x A vector
#' @param size Size of each block
#' @examples
#' split_equal(c(1:11), size = 3)
#' @export
split_equal <- function(x, size = 10) {
  split(x, ceiling(seq_along(x) / size))
}
