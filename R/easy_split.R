#' Split a vector into a list of vectors
#'
#' Easily split a list into a list of equally sized vectors.
#'
#' You can either specify the length of the list (via `number_of_groups`) or
#' the length of the equally sized vectors within each list element (via `size_of_each_group`).
#' The last element of the list can be shorter than the other elements.
#'
#' @param x The vector to be split
#' @param size_of_each_group If you want to split `x` into a number of groups, each of `size_of_each_group` size
#' @param number_of_groups How many equally sized groups do you want?
#' @examples
#' easy_split(letters[1:20], size_of_each_group = 3)
#' easy_split(letters[1:20], number_of_groups = 3)
#' @return A list containing equally sized vectors.
#' @export
easy_split <- function(x, size_of_each_group = NULL, number_of_groups = NULL) {
  if (is.null(size_of_each_group) & is.null(number_of_groups)) stop("you must specify ONE of size_of_each_group OR number_of_groups")
  if (!is.null(size_of_each_group) & !is.null(number_of_groups)) stop("you must specify ONE of size_of_each_group OR number_of_groups")

  if (!is.null(size_of_each_group)) {
    return(split(x, ceiling(seq_along(x) / size_of_each_group)))
  }

  if (!is.null(number_of_groups)) {
    splitting_index <- rep(1:number_of_groups, each = ceiling(length(x) / number_of_groups))[1:length(x)]
    return(split(x, splitting_index))
  }
}
