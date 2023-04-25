#' Apply a function via hash table
#'
#' This function extracts the unique input values, applies the given function to
#' it to create a hash table (containing unique input/output combinations), and
#' then matches the original input to the hash table to obtain the desired output.
#'
#' This can dramatically speed up computation if there is a lot of data and
#' a limited amount of unique values.
#' @param x A vector data that needs a function applied to it.
#' @param fn A function that will be applied to x.
#' @param ... Arguments that will be passed to `fn`.
#' @export
apply_fn_via_hash_table <- function(x, fn, ...){
  . <- NULL
  input <- NULL
  output <- NULL

  match <- data.table(input = unique(x))
  match[, output := fn(input, ...)]
  setkey(match, "input")

  return(match[.(x)]$output)
}

