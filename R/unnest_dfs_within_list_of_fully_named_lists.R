#' Unnest data.frames within fully named list
#' @param x A list of fully named lists (which then contain data.frames)
#' @param returned_name_when_dfs_are_not_nested When x is a single list of data.frames, what name should be returned?
#' @param ... parameters passed to data.table::rbindlist
#' @examples
#' x <- list(
#'   list(
#'     "a" = data.frame("v1"=1),
#'     "b" = data.frame("v2"=3)
#'   ),
#'   list(
#'     "a" = data.frame("v1"=10),
#'     "b" = data.frame("v2"=30),
#'     "d" = data.frame("v3"=50)
#'   ),
#'   list(
#'     "a" = NULL
#'   ),
#'   NULL
#' )
#' print(x)
#' splutil::unnest_dfs_within_list_of_fully_named_lists(x)
#'
#' x <- list(
#'   data.frame("v1"=1),
#'   data.frame("v3"=50)
#' )
#' print(x)
#' splutil::unnest_dfs_within_list_of_fully_named_lists(
#'   x,
#'   returned_name_when_dfs_are_not_nested = "NAME",
#'   fill = TRUE
#' )
#' @export
unnest_dfs_within_list_of_fully_named_lists <- function(x, returned_name_when_dfs_are_not_nested = "data", ...){
  if(!inherits(x, "list")) return(NULL)
  if(all_list_elements_null_or_df(x)){
    retval <- list(rbindlist(x, ...))
    names(retval) <- returned_name_when_dfs_are_not_nested
    return(retval)
  }
  if(!all_list_elements_null_or_fully_named_list(x)) stop("All list elements must be either null or a fully named list")

  list_names <- lapply(x, function(y) names(y))
  list_names <- sort(unique(unlist(list_names)))

  if(length(list_names)==0) return(NULL)

  retval <- vector("list", length = length(list_names))
  for(i in seq_along(retval)){
    retval[[i]] <- vector("list", length = length(x))
    for(j in seq_along(x)){
      retval[[i]][[j]] <- x[[j]][[list_names[i]]]
    }
    retval[[i]] <- rbindlist(retval[[i]], ...)
  }
  names(retval) <- list_names

  return(retval)
}
