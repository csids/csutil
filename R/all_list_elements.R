#' Is this a fully named list?
#' @param x An object
#' @examples
#' is_fully_named_list(list())
#' is_fully_named_list(list(1))
#' is_fully_named_list(list("a"=1))
#' is_fully_named_list(list("a"=1, 2))
#' @export
is_fully_named_list <- function(x){
  if(inherits(x, "list")){
    n <- names(x)
    n <- n[n!=""]
    if(length(n)==length(x) & length(n) > 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Are all elements in a list null or data.frames?
#' @param x An object
#' @examples
#' all_list_elements_null_or_df(data.frame())
#' all_list_elements_null_or_df(list(data.frame()))
#' all_list_elements_null_or_df(list(1, NULL))
#' all_list_elements_null_or_df(list(data.frame(), NULL))
#' all_list_elements_null_or_df(list("a"=1, 2))
#' @export
all_list_elements_null_or_df <- function(x){
  if(!inherits(x, "list")) return(FALSE)

  retval <- lapply(x, function(x) !inherits(x, c("data.frame", "NULL")))
  retval <- unlist(retval)
  retval <- sum(retval)
  retval <- retval == 0
  return(retval)
}

#' Are all elements in a list null or lists?
#' @param x An object
#' @examples
#' all_list_elements_null_or_list(data.frame())
#' all_list_elements_null_or_list(list(data.frame()))
#' all_list_elements_null_or_list(list(1, NULL))
#' all_list_elements_null_or_list(list(list(), NULL))
#' all_list_elements_null_or_list(list("a"=1, 2))
#' @export
all_list_elements_null_or_list <- function(x){
  if(!inherits(x, "list")) return(FALSE)

  retval <- lapply(x, function(x) !inherits(x, c("list", "NULL")))
  retval <- unlist(retval)
  retval <- sum(retval)
  retval <- retval == 0
  return(retval)
}

#' Are all elements in a list null or fully named lists?
#' @param x An object
#' @examples
#' all_list_elements_null_or_fully_named_list(data.frame())
#' all_list_elements_null_or_fully_named_list(list(data.frame()))
#' all_list_elements_null_or_fully_named_list(list(1, NULL))
#' all_list_elements_null_or_fully_named_list(list(list(), NULL))
#' all_list_elements_null_or_fully_named_list(list(list("a" = 1), NULL))
#' all_list_elements_null_or_fully_named_list(list("a"=1, 2))
#' @export
all_list_elements_null_or_fully_named_list <- function(x){
  if(!inherits(x, "list")) return(FALSE)

  are_any_lists_unnamed <- lapply(x, function(y){
    if(inherits(y, "list")){
      if(is_fully_named_list(y)){
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else {
      return(FALSE)
    }
  })
  are_any_lists_unnamed <- unlist(are_any_lists_unnamed)
  are_any_lists_unnamed <- sum(are_any_lists_unnamed)
  are_any_lists_unnamed <- are_any_lists_unnamed != 0

  are_lists_named <- !are_any_lists_unnamed

  retval <- all_list_elements_null_or_list(x) & are_lists_named

  return(retval)
}



