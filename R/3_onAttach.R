#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
    version <- tryCatch(
      utils::packageDescription("splutil", fields = "Version"),
      warning = function(w){
        1
      }
    )

  packageStartupMessage(paste0(
    "splutil ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/splutil/"
  ))
}
