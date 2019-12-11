#' Check usage function name.
#'
#' Checks whether name of custom defined function is already bound
#' in other environments.
#'
#' @param fun Function, object.
#'
#' @return Prints a message: function bound with same name in other environments
#' or not.
#'
#' @examples
#' check_fun.name(panel_hpfilter)
#'
#' @export

check_fun.name <- function(fun){
  if (!is.function(fun)){
    stop("fun is not class function, is class ", class(fun))
  }
  if (exists(deparse(substitute(fun)), search()[1], inherits = FALSE)) {
    if (!exists(deparse(substitute(fun)), search()[2])){
      print("No function bound with same name in other environments.")
    }
    else {
      print("Function bound with same name in other environments.")
    }
  }
}
