#' A Refactor Function
#' 
#' Factor levels trimming
#' @param a_factor Factor variable to be changed.
#' @keywords factors, levels
#' @export
#' @examples
#' refactor(education_types)

refactor <- function(a_factor){
  as.factor(as.vector(a_factor))
}
