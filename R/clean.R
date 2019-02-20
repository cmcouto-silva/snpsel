#' @title Clean workspace
#' @description Alias/shorcut for "rm(list = ls())" followed by "gc(reset = TRUE)". It removes all objects and frees memory used when storing them.
#' @return Workspace freely from objects and unused memory.
#' @author Cainã Max Couto-Silva
#' @export

clean <- function() {
  rm(list = ls(pos = 1), pos = 1)
  invisible(gc(reset = TRUE))
}
