#' @export
getout_path <- function(path){
  if(!grepl("/$", path)){path <- paste0(path, "/")}
  .path <<- paste(getwd(), path, sep = "/")
}

<<<<<<< HEAD
#' @export
=======
>>>>>>> 1bfc70293ec4e825fc5e78c2e5f03d8f7e9741ea
getout_name <- function(output){
  paste0(.path, output)
}
