#' @export
get_path <- function(path){
  if(!grepl("/$", path)){path <- paste0(path, "/")}
  .path <<- paste(getwd(), path, sep = "/")
}

get_out <- function(output){
  paste0(.path, output)
}
