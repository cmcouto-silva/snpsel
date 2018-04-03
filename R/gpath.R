#' @export
getout_path <- function(path){
  if(!grepl("/$", path)){path <- paste0(path, "/")}
  .path <<- paste(getwd(), path, sep = "/")
}

getout_name <- function(output){
  paste0(.path, output)
}
