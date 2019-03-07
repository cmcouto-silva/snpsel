#' @export
get_path <- function(file) {
  
  sapply(file, function(vec) {
  
      if(grepl("/", vec)) {
      gsub("(^.*/).*$", "\\1", vec)
    } else {
      path <- "./"
    }
    
  })
  
}
