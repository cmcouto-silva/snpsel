get.file.path <- function(file) {
  if(grepl("/", out)){
    gsub("^(.*/).*$", "\\1", out)
  } else {
    "./"
  }
}
