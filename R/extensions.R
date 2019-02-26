### Dealing with extensions

# Extracting last characters
#' @export
substr_last <- function(x, digits){
  substr(x, nchar(x) - digits + 1L, nchar(x))
}

# Checking extension existence
check.extension <- function(x, digits = 3L) {
  grepl(pattern = paste0("^.*\\..{", digits, "}$"), x = x, perl = T)
}

#' @export
# Adding extension
add.extension <- function(x, ext, digits = 3L) {
  sapply(x, function(x) {
    ext <- ifelse(test = grepl("^\\.", ext), ext, paste0('.', ext))
    if(substr_last(x, digits) == substr_last(ext, 3L)) {
      return(x)
    } else {
      return(paste0(x, ext))
    }
  })

}

#' @title Remove file extension
#' @description Remove file extension from a character vector.
#' @param file Character vector. A vector with one or more characters to get extension removed.
#' @param digits Scalar integer. Number of characteres in the extension (after the last file dot). 
#' @return Strings with no extension (file names without extension).
#' @export
#' @author Cainã Max Couto da Silva

rm.extension <- function(file, digits = 3L) {
  
  sapply(file, function(file) {
    if(!grepl("\\.", file)) stop("There is no extension in this file.")
    
    ext.digits <- unlist(strsplit(file, "\\."))
    ext.digits <- ext.digits[length(ext.digits)]
    
    if (nchar(ext.digits) != digits)
      stop("File extension (size = ", nchar(ext.digits), ") after dot differs from ", digits, " digits.")
    
    pat <- paste0("^(.*)\\..{", digits, "}$")
    res <- gsub(pat, "\\1", file)
    return(res)
  })
  
}
