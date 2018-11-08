#' @title Remove file extension
#' @description Remove file extension from a character vector.
#' @param file Character vector. A vector with one or more characters to get extension removed.
#' @param digits Scalar integer. Number of characteres in the extension (after the last file dot). 
#' @return Strings with no extension (file names without extension).
#' @export
#' @author Cainã Max Couto da Silva

rm.extension <- function(file, digits = 3L) {
  
  check.ext <- unlist(strsplit(file, "\\."))
  if (nchar(check.ext[length(check.ext)]) != digits)
    stop("File extension (size = ", nchar(check.ext[length(check.ext)]), ") after dot differs from ", digits, " digits.")
  
  pat <- paste0("^(.*)\\..{", digits, "}$")
  gsub(pat, "\\1", file)
}
