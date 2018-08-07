#' @title Remove additional spaces/paragraphs from clipboard text
#' @description This function removes any additional whitespace or paragraphs from copied text (clipboard)
#' in a single block of text, adding it to the clipboard OS
#'
#' @return Single block text in the clipboard.
#' @export
#'
#' @author Cainã Max Couto-Silva

xclip <- function(){
  x <- tm::stripWhitespace(clipr::read_clip())
  x <- paste(x, collapse = " ")
  clipr::write_clip(x)
}
