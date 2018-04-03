#' @title Flip DNA strand
#' @description Flip DNA strand' bases to correspondent the strand bases.
#' @param flip Vector. A vector containg DNA bases (i.e., A/C/G/T)
#' @return Vector with correspondent bases
#' @examples
#' \dontrun{
#' (vec <- unlist(strsplit("ACGTAC", "")))
#' flip_strand(vec)
#' }
#' @export

flip_strand <- function(flip) {
  ifelse(flip == "A", "T",
         ifelse(flip == "C", "G",
                ifelse(flip == "G", "C",
                       ifelse(flip == "T", "A", flip))))
}
