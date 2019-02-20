#' @export

plink_version <- function(){

  plink_v <- gsub(".*v(\\S..).*", "\\1", system('plink --version', T))

  if (plink_v >= 1.9) {
    plink_v <- "plink"
  } else {
    plink_v <- "plink --noweb"
  }
  return(plink_v)
}

