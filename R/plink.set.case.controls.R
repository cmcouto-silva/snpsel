#' @title Set cases and controls in .fam file from Plink
#' @importFrom data.table fread fwrite
#' @param fam Character. The name of the fam file, with or without extension.
#' @param cases Character. The name of the text file containing desired FIDs/IDs to be set as cases (one per line).
#' @param controls Character. The name of the text file containing desired FIDs/IDs to be set as controls (one per line).
#' @param column Character. Tell the program if the text files contain IDs or FIDs. Only two options are possible: 'fid' or 'id'.
#' Default set to 'id'.
#' @return Overwritten file with setted case-control states.
#' @author Cainã Max Couto-Silva
#' @export

plink.set.case.controls <- function(fam, cases, controls, column = "id") {
  
  if(!column %in% c('id', 'fid'))
    stop('column parameter accepts only "id" or "fid" as arguments!')
  
  if (!grepl("\\.fam$", fam)) fam <- paste0(fam, ".fam")
  fam_cc <-  data.table::fread(fam)
  
  if(nrow(fam_cc[!V6 %in% c(0L,-9L)]) > 0L)
    stop("There are cases and/or controls in the fam file. Please reseat them first!")
  
  controls <- readLines(controls)
  cases <- readLines(cases)
  
  if(column == "id") {
    fam_cc[V2 %in% cases, V6 := 1L]
    fam_cc[V2 %in% controls, V6 := 2L]
  } else {
    fam_cc[V1 %in% cases, V6 := 1L]
    fam_cc[V1 %in% controls, V6 := 2L]
  }
    
  data.table::fwrite(x = fam_cc, file = fam, sep = " ", row.names = F, col.names = F)
}
