#' @title Reset fam state from Plink
#' @importFrom data.table fread fwrite
#' @description Reset case-control states from Plink's .fam file to undefined.
#' @param fam Character. The name of the fam file, with or without extension.
#' @return Overwritten file with reseted state (case-control).
#' @author Cainã Max Couto-Silva
#' @export

reset_fam_state <- function(fam) {
  if (!grepl("\\.fam$", fam)) fam <- paste0(fam, ".fam")
  
  famr <-  data.table::fread(fam)[, V6 := -9L]
  data.table::fwrite(x = famr, file = fam, sep = " ", row.names = F, col.names = F)
}
