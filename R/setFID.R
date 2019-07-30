#' @title Set Family ID
#' @description This function updates family IDs from .fam file according to a reference table.
#' @param fam Path to .fam file.
#' @param ref Path to reference file.
#' @param merge.col Either the name or index from the column that will be merged (with same Individual IDs from .fam file).
#' @param target.col Either the name or index from the target column in reference that will be set as family IDs.
#' @param header Logical. Does reference table has a header?
#' @param force Logical. Should family IDs be updated even if not all individuals from fam file is on reference (merge.col).
#' @param ... Graphical parameters
#' @return Admixture barplot
#' @author Cainã Max Couto-Silva.
#' @export

setFID <- function(fam, ref, merge.col = 1, target.col = 3, header = F, force = F) {
  
  fam_file <- fam
  fam <- fread(fam_file)
  ref <- fread(ref, header = header)
  
  merge.col <- ifelse(is.numeric(merge.col), names(ref)[merge.col], merge.col)
  target.col <- ifelse(is.numeric(target.col), names(ref)[target.col], target.col)
  target.col <- ifelse(target.col %in% colnames(fam), paste0(target.col, '.y'), target.col)
  
  m <- merge(fam, ref, by.x = "V2", by.y = merge.col, sort = F)
  
  if(nrow(fam) == nrow(m) && force == F)
    stop("Not all Individuals from Plink file is on reference. If you want to continue anyway, re-run with force = TRUE.")
  
  fam[, V1 := m[, target.col, with = F]]
  fwrite(fam, fam_file, header = " ", col.names = F)
  
}
