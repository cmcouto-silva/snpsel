#' @export
plink_write_idlist <- function(fam_file, iid, out) {
  
  fam <- data.table::fread(fam_file, select = 1:2)
  target_ids <- fam[V2 %in% iid]
  
  if(nrow(target_ids) == 0) {
    stop("There is no IID present on .fam file!")
  }
  
  if(!all(iid %in% target_ids[, V2])) {
    warning("Not all IIDs are present on .fam file!")
  }
  
  data.table::fwrite(target_ids, out, sep = " ", col.names = F)
  
}
