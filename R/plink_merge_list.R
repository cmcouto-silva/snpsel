#' @title Merge Multiple Plink files
#' @author Cainã Max Couto-Silva
#' @export

plink_merge_list <- function(bfile, merge_list, out) {
  
  # First merge (generating a merged dataset)
  gt::plink_merge(dataset01 = bfile, dataset02 = merge_list[1], out = out)
  
  # Merge all datasets
  for (i in 2:length(merge_list)) {
    gt::plink_merge(out, merge_list[i], out)
  }
}
