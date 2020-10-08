#' @title Explode table based on genes
#' @description This function takes one column and split the the table into multiple rows based on the split
#' character. It's specifically created for using the update_dist_genes function, after annotating with 
#' ANNOVAR.
#' @param data A data.frame of data.table with the a column (default="GENE") submitted to update_dist_genes() function.
#' @param GENE Character vector. Target-column(s) name(s).
#' @param setdt Logical. If TRUE (default), returns an object from the class "data.table".
#' @return Exploded table with unique genes by row (observation) and duplicated values on the other columns.
#' @author Cainã Max Couto-Silva
#' @import data.table magrittr
#' @export

separate_genes <- function(data, gene_col="GENE", keep_empty = FALSE, setdt = TRUE) {
  
  dt <- tidyr::separate_rows(data, all_of(gene_col), sep=",")
  
  if(isFALSE(keep_empty)) {
    empty_idx <- apply(dt[, gene_col], 1, function(x) any(x=="" | is.na(x)))
    dt <- dt[!empty_idx, ]
  }
  
  if(setdt) dt <- as.data.table(dt)
  return(dt)
}
