#' @title Explode table based on genes
#' @description This function takes one column and split the the table into multiple rows based on the split
#' character. It's specifically created for using the update_dist_genes function, after annotating with 
#' ANNOVAR.
#' @param data A data.frame of data.table with the a column (default="GENE") submitted to update_dist_genes() function.
#' @param GENE Character. Target-column name (still not well implemented)
#' @param FUNCTION Character. Target=column to FUNCTION column to replace ";" by ",". Set it to NULL to disable
#' this function.
#' @param setdt Logical. If TRUE (default), returns an object from the class "data.table".
#' @return Exploded table with unique genes by row (observation) and duplicated values on the other columns.
#' @author Cainã Max Couto-Silva
#' @import data.table magrittr
#' @export

separate_genes <- function(data, GENE = "GENE", FUNCTION = "FUNCTION", setdt = TRUE) {
  
  # unify_duplicates <- function(genes) {
  #   sapply(strsplit(genes, split = ",|;"), function(g) {
  #     ifelse(length(unique(g)) == 1, g[1], paste(g, collapse = ","))
  #   })
  # }
  # 
  # dt <- as.data.table(data)
  # dt[grepl(",|;", GENE), GENE := unify_duplicates(GENE)]
  # 
  # if(!is.null(FUNCTION)) dt[grepl(";", FUNCTION), FUNCTION := gsub(";", ",", FUNCTION)]
  # 
  # df <- tidyr::separate_rows(dt, "GENE", sep = ",|;")
  # if(setdt) df <- as.data.table(df)
  # return(df)
  # 
  
  if(is.null(FUNCTION)) {
    dt_exploded <- tidyr::separate_rows(data, GENE, sep=",")
  } else {
    dt_exploded <- tidyr::separate_rows(data, GENE, FUNCTION, sep=",")
  }
  if(setdt) {
    dt_exploded <- as.data.table(dt_exploded)   
  }
  return(dt_exploded)
}
