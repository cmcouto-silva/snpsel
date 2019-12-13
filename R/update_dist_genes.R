#' @title Remove annotation for SNPs distant grom genes
#' @description This function updates a vector of genes annotated with ANNOVAR, and keep only the ones inside 
#' or near the respective annotated gene. Also, it removes the parentheses with distance and other information.  
#' @param genes Character vector. A vector of genes annotated with ANNOVAR.
#' @param thres Integer. A single integer to set the maximum allowed distance (bp) from the annotated gene
#' to keep it or not. Default set to 10kb.
#' @param rm_gene_info Logical. If TRUE (default), gene info within parantheses will be removed.
#' @return Vector of genes within or near the annotated gene.
#' @examples
#' \dontrun{
#' update_dist_genes(genes)
#' dt[, GENE := update_dist_genes(GENE)]
#' }
#' @import data.table magrittr
#' @author Cainã Max Couto-Silva
#' @export

update_dist_genes <- function(genes, thres = 10000, rm_gene_info = TRUE) {
  
  idx <- grepl("dist=", genes)
  
  isolated_dist <- regmatches(genes[idx], gregexpr("\\(.*?\\)", text = genes[idx])) %>%
    lapply(gsub, pattern = "\\(|\\)|dist=", replacement = "") %>%
    lapply(function(x) gsub("NONE", NA, x)) %>%
    lapply(as.integer)
  
  isolated_genes <- gsub("\\s*\\([^\\)]+\\)","", genes[idx]) %>%
    strsplit(",")
  
  idx_thres <- lapply(isolated_dist, function(x) x <= thres)
  
  genes_final <- mapply(function(g, t) {
    paste0(g[t], collapse = ",")
  }, g = isolated_genes, t = idx_thres)
  
  genes[idx] <- genes_final
  
  if(rm_gene_info) {
    idx <- grepl("\\(", genes)
    genes[idx] <- gsub("\\s*\\([^\\)]+\\)","", genes[idx])
  }
  
  return(genes)
  
}
