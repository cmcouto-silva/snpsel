#' @title Get Gene info from SNP ID
#' @description Parse gene IDs from list of SNP IDs (rs IDs). Annotation file is required!
#' @param snp Character. Vector with rs IDs.
#' @param ref Scalar character. Path to file with SNP and gene info (table with "SNP" and "GENE" columns).
#' @return Character vector with gene IDs.
#' @author Cainã Max Couto-Silva
#' @export

get_snp_genes <- function(snp, ref = "~/cmcouto.silva@usp.br/lab_files/all_datasets/Reference_annotation", 
                          include_function = F) {
  ref <- data.table::fread(ref)
  snp <- data.table::data.table(SNP = snp)
  snp.genes <- merge(snp, ref, by = "SNP", sort = F, all.x = T)
  if(include_function) {
    snp.genes <- snp.genes[, .(GENE, FUNCTION)]
  } else {
    snp.genes <- snp.genes[, GENE]
  }
  return(snp.genes)
}
