#' @title Annotate genes from a reference table
#' @description Given a reference tidy table with the columns "SNP", "GENE", "DIST", and "FUNCTION", 
#' this function merges it with the dataset by SNP (so each SNP must be unique), and then concatenate
#' the genes and their respective function by comma to set unique records.
#' @param snp vector. A vector with SNP names to match the ref[["SNP"]] column.
#' @param thres Integer. A single integer to set the maximum allowed distance (bp) from the annotated gene. 
#' Default set to 10kb.
#' @examples
#' \dontrun{
#' annotate_fromtbl(genes)
#' dt[, c("GENE", "FUNCTION) := annotate_fromtbl(SNP)]
#' }
#' @import data.table
#' @return vector of genes or list with two vectors (genes and functions)
#' @author Cainã Max Couto-Silva
#' @export

annotate_fromtbl <- function (snp, ref, thres = 10000, include_function = T) {
  
  snp_dt <- data.table(SNP=snp)
  ref <- fread(ref)
  
  if(!all(snp_dt[, SNP] %in% ref[, SNP])) {
    stop("Not all SNPs from dataset annotated in ref file. Please fix the reference.")
  }
  
  snp_dt_tmp <- snp_dt[!duplicated(SNP)]
    
  dt <- merge(snp_dt_tmp, ref[, .(SNP, GENE, DIST, FUNCTION)], by="SNP", sort = F)
  dt[DIST > thres, c("GENE", "FUNCTION") := ""]
  
  dt <- dt[, lapply(.SD, function(x) {
    if(length(x)==1) {
      x
    } else if (length(unique(x)) == 1 && unique(x) == "") {
      ""
    } else {
      trimws(paste(x, collapse = ","), whitespace = "[,\t\r\n]")
    }
  }), by = SNP, .SDcols = c("GENE", "FUNCTION")]
  
  dt <- merge(snp_dt, dt, by = "SNP", sort = F)
  
  if(!all(dt[, SNP] == snp)) {
    stop("SNPs didn't keep the order. Please check the original function.")
  }
  
  if(include_function) {
    annot <- as.list(dt[, .(GENE, FUNCTION)])
  } else {
    annot <- dt[, GENE]
  }
  
  return(annot)
  
}
