#' @title Write .vcf
#' @description Write vcf file
#' @param vcf Dataframe. A data.table, data.frame or matrix object to be saved as vcf file.
#' @param output Character scalar. A character containg the name for desired output.
#' @return Written file
#' @export

write.vcf <- function(vcf, output, ...) {
  data.table::fwrite(x = vcf, file = output, append = F, quote = F, sep = "\t", row.names = F, col.names = T, ...)
}
