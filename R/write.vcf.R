#' @title Write .vcf
#' @description Write vcf file
#' @param vcf Dataframe. A data.table, data.frame or matrix object to be saved as vcf file.
#' @param output Character scalar. A character containg the name for desired output.
#' @return Written file
#' @export

write.vcf <- function(vcf, out, append = F, quote = F, sep = "\t", row.names = F, col.names = T, ...) {
  data.table::fwrite(x = vcf, file = out, append = append, quote = quote, sep = sep, row.names = row.names, col.names = col.names, ...)
}
