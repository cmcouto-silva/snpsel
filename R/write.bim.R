#' @title Write .bim
#' @description Write (plink) bim file
#' @param bim Dataframe. A data.table, data.frame or matrix object to be saved as bim file.
#' @param out Character scalar. A character containg the name for desired output.
#' @return Written file
#' @export

write.bim <- function(bim, out, append = F, quote = F, sep = "\t", row.names = F, col.names = F, ...) {
  data.table::fwrite(x = bim, file = out, append = append, quote = quote, sep = sep, row.names = row.names, col.names = col.names, ...)
}
