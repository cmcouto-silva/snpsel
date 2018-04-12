#' @title Write .bim
#' @description Write (plink) bim file
#' @param bim Dataframe. A data.table, data.frame or matrix object to be saved as bim file.
#' @param output Character scalar. A character containg the name for desired output.
#' @return Written file
#' @export

<<<<<<< HEAD
write.bim <- function(bim, output, append = F, quote = F, sep = "\t", row.names = F, col.names = F, ...) {
=======
write.bim <- function(bim, output, ...) {
>>>>>>> c68d7f73fabbc1e09b3c5ad015c75e69132ce81c
  data.table::fwrite(x = bim, file = output, append = F, quote = F, sep = "\t", row.names = F, col.names = F, ...)
}
