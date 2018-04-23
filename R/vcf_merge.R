#' @title Merge vcf files
#' @description Merge multiple vcf files in a single one
#' @param ... Character vector. Vcf file must be provided as strings (with full or relative paths).
#' @return Data.table/data.frame object with merged vcf files.
#' @examples
#' \dontrun {
#' vcf_files <- gtools::mixedsort(list.files(path = ".", pattern = "\\.vcf$"))
#' vcf_merged <- vcf_merge(vcf_files)
#' }
#' @export
#'
#'
vcf_merge <- function(...) {
  vcf_with_header <- data.table::fread(...[1], header = TRUE)
  another_vcfs <- lapply(...[-1], data.table::fread, header = FALSE)
  vcf_merged <- data.table::rbindlist(l = c(list(vcf_with_header), another_vcfs))
  return(vcf_merged)
}

