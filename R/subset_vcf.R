#' @export
#' @author Cainã Max Couto-Silva

subset_vcf <- function(vcf, ids, out) {
  # Create temporary file to store list of individuals
  tmp <- paste0(gsub(" ", "_", Sys.time()), "_tmp.txt")
  writeLines(ids, tmp)
  # Run Plink to remove them
  plink2 (
    `--vcf` = vcf,
    `--keep` = tmp,
    `--export` = "vcf bgz",
    `--out` = out
    )
  # Remove temporary file
  unlink(tmp)
}
