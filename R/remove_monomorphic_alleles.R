#' @title Remove Monomorphic Alleles
#' @description Alleles equals to 0 or equals among themselves (Ref vs Alt) will be removed from dataset.
#' This functions accepts Plink and Shapeit files, altough vcf will be included soon. For Plink format,
#' Plink Software must be installed on system path (it will be check inside the function).
#' @param file Character. Name of file (.bim or .haps)
#' @param output Character. Desired name for output. If missing, the original file will be overwritten.
#' @return Dataset without monomorphic alleles.
#' @examples
#'
#' \dontrun{
#' bim_file <- "myfile.bim"
#' remove_monomorphic_alleles(file = bim_file)
#' }
#'
#' @importFrom magrittr`%>%` `%<>%`
#' @export
#' @author Cainã Max Couto da Silva

remove_monomorphic_alleles <- function(file, output) {

  # Checking file extension
  if(!grepl("\\.bim$|\\.haps$", file)) {
    stop("Files must be in Plink or Shapeit formats (.bim or .haps, respectively)")
  }

  # For Plink files (.bim)
  if(grepl("\\.bim$", file)) {

    # Check if Plink is installed on OS
    if( !system("plink --version", ignore.stdout = T) == 0 ) {
      stop("Plink is not installed in the system path")
    }

    bim_file <- file
    bim <- gt::read.bim(bim_file)

    if (nrow(bim[A1 == 0 | A2 == 0 | A1 == A2]) > 1L) {
      snps <- bim[A1 == 0 | A2 == 0, SNP]
    } else {
      stop(" There are no zeros in alleles")
    }

    plink <- gt::check_plink_version()
    plink_file <- strsplit(bim_file, "\\.bim$") %>% unlist

    if(missing(output)) output <- plink_file
    rm_snps <- paste0(plink_file, "_rm_snps.txt")
    writeLines(snps, rm_snps)

    paste(plink, "--bfile", plink_file, "--exclude", rm_snps, "--make-bed --out", output) %>%
      system()
  }

  # For Shapeit files (.haps)
  if(grepl("\\.haps$", file)) {

    haps_file <- file
    haps <- data.table::fread(haps_file, header = F)

    if (nrow(haps[V4 == 0 | V5 == 0 | V4 == V5]) > 1L) {
      rm_snps <- which(haps[,V4] == 0 | haps[,V5] == 0 | haps[,V4] == haps[,V5])
      haps <- haps[-rm_snps]

      if(missing(output)) output <- haps_file
      data.table::fwrite(haps, output, sep = " ", row.names = F, col.names = F)

    } else {
      stop(" There are no zeros in alleles")
    }

  }

}
