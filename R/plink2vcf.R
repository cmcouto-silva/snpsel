#' @export

plink2vcf <- function(plink_files, out, mode = 1L) {
  
  if(missing(out)) out <- plink_files
  
  # Checking if required programs are installed on system path
  program_on_path("plink")
  
  # Checking mode
  if(!mode %in% 1:2)
    stop("mode must be 1 (for binary files ─ .bed/.bim/.fam) or 2 (for human-readable files ─ .ped/.map).")
  
  # Line command according to Plink version
  plink <- plink_version()
  
  # Conversion from Plink to VCF format
  if(mode == 1L) {
    
    # Conversion from Plink binary format to VCF
    system( paste (
      plink, "--bfile", plink_files, "--recode vcf", "--out", out
    ))
  } else {
    # Conversion from Plink human-readable format to VCF
    system( paste (
      plink, "--file", plink_files, "--recode vcf", "--out", out
    ))
  }  
  
}
