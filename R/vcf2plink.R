#' @export
vcf2plink <- function(vcf, out, mode = 1L) {
  
  if(missing(out)) out <- rm.extension(vcf)
  
  # Checking if required programs are installed on system path
  program_on_path("plink")
  
  # Checking mode
  if(!mode %in% 1:2)
    stop("mode must be 1 (for binary files ─ .bed/.bim/.fam) or 2 (for human-readable files ─ .ped/.map).")
  
  # Line command according to Plink version
  plink <- plink_version()
  
  # Conversion from VCF to Plink format
  if(mode == 1L) {
    
    # Conversion from VCF to Plink binary format
    system( paste (
      plink, "--vcf", vcf, "--make-bed", "--out", out
    ))
  } else {
    
    # Conversion from VCF to Plink human-readable format
    system( paste (
      plink, "--vcf", vcf, "--recode", "--out", out
    ))
  }  
  
}
