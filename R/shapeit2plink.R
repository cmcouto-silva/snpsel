#' @export

shapeit2plink <- function(shapeit_files, out, mode = 1L, keep.vcf = F) {
  
  # Checking if required programs are installed on system path
  program_on_path("shapeit")
  program_on_path("plink")
  
  # Checking mode
  if(!mode %in% 1:2)
    stop("mode must be 1 (for binary files ─ .bed/.bim/.fam) or 2 (for human-readable files ─ .ped/.map).")
  
  # Line command according to Plink version
  plink <- plink_version()
  
  if(missing(out)) out <- shapeit_files
  
  # Conversion from Shapeit to VCF format
  system( paste (
    "shapeit -convert --input-haps ", shapeit_files, "--output-vcf ", paste0(out, '.vcf')
  ))
  
  if(mode == 1L) {
    
    # Conversion from VCF to Plink binary format
    system( paste (
      plink, "--vcf", paste0(out, '.vcf'), "--make-bed", "--out", out
    ))
  } else {
    
    # Conversion from VCF to Plink human-readable format
    system( paste (
      plink, "--vcf", paste0(out, '.vcf'), "--recode", "--out", out
    ))
  }
  
  # Conditionally remove VCF file
  if(isFALSE(keep.vcf)) {
    unlink(paste0(out, '.vcf'))
  }
  
  # Removing temporary files
  shapeit_date <- paste(unlist(strsplit(as.character(Sys.Date()), "-"))[3:1], collapse = "")
  unlink(paste0(
    "shapeit_", shapeit_date, "_*.log"
  ))
  
}

