#' @export
shapeit2plink <- function(shapeit_files, out, mode = 1L, keep.vcf = F, ...) {
  
  # Checking if required programs are installed on system path
  program_on_path("shapeit")
  program_on_path("plink")
  
  # Checking mode
  if(!mode %in% 1:2)
    stop("mode must be 1 (for binary files ─ .bed/.bim/.fam) or 2 (for human-readable files ─ .ped/.map).")
  
  # Missing Args
  if(missing(out)) out <- shapeit_files
  
  # Verify shapeit2vcf Aditional Arguments
  args <- list(...)
  
  if (!all(names(args) %in% c("split", "keep.split"))) {
    stop('Only "split" and "keep.split" are permitted as aditional arguments.')
  }
  
  # Set shapeit2vcf aditional args
  split <- ifelse(any(names(args) == "split"), args$split, F)
  keep.split <- ifelse(any(names(args) == "keep.split"), args$keep.split, F)
  
  # Line command according to Plink version
  plink <- plink_version()
  
  # Conversion from Shapeit to VCF format
  if(split) {
    shapeit2vcf(shapeit_files, out, split = split, keep.split = keep.split)
  } else {
    shapeit2vcf(shapeit_files, out)
  }
  
  if(!file.exists(paste0(out, '.vcf')))
    stop("VCF file was not created. This is possible to multiple chromosomes in the dataset (Shapeit is not suited to deal with them).
  Please try running this function again with parameter split = TRUE.")
  
  ## Conversion from VCF to Plink
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


