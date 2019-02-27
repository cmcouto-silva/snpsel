#' @export
shapeit2plink <- function(shapeit_files, out, mode = 1L, keep.family.names = T, keep.vcf = F, ...) {
  
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
  
  if(mode == 1L | (mode == 2L & keep.family.names)) {
    # Conversion from VCF to Plink binary format
    plink(`--vcf` = paste0(out, '.vcf'), "--double-id", "--make-bed", `--out` = out)
  }
  
  if(mode == 2L & isFALSE(keep.family.names)) {
    # Conversion from VCF to Plink human-readable format
    plink(`--vcf` = paste0(out, '.vcf'), "--double-id", "--recode", `--out` = out)
  }
    
  # Updating Family IDs
  if(keep.family.names) {
    sample <- data.table::fread(paste0(shapeit_files, '.sample'), header = F, select = 1:2, skip = 2L)
    fam <- data.table::fread(paste0(out, '.fam'), select = 1:2)
    out_recode <- paste0(out, "_recodeIDs.txt")
    if(file.exists(out_recode)) stop('"',out_recode,'"', ' file exists. Please remove/rename this file in order to run again this function.')
    
    colnames(sample) <- c("V3","V4")
    recode_ids <- cbind(fam, sample)
    data.table::fwrite(recode_ids, out_recode, sep = " ", col.names = F)
    
    if(mode == 1L) {
      plink(`--bfile` = out, `--update-ids` = out_recode, '--make-bed', `--out` = out)
      unlink(c(paste0(out,"*~"), out_recode))
    } else {
      plink(`--bfile` = out, `--update-ids` = out_recode, '--recode', `--out` = out)
      unlink(c(paste0(out, c('.bed','.bim','.fam','.log')), paste0(out,"*~"), out_recode))
    }
  }
  
  # Conditionally removing VCF file
  if(isFALSE(keep.vcf)) {
    unlink(paste0(out, '.vcf'))
  }
  
}
