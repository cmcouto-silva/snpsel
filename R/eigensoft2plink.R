#' @title Conversion from EIGEN to Plink format
#' @param input Scalar character. Name of the file in EIGEN format without extensions (i.e. eigenstratgeno, .snp, .ind)
#' @param out Scalar character. Name of the output file without extensions. If missing, it will be named like the input.
#' @param mode Integer. Mode 1 converts to Plink binary format (.bed/.bim/.fam; default),
#' while mode = 2 converts to Plink human-readable format (.ped/.map).
#' @param ... Could be "geno", "snp", or "ind", indicating the specific (full) names of the Eigensoft files.
#' These arguments stand by the options "genotypename", "snpname", "indivname", respectivelly.
#' Also, "familynames" could specified as "YES" or "NO" (default set to YES; only appliable when mode = 1).
#' @return Genotype data in Plink format.
#' @author Cainã Max Couto-Silva
#' @export

eigensoft2plink <- function(input, out, mode = 1L, ...) {
  
  # Check if convertf is intalled on system path
  # program_on_path("convertf")
  
  # Check if Plink Software is on system path for mode = 2
  if(mode == 2) program_on_path("plink")
  
  # Fix possible input/output erros
  input <- paste0(dirname(input), "/", basename(input))
  if(missing(out)) out <- input
  out <- paste0(dirname(out), "/", basename(out))
  
  # Setting up specific arguments
  args <- list(...)
  geno <- ifelse(any(names(args) %in% "geno"), args$geno, paste0(input, '.geno'))
  snp <- ifelse(any(names(args) %in% "snp"), args$snp, paste0(input, '.snp'))
  ind <- ifelse(any(names(args) %in% "ind"), args$ind, paste0(input, '.ind'))
  fid <- ifelse(any(names(args) %in% "familynames"), args$familynames, TRUE)
  
  # Creating par file
    par.file <- list (
      genotypename = geno,
      snpname = snp,
      indivname = ind,
      outputformat = "PACKEDPED",
      genotypeoutname = paste0(out, '.bed'),
      snpoutname = paste0(out, '.bim'),
      indivoutname = paste0(out, '.fam')
    )
  
  # Writting par file
  if(file.exists("par.eigen2plink")) stop("File 'par.eigen2plink' exists. Please rename or delete it before running this function.")
  writeLines(text = paste0(names(par.file), ": ", par.file), con = "par.eigen2plink")
  
  # Conversion from EIGENSOFT to Plink format
  system("convertf -p par.eigen2plink")
  unlink("par.eigen2plink")
  
  if(fid) {
    ind <- data.table::fread(ind, header = F)
    fam_file <- paste0(out, ".fam")
    fam <- data.table::fread(fam_file, header = F)
    fam[, V1 := ind[, V3]]
    data.table::fwrite(fam, fam_file, sep = "\t", col.names = F)
    
    if(mode == 1L) {
      bim_file <- paste0(out, ".bim")
      bim <- read.bim(bim_file, header = F)
      write.bim(bim, bim_file)
    } else {
      plink(`--bfile` = out, "--keep-allele-order --allow-no-sex --recode", `--out` = out)
      unlink(paste0(out, c("bed","bim","fam","*~")))
    }
    
  }
  
}

