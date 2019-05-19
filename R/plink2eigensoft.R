#' @title Conversion from Plink to Eigenstrat format
#' @param input Scalar character. Name of the file in Plink format without extensions.
#' @param out Scalar character. Name of the output file without extensions. If missing, it will be named like the input.
#' @param mode Integer. Mode 1 considers input data as in binary format (.bed/.bim/.fam; default),
#' while mode = 2 considers the Plink human-readable format (.ped/.map).
#' @param ... Could be "geno", "snp", or "ind", indicating the specific (full) names of the Eigensoft files.
#' These arguments stand by the options "genotypename", "snpname", "indivname", respectivelly.
#' @return Genotypic data in Engeinstrat format.
#' @author Cainã Max Couto-Silva
#' @export

plink2eigensoft <- function(input, out, mode = 1L, ...) {
  
  # Check if convertf is intalled on system path
  program_on_path("convertf")
  
  # Setting name to output file
  if(missing(out)) out <- input
  
  # Setting up specific arguments
  args <- list(...)
  
  if(mode == 1L) {
    geno <- ifelse(any(names(args) %in% "genotypename"), args$genotypename, paste0(input, '.bed'))
    snp <- ifelse(any(names(args) %in% "snpname"), args$snpname, paste0(input, '.bim'))
    ind <- ifelse(any(names(args) %in% "indivname"), args$indivname, paste0(input, '.fam'))
    fid <- ifelse(any(names(args) %in% "familynames"), args$familynames, "NO")
  } else {
    geno <- ifelse(any(names(args) %in% "genotypename"), args$genotypename, paste0(input, '.ped'))
    snp <- ifelse(any(names(args) %in% "snpname"), args$snpname, paste0(input, '.map'))
    ind <- ifelse(any(names(args) %in% "indivname"), args$indivname, paste0(input, '.ped'))
  }
  
  # Creating par file
  if(mode == 1L) {
    par.file <- list (
      genotypename = geno,
      snpname = snp,
      indivname = ind,
      outputformat = "EIGENSTRAT",
      genotypeoutname = paste0(out, '.eigenstratgeno'),
      snpoutname = paste0(out, '.snp'),
      indivoutname = paste0(out, '.ind'),
      familynames = fid
    )
  } else {
    par.file <- list (
      genotypename = geno,
      snpname = snp,
      indivname = ind,
      outputformat = "EIGENSTRAT",
      genotypeoutname = paste0(out, '.eigenstratgeno'),
      snpoutname = paste0(out, '.snp'),
      indivoutname = paste0(out, '.ind')
    )
  }
  
  # Writting par file
  if(file.exists("par.plink2eigenstrat")) stop("File 'par.plink2eigenstrat' exists. Please rename or delete it before running this function.")
  writeLines(text = paste0(names(par.file), ": ", par.file), con = "par.plink2eigenstrat")
  
  # Conversion from EIGENSOFT to PLink format
  system("convertf -p par.plink2eigenstrat")
  unlink("par.plink2eigenstrat")
  
}

