#' @title Conversion from Plink to Eigenstrat format
#' @param input Scalar character. Name of the file in Plink format without extensions.
#' @param out Scalar character. Name of the output file without extensions. If missing, it will be named like the input.
#' @param mode Integer. Mode 1 considers input data as in binary format (.bed/.bim/.fam; default),
#' while mode = 2 considers the Plink human-readable format (.ped/.map).
#' @details Important note: This function assumes there's no ":" (colon) in the family names and individual IDs. Also, if all phenotypes are missing
#' (6th column of .ped/.map file), then all individuals will be considered. Otherwise only individuals with phenotype info will be taken.
#' @return Genotypic data in Engeinstrat format.
#' @author Cainã Max Couto-Silva
#' @export

plink2eigensoft <- function(input, out, mode = 1L) {
  
  # Check if convertf is intalled on system path
  program_on_path("convertf")
  
  # Fix possible input/output erros
  input <- paste0(dirname(input), "/", basename(input))
  out <- paste0(dirname(out), "/", basename(out))
  
  # Setting name to output file
  if(missing(out)) out <- input
  
  if(mode == 1L) {
    geno <- paste0(input, '.bed')
    snp <- paste0(input, '.bim')
    ind <- paste0(input, '.fam')
  } else {
    geno <- paste0(input, '.ped')
    snp <- paste0(input, '.map')
    ind <- geno
  }
  
  # Creating par file
    par.file <- list (
      genotypename = geno,
      snpname = snp,
      indivname = ind,
      outputformat = "EIGENSTRAT",
      genotypeoutname = paste0(out, '.geno'),
      snpoutname = paste0(out, '.snp'),
      indivoutname = paste0(out, '.ind'),
      familynames = "YES"
    )
    
  # Writting par file
  if(file.exists("par.plink2eigenstrat")) stop("File 'par.plink2eigenstrat' exists. Please rename or delete it before running this function.")
  writeLines(text = paste0(names(par.file), ": ", par.file), con = "par.plink2eigenstrat")
  
  # Conversion from EIGENSOFT to PLink format
  system("convertf -p par.plink2eigenstrat")
  unlink("par.plink2eigenstrat")
  
  ind <- data.table::fread(paste0(out, ".ind"), h = F)
  snp <- data.table::fread(paste0(out, ".snp"), h = F)
  
  ind_lst <- strsplit(ind[, V1], ":")
  ind[, V1 := sapply(ind_lst, "[[", 2)]
  ind[, V3 := sapply(ind_lst, "[[", 1)]
  
  fwrite(ind, paste0(out, ".ind"), sep = "\t", col.names = F)
  fwrite(snp, paste0(out, ".snp"), sep = "\t", col.names = F)
  
}

