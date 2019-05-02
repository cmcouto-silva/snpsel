#' @title ADMIXTURE
#' @description Run ADMIXTURE Software inside R. It must be installed on the system path.
#' @usage admixture(file, K, se = F, j, plot = F, ...)
#' @param file Character vector. File name (.ped, .bed, or .geno).
#' Note that the corresponding SNP and Pedigree files must be alocated in the same folder. 
#' Please see the Admixture documentation.
#' @param K Integer scalar. Number of ancestral populations to be computed.
#' @param ... Scalar character. A single string with all desired parameters
#' (e.g. -B, -C, --cs, -s, -se, --supervised, etc).
#' @return Admixture output files (.Q and .P).
#' @author Cainã Max Couto-Silva.
#' @export

admixture <- function(file, K, ...) {
  
  program_on_path("admixture")
  further_args <- c(...)
  system(paste("admixture", file, K, further_args))

}

