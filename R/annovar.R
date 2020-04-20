#' @title Annotate Genes
#' 
#' @param bim Scalar character. Path to Plink bim file.
#' @param out Scalar character with desired name for the output file (without extension).
#' @param build Scalar character. Build version of the downloaded human database (e.g. hg18 or hg19).
#' @param ref_annovar Scalar character. Path to the folder with downloaded ANNOVAR database.
#' @details
#' ANNOVAR (annotate_variation.pl) must be installed on the system path.
#' 
#' @examples
#' \dontrun{
#' dir.create(annot)
#' bim <- "path_to_bim_file/file.bim"
#' out <- "annot/my_annotation" # can be missing, then directory with bim file will be taken
#'
#' annovar(bim, out, build = "hg19", ref_annovar = "path_to_humandb/")
#' }
#' 
#' @return Tidy text file with chromosome, position, gene and its functional location.
#' @export
#' @author Cainã Max Couto-Silva

annovar <- function(bim, out, build = "hg19", ref_annovar = "~/cmcouto.silva@usp.br/lab_files/datasets/Reference_annotation/annovar") {
  
  # Store bim file path
  bim_file <- bim
  
  # Read bim file
  bim <- read.bim(bim_file)
  
  # Check output
  if(missing(out)) {
    out <- rm.extension(bim_file)
  }
  
  # Add .avinput extension to output
  avinput_file <- paste0(out, ".avinput")
  
  # Produce ANNOVAR input
  avinput <- bim[, .(CHR, POS, POS, A1, A2)]
  
  # Write out annovar input
  data.table::fwrite(avinput, avinput_file, sep = " ", col.names = F)
  
  # Check if ANNOVAR is installed on path
  program_on_path("annotate_variation.pl")
  
  # Running ANNOVAR
  system(paste(
    "annotate_variation.pl", "-build", build, avinput_file, ref_annovar, "-out", out
  ))
  
  # Load ANNOVAR annotation & tidying output
  annot <- data.table::fread(paste0(out, ".variant_function"), header = F)
  chrpos <- annot[, data.table::tstrsplit(V3, " ")][, .(V1, V2)]
  
  annot[, V3 := NULL]
  annot[, c("CHR", "POS") := chrpos]
  colnames(annot)[1:2] <- c("FUNC", "GENE")
  annot <- annot[, .(CHR, POS, GENE, FUNC)]
  data.table::fwrite(annot, paste0(out, ".annot"), sep = "\t")
  
}
