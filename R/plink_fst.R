#' Calculate Fst via Plink
#' 
#' Description
#' 
#' @usage plink_fst(bfile, case, control, out, out,)
#' 
#' @param bfile description
#' @param case description
#' @param control description
#' @param out description
#' 
#' @return description
#' 
#' @export
#' 

plink_fst <- function(bfile, case, control, out) {
  
  # Check if Plink is intalled on path
  program_on_path("plink")
  
  # Reset phenotypes from fam file
  fam_file <- paste0(bfile, ".fam")
  reset_fam_state(fam_file)
  
  # Read fam file
  fam <- data.table::fread(fam_file)
  
  # Specify case & controls
  fam[V1 == case, V6 := 2L]
  fam[V1 == control, V6 := 1L]
  data.table::fwrite(fam, fam_file, sep = " ", col.names = F)
  
  # Running Plink
  plink(`--bfile` = bfile, `--fst` = "case-control", "--allow-no-sex", `--out` = out)
  
  # Resetting fam file
  reset_fam_state(fam_file)
  
}

