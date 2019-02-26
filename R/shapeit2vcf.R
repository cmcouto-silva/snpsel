#' @export
shapeit2vcf <- function(shapeit_files, out) {
  
  if(missing(out)) out <- shapeit_files
  
  # Checking if required programs are installed on system path
  program_on_path("shapeit")
  
  # Conversion from Shapeit to VCF format
  system( paste (
    "shapeit -convert --input-haps ", shapeit_files, "--output-vcf ", add.extension(out, '.vcf')
  ))
  
  # Removing temporary files
  shapeit_date <- paste(unlist(strsplit(as.character(Sys.Date()), "-"))[3:1], collapse = "")
  unlink(paste0(
    "shapeit_", shapeit_date, "_*.log"
  ))
  
}
