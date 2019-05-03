#' @export
shapeit2vcf <- function(shapeit_files, output, split = F, keep.split = F) {
  
  if(missing(output)) output <- shapeit_files
  
  # Checking if required programs are installed on system path
  program_on_path("shapeit")
  
  ## If data has multiple chromosomes
  if(split) {
    
    # Split chromosomes in the original file
    dir_split <- paste0(shapeit_files, "_split")
    if(dir.exists(dir_split)) stop('"', dir_split,'" already exists! Please remove/rename this folder in order to run this function.')
    
    # Create directory for splitting chromosomes
    dir.create(dir_split, F)
    
    # Splitting chromosomes
    shapeit_slipt(haps_file = shapeit_files, output_dir = dir_split)
    
    # List of split Shapeit files without extension
    sfiles <- gtools::mixedsort(list.files(path = dir_split, pattern = ".haps", full.names = T)) %>%
      rm.extension(digits = 4)
    
    # Conversion from Shapeit to VCF format
    for (sfile in sfiles) {
      system( paste (
        "shapeit -convert --input-haps ", sfile, "--output-vcf ", add.extension(sfile, '.vcf')
      ))
    }
    
    # Removing temporary files
    shapeit_date <- paste(unlist(strsplit(as.character(Sys.Date()), "-"))[3:1], collapse = "")
    unlink(paste0(
      "shapeit_", shapeit_date, "_*.log"
    ))
    
    # Merging split VCF files
    vcf_files <- gtools::mixedsort(list.files(path = dir_split, pattern = "\\.vcf$", full.names = T))
    vcf_concat <- ifelse(Sys.which("vcf-concat") == "", FALSE, TRUE)
    
    # Make use of vcftools if available
    if(vcf_concat) {
      system(paste (
        "vcf-concat", paste(vcf_files, collapse = " "), ">", add.extension(output, '.vcf') ))
    } else { # or with inside-function
      vcf_merged <- vcf_merge(vcf_files)
      fwrite(vcf_merged, output, sep = "\t", col.names = T)
    }
    
    if(isFALSE(keep.split)){
      unlink(x = dir_split, recursive = T)
    }
    
  } else { ## If data an unique chromosome (in theory, no check has been performed!)
    
    # Conversion from Shapeit to VCF format
    shapeit.log <- system( paste (
      "shapeit -convert --input-haps ", shapeit_files, "--output-vcf ", add.extension(output, '.vcf')
    ))
    
    
    # Launch error message if Shapeit fail
    if(shapeit.log == 1L) {
      message("ERROR: VCF conversion has failed. Please consider running this function with parameter split = TRUE.\n")
      unlink(add.extension(output, '.vcf'))
    }
    
    # Removing temporary files
    shapeit_date <- paste(unlist(strsplit(as.character(Sys.Date()), "-"))[3:1], collapse = "")
    unlink(paste0(
      "shapeit_", shapeit_date, "_*.log"
    ))
  }
  
}
