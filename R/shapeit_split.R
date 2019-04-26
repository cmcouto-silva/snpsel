#' @title Split Shapeit files (.haps/.sample)
#' @description Split a single dataset (.haps/.sample) by chromossome into multiple files.
#' @param haps_file Character. File name (with or without .haps extension).
#' @param output_dir Character. Desired folder to outputs. Default set to the current folder.
#' @param output_prefix Character. Desired name to output files after "chrX", where "X" stands by the chromosome ID. Default set to none.
#' @return Multiple files from dataset splitted by chromosome unique IDs.
#' @examples
#'
#' \dontrun{
#' dataset <- "merged.haps" # or just "merged" without .haps extension
#' split_haps(haps_file = dataset, output_dir = "splitted_files/", output_prefix = "_phased")
#'
#' # Then, multiple files splitted by chromosomes will be created at the folder "splitted_files",
#' named "chrID_phased.EXT", where ID is the unique chromosome ID, and EXT is the correspoding extension (.haps/.sample)
#'
#' }
#'
#' @export
#' @author Cainã Max Couto da Silva

shapeit_split <- function(haps_file, output_dir, output_prefix) {

  # Standard output
  if(missing(output_dir)) output_dir <- "."
  if(missing(output_prefix)) output_prefix <- ""

  output_dir <- add_last_bar(output_dir)

  # Removing possible extension
  if(grepl("\\.haps$", haps_file)) haps_file <- unlist(strsplit(haps_file, "\\.haps$"))

  # Reading dataset files
  haps_files <- data.table::fread(paste0(haps_file, ".haps"), header = F)
  sample_files <- data.table::fread(paste0(haps_file, ".sample"), header = F)

  # Writing files .haps/.sample
  for(chr in unique(haps_files[, V1])){
    data.table::fwrite(haps_files[V1 == chr], paste0(output_dir, "chr", chr, output_prefix, ".haps"), sep = " ", col.names = F)
    data.table::fwrite(sample_files, paste0(output_dir, "chr", chr, output_prefix, ".sample"), sep = " ", col.names = F)
  }

}
