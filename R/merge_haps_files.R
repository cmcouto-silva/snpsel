#' @title Merge Shapeit files (.haps/.sample)
#' @description Merge Shapeit files (.haps/.sample) to a single one. All files in the folder will be merged (if correspondents)!
#' @param haps_files Character. Path to the folder where files are alocated (.haps and correspondent .sample files)
#' @param output Character. Desired name for output WITHOUT extension. Default set to "merged".
#' @return Dataset without monomorphic alleles.
#' @examples
#'
#' \dontrun{
#' my_folder <- "path_to_folder"
#' merge_haps_files(haps_path = my_folder, output = "my_merged_files")
#' }
#'
#' @importFrom magrittr %>%
#' @export
#' @author Cainã Max Couto da Silva

merge_haps_files <- function(haps_files, output) {

  # List all haps files in the folder
  haps_files <- list.files(path = haps_path, pattern = "\\.haps$", full.names = T) %>%
    gtools::mixedsort()

  # List all sample files in the folder
  sample_files <- list.files(path = haps_path, pattern = "\\.sample$", full.names = T) %>%
    gtools::mixedsort()

  # Check if files are correspondents
  if(length(haps_files) != length(sample_files)) {
    stop("Number of .haps/.sample doesn't match!")
  }

  # Set default output name if user doesn't specify one
  if(missing(output)) output <- "merged"

  # sample_names <- sapply(sample_files, ignore.path)
  # sample_files <- lapply(sample_files, data.table::fread, header = F)

  # Stop if not all sample files are identical
  if(!all(sapply(sample_files, function(dt) identical(sample_files[[1]], dt)))) {
    stop("Sample files are not identical Please verify this!")
  }

  # Reading haps_files
  haps_names <- sapply(haps_files, ignore.path)
  haps_files <- lapply(haps_files, data.table::fread, header = F)
  all_haps <- data.table::rbindlist(haps_files)

  # Writing new files
  data.table::fwrite(all_haps, paste0(output, ".haps"), sep = " ", col.names = F)
  data.table::fwrite(sample[[1]], paste0(output, ".sample"), sep = " ", col.names = F)

}

