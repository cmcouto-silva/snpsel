#' @export

split_chrs_haps <- function(haps, output, prefx = T) {

  sample <- data.table::fread(paste0(haps, ".sample"), header = F)
  haps <- data.table::fread(paste0(haps, ".haps"))
  log <- missing(output)

  file_name <- parse(text = 'ifelse(test = { prefx == T },
                      yes = paste0("chr", i, if(!log) {paste0("_", output)}),
                      no = paste0(if(!log) {paste0(output, "_")}, "chr", i))')

  for (i in unique(haps[, V1])){
    fwrite(x = haps[V1 == i], file = paste0(eval(file_name), ".haps"), quote = F, sep = " ", row.names = F, col.names = F)
    fwrite(x = sample, file = paste0(eval(file_name), ".sample"), quote = F, sep = " ", row.names = F, col.names = F)
  }
}

# Usage
# split_chrs_haps(haps = "all_chrs_anc_adjusted", output = "phased")
