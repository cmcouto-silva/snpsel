#' @export

haps_extract_ids <- function(haps, sample, pop, out) {
  
  pop <- readLines(pop)
  
  haps <- fread(haps, colClasses = "character")
  sample <- fread(sample, header = F)
  
  indexes <- which(sample[-c(1L:2L), V2] %in% pop)
  sample <- rbindlist(list(sample[1:2], sample[V2 %in% pop]))
  
  pos_in_haps_a1 <- indexes * 2 - 1
  pos_in_haps_a2 <- indexes * 2
  pos_in_haps_both_alleles <- c(rbind(pos_in_haps_a1, pos_in_haps_a2))
  
  cbind2 <- function(...) setDT(do.call(c,list(...)))
  haps <- cbind2(haps[, 1L:5L], haps[, -c(1L:5L)][, pos_in_haps_both_alleles, with = F])
  
  fwrite(x = haps, file = paste0(out, '.haps'), quote = F, sep = " ", row.names = F, col.names = F)
  fwrite(x = sample, file = paste0(out, '.sample'), quote = F, sep = " ", row.names = F, col.names = F)
}