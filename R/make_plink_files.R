#' @export

make_plink_files <- function(indiv_n, snp_n, out) {
  
  fid <- paste0("FID", 1:indiv_n)
  id <- paste0("ID", 1:indiv_n)
  
  bases_N <- c("A","C","G","T")
  
  ped <- data.table(fid, id, 0,0,0,-9)
  map <- data.table(1, paste0("snp", 1:snp_n), 0, 1:snp_n)
  
  for(i in 1:snp_n) {
    sel_bases <- sample(bases_N)[1:2]
    a1 <- sample(sel_bases, size = indiv_n, replace = T, prob = runif(2))
    a2 <- sample(sel_bases, size = indiv_n, replace = T, prob = runif(2))
    ped %<>% cbind(a1,a2)  
  }
  
  colnames(ped) <- paste0("V", 1:ncol(ped))
  
  fwrite(ped, paste0(out, ".ped"), sep = " ", col.names = F)
  fwrite(map, paste0(out, ".map"), sep = "\t", col.names = F)
  
}
