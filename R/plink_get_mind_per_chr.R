#' @export
#' 
plink_get_mind_per_chr <- function(plink_file, mind = 0.1, chrs = 1:22, out) {
  
  if(dir.exists("plink_split_tmp")) {
    stop('Directory "plink_split_tmp" existis. Please remove/rename it before running this function.')
  }
  
  dir.create("plink_split_tmp")
  
  for(chr in chrs) {
    plink(`--bfile` = plink_file, `--chr` = chr, `--mind` = mind, '--write-snplist',
          `--out` = paste0("plink_split_tmp/chr", chr))
  }
  
  system(paste("cat plink_split_tmp/*irem | sort | uniq >", out))
  unlink("plink_split_tmp", recursive = T)
  
}
