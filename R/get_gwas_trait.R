#' @author Cainã Max Couto-Silva
#' @importFrom gwasrapidd get_studies
#' @export
get_gwas_trait <- function(snp, batch = 1000) {
  q <- length(snp) %/% batch
  r <- length(snp) %% batch
  
  idx <- c(0, rep(batch, q), r) %>% cumsum %>% unique
  idx_list <- list()
  
  for(i in seq_along(idx)[-length(idx)]) {
    idx_list[[i]] <- (idx[i]+1):(idx[i+1])
  }
  
  gwas_list <- lapply(idx_list, function(idx_vec) {
    lapply(snp[idx_vec], function(id) {
      gwasrapidd::get_studies(variant_id = id, warnings = F)@studies$reported_trait
    })
  })
  
  
  for(i in seq_along(gwas_list)) {
    gwas_list[[i]][sapply(gwas_list[[i]], length) == 0] <- NA
  }
  
  for(i in seq_along(gwas_list)) {
    for(j in which(sapply(gwas_list[[i]], length) > 1)) {
      gwas_list[[i]][[j]] <- paste0(gwas_list[[i]][[j]], collapse = ",")
    }
  }
  
  gwas_trait <- unlist(gwas_list)
  return(gwas_trait)
}
