#' @title Annotate Gene info from SNP ID
#' @description Annotate gene IDs from list of SNP IDs (rs IDs). It uses Ensembl Database.
#' @param snp Character. Vector with rs IDs.
#' @param mode Integer. If mode = 1, it will annotate Entrez ID, while mode = 2 annotates gene symbol names.
#' @return Character vector with gene IDs.
#' @author Cainã Max Couto-Silva
#' @export

annot_snp_genes <- function(snp, mode = 2L) {
  
  # Set Mart SNP Database
  mart.snp <- biomaRt::useMart(biomart = "ENSEMBL_MART_SNP", "hsapiens_snp")
  
  # Annotate Ensembl IDs
  annot.ens <- biomaRt::getBM(attributes = c("refsnp_id", "ensembl_gene_stable_id"), 
                              filters = "snp_filter", values = snp, mart = mart.snp)
  
  annot.ens <- data.table::as.data.table(annot.ens)
  annot.ens <- annot.ens[ensembl_gene_stable_id != ""]
  
  # Set Mart Gene Database
  mart.gene <- biomaRt::useDataset(dataset = "hsapiens_gene_ensembl", mart = biomaRt::useMart("ensembl"))
  
  # Annotate gene info from all Ensembl IDs
  ensembl.id <- biomaRt::getBM(filters = "ensembl_gene_id", attributes = c("ensembl_gene_id", ifelse(mode == 1, "entrezgene", "external_gene_name")),
                               values = annot.ens[grepl("^ENSG", ensembl_gene_stable_id), unique(ensembl_gene_stable_id)], mart = mart.gene)
  ensembl.id <- data.table::as.data.table(ensembl.id)
  
  # Annotate gene info from all Transcript IDs
  lrg.logi <- annot.ens[, any(grepl("^LRG", ensembl_gene_stable_id))]
  if(lrg.logi) {
    transcript.id <- biomaRt::getBM(filters = c("ens_lrg_gene"), attributes = c("ensembl_gene_id", ifelse(mode == 1, "entrezgene", "external_gene_name")),
                                    values = annot.ens[grepl("^LRG", ensembl_gene_stable_id), unique(ensembl_gene_stable_id)], mart = mart.gene)
    transcript.id <- data.table::as.data.table(transcript.id)
  }
  
  # Merging Ensembl and Transcript IDs
  if(lrg.logi) {
    ref.genes <- rbind(ensembl.id, transcript.id)  
  } else {
    ref.genes <- ensembl.id
  }
  
  # Merge SNP-Ensembl IDs with gene info
  ref.genes <- merge(annot.ens, ref.genes, by.x = "ensembl_gene_stable_id", by.y = "ensembl_gene_id")
  
  # Make gene info unique by SNP
  ref.genes <- ref.genes[, paste(external_gene_name, collapse = ","), by = refsnp_id]
  
  # Get gene info for all SNPs
  snp <- data.table::data.table(refsnp_id = snp)
  snp.gene <- merge(snp, ref.genes, by = "refsnp_id", all.x = T, sort = F)[, V1]
  
  return(snp.gene)
  
}

