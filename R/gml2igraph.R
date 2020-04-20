# #' @importFrom igraph graph_from_data_frame
# #' @export
 
gml2igraph <- function(gml, bim, out) {
  if(missing(out)) {
    out <- gt::rm.extension(gml)
  }
  
  gml <- readLines(gml) %>%
    gsub("\t", "", .)
  
  # Extracting source, target and weight
  gml.source <- gml[grepl("source", gml)]
  gml.target <- gml[grepl("target", gml)]
  gml.weight <- gml[grepl("weight", gml)]
  
  # Keeping only digits
  gml.source <- as.numeric(gsub("[^\\d]+", "", gml.source, perl = T))
  gml.target <- as.numeric(gsub("[^\\d]+", "", gml.target, perl = T))
  gml.weight <- as.numeric(gsub("weight ", "", gml.weight))
  
  # Get SNPs from EDAR and FAD1S/FAD2S/FAD3S 
  # edar <- NCBI2R::GetSNPsInGenes(10913)
  # fads <- NCBI2R::GetSNPsInGenes(c(3992,9415,3995))
  
  # Load bim file
  bim <- read.bim(bim)
  
  # genes <- bim[, LD::snp.annot(SNP)]
  # bim[SNP %in% edar, GENE := "EDAR"]
  # bim[SNP %in% fads, GENE := "FADS"]
  
  # Identify alleles on SNPs
  snp_alleles <- c(rbind(
    bim[, paste(SNP, A1, sep = "_")],
    bim[, paste(SNP, A2, sep = "_")]
  ))
  
  # nodes <- data.table(id = 1L:(nrow(bim)*2L), chr = bim[, rep(CHR, each = 2)], snp_alleles, 
  #                     pos = bim[, rep(POS, each = 2)], gene = bim[, rep(GENE, each = 2)])
  # edges <- data.table(from = gml.source, to = gml.target, weight = gml.weight)
  
  nodes <- data.table(id = 1L:(nrow(bim)*2L), chr = bim[, rep(CHR, each = 2)], snp_alleles, 
                      pos = bim[, rep(POS, each = 2)])
  edges <- data.table(from = gml.source, to = gml.target, weight = gml.weight)
  
  net <- igraph::graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
  save(net, nodes, edges, file = paste0(out, ".Rds"))
  
}

add.genes <- function(rds, bim, out) {
  
  if(missing(out)) out <- rds
  
  load(rds)
  bim <- gt::read.bim(bim)
  
  edar <- NCBI2R::GetSNPsInGenes(10913)
  fads <- NCBI2R::GetSNPsInGenes(c(3992,9415,3995))
  
  bim[SNP %in% edar, GENE := "EDAR"]
  bim[SNP %in% fads, GENE := "FADS"]
  
  nodes <- cbind(nodes, gene = bim[, GENE])
  
  net <- igraph::graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
  save(net, nodes, edges, file = out)
  
}

