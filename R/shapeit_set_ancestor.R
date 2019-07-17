#' @title Adjust Ancentral/Derived Alleles According to Reference
#' 
#' @param shapeit Scalar character with the name of the dataset file on Shapeit format without extension (.haps/.sample).
#' @param ref Scalar character with the path of the reference fasta files (1000G).
#' @param out Scalar character with desired name for the output file (without extension).
#' @details
#' Freely download of 1000 Genomes Ancestor alleles available at: 
#' http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase1/analysis_results/supporting/ancestral_alignments/
#' 
#' @examples
#' \dontrun{
#' haps <- "dataset/my_phased_dataset"
#' ref <- "path_to_fasta_files/"
#' out <- "out_folder/phased_data_with_fixed_ancestor"
#'
#' set_anc_alleles(sfile, ref, out)
#' }
#' 
#' @return Dataset with adjusted ancestral/derived alleles (.haps and .sample files).
#' @export
#' @author Cainã Max Couto-Silva

shapeit_set_ancestor <- function(shapeit,
                                 ref = "~/cmcouto.silva@usp.br/lab_files/datasets/Reference_annotation/1000g_ancestor/human_ancestor_GRCh37_e59/",
                                 out) {
  
  # Read shapeit files
  haps <- data.table::fread(paste0(shapeit, ".haps")) 
  sample <- data.table::fread(paste0(shapeit, ".sample"), h = F)
  
  # Set temporary names for .haps info
  haps_info <- c("CHR", "SNP", "POS", "A1", "A2")
  names(haps)[1:5] <- haps_info
  
  # Verifying target-chromosomes in dataset
  chrs <- unique(haps[, CHR])
  
  # Read fasta files from target-chromosomes
  list.fa <- lapply(chrs, function(chr) {
    paste(readLines(paste0(ref, "/human_ancestor_", chr, ".fa"))[-1], collapse = "")
  }); names(list.fa) <- chrs
  
  # Find ancestral alleles for each chr/SNP
  for(chr in chrs) {
    haps[CHR == chr, AA := toupper(sapply(POS, function(pos) substring(list.fa[[as.character(chr)]], pos, pos))) ][]
  }
  
  # Reorder columns
  data.table::setcolorder(haps, c(haps_info, "AA"))
  
  # Excluding variants without reference ancestral allele
  haps <- haps[!AA %in% c("N", "-", ".")]
  
  # Flipping ancestral alleles with dataset mismatch 
  haps[A1 != AA & A2 != AA, AA := gt::flip_strand(AA)]
  
  # Excluding variants without match in dataset (even after flipping)
  haps <- haps[!(A1 != AA & A2 != AA)]
  
  # Getting indexes to invert alleles to change SNP IDs and Genotypes
  # swap_geno <- c(rbind(colnames(haps)[-(1:6)][c(F,T)], colnames(haps)[-(1:6)][c(T,F)])) # change pair column order
  snp_inv_idx <- haps[, A1 != AA]
  
  # Invert SNP IDs
  haps[snp_inv_idx, c('A1', 'A2'):= .(A2, A1)]
  
  # Invert SNP Alleles
  # haps[snp_inv_idx, colnames(haps)[-(1:6)] := .SD, .SDcols = swap_geno] # change pair column order
  haps[snp_inv_idx, colnames(haps)[-(1:6)] := lapply(.SD, function(allele) {
    ifelse(allele == 1L, 0L, 1L)
  }), .SDcols = colnames(haps)[-(1:6)]]
  
  # Remove AA column
  haps[, AA := NULL]
  
  # Write output files
  data.table::fwrite(haps, paste0(out, ".haps"), sep = " ", col.names = F)
  data.table::fwrite(sample, paste0(out, ".sample"), sep = " ", col.names = F, quote = F, na = 0L)
  
}
