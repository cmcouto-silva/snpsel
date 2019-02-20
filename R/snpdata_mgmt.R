#' @title SNP Data Management
#' @description Remove SNPs with duplicated positions, as well as no congruent match to reference table.
#' It provides the possibility of updating missing alleles (0 to the correspondent allele) and SNP IDs (
#' RefSNP (rs), and Affymetrix ID (Affy), if there is no information for RefSNP).
#' 
#' @param bim_file Character. Name of bim file.
#' @param Affx_HuOrigin Character. <path_to> Axiom_GW_HuOrigin table, available for download at:
#' https://www.thermofisher.com/br/en/home/life-science/microarray-analysis/microarray-data-analysis/genechip-array-annotation-files.html
#' @param update_snpID Logical. Informs if SNPs will be named according to the reference information (default = TRUE).
#' @param update_alleles Logical. Informs if SNPs with zeros will be updated to its basis (default = TRUE).
#' @param remove_dup_pos Logical. Informs if duplicated SNPs in dataset would be removed (via Plink Software); otherwise, 
#' if any duplicated positions is present on dataset, this function will not progress. Default set to FALSE. 
#' @return .bim file with updated SNP IDs
#' @examples
#'
#' \dontrun{
#'
#' bim_file <- "myfile.bim"
#' Affx_HuOrigin <- "Axiom_GW_HuOrigin.na35.annot.csv"
#'
#' # Running function
#' snpdata_mgmt(bim_file, Affx_HuOrigin)
#' }
#'
#' @importFrom magrittr set_names %<>% %>%
#' @export
#' @author Cainã Max Couto-Silva

snpdata_mgmt <- function(bim_file, 
                         Affx_HuOrigin = "~/cmcouto.silva@usp.br/lab_files/all_datasets/Annot/Axiom_GW_HuOrigin.na35.annot.csv",
                         update_snpID = T,
                         update_alleles = T,
                         remove_dup_pos = F) {
  
  #### Managing dataset ####
  ####################################################################################################!
  
  # Read data
  bim <- gt::read.bim(bim_file)
  
  # Remove duplicated chromosome/positions in dataset
  dup_pos <- bim[, duplicated(POS), by = CHR][, V1]
  
  if(any(dup_pos)) {
    
    if(remove_dup_pos) {
      
      dup_pos <- bim[dup_pos, POS]
      snps <- bim[POS %in% dup_pos, SNP]
      
      plink <- gt::check_plink_version()
      plink_file <- gt::rm.extension(bim_file)
      rm.snps <- paste0(plink_file, ".Remove.txt")
      writeLines(snps, rm.snps)
      
      # Running plink
      system(paste(
        plink, "--bfile", plink_file, "--exclude", rm.snps, "--make-bed", "--out", plink_file
      )); unlink(c(rm.snps, "*~"))
      
      bim <- gt::read.bim(bim_file)
      
    } else {
      stop (paste0("You should not have duplicate positions in your data.\n",
                   "There is ", sum(dup_pos), " duplicated positions. Run again with `remove_dup_pos = T` parameter in order to remove these SNPs."))
    }
  }
  
  #### Managing Reference Annotation Table ####
  ####################################################################################################!
  
  # Read Affymetrix annotation's table
  annot.table <- data.table::fread(Affx_HuOrigin,  header = T, na.strings = "---",
                                   select = c("Affy SNP ID","Chromosome","dbSNP RS ID", "Physical Position", "Ref Allele", "Alt Allele"))
  
  # Remove uninformative lines
  annot.table <- annot.table[!which(`Ref Allele` == "---" & `Alt Allele` == "---")]
  
  ## Verifying unique alleles
  # annot.table[, unique(`Ref Allele`)]
  # annot.table[, unique(`Alt Allele`)]
  
  ### Fixing Ref/Alt allele problems in reference
  alt_splitted <- data.table::tstrsplit(annot.table[, `Alt Allele`], split = "/")
  idx <- which(!is.na(alt_splitted[[2]]))
  
  if(length(alt_splitted) != 2L) stop("There are more than two alleles in `Alt Allele` reference!")
  if(!all(annot.table[idx, `Ref Allele` == "."])) stop ("Already informed `Ref Allele` where `Alt Allele` has two alleles;")
  annot.table[idx, c("Ref Allele", "Alt Allele") := .(alt_splitted[[1]][idx], alt_splitted[[2]][idx])]
  
  # Removing duplicated chromosome/positions
  DUP <- duplicated(annot.table[, .(Chromosome, `Physical Position`)])
  annot.table <- annot.table[!DUP]
  
  #### Merge Dataset & Reference Annotation Table ####
  ####################################################################################################!
  
  # Merge bim and annot.table by pos/chr
  bim[, c("CHR", "POS") := lapply(.(CHR, POS), as.character)]
  bim.merge <- merge(bim, annot.table, by.x = c("CHR","POS"), by.y = c("Chromosome", "Physical Position"), all.x = T, sort = F)
  if(nrow(bim.merge) != nrow(bim)) stop("Merge between dataset and reference has a mismatch. Please verify this manually!")
  
  # Flip strands (if necessary)
  bim.merge[A1 != 0L & A1 != `Ref Allele` & A1 != `Alt Allele` & A2 != `Ref Allele` & A2 != `Alt Allele`, 
            c('A1', 'A2') := .(gt::flip_strand(A1), gt::flip_strand(A2))]
  
  mismatch_alleles <- bim.merge[A1 != 0L & A1 != `Ref Allele` & A1 != `Alt Allele` & A2 != `Ref Allele` & A2 != `Alt Allele`, .N]
  if(mismatch_alleles > 0L) stop("Alleles from dataset do not correspond to alleles from reference, even after flipping strands!")
  
  #### Running arguments ####
  ####################################################################################################!
  
  # Update SNP IDs
  if(update_snpID) {
    bim.merge[!is.na(`dbSNP RS ID`) & !is.na(`Affy SNP ID`), 
              SNP := ifelse(grepl("^rs", `dbSNP RS ID`), `dbSNP RS ID`, `Affy SNP ID`)]
  }
  
  # Update alleles
  if(update_alleles) {
    bim.merge[!is.na(`Ref Allele`) & !is.na(`Alt Allele`) & A1 != 0L, 
              A1 := ifelse(A2 == `Alt Allele`, `Ref Allele`, `Alt Allele`)]
  }
  
  NAs <- any(bim.merge[, is.na(.SD), .SDcols = c('dbSNP RS ID', 'Affy SNP ID', 'Ref Allele', 'Alt Allele')])
  nbar <- ifelse((update_snpID | update_alleles) & NAs, "\n", "")
  
  if((update_snpID | update_alleles) & NAs) {
    cat('\n')
    warning("There is NAs in the reference table (SNPs and/or alleles), so that not all SNP will be updated! Try to keep only autossomes.", immediate. = T)
  }
  
  # Saving file
  if (!identical(bim, bim.merge[, .(CHR, SNP, GD, POS, A1, A2)]) | (any(dup_pos) & remove_dup_pos)) {
    gt::write.bim(bim = bim.merge[, .(CHR, SNP, GD, POS, A1, A2)], output = bim_file)
    cat(paste0(nbar, "File '", bim_file, "'", " has been overwritten.\n"))
  } else {
    cat(nbar, "No changes on dataset has been done!\n")
  }
  
}
