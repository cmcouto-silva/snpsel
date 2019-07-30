#' @title Merge Plink files
#' @author Cainã Max Couto-Silva
#' @export

plink_merge <- function(dataset01, dataset02, out, keep.snp.names = NULL) {
  
  # Loading datasets
  ds1 <- read.bim(paste0(dataset01, '.bim'))
  ds2 <- read.bim(paste0(dataset02, '.bim'))
  
  # Extract Mutual Variants by SNP ID
  if(is.null(keep.snp.names)) {
    
    # Write down intersect SNPs
    snps <- intersect(ds1[, SNP], ds2[, SNP])
    writeLines(snps, "mutual_vars_Rtmp.txt")
    
    # Running Plink for removing non-match SNPs in each dataset
    for (ds in c(dataset01, dataset02)) {
      plink(`--bfile` = ds, `--extract` = "mutual_vars_Rtmp.txt",
            "--keep-allele-order --allow-no-sex", "--make-bed",
            `--out` = paste0(ds, '_Rtmp'))
      
      unlink(paste0(ds, '_Rtmp.txt'))
    }
    
    # Loading datasets with matching IDs
    ds1 <- read.bim(paste0(dataset01, '_Rtmp.bim'))
    ds2 <- read.bim(paste0(dataset02, '_Rtmp.bim'))
    
    # Checking duplicated positions by chromosome
    if(nrow(ds1) != nrow(ds2)) {
      stop("There are duplicated SNP IDs. Please remove them before running thins function.")
    }
    unlink("mutual_vars_Rtmp.txt")
  }
  
  # Extract Mutual Variants by chr:pos
  if(!is.null(keep.snp.names)) {
    
    # Verify matching positions
    ds_match <- merge(ds1, ds2, by = c("CHR","POS"), sort = F)
    
    # Write matching SNP in both datasets
    writeLines(ds_match[, SNP.x], paste0(dataset01, '_Rtmp.txt'))
    writeLines(ds_match[, SNP.y], paste0(dataset02, '_Rtmp.txt'))
    
    # Running Plink for removing non-match SNPs in each dataset
    for (ds in c(dataset01, dataset02)) {
      plink(`--bfile` = ds, `--extract` = paste0(ds, '_Rtmp.txt'),
            "--keep-allele-order --allow-no-sex", "--make-bed",
            `--out` = paste0(ds, '_Rtmp'))
      
      unlink(paste0(ds, '_Rtmp.txt'))
    }
    
    # Loading datasets with matching positions
    ds1 <- read.bim(paste0(dataset01, '_Rtmp.bim'))
    ds2 <- read.bim(paste0(dataset02, '_Rtmp.bim'))
    
    # Checking duplicated positions by chromosome
    if(nrow(ds1) != nrow(ds2)) {
      stop("There are duplicated SNP positions by chromosome. Please remove them before running thins function.")
    }
    
    # Equalizing SNP IDs in both datasets
    if(keep.snp.names == 1L){
      ds2[, SNP := ds1[, SNP]]
      write.bim(ds2, paste0(dataset02, '_Rtmp.bim'))
    } else {
      ds1[, SNP := ds2[, SNP]]
      write.bim(ds1, paste0(dataset02, '_Rtmp.bim'))
    }
    
  }
  
  # Merge datasets
  plink(`--bfile` = paste0(dataset01, '_Rtmp'),
        `--bmerge` = paste0(paste0(dataset02, '_Rtmp'), c('.bed','.bim','.fam'), collapse = " "), 
        "--make-bed", "--keep-allele-order --allow-no-sex", `--out` = out)
  
  files.to.remove <- c (
    paste0(out, ".log"),
    paste0(paste0(dataset01, '_Rtmp'), c(".bed", ".bim", ".fam", ".nosex", ".log")),
    paste0(paste0(dataset02, '_Rtmp'), c(".bed", ".bim", ".fam", ".nosex", ".log"))
  )
  
  #### IF ERROR EMERGES ####
  # Dealing with problematic SNPs (flipping strand and removing triallelic+ SNPs )
  
  if(file.exists(paste0(out, "-merge.missnp"))) {
    file.rename(paste0(out, "-merge.missnp"), paste0(out, "-merge_Rtmp.missnp"))
    
    # Flipping
    plink(`--bfile` = paste0(dataset01, '_Rtmp'), `--flip` = paste0(out, "-merge_Rtmp.missnp"),
          "--make-bed", "--keep-allele-order --allow-no-sex", `--out` = paste0(dataset01, '_Rtmp'))
    
    # Trying to merge
    plink(`--bfile` = paste0(dataset01, '_Rtmp'),
          `--bmerge` = paste0(paste0(dataset02, '_Rtmp'), c('.bed','.bim','.fam'), collapse = " "),
          "--make-bed", "--keep-allele-order --allow-no-sex", `--out` = out)
    
    if(file.exists(paste0(out, "-merge.missnp"))) {
      
      # Remove problematic SNPs
      for (ds in c(paste0(dataset01, '_Rtmp'), paste0(dataset02, '_Rtmp'))) {
        plink(`--bfile` = ds, `--exclude` = paste0(out, "-merge.missnp"), "--make-bed", "--allow-no-sex", `--out` = ds)
        files.to.remove <- append(files.to.remove, paste0(ds, paste0(c('.bed~','.bim~','.fam~'))))
      }
      
      # Final merge
      plink(`--bfile` = paste0(dataset01, '_Rtmp'),
            `--bmerge` = paste0(paste0(dataset02, '_Rtmp'), c('.bed','.bim','.fam'), collapse = " "),
            "--make-bed", "--keep-allele-order --allow-no-sex", `--out` = out)
      
      files.to.remove <- append(files.to.remove, paste0(out, "-merge.missnp"))
      
    }
    
    files.to.remove <- append(files.to.remove, c(
      paste0(out, '-merge_Rtmp.missnp'),
      paste0(dataset01, '*~'))
    )
    unlink(files.to.remove)
  }
}
