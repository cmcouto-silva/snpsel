#' @title Plink PCA analysis
#' @import data.table ggplot2 ggrepel
#' @param input Character. The name (and path) of the plink binary files without extension.
#' @param output Character. The prefix name for the output files.
#' @param pop Optional. Data.frame or data.table containing the first two columns of the fam file (named as FID and IID, respectively),
#' and additional columns with desired information for grouping populations (color and/or ellipses).
#' @param col Optional. A named vector containing color name/code for each population. 
#' @param iid_col Optional. Character. Column name of the pop data.frame/data.table indicating the target-individuals.
#' @param pop_col Optional. Character. Column name of the pop data.frame/data.table indicating the target-populations.
#' @param ell_col Optional. Character. Column name of the pop data.frame/data.table for plotting ellipses.
#' These ellipses are normal confidence ellipses computed by 'stat_ellipse' function from 'ggplot2' package.
#' @param ld Numeric vector of length = 3. Indicates the window, step size and correlation threshold for LD filter, respectively.
#' Default set to c(50, 5, 05). Set it to NULL if no LD filter is desired.
#' @param pc.x Numeric vector. PC IDs for plotting. Default set to c(2, 3, 4, 3, 4, 4).
#' @param pc.y Numeric vector. PC IDs for plotting. Default set to c(1, 1, 1, 2, 2, 3).
#' @param k Numeric. Max number of PCs to be combined as figures. Default set to the length of pc.x/pc.y
#' @param plot Logical. Should PCA figures be generated? Default set to TRUE.
#' @param plot_type Character. File format for output figures (e.g. bmp, jpeg, png, tiff, or pdf). Default set to png.
#' @param track_iid Character vector. Target individuals to be mapped on plot. Default set to FALSE (none).
#' @param track_outlier Logical. Should outlier individuals be tracked on the figures? Default set to FALSE.
#' @return List of PCA figures tracking down outlier individuals. Written PCA table and figures.
#' @export

plink_pca <- function(input, output, pop, col, iid_col, pop_col, ell_col, ld = c(50, 5, 0.5),
                      pc.x = c(2, 3, 4, 3, 4, 4), pc.y = c(1, 1, 1, 2, 2, 3), k, 
                      track_iid = F, track_outlier = F,
                      plot = T, plot_type = "png") {
  
  # Check if plink is installed on path
  program_on_path("plink")
  
  # Verify required files already exist in the current path 
  plink_files <- paste0("plink.", c("bed", "bim", "fam", "log", "nosex", "prune.in", "prune.out"))#, "eigenval", "eigenvec"))
  plink_files_on_path <- plink_files[plink_files %in% dir()]
  
  if(length(plink_files_on_path > 0)) {
    stop(paste0('["', paste0(plink_files_on_path, collapse = '","'), '"]'),
         " file(s) exist on the current path. Please remove/rename them first.")
  }
  
  # Run Plink PCA Analysis
  if(!is.null(ld)) {
    plink(`--bfile` = input, `--indep-pairwise` = paste(ld, collapse = " "))
    plink(`--bfile` = input, `--extract` = "plink.prune.in", "--make-bed")
    plink(`--bfile` = "plink", "--pca")
  } else {
    plink(`--bfile` = input, "--pca")
  }
  
  # Load Plink PCA Results
  # eigenval <- data.table::fread("plink.eigenval")
  eigenvec <- data.table::fread("plink.eigenvec")
  names(eigenvec) <- c("FID", "IID", paste0("PC", 1:(length(eigenvec)-2)))
  
  # Write PCA Results
  fwrite(eigenvec, paste0(output, '.pca.txt'), sep = "\t")
  
  # Setting up groups and ellipses
  if(missing(pop)) {
    
    pca <- eigenvec
    pca[, Population := as.factor(FID)]
    
  } else {
    pop <- as.data.table(pop)
    pca <- merge(eigenvec, pop, by.x = iid_col, by.y = "IID", sort = F)
    
    if(nrow(eigenvec) != nrow(pca)) {
      stop("All IDs from plink file must be present in the annotation file.")
    }
    
    # if(!missing(pop_col)) {
    #   pca[, Population := as.factor(get(pop_col))]
    # } else {
    #   pca[, Population := as.factor(FID)]
    # }
    
    pca[, Population := as.factor(get(pop_col))]
    
    if(!missing(ell_col)) pca[, Ellipse := as.factor(get(ell_col))]
    if(missing(pop_col) & missing(ell_col)) {
      cat("\n")
      warning("Population Reference provided, but not column selected. It'll use Family ID as groups.")
    }
  }
  
  if(isTRUE(plot)) {
    
    # Filter k PCs
    if(missing(k)) k <- length(pc.x)
    pc.x <- pc.x[1:k]
    pc.y <- pc.y[1:k]
    
    # Saving plots for PC combinations
    pc_list <- mapply(function(x, y, color, ellipse) {
      
      pc <- ggplot(pca, aes(x = get(paste0("PC", x)), y = get(paste0("PC", y)))) +
        geom_point(aes(col = Population), alpha = 1, size = 2) +
        labs(x = paste0("PC", x), y = paste0("PC", y)) +
        theme_classic() +
        theme (
          text = element_text(family = "Arial", size = 12),
          legend.position = "right"
        )
      
      if(color) pc <- pc + scale_colour_manual(values = col)
      if(ellipse) pc <- pc + stat_ellipse(aes(group = Ellipse), colour = "darkgray", type = "t", linetype = 2, level = 0.95)
      
      # Verify & tracking down outliers
      if(track_outlier) {
        
        outlier_iid <- intersect (
          outlier(pca, "PC1", extreme = F)[, IID],
          outlier(pca, "PC2", extreme = F)[, IID]
        )
        
        if(length(outlier_iid) > 0L) {
          pc$outliers <- outlier_iid
          outlier_dt <- pca[IID %in% outlier_iid]
          pc <- pc + ggrepel::geom_label_repel(outlier_dt, mapping = aes(label = IID), point.padding = 1, nudge_x = 0.01)
        }
      }
      
      if(!isFALSE(track_iid)) {
        pc$target_iid <- track_iid
        target_dt <- pca[IID %in% track_iid]
        pc <- pc + ggrepel::geom_label_repel(target_dt, mapping = aes(label = IID), point.padding = 1, nudge_x = 0.01)
      }
      
      return(pc)
      
    }, pc.x, pc.y, !missing(col), !missing(ell_col), SIMPLIFY = F)
    
    for(i in seq_along(pc.x)) {
      
      if(plot_type == "pdf") {pc.x = 
        pdf(file = paste0(output, "_pc.x", pc.x[i], ".y", pc.y[i], ".pdf"), width = 14, height = 14, family = "Times")
        pc_list[[i]]
        dev.off()
      }
      
      if(plot_type %in% c("bmp", "jpeg","png", "tiff")) {
        out <- paste0(output, "_pc.x", pc.x[i], ".y", pc.y[i], ".", plot_type)
        eval(parse(text = paste0(
          plot_type, "(", "'", out, ".", plot_type, "'", ", width = 36, height = 20, units = 'cm', res = 600)"
        )))
        
        print(pc_list[[i]])
        dev.off()
      }
      
    }
    
    # Remove temporary files
    unlink(plink_files)
    
    return(pc_list)    
    
  }
  
}
