#' @title Identify Outliers in PCA per group
#' @param plt Output from plink_pca() function.
#' @param cols Character vector. Names from desired columns in plt$data object.
#' Default set to c("IID", "Population") so that no error occurs.
#' @param PCx Integer scalar. PC from x axis.
#' @param PCy Integer scalar. PC from x axis.
#' @return List of data.table(s) with outlier individuals per population.
#' @author Cainã Max Couto-Silva
#' @export

get_pca_outliers <- function(plt, cols = c("IID", "Population"), PCx = 2, PCy = 1, extreme = FALSE) {
  
  populations <- plt$data[, unique(Population)]
  
  outliers_lst <- lapply(populations, function(pop) {
    plt <- plt$data[Population == pop]
    out <- intersect (
      gt::outlier(plt, col_name = paste0("PC", PCx), extreme)[, IID],
      outlier(plt, col_name = paste0("PC", PCy), extreme)[, IID])
    plt <- plt[IID %in% out, .SD, .SDcols = cols]
    return(plt)
  })
  
  names(outliers_lst) <- populations
  return(outliers_lst)
  
}
