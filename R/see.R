#' @title Print first lines & columns from a data frame
#' @description This functions allows you to print your dataframe/matrix without noise. Explore it just like the "head()" function, but with limited columns showed.
#' It's an alias for df[1:10,1:10], but handling these values (10) if your data has fewer dimensions (lines or columns).
#' @param df A data frame or matrix.
#' @return Printing of the very first lines and columns of your data frame / matrix.
#' @author Cainã Max Couto-Silva
#' @export
#'

see <- function(df){
  r <- ifelse(test = nrow(df) < 10, yes = nrow(df), no = 10)
  c <- ifelse(test = ncol(df) < 10, yes = ncol(df), no = 10)

  return(df[1:r,1:c])
}
