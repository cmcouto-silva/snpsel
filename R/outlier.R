#' @export

outlier <- function(x, extreme = F) {
  qt <- quantile(x, probs = c(0.25, 0.75))
  
  mult <- ifelse(extreme, 3, 1.5)
  IQR <- mult*(qt[2]-qt[1])
  
  lower <- qt[1] - IQR
  upper <- qt[2] + IQR
  
  x[x <= lower | x >= upper]
}
