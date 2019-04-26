#' @export

outlier <- function(x, col_name = NULL, extreme = F) {
  
  if (any(class(x) %in% c("data.table", "data.frame", "matrix"))) {
    
    if(is.null(col_name)) {
      stop("Column named must be specified for objects from class data.table, data.frame, or matrix.")
    }
    
    if(any(class(x) == "data.table")) {
      qt <- quantile(unlist(x[, col_name, with = F]), probs = c(0.25, 0.75))
      mult <- ifelse(extreme, 3, 1.5)
      IQR <- mult*(qt[2]-qt[1])
      lower <- qt[1] - IQR
      upper <- qt[2] + IQR
      idx <- as.logical(x[, .SD <= lower | .SD >= upper,, .SDcols = col_name])
      return(x[idx])
      
    } else {
      
      qt <- quantile(x[, col_name], probs = c(0.25, 0.75))
      mult <- ifelse(extreme, 3, 1.5)
      IQR <- mult*(qt[2]-qt[1])
      lower <- qt[1] - IQR
      upper <- qt[2] + IQR
      return(x[x[, col_name] <= lower | x[, col_name] >= upper, ])
      
    }
    
  } else {
    qt <- quantile(x, probs = c(0.25, 0.75))
    
    mult <- ifelse(extreme, 3, 1.5)
    IQR <- mult*(qt[2]-qt[1])
    
    lower <- qt[1] - IQR
    upper <- qt[2] + IQR
    
    return(x[x <= lower | x >= upper])
  }
  
}

