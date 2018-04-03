#' @title Dataframe Subsetting
#' @description This function quickly subsets a dataframe or matrix based on a vector of IDs.
#'
#' @param data A data frame or matrix.
#' @param axis You must specify in which axis is the target-vector of names/IDs. If they are arranged in line, axis must be set to 1, otherwise (vector arranged in a column) it must be set to 2.
#' @param index Integer. It specifies in which line or column there is the target-vector for matching (like an ordinal number).
#' @param list.names A vector of names for matching the target-vector in the dataframe/matrix.
#' @return A data frame subset.
#' @author Cainã Max Couto-Silva
#' @export

df.sub <- function(data, axis = 2, index = 1, list.names) {
  margin <- axis
  list.names <- unlist(list.names, use.names = F)

  # If axis = lines (1)
  if (margin == 1) {
    matches <- which(data[index, ] %in% list.names)
    new.data <- data[, matches]
  }

  # If axis = columns (2)
  if (margin == 2) {
    matches <- which(data[, index] %in% list.names)
    new.data <- data[matches, ]
  }

  return(new.data)
}
