#' @title Admixture Barplot
#' @description Generate a barplot for admixture output (.Q file).
#' @usage admixture(file.Q, col = rainbow(3), xlab = "Individual #", ylab = "Ancestry", border = NA, ...)
#' @param file.Q Character vector. Admixture output file name (ending with .Q)
#' @param ... Graphical parameters
#' @return Admixture barplot
#' @author Cainã Max Couto-Silva.
#' @export

admixture.plot <- function(file.Q, col = rainbow(3), xlab = "Individual #", ylab = "Ancestry", border = NA, ...) {
  file.Q <- t(as.matrix(read.table(file.Q)))
  barplot(file.Q, col = rainbow(3), xlab = xlab, ylab = ylab, border = border, ...)
}