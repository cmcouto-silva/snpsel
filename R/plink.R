#' @export
plink <- function(...) {
  values <- c(...)
  args <- names(values)
  plink <- gt::plink_version()
  
  commands <- ifelse(args == "", values, paste(args, values))
  commands <- paste(commands, collapse = " ")
  
  system(paste(plink, commands))
}
