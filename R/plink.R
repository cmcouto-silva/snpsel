#' @export
plink <- function(...) {
  values <- c(...)
  args <- names(values)
  plink <- plink_version()
  program_on_path("plink")
  
  if(is.null(args)) args <- ""
  commands <- ifelse(args == "", values, paste(args, values))
  commands <- paste(commands, collapse = " ")
  
  system(paste(plink, commands))
}
