as.strings = function(...) {
  strings <-  sys.call(0)
  strings <- as.character(strings)[-1]
  return(strings)
}
