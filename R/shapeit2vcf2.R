#' @export
shapeit2vcf2 <- function(shapeit_files, out) {
  
  # Checking if required programs are installed on system path
  program_on_path("shapeit")
  program_on_path("plink2")
  
  # Missing output
  if(missing(out)) out <- shapeit_files
  
  haps <- paste0(shapeit_files, ".haps")
  sample <- paste0(shapeit_files, ".sample")
  
  sample_tmp <- fread(sample, header = F)
  sample_tmp[3:.N, paste0("V", 3:7) := lapply(.SD, function(x){
    ifelse(x %in% c(0,-9), "NA", x)
  }), .SDcols = paste0("V", 3:7)]
  
  sample <- paste0(shapeit_files, "_tmp.sample")
  fwrite(sample_tmp, sample, sep = " ", col.names = F)
  
  ## Conversion from Shapeit to Plink binary format
  plink2(`--haps` = haps, `--sample` = sample, "--double-id", `--export` = "vcf", `--out` = out)
  
  # Remove temporarily files
  unlink(c(paste0(shapeit_files, ".log"), sample))
}
