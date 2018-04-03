#' @title Read vcf file
#' @description Read vcf file
#' @param bim_file <path_to_vcf_file>
#' @return Data.table/data.frame
#' @examples
#' \dontrun{
#' DT <- read.vcf("./myfile.vcf")
#' }
#' @export
#'

read.vcf <- function(vcf_file){
    data.table::fread(input = vcf_file, colClasses = "character", header = T)
}
