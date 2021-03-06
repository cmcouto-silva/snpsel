% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcf_merge.R
\name{vcf_merge}
\alias{vcf_merge}
\title{Merge vcf files}
\usage{
vcf_merge(...)
}
\arguments{
\item{...}{Character vector. Vcf file must be provided as strings (with full or relative paths).}
}
\value{
Data.table/data.frame object with merged vcf files.
}
\description{
Merge multiple vcf files in a single one
}
\examples{
\dontrun{
vcf_files <- gtools::mixedsort(list.files(path = ".", pattern = "\\\\.vcf$"))
vcf_merged <- vcf_merge(vcf_files)
}
}
