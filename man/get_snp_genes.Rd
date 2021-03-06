% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_snp_genes.R
\name{get_snp_genes}
\alias{get_snp_genes}
\title{Get Gene info from SNP ID}
\usage{
get_snp_genes(
  snp,
  ref = "~/cmcouto.silva@usp.br/lab_files/all_datasets/Reference_annotation",
  include = NULL
)
}
\arguments{
\item{snp}{Character. Vector with rs IDs.}

\item{ref}{Scalar character. Path to file with SNP and gene info (table with "SNP" and "GENE" columns).}
}
\value{
Character vector with gene IDs.
}
\description{
Parse gene IDs from list of SNP IDs (rs IDs). Annotation file is required!
}
\author{
Cainã Max Couto-Silva
}
