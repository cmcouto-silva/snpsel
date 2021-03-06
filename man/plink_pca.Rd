% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plink_pca.R
\name{plink_pca}
\alias{plink_pca}
\title{Plink PCA analysis}
\usage{
plink_pca(
  input,
  output,
  pop,
  col,
  iid_col,
  pop_col,
  ell_col,
  ld = c(50, 5, 0.5),
  pc.x = c(2, 3, 4, 3, 4, 4),
  pc.y = c(1, 1, 1, 2, 2, 3),
  k,
  track_iid = F,
  track_outlier = F,
  plot = T,
  plot_type = "png"
)
}
\arguments{
\item{input}{Character. The name (and path) of the plink binary files without extension.}

\item{output}{Character. The prefix name for the output files.}

\item{pop}{Optional. Data.frame or data.table containing the first two columns of the fam file (named as FID and IID, respectively),
and additional columns with desired information for grouping populations (color and/or ellipses).}

\item{col}{Optional. A named vector containing color name/code for each population.}

\item{iid_col}{Optional. Character. Column name of the pop data.frame/data.table indicating the target-individuals.}

\item{pop_col}{Optional. Character. Column name of the pop data.frame/data.table indicating the target-populations.}

\item{ell_col}{Optional. Character. Column name of the pop data.frame/data.table for plotting ellipses.
These ellipses are normal confidence ellipses computed by 'stat_ellipse' function from 'ggplot2' package.}

\item{ld}{Numeric vector of length = 3. Indicates the window, step size and correlation threshold for LD filter, respectively.
Default set to c(50, 5, 05). Set it to NULL if no LD filter is desired.}

\item{pc.x}{Numeric vector. PC IDs for plotting. Default set to c(2, 3, 4, 3, 4, 4).}

\item{pc.y}{Numeric vector. PC IDs for plotting. Default set to c(1, 1, 1, 2, 2, 3).}

\item{k}{Numeric. Max number of PCs to be combined as figures. Default set to the length of pc.x/pc.y}

\item{track_iid}{Character vector. Target individuals to be mapped on plot. Default set to FALSE (none).}

\item{track_outlier}{Logical. Should outlier individuals be tracked on the figures? Default set to FALSE.}

\item{plot}{Logical. Should PCA figures be generated? Default set to TRUE.}

\item{plot_type}{Character. File format for output figures (e.g. bmp, jpeg, png, tiff, or pdf). Default set to png.}
}
\value{
List of PCA figures tracking down outlier individuals. Written PCA table and figures.
}
\description{
Plink PCA analysis
}
