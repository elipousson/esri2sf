% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esriIndex.R
\name{esriIndex}
\alias{esriIndex}
\alias{esriIndexLayers}
\alias{esriindex}
\title{Create an index of folders, services, layers, and tables for an ArcGIS Server}
\usage{
esriIndex(
  url,
  folderPath = NULL,
  serviceName = NULL,
  recurse = FALSE,
  token = NULL,
  ...
)

esriIndexLayers(url, folderPath = NULL, serviceName = NULL, token = NULL, ...)

esriindex(
  url,
  folderPath = NULL,
  serviceName = NULL,
  recurse = FALSE,
  token = NULL,
  ...
)
}
\arguments{
\item{url}{URL for ArcGIS server, folder, or service.}

\item{folderPath, serviceName}{Name of parent folder or service; only used
internally (not intended for user).}

\item{recurse}{If \code{TRUE}, recursively check folders and services to return an
index that includes services in folders, subfolders, layers, and tables.
Defaults to \code{FALSE}. If \code{url} starts with "https://services.arcgis.com",
emit a warning and recurse is set to \code{FALSE}.}

\item{token}{String for authentication token; defaults to \code{NULL}.}

\item{...}{Additional parameters passed to \code{\link[=esriCatalog]{esriCatalog()}} (not typically
required)}
}
\description{
Recurse over a ArcGIS Server url or folder url to return a data frame index
of folders, services, layers, and tables. This function returns additional
information than \code{\link[=esriCatalog]{esriCatalog()}} using \code{f = "sitemap"} or \code{f = "geositemap"}.
}
