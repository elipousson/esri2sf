% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esriIndex.R
\name{esriIndex}
\alias{esriIndex}
\alias{esriIndexLayers}
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

esriIndexLayers(url, folderPath = NULL, serviceName = NULL, token = "", ...)
}
\arguments{
\item{url}{URL for ArcGIS server, folder, or service.}

\item{folderPath, serviceName}{Name of parent folder or service; only used
internally (not intended for user).}

\item{recurse}{If \code{TRUE}, recursively check folders and services to return an
index that includes services in folders, subfolders, layers, and tables.
Defaults to \code{FALSE}.}

\item{token}{String for authentication token; defaults to \code{NULL}.}

\item{...}{Additional parameters passed to \link{esriCatalog}}
}
\description{
Currently only returns layers and tables for MapServer and FeatureServer
services. Consider using \link{esriCatalog} with \code{format = "sitemap"} or \code{format = "geositemap"}
as an alternative approach to indexing available folders and services.
}