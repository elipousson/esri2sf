#' Import data from ArcGIS ImageServer url using the exportImage API
#'
#' See the ArcGIS REST API documentation for more information on the exportImage
#' API
#' <https://developers.arcgis.com/rest/services-reference/enterprise/export-image.htm>
#'
#' @noRd
#' @param url ImageServer url
#' @param bbox Bounding box for image to return; defaults to `NULL`.
#' @param token defaults to `NULL`.
#' @param format defaults to "jpgpng". Options include "jpgpng", "png", "png8",
#'   "png24", "jpg", "bmp", "gif", "tiff", "png32", "bip", "bsq", and "lerc"
#' @param adjustAspectRatio defaults to `FALSE`
#' @return SpatRaster object from `terra::rast`
esri2rast <- function(url,
                      bbox = NULL,
                      token = NULL,
                      format = "jpgpng",
                      adjustAspectRatio = FALSE,
                      ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("{.pkg terra} must be installed to use {.fn esri2rast}.")
  }

  layerInfo <- esrimeta(url = url, token = token)

  layerCRS <- getLayerCRS(layerInfo$extent$spatialReference)

  bbox_ext <- bbox_transform(bbox, layerCRS)

  bbox <-
    sf2geometry(
      x = bbox,
      geometryType = "esriEnvelope",
      layerCRS = layerCRS
    )

  format <-
    match.arg(
      tolower(format),
      c(
        "jpgpng", "png", "png8", "png24", "jpg", "bmp",
        "gif", "tiff", "png32", "bip", "bsq", "lerc"
      )
    )

  req <-
    esriRequest(
      url = url,
      append = "exportImage",
      bbox = bbox,
      format = format,
      f = "image",
      adjustAspectRatio = adjustAspectRatio,
      .perform = FALSE,
      ...
    )

  ras <- terra::set.ext(terra::rast(req$url), as.numeric(bbox_ext))

  terra::set.crs(ras, layerCRS)
}

#' @noRd
bbox_transform <- function(x, crs = NULL) {
  if (is.null(crs)) {
    return(sf::st_bbox(x))
  }

  if (inherits(x, "bbox")) {
    x <- sf::st_as_sfc(x)
  }

  sf::st_bbox(sf::st_transform(x, crs))
}
