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
#' @importFrom rlang check_installed
esri2rast <- function(url,
                      bbox = NULL,
                      token = NULL,
                      format = "jpgpng",
                      adjustAspectRatio = FALSE,
                      ...) {
  check_installed(
    "terra",
    reason = "{.pkg terra} must be installed to use {.fn esri2rast}."
  )

  layerInfo <- esrimeta(url = url, token = token)

  layerCRS <- getLayerCRS(layerInfo$extent$spatialReference)

  rast_ext <- bbox2extent(bbox, crs = layerCRS)

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

  esri_rast <- suppressWarnings(terra::rast(x = req[["url"]]))

  terra::crs(esri_rast) <- layerCRS
  terra::ext(esri_rast) <- rast_ext

  esri_rast
}

#' @noRd
bbox2extent <- function(bbox,
                        ...,
                        crs = NULL) {
  check_installed(
    "terra",
    call = caller_env()
  )

  bbox <- bbox_transform(bbox, crs)

  rast_ext <-
    terra::rast(
      xmin = bbox[["xmin"]],
      xmax = bbox[["xmax"]],
      ymin = bbox[["ymin"]],
      ymax = bbox[["ymax"]],
      crs = crs
    )

  terra::ext(rast_ext)
}

#' @noRd
bbox_transform <- function(x, crs = NULL) {
  if (is_null(crs)) {
    return(sf::st_bbox(x))
  }

  if (is_bbox(x)) {
    x <- sf::st_as_sfc(x)
  }

  sf::st_bbox(sf::st_transform(x, crs))
}
