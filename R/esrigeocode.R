#' Use an ArcGIS GeocodeServer to geocode an address or reverse geocode
#' coordinates
#'
#' This function allows the use of an ArcGIS GeocodeServer url to support the
#' [Find Address
#' Candidates](https://developers.arcgis.com/rest/services-reference/enterprise/find-address-candidates.htm)
#' and [Reverse
#' Geocode](https://developers.arcgis.com/rest/services-reference/enterprise/reverse-geocode.htm)
#' REST APIs. Provide an address parameter to use Find Address Candidates or a
#' coords parameter to use Reverse Geocode.
#'
#' @param url A GeocodeServer service url. Required.
#' @param address Single line address passed at the "SingleLine" parameter to
#'   the ArcGIS REST API. Specific format may depend on specific server
#'   configuration, Default: `NULL`
#' @param coords Numeric vector with longitude, latitude coordinates or a sf
#'   object where the centroid is used as the coordinates. Default: `NULL`
#' @param score Accuracy score, if provided only return results with provided
#'   accuracy score or greater Default: 0.95
#' @param n Number of candidates to return, Default: 1
#' @param token Token, Default: `NULL`
#' @param crs Coordinate reference system to return.
#' @param geometry If `TRUE` (default), return a simple feature object. If
#'   `FALSE`, return a data frame.
#' @param ... Additional parameters passed to [esriRequest()].
#' @export
#' @importFrom cli cli_alert_warning
#' @importFrom sf st_crs
#' @importFrom dplyr case_when bind_cols select any_of
#' @importFrom httr2 resp_body_json
esrigeocode <- function(url,
                        address = NULL,
                        coords = NULL,
                        score = 0.95,
                        n = 1,
                        token = NULL,
                        crs = getOption("esri2sf.crs", 4326),
                        geometry = TRUE,
                        ...) {

  cli_abort_ifnot(
    x = grepl("GeocodeServer", url),
    message = "{.arg url} must be a {.val GeocodeServer} url."
  )

  cli_abort_ifnot(
    x = !is_null(address) || !is_null(coords),
    message = "{.arg address} or {.arg coords} must be supplied."
  )

  location <- NULL
  SingleLine <- NULL

  layerInfo <- esrimeta(url)

  if (is_null(layerInfo$spatialReference)) {
    cli::cli_alert_warning(
      "Can't find a spatial reference at the provided url."
    )
    layerCRS <- sf::st_crs(crs)$srid
  } else {
    layerCRS <- getLayerCRS(spatialReference = layerInfo$spatialReference)
  }

  operation <- dplyr::case_when(
    !is_null(address) && has_length(address, 1) ~ "findAddressCandidates",
    !is_null(address) ~ "geocodeAddresses",
    !is_null(coords) ~ "reverseGeocode"
  )

  cli_abort_ifnot(
    x = operation != "geocodeAddresses",
    message = "{.fn esrigeocode} currently only supports length 1
      character vectors for the {.arg address} argument."
  )

  if (operation == "findAddressCandidates") {
    SingleLine <- address
  }

  if (operation == "reverseGeocode") {
    if (is_sf(coords)) {
      coords <- sf2coords(coords, layerCRS)
    }

    cli_abort_ifnot(
      x = is.numeric(coords) && has_length(coords, 2),
      message = "{.arg coords} must be a {.cls sf} object or a length 2 numeric vector."
    )

    location <- paste0(coords, collapse = ",")
  }

  resp <-
    esriRequest(
      url = url,
      append = operation,
      f = "json",
      token = token,
      SingleLine = SingleLine,
      location = location,
      ...
    )

  resp <-
    httr2::resp_body_json(
      resp,
      simplifyVector = TRUE
    )

  if (operation == "findAddressCandidates") {
    candidates <- resp[["candidates"]]

    cli_abort_ifnot(
      x = nrow(candidates) > 0,
      message = "Address candidates can't be found to match the {.arg address}
      or {.arg coords} supplied."
    )

    if (is.numeric(score)) {
      candidates <- candidates[candidates[["score"]] >= score, ]
    }

    results <-
      dplyr::bind_cols(
        dplyr::select(
          candidates,
          -dplyr::any_of(c("location", "attributes", "extent"))
        ),
        candidates[["attributes"]],
        candidates[["extent"]],
        candidates[["location"]]
      )

    if (!is_null(n)) {
      results <- results[seq(n), ]
    }
  }

  if (operation == "reverseGeocode") {
    results <-
      suppressWarnings(
        dplyr::bind_cols(
          resp[["address"]],
          resp[["location"]]
        )
      )
  }

  check_bool(geometry)

  if (geometry) {
    return(geocoderesultss2sf(results, layerCRS = layerCRS, crs = crs))
  }

  results
}

#' Helper to convert results to an sf object
#'
#' @noRd
#' @importFrom sf st_as_sf st_transform
geocoderesultss2sf <- function(x,
                               coords = NULL,
                               layerCRS = 4326,
                               crs = NULL) {
  if (is_null(coords)) {
    coords <- c("x", "y")
  }

  stopifnot(
    all(coords %in% names(x))
  )

  x <-
    sf::st_as_sf(
      x,
      coords = coords,
      crs = layerCRS
    )

  if (is_null(crs)) {
    return(x)
  }

  sf::st_transform(x, crs = crs)
}

#' Convert a sf object to a coords numeric vector
#'
#' @noRd
#' @importFrom sf st_transform st_union st_centroid st_coordinates
sf2coords <- function(x,
                      crs = NULL) {
  if (!is_null(crs)) {
    x <- sf::st_transform(x, crs = crs)
  }

  coords <- sf::st_union(x)
  coords <- suppressWarnings(sf::st_centroid(coords))
  as.numeric(sf::st_coordinates(coords, by_geometry = FALSE))
}
