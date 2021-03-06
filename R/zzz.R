#' @importFrom dplyr %>%
#' @importFrom httr POST GET content config
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_sf st_sfc st_point st_multipolygon st_multilinestring

NULL

generateToken <- function(server, uid, pwd = "", expiration = 5000) {
  # generate auth token from GIS server
  if (pwd == "") pwd <- rstudioapi::askForPassword("pwd")

  query <- list(
    username = uid,
    password = pwd,
    expiration = expiration,
    client = "requestip",
    f = "json"
  )

  r <- POST(paste(server, "arcgis/admin/generateToken", sep = "/"),
    body = query, encode = "form")
  fromJSON(content(r, "parsed"))$token
}

# Generate a OAuth token for Arcgis Online
# How to obtain clientId and clientSecret is described here:
# https://developers.arcgis.com/documentation/core-concepts/security-and-authentication/accessing-arcgis-online-services/
generateOAuthToken <- function(clientId, clientSecret, expiration = 5000) {

  query = list(
    client_id = clientId,
    client_secret = clientSecret,
    expiration = expiration,
    grant_type = "client_credentials"
  )

  r <- POST("https://www.arcgis.com/sharing/rest/oauth2/token", body = query)
  content(r, type = "application/json")$access_token
}


getObjectIds <- function(queryUrl, where, bbox, token = "", ...) {

  # create Simple Features from ArcGIS servers json response
  query <- list(where = where, geometryType = "esriGeometryEnvelope", geometry = bbox, returnIdsOnly = "true", token = token, f = "json",
    ...)

  responseRaw <- content(POST(queryUrl, body = query, encode = "form",
    config = config(ssl_verifypeer = FALSE)), as = "text")

  response <- fromJSON(responseRaw)
  response$objectIds
}


getEsriTable <- function(jsonFeats) {
  atts <- lapply(lapply(jsonFeats, `[[`, 1),
    function(att) lapply(att, function(x) ifelse(is.null(x), NA, x)))
  df <- dplyr::bind_rows(lapply(atts, as.data.frame.list, stringsAsFactors = FALSE))
  dplyr::as_tibble(df)
}


getEsriFeaturesByIds <- function(ids, queryUrl, fields, token = "", ...) {
  # create Simple Features from ArcGIS servers json response
  query <- list(objectIds = paste(ids, collapse = ","), outFields = paste(fields,
    collapse = ","), token = token, outSR = "4326", f = "json", ...)

  responseRaw <- content(POST(queryUrl, body = query, encode = "form",
    config = config(ssl_verifypeer = FALSE)), as = "text")

  response <- fromJSON(responseRaw, simplifyDataFrame = FALSE, simplifyVector = FALSE,
    digits = NA)
  response$features
}

esri2sfPoint <- function(features) {
  getPointGeometry <- function(feature) {
    if (is.numeric(unlist(feature$geometry))) {
      st_point(unlist(feature$geometry))
    } else st_point()
  }
  st_sfc(lapply(features, getPointGeometry))
}

esri2sfPolygon <- function(features) {
  ring2matrix <- function(ring) do.call(rbind, lapply(ring, unlist))
  rings2multipoly <- function(rings)
    st_multipolygon(list(lapply(rings, ring2matrix)))

  getGeometry <- function(feature) {
    if (is.null(unlist(feature$geometry$rings))) {
      st_multipolygon()
    } else rings2multipoly(feature$geometry$rings)
  }

  st_sfc(lapply(features, getGeometry))
}

esri2sfPolyline <- function(features) {
  path2matrix <- function(path) do.call(rbind, lapply(path, unlist))
  paths2multiline <- function(paths) st_multilinestring(lapply(paths, path2matrix))

  getGeometry <- function(feature) paths2multiline(feature$geometry$paths)

  st_sfc(lapply(features, getGeometry))
}

getEsriFeatures <- function(queryUrl, fields, where, bbox, token = "", ...) {
  ids <- getObjectIds(queryUrl, where, bbox, token, ...)
  if (is.null(ids)) {
    warning("No records match the search criteria.")
    return()
  }
  idSplits <- split(ids, seq_along(ids) %/% 500)

  results <- lapply(idSplits, getEsriFeaturesByIds, queryUrl, fields, token, ...)
  unlist(results, recursive = FALSE)
}

esri2sfGeom <- function(jsonFeats, geomType, crs = 4326) {
  # convert esri json to simple feature
  geoms <- switch(geomType,
    esriGeometryPolygon = esri2sfPolygon(jsonFeats),
    esriGeometryPoint = esri2sfPoint(jsonFeats),
    esriGeometryPolyline = esri2sfPolyline(jsonFeats)
  )

  # attributes
  atts <- lapply(lapply(jsonFeats, `[[`, 1),
    function(att) lapply(att, function(x) ifelse(is.null(x), NA, x)))

  af <- dplyr::bind_rows(lapply(atts, as.data.frame.list, stringsAsFactors = FALSE))
  # geometry + attributes
  st_sf(geoms, af, crs = crs)
}
