#' @keywords internal
isWktID <- function(crs) {
  is.numeric(crs) || grepl(pattern = "^(EPSG|ESRI):[[:digit:]]+$", x = crs)
}

#' @keywords internal
WKTunPretty <- function(wkt) {
  gsub("\\n[[:blank:]]*", "", wkt)
}

#' @keywords internal
#' @importFrom yyjsonr write_json_str
#' @importFrom sf st_crs
set_features_crs <- function(crs = NULL) {
  if (is_null(crs)) {
    return("")
  }

  if (is.numeric(crs)) {
    return(as.character(crs))
  }

  if (isWktID(crs)) {
    return(sub(pattern = "^(EPSG|ESRI):", replacement = "", x = crs))
  }

  # FIXME: Unsure how to test this refactoring
  crs <- yyjsonr::write_json_str(
    list("wkt" = WKTunPretty(sf::st_crs(crs)$WKT1_ESRI))#,
    # auto_unbox = TRUE
  )

  as.character(crs)
}

#' Get object ids
#'
#' @keywords internal
#' @noRd
#' @importFrom httr2 resp_body_json
getObjectIds <- function(url,
                         where = NULL,
                         token = NULL,
                         objectIds = NULL,
                         geometry = NULL,
                         geometryType = NULL,
                         ...) {
  resp <- esriRequest(
    url = url,
    append = "query",
    token = token,
    f = "json",
    objectIds = objectIds,
    where = where %||% "1=1",
    geometryType = geometryType,
    geometry = geometry,
    returnIdsOnly = TRUE,
    ...
  )

  resp <- httr2::resp_body_json(resp = resp, check_type = FALSE)

  resp[["objectIds"]]
}

#' Get count of maximum records per request
#'
#' @keywords internal
#' @noRd
getMaxRecordsCount <- function(url,
                               token = NULL,
                               maxRecords = NULL,
                               upperLimit = FALSE) {
  if (!is_null(maxRecords)) {
    return(as.integer(maxRecords))
  }

  urlInfo <- esriCatalog(url = url, token = token)

  if (is_null(urlInfo[["maxRecordCount"]])) {
    return(500L)
  }

  if (urlInfo[["maxRecordCount"]] > 25000 && upperLimit) {
    return(25000L)
  }

  urlInfo[["maxRecordCount"]]
}

#' Get table for Table layer
#'
#' @keywords internal
#' @noRd
#' @importFrom tibble as_tibble
getEsriTable <- function(jsonFeats, .name_repair = "check_unique") {
  atts <- lapply(
    lapply(jsonFeats, `[[`, 1),
    function(att) lapply(att, function(x) ifelse(is_null(x), NA, x))
  )

  df <- list_rbind(
    lapply(atts, as.data.frame.list, stringsAsFactors = FALSE)
  )

  tibble::as_tibble(df, .name_repair = .name_repair)
}


#' Get features using feature ids from getObjectIds
#'
#' @keywords internal
#' @noRd
#' @importFrom httr2 resp_body_json
getEsriFeaturesByIds <- function(objectIds = NULL,
                                 url,
                                 fields = NULL,
                                 token = NULL,
                                 crs = NULL,
                                 simplifyDataFrame = FALSE,
                                 simplifyVector = simplifyDataFrame,
                                 ...) {
  fields <- fields %||% c("*")

  outFields <- I(paste(fields, collapse = ","))

  resp <- esriRequest(
    url,
    append = "query",
    f = "json",
    objectIds = objectIds,
    outFields = outFields,
    outSR = crs,
    token = token,
    ...
  )

  resp <- httr2::resp_body_json(
    resp,
    check_type = FALSE,
    # Additional parameters passed to jsonlite::fromJSON
    digits = NA,
    simplifyDataFrame = simplifyDataFrame,
    simplifyVector = simplifyVector
  )

  resp[["features"]]
}


#' Get ESRI features
#'
#' @keywords internal
#' @noRd
#' @importFrom cli cli_warn cli_progress_along
getEsriFeatures <- function(url,
                            fields = NULL,
                            where = NULL,
                            geometry = NULL,
                            geometryType = NULL,
                            objectIds = NULL,
                            token = NULL,
                            crs = NULL,
                            progress = FALSE,
                            call = caller_env(),
                            ...) {
  crs <- set_features_crs(crs)

  ids <- getObjectIds(
    url = url,
    where = where,
    geometry = geometry,
    geometryType = geometryType,
    token = token,
    objectIds = objectIds,
    ...
  )

  if (is_null(ids)) {
    cli::cli_warn("No records match the search criteria.")
    return(invisible(NULL))
  }

  # Get max record count and split ids based on count
  maxRC <- getMaxRecordsCount(url, token, upperLimit = TRUE)
  idSplits <- split(ids, seq_along(ids) %/% maxRC)

  seq_fn <- seq_along

  # Check if cli_progress_along progress bar can be used
  if (progress) {
    seq_fn <- cli::cli_progress_along
  }

  results <- lapply(
    seq_fn(idSplits),
    function(x) {
      getEsriFeaturesByIds(
        objectIds = idSplits[[x]],
        url = url,
        fields = fields,
        token = token,
        crs = crs,
        ...
      )
    }
  )

  unlist(results, recursive = FALSE)
}
