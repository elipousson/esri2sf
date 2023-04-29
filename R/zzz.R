# @staticimports pkg:stringstatic
#  str_extract

.onLoad <- function(libname, pkgname) {
  utils::data(
    list = c(
      "esri_version_ref"
    ),
    package = pkgname,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c("name", "serviceType", "type", "urlType")
)

#' Does x inherit a bbox class?
#'
#' @noRd
is_bbox <- function(x) {
  inherits(x, "bbox")
}

#' Does x inherit a sf or (optionally) sfc class?
#'
#' @noRd
is_sf <- function(x, allow_sfc = TRUE) {
  inherits(x, "sf") || (allow_sfc && inherits(x, "sfc"))
}


#' @noRd
cli_abort_ifnot <- function(x = NULL,
                            ...,
                            .fn = NULL,
                            call = caller_env()) {
  cli_ifnot(
    x = x,
    ...,
    .predicate = is_false,
    .fn = .fn,
    .default = cli::cli_abort,
    call = call
  )
}

#' @noRd
cli_abort_if <- function(x = NULL,
                         ...,
                         .fn = NULL,
                         call = caller_env()) {
  cli_if(
    x = x,
    ...,
    .predicate = is_true,
    .fn = .fn,
    .default = cli::cli_abort,
    call = call
  )
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_rbind
list_rbind <- function(x, names_to = zap(), ptype = NULL) {
  vctrs::vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = current_env()
  )
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_cbind
list_cbind <- function(x,
                       name_repair = c("unique", "universal", "check_unique"),
                       size = NULL) {
  vctrs::vec_cbind(
    !!!x,
    .name_repair = name_repair,
    .size = size,
    .error_call = current_env()
  )
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
  resp <-
    esriRequest(
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

  if (!is_null(urlInfo[["maxRecordCount"]])) {
    if (urlInfo[["maxRecordCount"]] > 25000 && upperLimit) {
      return(25000L)
    }

    return(urlInfo[["maxRecordCount"]])
  }

  500L
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

  df <-
    list_rbind(
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

  resp <-
    esriRequest(
      url,
      append = "query",
      f = "json",
      objectIds = objectIds,
      outFields = outFields,
      outSR = crs,
      token = token,
      ...
    )

  resp <-
    httr2::resp_body_json(
      resp,
      check_type = FALSE,
      # Additional parameters passed to jsonlite::fromJSON
      digits = NA,
      simplifyDataFrame = simplifyDataFrame,
      simplifyVector = simplifyVector,
    )

  resp[["features"]]
}


#' Get ESRI features
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr case_when
#' @importFrom jsonlite toJSON
#' @importFrom sf st_crs
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
  crs <-
    dplyr::case_when(
      is_null(crs) ~ "",
      is.numeric(crs) ~ as.character(crs),
      isWktID(crs) ~ sub(pattern = "^(EPSG|ESRI):", replacement = "", x = crs),
      TRUE ~ as.character(jsonlite::toJSON(list("wkt" = WKTunPretty(sf::st_crs(crs)$WKT1_ESRI)), auto_unbox = TRUE))
    )

  ids <-
    getObjectIds(
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
    invisible(return(NULL))
  }

  # Get max record count and split ids based on count
  maxRC <- getMaxRecordsCount(url, token, upperLimit = TRUE)
  idSplits <- split(ids, seq_along(ids) %/% maxRC)

  seq_fn <- seq_along

  # Check if pbapply progress bar can be used
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

#' @keywords internal
isWktID <- function(crs) {
  is.numeric(crs) || grepl(pattern = "^(EPSG|ESRI):[[:digit:]]+$", x = crs)
}

#' @keywords internal
WKTunPretty <- function(wkt) {
  gsub("\\n[[:blank:]]*", "", wkt)
}
