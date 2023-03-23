#' Get data or metadata from an ArcGIS MapServer or FeatureServer
#'
#' These functions are the interface to the user.
#'
#' @param url A service url, e.g.
#'   <https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/2>
#'    or an ArcGIS Online item url if the item contains a single feature or
#'   table layer.
#'
#' @param outFields vector of fields you want to include. default is `NULL` for
#'   all fields.
#' @param where string for where condition. Default is `NULL` (equivalent to
#'   `1=1`) to return all rows.
#' @param token string for authentication token. defaults to `NULL`.
#' @param crs coordinate reference system (see [sf::st_sf()]). Should either be
#'   `NULL` or a CRS that can be handled by GDAL through sf::st_sf(). Default is
#'   `getOption("esri2sf.crs", 4326)` which sets the CRS to EPSG:4326 if no
#'   option is set. If CRS is `NULL` feature is returned with the same CRS that
#'   the layer is hosted as in the Feature/Map Server.
#' @param bbox bbox class object from [sf::st_bbox()] or a simple feature object
#'   that can be converted to a bounding box.
#' @param geometry An `sf` or `bbox` object. Currently, `sf` objects with a
#'   single POINT feature are supported. All other `sf` objects are converted to
#'   `bbox` objects.
#' @param progress Show progress bar from [cli::cli_progress_along()] if `TRUE`.
#'   Default `FALSE`.
#' @param geomType string specifying the layer geometry ('esriGeometryPolygon'
#'   or 'esriGeometryPoint' or 'esriGeometryPolyline' - if `NULL`, will try to
#'   be inferred from the server)
#' @param spatialRel Spatial relationship applied to the input `geometry` when
#'   performing the query; defaults to `NULL` (equivalent to
#'   "esriSpatialRelIntersects"). Additional supported options include
#'   "esriSpatialRelContains", "esriSpatialRelCrosses",
#'   "esriSpatialRelEnvelopeIntersects", "esriSpatialRelIndexIntersects",
#'   "esriSpatialRelOverlaps", "esriSpatialRelTouches", "esriSpatialRelWithin"
#' @param replaceDomainInfo If `TRUE`, add domain information to the return data
#'   frame. Default `FALSE`.
#' @param quiet If `TRUE`, use [suppressMessages()] to prevent the printing of
#'   messages about the requested layer. Defaults to `FALSE`.
#' @param .name_repair Treatment of problematic column names:
#'   * `"minimal"`: No name repair or checks, beyond basic existence,
#'   * `"unique"`: Make sure names are unique and not empty,
#'   * `"check_unique"`: (default value), no name repair, but check they are
#'     `unique`,
#'   * `"universal"`: Make the names `unique` and syntactic
#'   * a function: apply custom name repair (e.g., `.name_repair = make.names`
#'     for names in the style of base R).
#'   * A purrr-style anonymous function, see [rlang::as_function()]
#'
#'   This argument is passed on as `repair` to [vctrs::vec_as_names()].
#'   See there for more details on these terms and the strategies used
#'   to enforce them.
#' @param ... additional named parameters to pass to the query. (e.g.
#'   `"resultRecordCount = 3"`). See the [ArcGIS REST APIs
#'   documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-map-service-layer-.htm)
#'   for more information on all supported parameters.
#' @return simple feature (`esri2sf`) or tibble (`esri2df`) or list or
#'   tibble (`esrimeta`).
#'
#' @describeIn esri2sf Retrieve spatial object
#'
#' @note When accessing services with multiple layers, the layer number must be
#' specified at the end of the service url (e.g.,
#' <https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/2>).
#' #' The list of layers and their respective id numbers can be found by viewing
#' the service's url in a web browser and viewing the "Layers" heading.
#'
#' @examples
#' baseURL <- "https://sampleserver6.arcgisonline.com/arcgis/rest/"
#' url <- paste0(baseURL, "services/Census/MapServer/2")
#' outFields <- c("POP2007", "POP2000")
#' where <- "STATE_NAME = 'Michigan'"
#' df <- esri2sf(url, outFields = outFields, where = where)
#' plot(df)
#'
#' @export
#' @importFrom cli cli_rule cli_alert_warning cli_alert_info cli_dl cli_par
esri2sf <- function(url,
                    outFields = NULL,
                    where = NULL,
                    geometry = NULL,
                    bbox = NULL,
                    token = NULL,
                    crs = getOption("esri2sf.crs", 4326),
                    progress = FALSE,
                    geomType = NULL,
                    spatialRel = NULL,
                    replaceDomainInfo = FALSE,
                    .name_repair = "check_unique",
                    quiet = FALSE,
                    ...) {
  cli_quiet(quiet)

  url <- check_esriUrl(url, token)

  layerInfo <- esrimeta(url = url, token = token)

  check_layerTypes(layerInfo, url, token)

  cli::cli_rule(
    "Downloading {.val {layerInfo$name}} from {.url {url}}"
  )

  if (is_groupLayer(layerInfo)) {
    return(
      esrigroup(
        layerInfo,
        url = url,
        outFields = outFields,
        where = where,
        geometry = geometry,
        bbox = bbox,
        token = token,
        crs = crs,
        progress = progress,
        geomType = geomType,
        spatialRel = spatialRel,
        replaceDomainInfo = replaceDomainInfo,
        .name_repair = .name_repair,
        .fn = esri2sf,
        ...
      )
    )
  }

  # Get the layer geometry type
  if (is.null(geomType)) {
    if (is_tableLayer(layerInfo) | is_missing_geomType(layerInfo)) {
      cli::cli_alert_warning(
        "{.arg geomType} is {.val NULL} and a layer geometry type
          can't be found for this url."
      )

      cli::cli_alert_info(
        "Trying to download with {.fn esri2df}{cli::symbol$ellipsis}"
      )

      return(
        esri2df(
          url = url,
          outFields = outFields,
          where = where,
          token = token,
          progress = progress,
          replaceDomainInfo = replaceDomainInfo,
          .name_repair = .name_repair,
          quiet = quiet,
          ...
        )
      )
    }

    layerGeomType <- layerInfo$geometryType
  } else {
    geomType_mismatch <-
      !is.null(layerInfo$geometryType) && (layerInfo$geometryType != geomType)

    if (geomType_mismatch) {
      cli::cli_alert_warning(
        "The provided {.arg geomType} value {.val {geomType}} does not
        match the layer geometryType value {.val {layerInfo$geometryType}}."
      )
    }

    layerGeomType <- geomType
  }

  cli::cli_dl(
    items = c(
      "Layer type" = "{.val {layerInfo$type}}",
      "Geometry type" = "{.val {layerGeomType}}"
    )
  )

  if (!is.null(layerInfo$extent$spatialReference)) {
    layerCRS <-
      getLayerCRS(spatialReference = layerInfo$extent$spatialReference)

    cli::cli_dl(
      c("Service CRS" = "{.val {sf::st_crs(layerCRS)$srid}}")
    )
  } else {
    cli::cli_alert_warning(
      "The spatial reference for this layer is missing."
    )

    if (!is.null(crs)) {
      cli::cli_alert_info(
        "Trying to access the layer using the
        provided {.arg crs}: {.val {crs}}."
      )

      layerCRS <- crs
    }
  }

  if (is.null(crs)) {
    crs <- layerCRS
  }

  cli::cli_dl(
    c("Output CRS" = "{.val {sf::st_crs(crs)$srid}}")
  )

  if (!is.null(bbox) && is.null(geometry)) {
    geometry <- bbox2geometry(bbox)
  }

  # Set default geometryType for spatial filter
  geometryType <- NULL

  if (!is.null(geometry)) {
    # Set geometryType based on geometry type of simple feature
    geometryType <-
      sf2geometryType(
        x = geometry
      )

    geometry <-
      sf2geometry(
        x = geometry,
        geometryType = geometryType,
        layerCRS = layerCRS
      )

    if (!is.null(spatialRel)) {
      spatialRel_opts <-
        c(
          "esriSpatialRelIntersects", "esriSpatialRelContains",
          "esriSpatialRelCrosses", "esriSpatialRelEnvelopeIntersects",
          "esriSpatialRelIndexIntersects", "esriSpatialRelOverlaps",
          "esriSpatialRelTouches", "esriSpatialRelWithin"
        )

      spatialRel <-
        match.arg(
          spatialRel,
          spatialRel_opts
        )
    }
  }

  cli::cli_par()

  # Get layer features
  esriFeatures <-
    getEsriFeatures(
      url = url,
      fields = outFields,
      where = where,
      geometry = geometry,
      geometryType = geometryType,
      token = token,
      crs = crs,
      progress = progress,
      spatialRel = spatialRel,
      ...
    )

  # Convert geometry to simple features
  sfdf <-
    esri2sfGeom(
      jsonFeats = esriFeatures,
      layerGeomType = layerGeomType,
      crs = crs,
      .name_repair = .name_repair
    )

  if (!replaceDomainInfo) {
    return(sfdf)
  }

  addDomainInfo(sfdf, url = url, token = token)
}

#' Is layerInfo missing geometryType?
#'
#' @noRd
is_missing_geomType <- function(layerInfo) {
  any(c(is.null(layerInfo$geometryType), (layerInfo$geometryType == "")))
}

#' @describeIn esri2sf Retrieve table object (no spatial data).
#' @export
#' @importFrom cli cli_alert_warning cli_alert_info cli_rule cli_dl cli_par
esri2df <- function(url,
                    outFields = NULL,
                    where = NULL,
                    token = NULL,
                    progress = FALSE,
                    replaceDomainInfo = FALSE,
                    .name_repair = "check_unique",
                    quiet = FALSE,
                    ...) {
  cli_quiet(quiet)

  url <- check_esriUrl(url, token)

  layerInfo <- esrimeta(url = url, token = token)

  check_layerTypes(layerInfo, url, token)

  if (!is_tableLayer(layerInfo)) {
    cli::cli_alert_warning(
      "The layer {.var {layerInfo$name}} must be a {.val 'Table'} service to
      use {.fn esri2df}."
    )

    cli::cli_alert_info("Trying to download with {.fn esri2sf} {cli::symbol$ellipsis}")
    return(
      esri2sf(
        url = url,
        outFields = outFields,
        where = where,
        token = token,
        progress = progress,
        replaceDomainInfo = replaceDomainInfo,
        ...
      )
    )
  }

  cli::cli_rule(
    "Downloading {.val {layerInfo$name}} from {.url {url}}"
  )

  cli::cli_dl(
    items = c("Layer type" = "{.val {layerInfo$type}}")
  )
  cli::cli_par()

  esriFeatures <-
    getEsriFeatures(
      url = url,
      fields = outFields,
      where = where,
      token = token,
      progress = progress,
      ...
    )

  df <- getEsriTable(esriFeatures, .name_repair = .name_repair)

  if (!replaceDomainInfo) {
    return(df)
  }

  addDomainInfo(df, url = url, token = token)
}


#' Retrieve layer metadata
#'
#' @name esrimeta
#' @param url url to retrieve metadata for.
#' @inheritParams esriRequest
#' @param fields `esrimeta` returns data frame with fields if `TRUE`. Default
#'   `FALSE`.
#' @param call Defaults to [parent.frame()]. Passed to [cli::cli_abort()] to
#'   improve error messages when [esrimeta()] is called by another function.
#' @export
#' @importFrom dplyr bind_rows
esrimeta <- function(url,
                     token = NULL,
                     fields = FALSE,
                     ...,
                     call = parent.frame()) {
  layerInfo <- esriCatalog(
    url = url,
    token = token,
    simplifyVector = TRUE,
    ...
  )

  # check_layerInfo(layerInfo, call = call)

  if (!fields) {
    return(layerInfo)
  }

  list_rbind(layerInfo["fields"])
}


#' Helper function to trigger error if layerInfo returns an erro
#'
#' @noRd
#' @importFrom utils hasName
#' @importFrom cli cli_abort
check_layerInfo <- function(layerInfo, call = parent.frame()) {
  if (!utils::hasName(layerInfo, "error")) {
    return(invisible())
  }

  message <- paste0(
    layerInfo$error$message,
    " - code: ", layerInfo$error$code
  )

  if (utils::hasName(layerInfo$error, "details") &&
    !identical(layerInfo$error$details, layerInfo$error$message)) {
    message <- c(message, "i" = as.character(layerInfo$error$details))
  }

  cli::cli_abort(
    message,
    call = call
  )
}

#' Helper function for getting layer CRS based on spatialReference
#'
#' @noRd
#' @importFrom sf st_crs
#' @importFrom cli cli_abort
#' @importFrom rlang has_name
getLayerCRS <- function(spatialReference, layerCRS = NULL, call = parent.frame()) {
  # Get the layer CRS from the layer spatial reference
  if (rlang::has_name(spatialReference, "latestWkid")) {
    layerCRS <- spatialReference$latestWkid
  } else if (rlang::has_name(spatialReference, "wkid")) {
    layerCRS <- spatialReference$wkid
  } else if (rlang::has_name(spatialReference, "wkt")) {
    layerCRS <- spatialReference$wkt
  }

  # Format CRS (from esri2sfGeom)
  if (isWktID(layerCRS)) {
    layerCRS <- sf::st_crs(layerCRS)$srid
  }

  if (is.null(layerCRS)) {
    cli::cli_abort(
      c("A valid layer coordinate reference system can't be found.",
        "*" = "Check that the layer at the {.arg url} has a spatial reference."
      ),
      call = call
    )
  }

  layerCRS
}


#' Helper function for setting geometryType based on geometry parameter
#'
#' @noRd
#' @importFrom cli cli_abort
#' @importFrom sf st_geometry_type
sf2geometryType <- function(x, by_geometry = FALSE, call = parent.frame()) {
  if (inherits(x, "bbox")) {
    return("esriGeometryEnvelope")
  }

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort(
      "{.arg geometry} must be a {.cls sf} or {.cls sfc}
      or {.cls bbox} object, not {.cls {class(x)}}.",
      call = call
    )
  }

  geometryType <- sf::st_geometry_type(x, by_geometry = by_geometry)

  switch(as.character(geometryType),
    "POINT" = "esriGeometryPoint",
    "POLYGON" = "esriGeometryPolygon",
    "MULTIPOLYGON" = "esriGeometryPolygon",
    "MULTIPOINT" = "esriGeometryMultipoint",
    "LINESTRING" = "esriGeometryPolyline",
    "MULTILINESTRING" = "esriGeometryPolyline"
  )
}


#' Helper function for converting simple feature object to geometry parameter
#' for spatial filter
#'
#' Currently only supports sf objects with POINT geometry. All other sf or bbox
#' objects are converted to a bbox.
#'
#' @noRd
#' @importFrom sf st_sf st_as_sfc st_transform st_geometry_type st_bbox
#'   st_coordinates
#' @importFrom cli cli_abort
sf2geometry <- function(x, geometryType = NULL, layerCRS = NULL) {
  if (inherits(x, "bbox")) {
    x <- sf::st_sf(sf::st_as_sfc(x))
  }

  if (!is.null(layerCRS)) {
    x <- sf::st_transform(x, layerCRS)
  }

  if (!(sf::st_geometry_type(x, by_geometry = FALSE) == "POINT")) {
    x <- sf::st_bbox(x)
    geometryType <- "esriGeometryEnvelope"
  }

  switch(geometryType,
    "esriGeometryEnvelope" = paste0(
      unlist(as.list(x), use.names = FALSE),
      collapse = ","
    ),
    "esriGeometryPoint" = paste0(
      sf::st_coordinates(x),
      collapse = ","
    )
  )
}

#' Helper to convert bbox to sf or error on non-sf and non-bbox objects
#'
#' @noRd
#' @importFrom sf st_bbox
#' @importFrom cli cli_abort
bbox2geometry <- function(bbox, call = parent.frame()) {
  # convert sf class bbox to bbox class
  if (inherits(bbox, c("sf", "sfc"))) {
    bbox <- sf::st_bbox(bbox)
  }

  if (!inherits(bbox, "bbox")) {
    cli::cli_abort(
      c("{.arg bbox} must be a {.code bbox} or {.code sf} class object.",
        "i" = "The class of the provided {.arg bbox} is {.val {class(bbox)}}"
      ),
      call = call
    )
  }

  bbox
}

#' Helper function to convert item URLs to Feature Server URLs if they only
#' include a single layer
#'
#' @noRd
check_esriUrl <- function(url,
                          token = NULL,
                          from = NULL,
                          to = "feature",
                          call = parent.frame()) {
  if (esriUrl_isValidType(url, type = to, call = call)) {
    return(url)
  }

  convert_esriUrl(url = url, token = token, from = from, to = to, call = call)
}

#' Helper function to abort if layerType is not supported
#'
#' @noRd
#' @importFrom cli cli_abort
#' @importFrom rlang has_name
check_layerTypes <- function(layerInfo,
                             url = NULL,
                             token = NULL,
                             layerTypes = c("Feature Layer", "Table", "Group Layer"),
                             call = parent.frame()) {
  if (!rlang::has_name(layerInfo, "type")) {
    cli::cli_abort(
      "{.arg url} must be a
      {.val {cli::cli_vec(layerTypes, style = list(vec_last = ' or '))}} type
      {.emph feature} url, not a
      {.emph {esriUrl_isValidType(url, returnType = TRUE)}} url.",
      call = call
    )
  }

  if (!is.null(layerInfo$type) & !all(layerInfo$type %in% layerTypes)) {
    cli::cli_abort(
      c("The {.arg url} must be for a
        {.val {cli::cli_vec(layerTypes, style = list(vec_last = ' or '))}}.",
        "i" = "The provided {.arg url} is a {.val {layerInfo$type}} service."
      ),
      call = call
    )
  }
}

#' Helper function to validate if a layer is a Table layer
#'
#' @noRd
is_tableLayer <- function(layerInfo) {
  !is.null(layerInfo$type) & (layerInfo$type == "Table")
}

#' Helper function to validate if a layer is a Group layer
#'
#' @noRd
is_groupLayer <- function(layerInfo) {
  !is.null(layerInfo$type) & (layerInfo$type == "Group Layer")
}

#' Helper function to download Group Layers for esri2sf
#'
#' @noRd
#' @importFrom cli cli_rule cli_ol cli_par cli_progress_along symbol pb_current
#'   pb_bar pb_percent
#' @importFrom rlang set_names
esrigroup <- function(layerInfo,
                      url,
                      outFields = NULL,
                      where = NULL,
                      geometry = NULL,
                      bbox = NULL,
                      token = NULL,
                      crs = getOption("esri2sf.crs", 4326),
                      progress = TRUE,
                      geomType = NULL,
                      spatialRel = NULL,
                      replaceDomainInfo = FALSE,
                      .name_repair = "check_unique",
                      quiet = FALSE,
                      .fn = esri2sf,
                      ...) {
  cli::cli_rule(cli::col_blue("Group sublayers include:"))
  sublayers <- as.character(layerInfo$subLayers$name)
  cli::cli_ol(
    sublayers
  )
  cli::cli_par()

  url <- vapply(
    layerInfo$subLayers$id,
    function(x) {
      gsub(paste0(basename(url), "$"), x, url)
    },
    NA_character_
  )

  sfdf <-
    lapply(
      cli::cli_progress_along(
        url,
        format = "{cli::symbol$info} Downloading {.val {sublayers[[cli::pb_current]]}} | {cli::pb_bar} {cli::pb_percent}",
        total = length(url)
      ),
      function(x) {
        .fn(
          url = url[[x]],
          outFields = outFields,
          where = where,
          geometry = geometry,
          bbox = bbox,
          token = token,
          crs = crs,
          progress = progress,
          geomType = geomType,
          spatialRel = spatialRel,
          replaceDomainInfo = replaceDomainInfo,
          quiet = TRUE,
          .name_repair = .name_repair,
          ...
        )
      }
    )

  rlang::set_names(sfdf, layerInfo[["subLayers"]][["name"]])
}
