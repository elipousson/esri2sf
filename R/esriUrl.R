serviceTypes <- c(
  "MapServer", "FeatureServer", "GPServer", "GeocodeServer",
  "GeometryServer", "ImageServer"
)

#' Is the ESRI url a valid type?
#'
#' Confirms if the url is an ESRI URL and optionally returns the URL type or, if
#' it is not valid, the reason the url appears to be invalid.
#'
#' @noRd
#' @importFrom stats setNames
#' @importFrom cli cli_inform
esriUrl_isValidType <- function(url,
                                token = NULL,
                                type = NULL,
                                displayReason = FALSE,
                                returnType = FALSE,
                                call = caller_env()) {
  servicesUrl <- grepl("/rest/services", url)
  servicesUrl_types <- c("root", "folder", "service", "feature", "content")

  siteUrl <- grepl(".html?", url)
  siteUrl_types <- c("search", "item", "group", "user", "app")

  if (!is_null(type)) {
    type <- match.arg(tolower(type), c(servicesUrl_types, siteUrl_types))
  } else if (identical(type, NA_character_)) {
    type <- NULL
  }

  isType <- NA

  if (isTRUE(servicesUrl)) {
    layerInfo <- esrimeta(url, token = token, call = call)
    check_layerInfo(layerInfo, call)

    isType <- c(
      "root" = grepl("/rest/services/?$", url),
      "folder" = ("folders" %in% names(layerInfo)),
      "service" = (grepl(paste0("/(", paste0(serviceTypes, collapse = "|"), ")/?$"), url)),
      "feature" = (grepl(paste0("/(", paste0(serviceTypes, collapse = "|"), ")/[[:digit:]]+/?$"), url)),
      "content" = grepl("/content/", url),
      stats::setNames(rep_len(FALSE, length(siteUrl_types)), siteUrl_types)
    )
  } else if (isTRUE(siteUrl)) {
    isType <- c(
      "search" = grepl("/home/search\\.html\\?q=", url),
      "item" = grepl("/home/item\\.html\\?id=", url),
      "group" = grepl("/home/group\\.html\\?id=", url),
      "user" = grepl("/home/user\\.html\\?user=", url),
      "app" = grepl("/index\\.html\\?appid=", url),
      stats::setNames(rep_len(FALSE, length(servicesUrl_types)), servicesUrl_types)
    )
  }

  url_types <- names(which(isType))
  out <- FALSE

  if (!is_null(type)) {
    out <- any(type %in% url_types)
  } else if (!is_null(url_types)) {
    out <- !identical(url_types, character(0)) | servicesUrl
  }

  if (!out && displayReason) {
    if (!is_null(type) && !returnType) {
      reason <-
        switch(type,
          "root" = "A {.val {type}} {.arg url} must end in '/rest/services':",
          "folder" = "A {.val {type}} {.arg url} must be a 'Folder' endpoint:",
          "service" = "A {.val {type}} {.arg url} must end in one of the supported service types ({serviceTypes}):",
          "feature" = "A {.val {type}} {.arg url} must end in a feature ID:",
          "content" = "A {.val {type}} {.arg url} must include '/content/':",
          "search" = "A {.val {type}} {.arg url} must include '/home/search.html?q=':",
          "item" = "An {.val {type}} {.arg url} must include '/home/item.html?id=':",
          "group" = "A {.val {type}} {.arg url} must include '/home/group.html?id=':",
          "user" = "A {.val {type}} {.arg url} must include '/home/user.html?user=':",
          "app" = "An {.val {type}} {.arg url} must include '/index.html?appid=':"
        )

      cli::cli_bullets(c("!" = reason, " " = "{.url {url}}"))
    }

    cli::cli_inform(c("!" = "Invalid {.arg url}: {.url {url}}"))
  }

  if (returnType) {
    if (!out && is_null(type)) {
      return(NA_character_)
    }

    return(names(isType[which(isType)[1]]))
  }

  out
}

#' @title Validate or parse the parts of a ESRI REST Server URL
#'
#' @description A collection of functions that pull select parts out of a
#'   ESRI Service URL. All urls should be a form similar to:
#' * `https://<host>/<instance>/rest/services/<folderPath>/serviceName>/<serviceType>/<featureID>`
#' * `http://<host>/<instance>/rest/services/serviceName>/<serviceType>`
#' * `<host>/<instance>/rest/services/<folderPath>/serviceName>/<serviceType>`
#' * `https://<host>/<instance>/rest/services/serviceName>/<serviceType>/<featureID>`
#' * `https://<host>/<instance>/rest/services/<folderPath>`
#' * `https://<host>/<instance>/rest/services`
#'
#' And having these rules:
#'  * The scheme: `https://` or `http://` part is optional
#'  * The `host` part is the domain of the url.
#'  * The `instance`  is the first subpage after the domain in the url.
#'  * The `/rest/services` is the second and third subpage in the url. These are
#'  standard for all ESRI REST Services.
#'  * The `folderPath` part is optional and indicates the file structure in the
#'  REST Service. It consists of all subpages between `/rest/services/` and the
#'  `serviceName` part (if available).
#'  * The `serviceName` part is the last subpage betore the `<serviceType>` in
#'  the url.
#'  * The `serviceType` specifies the type of service. Currently this package
#'  works to manage the following serviceTypes: 'MapServer', 'FeatureServer',
#'  'GPServer', 'GeocodeServer', 'GeometryServer', 'ImageServer'.
#'  * The `featureID` is optional and specifies the layer or table in the map
#'  service.
#'
#'
#' @param url The url for a Map/Feature server or for a layer/table in a
#'   Map/Feature Server.
#' @param token String for authentication token (if needed).
#' @param displayReason Should the reason for why a url is not valid be displayed.
#'
#' @return Character string of the request part of the url.


#' @describeIn esriUrl Check if url is valid for an ESRI REST Service. General
#'   to include potential layer id too.
#' @export
esriUrl_isValid <- function(url, token = NULL, displayReason = FALSE) {
  esriUrl_isValidType(
    url = url,
    token = token,
    type = NULL,
    displayReason = displayReason,
    returnType = FALSE
  )
}

#' @describeIn esriUrl Check if url is valid for the root of an ESRI REST
#'   Server.
#' @export
esriUrl_isValidRoot <- function(url, token = NULL, displayReason = FALSE) {
  esriUrl_isValidType(
    url = url,
    token = token,
    type = "root",
    displayReason = displayReason,
    returnType = FALSE
  )
}

#' @describeIn esriUrl Check if url is valid for a folder of an ESRI REST
#'   Server.
#' @export
esriUrl_isValidFolder <- function(url, token = NULL, displayReason = FALSE) {
  esriUrl_isValidType(
    url = url,
    token = token,
    type = "folder",
    displayReason = displayReason,
    returnType = FALSE
  )
}

#' @describeIn esriUrl Check if url is valid for a Service of an ESRI REST
#'   Server. No feature ID.
#' @export
esriUrl_isValidService <- function(url, token = NULL, displayReason = FALSE) {
  esriUrl_isValidType(
    url = url,
    token = token,
    type = "service",
    displayReason = displayReason,
    returnType = FALSE
  )
}

#' @describeIn esriUrl DEPRECATED Use esriUrl_isValidFeature
#' @export
esriUrl_isValidID <- function(url, token = NULL, displayReason = FALSE) {
  .Deprecated("esriUrl_isValidFeature")
  esriUrl_isValidFeature(url,
    token = token,
    displayReason = displayReason
  )
}

#' @describeIn esriUrl Check if url is valid for a feature of an ESRI REST
#'   Service.
#' @export
esriUrl_isValidFeature <- function(url, token = NULL, displayReason = FALSE) {
  esriUrl_isValidType(
    url = url,
    token = token,
    type = "feature",
    displayReason = displayReason,
    returnType = FALSE
  )
}


#' @describeIn esriUrl DEPRECATED Use esriUrl_serviceUrl
#' @export
esriUrl_ServerUrl <- function(url, token = NULL) {
  .Deprecated("esriUrl_serviceUrl")
  esriUrl_serviceUrl(url, token)
}

#' @describeIn esriUrl Retrieve Map/Feature Server URL
#' @export
esriUrl_serviceUrl <- function(url, token = NULL) {
  # Cut off layerID if present
  urlNoLayerID <- sub("/[[:digit:]]+/?$|/$", "", url)

  # make sure url is valid service and error otherwise
  tryCatch(
    {
      esriUrl_isValidService(
        url = urlNoLayerID, token = token,
        displayReason = TRUE
      )
    },
    message = function(m) {
      cli::cli_abort(m$message)
    }
  )

  return(urlNoLayerID)
}

#' @keywords internal
parse_url_scheme <- function(url) {
  regmatches(url, regexpr("^https://|^http://", url))
}

#' @keywords internal
parse_url_host <- function(url, scheme = NULL) {
  scheme <- scheme %||% parse_url_scheme(url)
  unlist(strsplit(sub(scheme, "", url), "/"))[1]
}

#' @keywords internal
parse_url_instance <- function(url, host = NULL, scheme = NULL) {
  host <- host %||% parse_url_host(url, scheme)
  sub("/rest/services.*", "", sub(paste0(".*", host, "/"), "", url))
}

#' @describeIn esriUrl Parse Url into parts.
#' @export
esriUrl_parseUrl <- function(url, token = NULL) {
  # make sure url is valid and error otherwise
  tryCatch(
    {
      esriUrl_isValid(url = url, token = token, displayReason = TRUE)
    },
    message = function(m) {
      cli::cli_abort(m$message)
    }
  )

  scheme <- parse_url_scheme(url)
  host <- parse_url_host(url, scheme)
  instance <- parse_url_instance(url, host, scheme)

  # Find type of URL
  urlType <- esriUrl_isValidType(url,
    token = token, type = NULL,
    displayReason = FALSE, returnType = TRUE
  )

  folderPath <- ""
  serviceName <- ""
  serviceType <- ""
  featureID <- integer(0)

  if (urlType == "folder") {
    folderPath <- sub("/$", "", sub(".*/rest/services/", "", url))
  } else if (urlType %in% c("service", "feature")) {
    folderService <- unlist(strsplit(sub(paste0("/(", paste0(serviceTypes, collapse = "|"), ").*"), "", sub(".*/rest/services/", "", url)), "/"))
    if (length(folderService) > 1) {
      folderPath <- paste0(folderService[-length(folderService)], collapse = "/")
    }
    serviceSplit <- unlist(strsplit(sub(paste0("/(", paste0(serviceTypes, collapse = "|"), ").*"), "", url), "/"))
    serviceName <- serviceSplit[length(serviceSplit)]
    serviceType <- gsub("/", "", regmatches(url, regexpr(paste0("/(", paste0(serviceTypes, collapse = "|"), ")/?"), url)))
    if (urlType == "feature") {
      featureID <- as.integer(gsub("/", "", regmatches(url, regexpr("/[0-9]+/?$", url))))
    }
  }

  list(
    "url" = url,
    "scheme" = scheme,
    "host" = host,
    "instance" = instance,
    "restIndicator" = "rest/services",
    "folderPath" = folderPath,
    "serviceName" = serviceName,
    "serviceType" = serviceType,
    "featureID" = featureID
  )
}

#' Extract identifier from the url
#'
#' @noRd
parse_id <- function(url) {
  str_extract(url, "(?<=id=)[A-Za-z0-9]+")
}

#' Convert ESRI item URL to feature URL
#'
#' @inheritParams rlang::args_error_context
#' @keywords internal
convert_esriUrl <- function(url,
                            token = NULL,
                            from = NULL,
                            to = "feature",
                            call = caller_env()) {
  type <- esriUrl_isValidType(url = url, token = token, returnType = TRUE)
  if (isTRUE(identical(type, to))) {
    return(url)
  }

  from <- from %||% type

  if ((from != "item") && (to == "root")) {
    url <- esriUrl_parseUrl(url, token)
    return(
      paste0(
        url["scheme"],
        paste0(url[c("host", "instance", "restIndicator")], collapse = "/")
      )
    )
  }

  if (from == "service") {
    layerInfo <- esrimeta(url, token)
    layers <- layerInfo[["layers"]]

    if (nrow(layers) == 1) {
      return(paste0(url, "/", layers[["id"]]))
    }

    check_installed("cliExtras")

    id <-
      cliExtras::cli_menu(
        choices = layers[["name"]],
        title = c(
          "i" = "{.val {layerInfo$serviceDescription}} is a service with
          {nrow(layers)} layer{?s}:"
        ),
        message = "{cli::symbol$tick} Enter your selection or press {.kbd 0} to exit.",
        prompt = "? Select a layer to download:",
        ind = TRUE
      )

    return(paste0(url, "/", layers[id, ][["id"]]))
  }

  if (from == "item") {
    url <-
      paste0(
        "https://www.arcgis.com/sharing/rest/content/items/",
        parse_id(url)
      )

    if (to == "item") {
      return(url)
    }

    layerInfo <- esrimeta(url = url, token = token, call = call)

    if (to == "feature") {
      if ("Singlelayer" %in% layerInfo$typeKeywords || has_length(layerInfo$url, 1)) {
        return(paste0(layerInfo$url, "/0"))
      }
    }

    layerInfo$url
  }
}
