#' Create an index of folders, services, layers, and tables for an ArcGIS Server
#'
#' Recurse over a ArcGIS Server url or folder url to return a data frame index
#' of folders, services, layers, and tables. This function returns additional
#' information than [esriCatalog()] using `f = "sitemap"` or `f = "geositemap"`.
#'
#' @rdname esriIndex
#' @param url URL for ArcGIS server, folder, or service.
#' @param folderPath,serviceName Name of parent folder or service; only used
#'   internally (not intended for user).
#' @param recurse If `TRUE`, recursively check folders and services to return an
#'   index that includes services in folders, subfolders, layers, and tables.
#'   Defaults to `FALSE`.
#' @inheritParams esriRequest
#' @param ... Additional parameters passed to [esriCatalog()] (not typically
#'   required)
#' @export
#' @importFrom dplyr bind_rows mutate if_else case_when relocate
esriIndex <- function(url,
                      folderPath = NULL,
                      serviceName = NULL,
                      recurse = FALSE,
                      token = NULL,
                      ...) {
  esriResp <- esriCatalog(url, token = token, ...)

  # check_layerInfo(esriResp)

  index <- NULL
  urlIndex <- url

  urlbase <-
    regmatches(
      urlIndex,
      regexpr(pattern = ".+(?=/)", text = urlIndex, perl = TRUE)
    )


  if (!!length(esriResp[["folders"]])) {
    folders <-
      list_cbind(
        list(
          "name" = unlist(esriResp[["folders"]]),
          "urlType" = "folder",
          "type" = NA
        )
      )

    index <-
      list_rbind(
        list(
          index,
          folders
        )
      )
  }

  if (!!length(esriResp[["services"]])) {
    services <- dplyr::bind_rows(esriResp[["services"]])

    services <-
      list_cbind(
        list(
          services,
          "urlType" = "service"
        )
      )

    index <-
      list_rbind(
        list(
          index,
          services
        )
      )
  }

  if (is.null(index)) {
    return(index)
  }

  index <-
    dplyr::mutate(
      index,
      url = NULL,
      url = dplyr::if_else(
        grepl(pattern = "/", x = name),
        urlbase,
        urlIndex
      )
    )

  na_type <- all(sapply(index[["type"]], is.na))

  index <-
    dplyr::mutate(
      index,
      url = dplyr::case_when(
        (urlType == "folder") ~ paste0(url, "/", name),
        !na_type ~ paste0(url, "/", name, "/", type),
        TRUE ~ url
      )
    )

  index <- list_cbind(
    list(
      index,
      "folderPath" = folderPath,
      "serviceName" = serviceName
    )
  )

  if (recurse) {
    folderIndex <- subset(index, urlType == "folder")

    if (nrow(folderIndex) > 0) {
      folderIndex <-
        list_rbind(
          map2(
            folderIndex[["url"]],
            folderIndex[["name"]],
            ~ esriIndex(
              url = .x,
              folderPath = .y,
              serviceName = serviceName,
              recurse = TRUE
            )
          )
        )

      index <-
        list_rbind(
          list(
            index,
            folderIndex
          )
        )
    }

    layerIndex <-
      subset(
        index,
        type %in% c(
          "MapServer", "FeatureServer", "ImageServer",
          "GeocodeServer", "GeometryServer", "GPServer"
        )
      )

    if (nrow(layerIndex) > 0) {
      layerIndex <-
        list_rbind(
          map2(
            layerIndex[["url"]],
            layerIndex[["name"]],
            ~ esriIndexLayers(
              url = .x,
              folderPath = folderPath,
              serviceName = .y,
              recurse = TRUE
            )
          )
        )

      index <-
        list_rbind(
          list(
            index,
            layerIndex
          )
        )
    }
  }

  index <-
    dplyr::mutate(
      index,
      serviceType = dplyr::case_when(
        grepl("FeatureServer", url) ~ "FeatureServer",
        grepl("MapServer", url) ~ "MapServer",
        grepl("ImageServer", url) ~ "ImageServer",
        grepl("GeocodeServer", url) ~ "GeocodeServer",
        grepl("GeometryServer", url) ~ "GeometryServer",
        grepl("GPServer", url) ~ "GPServer"
      )
    )

  dplyr::relocate(
    index,
    dplyr::any_of(c("urlType", "folderPath", "serviceName", "serviceType")),
    .after = "url"
  )
}

#' @name esriIndexLayers
#' @rdname esriIndex
#' @export
#' @importFrom dplyr bind_rows
esriIndexLayers <- function(url,
                            folderPath = NULL,
                            serviceName = NULL,
                            token = NULL,
                            ...) {
  esriResp <- esriCatalog(url, token = token, ...)

  # check_layerInfo(esriResp)

  index <- NULL

  if (!!length(esriResp[["layers"]])) {
    layers <-
      list_cbind(
        list(
          dplyr::bind_rows(esriResp$layers),
          "urlType" = "layer"
        )
      )

    index <-
      list_rbind(
        list(
          index,
          layers
        )
      )
  }

  if (!!length(esriResp[["tables"]])) {
    tables <-
      list_cbind(
        list(
          dplyr::bind_rows(esriResp$tables),
          "urlType" = "table"
        )
      )

    index <-
      list_rbind(
        list(
          index,
          tables
        )
      )
  }

  if (is.null(index)) {
    return(index)
  }

  index <-
    list_cbind(
      list(
        index,
        "url" = paste0(gsub("/$", "", url), "/", index$id),
        "folderPath" = folderPath,
        "serviceName" = serviceName
      )
    )

  dplyr::distinct(
    index,
    url,
    .keep_all = TRUE
  )
}

#' @rdname esriIndex
#' @name esriindex
esriindex <- esriIndex

#' Get information on folders, services, tables, and layers using the Catalog
#' service
#'
#' The Catalog resource from the ArcGIS REST API represents a catalog of folders
#' and services published on the host. More information:
#' <https://developers.arcgis.com/rest/services-reference/enterprise/catalog.htm>
#'
#' @param f Format to use for request. Supported options include "json",
#'   "sitemap", or "geositemap"; "html" and "kmz" are not currently supported.
#' @param option If `option = "footprints"` and the url is for a folder, spatial
#'   footprints of all map, feature, and image services in that folder are
#'   returned as a feature collection
#' @param outSR Output spatial reference of the geometry returned in footprints;
#'   only supported when `option = "footprints"`.
#' @inheritParams esriRequest
#' @export
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#'   resp_body_xml
#' @importFrom dplyr case_when bind_rows
#' @importFrom cli cli_abort
#' @importFrom rlang check_installed
esriCatalog <- function(url,
                        f = "json",
                        token = NULL,
                        option = NULL,
                        outSR = NULL,
                        ...) {
  f <- match.arg(f, c("json", "html", "kmz", "sitemap", "geositemap"))

  if (f %in% c("html", "kmz")) {
    cli::cli_abort(
      "{.fn esriCatalog} does not yet support {.arg f} = {.val {f}}."
    )
  }

  if (f == "json") {
    stopifnot(
      is.null(option) | (option == "footprints"),
      is.null(outSR) | (!is.null(outSR) && (option == "footprints"))
    )

    resp <-
      dplyr::case_when(
        is.null(option) ~ "catalog",
        is.null(outSR) ~ "option",
        !is.null(outSR) ~ "outSR"
      )

    resp <-
      switch(resp,
        "catalog" = esriRequest(
          url,
          f = f, token = token, ...
        ),
        "option" = esriRequest(
          url,
          f = f, token = token, option = option, ...
        ),
        "outSR" = esriRequest(
          url,
          f = f, token = token, option = option, outSR = outSR, ...
        )
      )

    resp <- httr2::resp_body_json(resp = resp, check_type = FALSE, ...)

    return(resp)
  }

  if (format %in% c("sitemap", "geositemap")) {
    rlang::check_installed(
      "xml2",
      reason = "The {.pkg xml2} package must be installed if {.arg format}
        is {.val sitemap} or {.val geositemap}."
    )

    sitemap <- httr2::resp_body_xml(resp = resp, ...)

    sitemap <- xml2::as_list(sitemap)

    list_rbind(
      list("url" = unlist(sitemap, use.names = FALSE))
    )
  }
}

#' @rdname esriCatalog
#' @name esricatalog
esricatalog <- esriCatalog
