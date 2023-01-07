#' Get info from a service url
#'
#' Return server info, service item info, metadata, or thumbnail. The {magick}
#' package is required to return a thumbnail.
#'
#' Additional documentation:
#' <https://developers.arcgis.com/rest/services-reference/enterprise/info.htm>
#'
#' @param info Info service to use. Options include "info", "item", "metadata",
#'   or "thumbnail". If info = "info" (default) basic server information is
#'   displayed and the body of the response is returned invisibly.
#' @param format If `info = "metadata"`, options include "fgdc" or "iso19139";
#'   no format is required if `info` is "item" or "thumbnail".
#' @inheritParams esriRequest
#' @rdname esriInfo
#' @export
#' @importFrom httr2 resp_body_json resp_body_xml resp_body_raw
#' @importFrom dplyr as_tibble
#' @importFrom cli cli_abort
esriInfo <- function(url, info = NULL, format = NULL, token = NULL, ...) {
  info <- match.arg(info, c("item", "metadata", "thumbnail", "info"))

  append <-
    switch(info,
      "info" = "info",
      "item" = "info/iteminfo",
      "metadata" = "info/metadata",
      "thumbnail" = "info/thumbnail"
    )


  if (info == "info") {
    resp <-
      esriRequest(
        url = url,
        f = "json",
        token = token
      )

    body <- httr2::resp_body_json(resp = resp, ...)

    v <- as.character(body[["currentVersion"]])
    v_url <- esri_version_ref[esri_version_ref[["version"]] == v, ][["url"]]
    folders <- body[["folders"]]
    services <- body[["services"]]
    service_names <- cli::ansi_collapse(
      vapply(services, function(x) {
        x[["name"]]
      }, NA_character_),
      trunc = 8
    )
    cli::cli_h1(c("{cli::col_br_blue(cli::symbol$info)} ArcGIS REST API Server Info"))
    cli::cli_rule("{.url {url}}", right = "v. {.href [{v}]({v_url})}")

    cli::cli_bullets(
      c(
        "*" = "📂 {.num {length(folders)}} folders including {cli::ansi_collapse(as.character(folders), trunc = 8)}.",
        "*" = "🗺️ {.num {length(services)}} top level services including {service_names}."
      )
    )

    return(invisible(body))
  }

  if (info == "item") {
    resp <-
      esriRequest(
        url = url,
        append = append,
        f = "json",
        token = token
      )

    resp <- httr2::resp_body_json(resp = resp, check_type = FALSE, ...)

    if (!is.null(resp[["extent"]])) {
      resp[["extent"]] <-
        list(
          extent2bbox(
            resp[["extent"]],
            crs = getLayerCRS(resp[["extent"]][["spatialReference"]])
          )
        )
    }

    resp[["typeKeywords"]] <- list(resp[["typeKeywords"]])
    resp[["tags"]] <- list(resp[["tags"]])

    return(dplyr::as_tibble(resp))
  }

  if (info == "metadata") {
    format <- match.arg(format, c("fgdc", "iso19139"))

    # Specifies metadata style.
    # The default is item description metadata style.
    resp <-
      esriRequest(
        url = url,
        append = append,
        format = format,
        token = token,
        output = "html"
      )

    return(httr2::resp_body_xml(resp = resp, check_type = FALSE, ...))
  }

  if (info == "thumbnail") {
    resp <-
      esriRequest(
        url = url,
        append = append,
        format = format,
        token = token
      )

    resp <- httr2::resp_body_raw(resp = resp)

    if (!requireNamespace("magick", quietly = TRUE)) {
      cli::cli_abort(
        "The {.pkg magick} package must be installed when {.arg info}
        is set to {.val thumbnail}."
      )
    }

    magick::image_read(resp)
  }
}


#' Convert numeric extent to bounding box object
#'
#' @noRd
#' @importFrom sf st_bbox
extent2bbox <- function(x, crs = 4326) {
  if (is.list(x)) {
    x <- vapply(unlist(x), as.numeric, 1)
  }

  if (any(is.na(c(x[["xmin"]], x[["ymin"]], x[["xmax"]], x[["ymax"]])))) {
    return(NULL)
  }

  sf::st_bbox(
    c(
      xmin = x[["xmin"]],
      ymin = x[["ymin"]],
      xmax = x[["xmax"]],
      ymax = x[["ymax"]]
    ),
    crs = crs
  )
}
