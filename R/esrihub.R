#' Get a feed of available data from an ArcGIS Hub site
#'
#' Get a feed in a DCAT US, DCAT AP, RSS, or OGC Definitions format of the
#' public data or other content published on an ArcGIS Hub site.
#'
#' @param url A url for an ArcGIS Hub site
#' @param format Format of data to return, See
#'   <https://doc.arcgis.com/en/hub/content/federate-data-with-external-catalogs.htm>
#'   for more information. Default: `c("dcat-us", "dcat-ap", "rss",
#'   "definition")`.
#' @param simplifyVector Passed to [httr2::resp_body_json()] if format is not
#'   rss. Default: `TRUE`
#' @inheritParams rlang::args_error_context
#' @return A list of metadata for the public content on the ArcGIS Hub site.
#' @rdname esrihub
#' @export
#' @importFrom rlang check_required
#' @importFrom httr2 req_url_path_append request req_perform resp_body_json
#'   resp_body_xml
esrihub <- function(url,
                    format = c("dcat-us", "dcat-ap", "rss", "definition"),
                    simplifyVector = TRUE,
                    call = caller_env()) {
  check_url(url, call = call)

  format <- match.arg(format)

  req <- httr2::req_url_path_append(
    httr2::request(url),
    switch (format,
            "dcat-us" = "/api/feed/dcat-us/1.1",
            "dcat-ap" = "/api/feed/dcat-ap/2.0.1",
            "rss" = "/api/feed/rss/2.0",
            "definition" = "/api/feed/search/definition"
    )
  )

  resp <- httr2::req_perform(req, error_call = call)

  if (format != "rss") {
    httr2::resp_body_json(resp, simplifyVector = simplifyVector)
  } else {
    body <- httr2::resp_body_xml(resp)
    rlang::check_installed("xml2")
    xml2::as_list(body)
  }
}



