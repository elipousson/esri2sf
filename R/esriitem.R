#' Is URL an ESRI content url?
#'
#' @param x A object to test as URL.
#' @name is_esri_content_url
is_esri_content_url <- function(x) {
  if (!is_url(x)) {
    return(FALSE)
  }

  if (!grepl(".html?", x)) {
    return(FALSE)
  }

  TRUE
}

#' @rdname is_esri_content_url
#' @name is_esri_item_url
is_esri_item_url <- function(x) {
  if (!is_esri_content_url(x)) {
    return(FALSE)
  }

  grepl("/home/item\\.html\\?id=", x)
}

#' @rdname is_esri_content_url
#' @name is_esri_item_url
is_esri_app_url <- function(x) {
  if (!is_esri_content_url(x)) {
    return(FALSE)
  }

  grepl("/home/item\\.html\\?id=", x)
}


#' Get ESRI item data or metadata
#'
#' [esriitem()] provides partial support from the ArcGIS Content API.
#'
#' @name esriitem
#' @param url A URL to use for the item.
#' @param type "data", "info", "metadata", "config" (app URLs only)
#' @param destfile Destination file used to download item if data is a PDF file.
#' @inheritParams httr2::resp_body_json
#' @inheritDotParams httr2::resp_body_xml
#' @returns A list, a xml document, or the response object from the request.
#' @export
esriitem <- function(url,
                     type = "data",
                     destfile = tempfile(fileext = "pdf"),
                     simplifyVector = TRUE,
                     ...) {
  if ((type != "config") && !is_esri_content_url(url)) {
    cli_warn(
      "{.arg url} must be an ESRI content URL."
    )
  }

  type <- arg_match(type, c("data", "info", "metadata", "config"))

  append <- switch(type,
    "data" = "/data",
    "info" = "/info/iteminfo.xml",
    "metadata" = "/info/metadata/metadata.xml",
    "config" = "config.json"
  )

  f <- switch(type,
    "data" = "json",
    "metadata" = "default",
    NULL
  )

  if (type != "config") {
    url <- build_esri_content_url(url)
  }

  resp <- esriRequest(
    url = url,
    append = append,
    f = f,
    format = f,
    .perform = TRUE
  )

  if (!httr2::resp_has_body(resp)) {
    cli_abort(
      "Item {type} can't be found for {.arg url}"
    )
  }

  if (type %in% c("metadata", "info")) {
    return(httr2::resp_body_xml(resp, ...))
  }

  content_type <- httr2::resp_content_type(resp)

  if (grepl("json", content_type)) {
    return(httr2::resp_body_json(resp, simplifyVector = simplifyVector))
  }

  if (content_type == "application/pdf") {
    download.file(
      httr2::resp_url(resp),
      destfile = destfile
    )
  }

  resp
}
