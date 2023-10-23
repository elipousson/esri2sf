#' Is URL an ESRI content url?
#'
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
#' [esricontent()] provides partial support from the ArcGIS Content API.
#'
#' @param type "data", "info", or "metadata"
#' @param destfile Destination file used to download item if data is a PDF file.
#' @inheritParams httr2::resp_body_json
#' @inheritDotParams httr2::resp_body_xml
#' @returns A list, a xml document, or the response object from the request.
#' @name esricontent
esricontent <- function(url,
                        type = "data",
                        destfile = tempfile(fileext = "pdf"),
                        simplifyVector = TRUE,
                        ...) {
  if (!is_esri_content_url(url)) {
    cli_warn(
      "{.arg url} must be an ESRI content URL."
    )
  }

  type <- arg_match(type, c("data", "info", "metadata"))

  append <- switch(type,
    "data" = "/data",
    "info" = "/info/iteminfo.xml",
    "metadata" = "/info/metadata/metadata.xml"
  )

  f <- switch (type,
    "data" = "json",
    "metadata" = "default",
    NULL
  )

  resp <- esriRequest(
    url = build_esri_content_url(url),
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

#' Get
#'
#' @param user_id ESRI user ID
#' @name esriuser
esriuser <- function(url = NULL,
                     user_id = NULL) {
  resp <- esriRequest(
    url = build_esri_community_url(url, user_id = user_id),
    f = "json",
    .perform = TRUE
  )

  httr2::resp_body_json(resp)
}
