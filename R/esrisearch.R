#' Search for items from ArcGIS Online or an ArcGIS Enterprise Server
#'
#' Use a query or bounding box to search for items on ArcGIS Online or on an
#' ArcGIS Enterprise Server. See
#' <https://developers.arcgis.com/rest/users-groups-and-items/search.htm> for
#' more information on the Search endpoint for the Portal Directory API (also
#' known as the Sharing API). A query or bbox argument must be provided.
#'
#' @param query Search terms, Default: `NULL`. Required if bbox is `NULL`.
#' @param bbox A `bbox`, `sf`, or `sfc` to use as a spatial filter for search
#'   results. `sf` or `sfc` objects are converted to bounding boxes using
#'   [sf::st_bbox()].  Default: `NULL`. Required if query is `NULL`.
#' @param url ArcGIS Enterprise request URL (e.g.
#'   "https://machine.domain.com/webadaptor/") or ArcGIS Online organization url
#'   (e.g. "https://org.arcgis.com"). If `NULL` (default), url is set to
#'   "https://www.arcgis.com"
#' @param num Maximum number of results to return, Default: 50 (must be between
#'   1 and 100).
#' @param start Start number for returned results, Default: 1 (returns results
#'   starting from the first result).
#' @param category_filter Terms to use in searching for items with matching
#'   categories. Only 3 or less terms currently supported, Default: `NULL`
#' @param sort Field to use for results sort order. Options include "modified",
#'   "title", "created", "type", "owner", "avgrating", "numratings",
#'   "numcomments", "numviews". Default: `NULL`.
#' @param desc If `TRUE`, return results in descending order. If `FALSE`
#'   (default), return results in ascending order. Ignored if sort is `NULL`.
#' @return A tibble data.frame with results from the item search.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   esrisearch(query = "park")
#'
#'   esrisearch(query = c("ocean", "basemap"))
#'
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'
#'   esrisearch(bbox = nc)
#' }
#' }
#' @rdname esrisearch
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom dplyr as_tibble
esrisearch <- function(query = NULL,
                       bbox = NULL,
                       url = NULL,
                       num = 50,
                       start = 1,
                       category_filter = NULL,
                       sort = NULL,
                       desc = FALSE) {
  if (is.null(query) && is.null(bbox)) {
    cli_abort(
      "{.arg q} or {.arg bbox} must be provided."
    )
  }

  if (!is.null(bbox)) {
    bbox <-
      sf2geometry(bbox, geometryType = "esriGeometryEnvelope", layerCRS = 4326)
  }

  if (is.null(url)) {
    url <- "https://www.arcgis.com"
  } else if (!is_url(url)) {
    cli_abort(
      "{.arg url} must be a valid url and {.val {url}} is not."
    )
  }

  if (num > 100 | num < 1) {
    cli_warn(
      "{.arg num} must be a positive number between 1 and 100,
      not {.val {num}}.",
      " " = "Setting {.arg num} to default value {.val 50}.",
      "i" = "Use the {.arg start} parameter to paginate across total results."
    )

    num <- 50
  }

  if (start < 1) {
    cli_warn(
      "{.arg num} must be a positive number.",
      " " = "Setting {.arg start} to default value {.val 1}."
    )

    start <- 1
  }

  if (!is.null(category_filter)) {
    check_category_filter(category_filter)
    category_filter <- paste0(category_filter, collapse = ",")
  }

  sortOrder <- NULL

  if (!is.null(sort)) {
    sort <-
      match.arg(
        sort,
        c(
          "modified", "title", "created", "type", "owner",
          "avgrating", "numratings", "numcomments", "numviews"
        )
      )

    if (desc) {
      sortOrder <- "desc"
    } else {
      sortOrder <- "asc"
    }
  }

  resp <-
    esriRequest(
      url = url,
      append = "sharing/rest/search",
      f = "json",
      q = gsub("\\s+", "+", paste(query, collapse = " ")),
      bbox = bbox,
      num = num,
      sortField = sort,
      sortOrder = sortOrder,
      categoryFilters = category_filter
    )

  resp <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (resp$total >= 10000) {
    total <- "10000+"
  } else {
    total <- resp$total
  }

  if (!is.null(resp$query)) {
    msg <- "Search completed for {.val {resp$query}} at {.url {url}}."
  } else if (!is.null(bbox)) {
    msg <- "Search completed with provided {.arg bbox} at {.url {url}}."
  }

  cli_inform(
    c(
      "v" = "Search completed for {.val {resp$query}} at {.url {url}}.",
      " " = "Returning {.val {num}} results ({.val {resp$start}} to
      {.val {resp$nextStart - 1}}) out of {total} total."
    )
  )

  results <-
    dplyr::as_tibble(
      resp[["results"]]
    )

  if (all(c("created", "modified") %in% names(results))) {
    results <-
      dplyr::mutate(
        results,
        dplyr::across(
          c("created", "modified"),
          ~ fmt_epoch_date(.x)
        )
      )
  }

  results
}

#' Is this a URL?
#'
#' @noRd
is_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' Check if the provided category_filter is valid
#'
#' @noRd
check_category_filter <- function(category_filter) {
  if (!is.character(category_filter)) {
    cli_abort(
      "{.arg category_filter} must be a character vector.",
      "i" = "{.arg category_filter} is {.cls {class(category_filter)}}."
    )
  }

  if (length(category_filter) > 3) {
    cli_abort(
      "{.arg category_filter} must have length 3 or less.",
      "i" = "{.arg category_filter} is length {.val {length(category_filter)}}."
    )
  }
}

#' Convert epoch dates to POSIXct dates
#'
#' @param x Numeric value corresponding to epoch date
#' @noRd
fmt_epoch_date <- function(x) {
  as.POSIXct(x / 1000, origin = "1970-01-01")
}