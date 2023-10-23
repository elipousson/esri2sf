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
#' @param quiet If `TRUE`, suppress warnings and informational messages.
#'   Defaults to `FALSE`.
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
#' @importFrom cli cli_abort cli_alert_warning cli_bullets
#' @importFrom httr2 resp_body_json
#' @importFrom dplyr mutate across
#' @importFrom tibble as_tibble
esrisearch <- function(query = NULL,
                       bbox = NULL,
                       url = NULL,
                       num = 50,
                       start = 1,
                       category_filter = NULL,
                       sort = NULL,
                       desc = FALSE,
                       quiet = FALSE) {
  cli_quiet(quiet)

  if (is_null(query) && is_null(bbox)) {
    cli::cli_abort(
      "{.arg query} or {.arg bbox} must be supplied."
    )
  }

  if (!is_null(bbox)) {
    bbox <- sf2geometry(
      bbox,
      geometryType = "esriGeometryEnvelope",
      layerCRS = 4326
    )
  }

  url <- url %||% "https://www.arcgis.com"
  check_url(url)
  check_number_whole(start, min = 1)
  check_number_whole(num)

  if (num > 100 || num < 1) {
    cli::cli_warn(
      "{.arg num} must be a positive number between 1 and 100,
      not {.val {num}}.",
      " " = "Setting {.arg num} to default value {.val 50}.",
      "i" = "Use the {.arg start} parameter to paginate across total results."
    )

    num <- 50
  }

  if (!is_null(query)) {
    check_character(query)
    query <- gsub("\\s+", "+", paste(query, collapse = " "))
  }

  if (!is_null(category_filter)) {
    check_category_filter(category_filter)
    category_filter <- paste0(category_filter, collapse = ",")
  }

  sortOrder <- NULL

  if (!is_null(sort)) {
    sort <- match.arg(
      sort,
      c(
        "modified", "title", "created", "type", "owner",
        "avgrating", "numratings", "numcomments", "numviews"
      )
    )

    sortOrder <- "asc"

    if (desc) {
      sortOrder <- "desc"
    }
  }

  resp <- esriRequest(
    url = url,
    append = "sharing/rest/search",
    f = "json",
    q = query,
    bbox = bbox,
    num = num,
    sortField = sort,
    sortOrder = sortOrder,
    categoryFilters = category_filter
  )

  resp <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (is_empty(resp[["results"]])) {
    cli::cli_bullets(
      c("!" = "Search completed with no results found")
    )

    return(tibble::as_tibble(data.frame()))
  }

  total <- resp[["total"]]

  msg <- "Search completed"

  if (!is_null(resp[["query"]])) {
    msg <- "Search completed for {.val {query}} at {.url {url}}"
  } else if (!is_null(bbox)) {
    msg <- "Search completed with supplied {.arg bbox} at {.url {url}}"
  }

  results_msg <- "{total} items found"

  if (total >= num) {
    results_msg <- "Returning {num} results out of {total} total"
  }

  if (total >= 10000) {
    total <- "10000+"
  }

  cli::cli_bullets(c("v" = msg, "i" = results_msg))

  results <-
    tibble::as_tibble(
      resp[["results"]]
    )

  if (all(c("created", "modified") %in% names(results))) {
    results <- dplyr::mutate(
      results,
      dplyr::across(
        c("created", "modified", "lastViewed"),
        ~ fmt_epoch_date(.x)
      )
    )
  }

  results
}

#' Check if the provided category_filter is valid
#'
#' @noRd
check_category_filter <- function(category_filter, call = caller_env()) {
  check_character(category_filter, call = call)

  filter_len <- length(category_filter)
  if (filter_len > 3) {
    cli::cli_abort(
      "{.arg category_filter} must have length 3 or less.",
      "i" = "{.arg category_filter} is length {.val {filter_len}}.",
      call = call
    )
  }
}
