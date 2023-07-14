#' Use httr2 to create a request for the ArcGIS REST API
#'
#' This function is primarily useful for the development of new functions to
#' access the ArcGIS REST API. Most users do not need to use this function
#' directly.
#'
#' @param url A folder, service, or layer URL that can be used with the ArcGIS
#'   REST API.
#' @param append String to append to url using [httr2::req_url_path_append];
#'   defaults to `NULL`.
#' @param f,format Return format to use as query parameter with
#'   [httr2::req_url_query]; defaults to "json".
#' @param objectIds Parameter used for layer query requests. The addition of
#'   objectIds to the query often leads the url length to exceed the 2048
#'   character maximum. In those cases, the query is added to the body of the
#'   request with [httr2::req_body_form]
#' @param token String for authentication token; defaults to `NULL`.
#' @param .perform If `TRUE`, perform the request with [httr2::req_perform] and
#'   return the response. If `FALSE`, return the request.
#' @param .cache If `TRUE`, pass a cache folder path created with
#'   [rappdirs::user_cache_dir] and `esri2sf` package to the path parameter of
#'   [httr2::req_cache].
#' @param .max_seconds Passed to max_seconds parameter of [httr2::req_retry]
#' @param .is_error If `FALSE`, .is_error is passed to the is_error parameter of
#'   [httr2::req_error] function. If `TRUE`, the request does not use
#'   [httr2::req_error].
#' @param .body_form If objectIds is `NULL` and .body_form is `TRUE`, generate
#'   the request using [httr2::req_body_form()]. Defaults to `FALSE`.
#' @param ... Additional parameters passed to [httr2::req_url_query]
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom httr2 request req_url_path_append req_url_query req_body_form
#'   req_user_agent req_retry req_cache req_error resp_body_json req_perform
#' @importFrom rappdirs user_cache_dir
esriRequest <- function(url,
                        append = NULL,
                        f = NULL,
                        format = NULL,
                        objectIds = NULL,
                        token = NULL,
                        .perform = TRUE,
                        .cache = FALSE,
                        .max_seconds = 3,
                        .is_error = TRUE,
                        .body_form = FALSE,
                        ...,
                        call = caller_env()) {
  check_url(url, call = call)

  # Create request based on url
  req <- httr2::request(url)

  # Append method or other url elements
  if (!is_null(append)) {
    req <- httr2::req_url_path_append(req, append)
  }

  req <-
    httr2::req_url_query(
      req,
      # Add f, format, and any additional query parameters
      f = f,
      format = format,
      # Set token to required default
      token = token %||% "",
      ...
    )

  if (!is_null(objectIds)) {
    # Add objectIds
    req <-
      req_object_ids(
        req,
        objectIds = objectIds,
        f = f,
        format = format,
        token = token,
        ...
      )
  } else if (.body_form) {
    req <-
      httr2::req_body_form(
        req,
        f = f,
        format = format,
        token = token,
        ...
      )
  }

  req <-
    httr2::req_user_agent(
      req,
      string = "esri2sf (https://github.com/elipousson/esri2sf)"
    )

  req <-
    httr2::req_retry(
      req = req,
      max_seconds = .max_seconds
    )

  # Check if rappdirs::user_cache_dir can be used
  if (.cache) {
    req <-
      httr2::req_cache(
        req,
        path = rappdirs::user_cache_dir("esri2sf")
      )
  }

  # Pass .is_error = FALSE to use httr2::req_error
  if (!.is_error) {
    req <-
      httr2::req_error(
        req,
        is_error = function(resp) {
          .is_error
        },
        body = function(resp) {
          httr2::resp_body_json(resp)$error
        }
      )
  }

  # perform the request if .perform is TRUE
  if (.perform) {
    return(httr2::req_perform(req = req, error_call = call))
  }

  # Otherwise return request
  req
}

#' Helper function to check if url with objectIds exceeds max length and use
#' form request if needed
#'
#' @noRd
#' @importFrom httr2 req_url_query req_body_form
req_object_ids <- function(req,
                           f = NULL,
                           format = NULL,
                           objectIds = NULL,
                           token = NULL,
                           ...) {
  if (!is_null(objectIds)) {
    objectIds <- I(paste(objectIds, collapse = ","))
  }

  req_ids <-
    httr2::req_url_query(
      req,
      objectIds = objectIds
    )

  if (nchar(req_ids$url) <= 2048) {
    return(req_ids)
  }

  # If url is more than 2048 characters long, add the query to the
  # body of the request
  httr2::req_body_form(
    req,
    f = f,
    format = format,
    token = token,
    objectIds = objectIds,
    ...
  )
}
