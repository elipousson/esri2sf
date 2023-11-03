#' Get ArcGIS Online user information
#'
#' @param url An ArcGIS Online community URL with a user ID. Optional if user_id is supplied.
#' @param user_id An ArcGIS Online user ID. Optional if url is supplied.
#' @name esriuser
#' @export
esriuser <- function(url = NULL,
                     user_id = NULL) {
  resp <- esriRequest(
    url = build_esri_community_url(url, user_id = user_id),
    f = "json",
    .perform = TRUE
  )

  httr2::resp_body_json(resp)
}
