#' Get ESRI user information
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
