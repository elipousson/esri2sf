#' Create authorization tokens
#'
#' @description Generate tokens for accessing credentialed ArcGIS REST Servers.
#'
#' `generateToken()` can create a token from the public token endpoint
#' `https://<host>:\<port\>/\<site\>/tokens/generateToken` or the admin endpoint
#' `https://\<host\>:\<port\>/\<site\>/admin/generateToken` for ArcGIS REST
#' Servers. See <https://developers.arcgis.com/rest/services-reference/enterprise/generate-token.htm>
#' or <https://developers.arcgis.com/rest/services-reference/enterprise/generate-admin-token.htm>
#' respectively for more information.
#'
#' If `server` is `NULL` and `url` is provided the endpoint is
#' `https://\<url\>/sharing/generateToken`. See
#' <https://developers.arcgis.com/rest/users-groups-and-items/generate-token.htm>
#' for more information.
#'
#' `generateOAuthToken()` can create an OAuth token for ArcGIS Online Services.
#' See `https://developers.arcgis.com/documentation/core-concepts/security-and-authentication/accessing-arcgis-online-services/`
#' or <https://developers.arcgis.com/documentation/mapping-apis-and-services/security/oauth-2.0/>
#' for more information.
#'
#' @param server The ArcGIS REST Server you want to connect to:
#'   `https://\<host\>:\<port\>`
#' @param url The url to connect to using
#'   `https://\<url\>/sharing/generateToken`.
#' @param uid The user id (username) of the account used to create the token
#'   connection to the server
#' @param pwd The password of the account used to create the token connection to
#'   the server. If `NULL`, you will be prompted for the password.
#' @param type Either 'tokens' or 'admin'. Specify the endpoint you use to
#'   create the token. Defaults to 'tokens' if server is provided.
#' @param client "requestip" (default), "referer", or "ip"
#' @param referer,ip Additional parameters required if client is "referer" or
#'   "ip"
#' @param expiration Set an expiration limit on the token in minutes. Max
#'   expiration date may be controlled by the server.
#' @param clientId Client ID
#' @param clientSecret Client Secret
#' @param ... Additional parameters passed to [esriRequest()]
#'
#' @return Character string with the token

#' @describeIn token Create ArcGIS REST Service Token
#' @export
generateToken <- function(server = NULL,
                          url = NULL,
                          uid,
                          pwd = NULL,
                          type = NULL,
                          client = "requestip",
                          expiration = 5000,
                          referer = NULL,
                          ip = NULL,
                          ...) {
  # generate auth token from GIS server
  if (is_null(pwd)) {
    cli::cli_inform(
      "Please provide a password to create the token connection to the server."
    )
    pwd <- readline(prompt = "> ")
  }

  if (!is_null(server)) {
    url <- server
    type <- match.arg(type, c("tokens", "admin"))
    append <- paste("arcgis", type, "generateToken", sep = "/")
    format <- "json"
    f <- NULL
  } else if (!is_null(url)) {
    append <- "sharing/generateToken"
    format <- NULL
    f <- "json"
  } else {
    cli::cli_abort("{.arg server} or {.arg url} must be provided.")
  }

  client <- match.arg(client, c("requestip", "referer", "ip"))

  check_required(uid)

  resp <-
    esriRequest(
      url = url,
      append = append,
      username = uid,
      password = pwd,
      expiration = expiration,
      client = client,
      referer = referer,
      ip = ip,
      f = f,
      format = format,
      ...
    )

  resp <- httr2::resp_body_json(resp)

  cli::cli_inform(
    "Token expires at:
    {.val {as.POSIXct(resp[['expires']]/1000, origin = '1970-01-01')}}"
  )

  resp[["token"]]
}

#' @describeIn token Create ArcGIS OAuth Token
#' @export
generateOAuthToken <- function(clientId,
                               clientSecret,
                               expiration = 5000,
                               ...) {
  resp <-
    esriRequest(
      url = "https://www.arcgis.com/sharing/rest/oauth2/token",
      client_id = clientId,
      client_secret = clientSecret,
      expiration = expiration,
      grant_type = "client_credentials",
      ...
    )

  httr2::resp_body_json(resp)[["access_token"]]
}
