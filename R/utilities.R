# @staticimports pkg:stringstatic
#  str_extract

.onLoad <- function(libname, pkgname) {
  utils::data(
    list = c(
      "esri_version_ref"
    ),
    package = pkgname,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c("name", "serviceType", "type", "urlType")
)

#' Does x inherit a bbox class?
#'
#' @noRd
is_bbox <- function(x) {
  inherits(x, "bbox")
}

#' Does x inherit a sf or (optionally) sfc class?
#'
#' @noRd
is_sf <- function(x, allow_sfc = TRUE) {
  inherits(x, "sf") || (allow_sfc && inherits(x, "sfc"))
}


#' @noRd
cli_abort_ifnot <- function(x = NULL,
                            ...,
                            .fn = NULL,
                            call = caller_env()) {
  cli_ifnot(
    x = x,
    ...,
    .predicate = is_false,
    .fn = .fn,
    .default = cli::cli_abort,
    call = call
  )
}

#' @noRd
cli_abort_if <- function(x = NULL,
                         ...,
                         .fn = NULL,
                         call = caller_env()) {
  cli_if(
    x = x,
    ...,
    .predicate = is_true,
    .fn = .fn,
    .default = cli::cli_abort,
    call = call
  )
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_rbind
list_rbind <- function(x, names_to = zap(), ptype = NULL) {
  vctrs::vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = current_env()
  )
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_cbind
list_cbind <- function(x,
                       name_repair = c("unique", "universal", "check_unique"),
                       size = NULL) {
  vctrs::vec_cbind(
    !!!x,
    .name_repair = name_repair,
    .size = size,
    .error_call = current_env()
  )
}
