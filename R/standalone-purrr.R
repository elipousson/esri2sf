# ---
# repo: r-lib/rlang
# file: standalone-purrr.R
# last-updated: 2022-06-07
# license: https://unlicense.org
# ---

#' @keywords internal
#' @importFrom rlang as_function global_env
map <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  lapply(.x, .f, ...)
}

#' @keywords internal
#' @importFrom rlang as_function global_env
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

#' @keywords internal
#' @importFrom rlang as_function global_env set_names
map2 <- function(.x, .y, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    rlang::set_names(out, names(.x))
  } else {
    rlang::set_names(out, NULL)
  }
}
