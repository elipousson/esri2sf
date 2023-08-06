#' Helpers for converting epoch dates to `POSIXct` dates
#'
#' [fmt_epoch_date()] converts a single numeric epoch data value to a `POSIXct`
#' class object. [fmt_epoch_dates()] converts a numeric vector to `POSIXct`
#' dates.
#'
#' @param x Numeric value corresponding to epoch date.
#' @inheritParams base::as.POSIXct
#' @keywords helper
#' @export
fmt_epoch_date <- function(x, tz = "") {
  try_fetch(
    as.POSIXct(
      as.numeric(x) / 1000,
      origin = "1970-01-01",
      tz = tz
    ),
    error = function(cnd) {
      NA_complex_
    }
  )
}

#' @rdname fmt_epoch_date
#' @export
fmt_epoch_dates <- function(x, tz = "") {
  vapply(x, fmt_epoch_date, NA_complex_, tz = tz)
}

#' Helpers for creating ANSI SQL queries
#'
#' @examples
#'
#' values <- c("a", "b", "c")
#'
#' glue_ansi_sql("letter", " IN ({values*})")
#'
#' @param ... Additional parameters passed to [glue::glue_sql()]
#' @inheritParams glue::glue_sql
#' @export
#' @importFrom DBI ANSI
#' @importFrom glue glue_sql
glue_ansi_sql <- function(...,
                          .con = DBI::ANSI(),
                          .envir = parent.frame()) {
  glue_sql(..., .con = .con, .envir = .envir)
}


#' @rdname glue_ansi_sql
#' @name glue_sql_bbox
#' @param bbox A `bbox`, `sfc`, or `sf` object to use as bounding box.
#' @param coords Column names with longitude and latitude values.
#' @param crs Coordinate reference system used for coordinate values.
#' @keywords helper
#' @export
#' @importFrom glue glue_sql_collapse
glue_sql_bbox <- function(bbox,
                          coords = c("longitude", "latitude"),
                          crs = 4326) {
  bbox <- bbox_transform(bbox, crs = crs)

  glue_sql_collapse(
    c(
      sprintf("(%s >= %s)", coords[1], bbox[["xmin"]]),
      sprintf("(%s <= %s)", coords[1], bbox[["xmax"]]),
      sprintf("(%s >= %s)", coords[2], bbox[["ymin"]]),
      sprintf("(%s <= %s)", coords[2], bbox[["ymax"]])
    ),
    sep = " AND "
  )
}
