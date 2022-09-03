# stringb.R is excerpted from stringb: https://github.com/hadley/stringb
#
# MIT License
# Copyright (c) 2020 RStudio
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# License: https://github.com/hadley/stringb/blob/master/LICENSE.md

#' Extract patterns from a string
#'
#' @inheritParams str_detect
#' @noRd
str_extract <- function(string, pattern) {
  check_pattern(pattern)

  str_sub(string, str_locate(string, pattern))
}

#' Extract substrings
#'
#' @inheritParams str_detect
#' @param start,end Integer vectors specifying `start` and `end` positions
#'   (inclusive). Negative values count backwards from the right hand side;
#'   -1 refers to the last character.
#'
#'   Instead of separate `start` and `end` parameters you can alternatively
#'   provide a two-column matrix (i.e. from `str_locate()`); the first column
#'   will be used for the starting position and the second for the ending
#'   position.
#' @noRd
str_sub <- function(string, start = 1L, end = -1L) {
  if (is.matrix(start)) {
    end <- start[, 2]
    start <- start[, 1]
  }

  start <- recycle(start, string)
  end <- recycle(end, string)

  n <- nchar(string)
  start <- ifelse(start < 0, start + n + 1, start)
  end <- ifelse(end < 0, end + n + 1, end)

  substr(string, start, end)
}

recycle <- function(x, to, arg = deparse(substitute(x))) {
  if (length(x) == length(to)) {
    return(x)
  }

  if (length(x) != 1) {
    stop("Can't recycle `", arg, "` to length ", length(to), call. = FALSE)
  }

  rep(x, length(to))
}

#' Locate patterns within a string
#'
#' @inheritParams str_detect
#' @noRd
#' @return
#' For `str_locate()`, an integer matrix with one row for each element of
#' `string`, and two columns ("start" and "end"). If the match is of length 0,
#' end will be one character less than start.
str_locate <- function(string, pattern) {
  check_pattern(pattern)

  out <- regexpr(pattern, string,
                 fixed = is_fixed(pattern),
                 perl = is_perl(pattern),
                 ignore.case = ignore_case(pattern)
  )

  location(out)
}

location <- function(x, all = FALSE) {
  start <- as.vector(x)
  if (all && identical(start, -1L)) {
    return(cbind(start = integer(), end = integer()))
  }

  end <- as.vector(x) + attr(x, "match.length") - 1

  no_match <- start == -1L
  start[no_match] <- NA
  end[no_match] <- NA

  cbind(start = start, end = end)
}

is_fixed <- function(x) inherits(x, "stringb_fixed")
is_perl <- function(x) inherits(x, "stringb_perl") || is.null(attr(x, "class"))
ignore_case <- function(x) isTRUE(attr(x, "ignore_case"))

check_pattern <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("`pattern` must be a single string", call. = FALSE)
  }

  if (is.na(x)) {
    stop("`pattern` can't be NA", call. = FALSE)
  }
}
