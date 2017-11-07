#' Create start/end times and interval information
#'
#' Given interval breaks points, returns data frame with information on
#' interval start time, interval end time, interval length and a interval factor
#' variable (left open intervals). If object of class ped is provided, extracts
#' unique interval information from object.
#'
#' @param x A numeric vector of cut points in which the follow-up should be
#' partitioned in or object of class \code{ped}.
#' @param ... Currently ignored.
#' @rdname int_info
#' @return A data frame containing the start and end times of the
#' intervals specified by the \code{x} argument. Additionally the interval
#' length, interval mid-point and a factor variable of the intervals themselves.
#' @export
int_info <- function(x, ...) {
  UseMethod("int_info",  x)
}


#' @inheritParams int_info
#' @param min.time Only intervals that have lower borders larger than
#' this value will be included in the resulting data frame.
#' @import checkmate dplyr
#' @examples
#' ## create interval information from cut points
#' int_info(c(1, 2.3, 5))
#'
#' @rdname int_info
#' @export
int_info.default <- function(
  x,
  min.time = 0L, ...) {

  # check inputs
  assert_numeric(x, lower = 0, any.missing = FALSE)
  assert_numeric(min.time, lower  = 0L)

  # sort x and add origin if necessary
  if (is.unsorted(x)) {
    x <- sort(x)
  }
  if (min(x != 0)) {
    x <- c(0, x)
  }

  intlen <- diff(x)
  tstart <- x[-length(x)]
  tend   <- tstart + intlen

  tdf <- data.frame(
    tstart = tstart,
    tend   = tend,
    intlen = intlen) %>%
    mutate(
      intmid = tstart + intlen / 2,
      interval = paste0("(", tstart, ",", tend, "]"),
      interval = factor(interval, levels = interval))

  filter(tdf, tstart >= min.time)

}


#' Information on intervals in which times fall
#'
#' @inheritParams int_info
#' @param x An object from which interval information can be obtained,
#' see \code{\link{int_info}}.
#' @param times A vector of times for which corresponding interval information
#' should be returned.
#' @param ... Further arguments passed to \code{\link[base]{findInterval}}.
#' @import dplyr
#' @return A \code{data.frame} containing information on intervals in which
#' values of \code{times} fall.
#' @examples
#' set.seed(111018)
#' brks <- c(0, 4.5, 5, 10, 30)
#' int_info(brks)
#' x <- runif(3, 0, 30)
#' x
#' get_intervals(brks, x)
#'
#' @seealso \code{\link[base]{findInterval}} \code{\link{int_info}}
#' @rdname get_intervals
#' @export
get_intervals <- function(x, times, ...) {
  UseMethod("get_intervals", x)
}

#' @inherit get_intervals
#' @inheritParams base::findInterval
#' @seealso \code{\link[base]{findInterval}}
#' @rdname get_intervals
#' @export
get_intervals.default <- function(
  x,
  times,
  left.open        = TRUE,
  rightmost.closed = TRUE,
  ...) {

  # check inputs
  assert_numeric(times, lower = 0, finite = TRUE, all.missing = FALSE)

  int_df <- int_info(x)
  int    <- findInterval(
    x                = times,
    vec              = union(int_df$tstart, int_df$tend),
    left.open        = left.open,
    rightmost.closed = rightmost.closed)

  int_df %>%
    slice(int) %>%
    mutate(times = times) %>%
    arrange(times) %>%
    select(times, everything())

}


#' Create index of breaks survived
#'
#' @inheritParams split_info2
#' @param time The time of the event.
#' @keywords internal
survived_breaks <- function(
  time,
  breaks,
  right   = TRUE) {

  if(!right) {
    n_survived <- sum(time > breaks)
  } else {
    n_survived <- sum(time >= breaks)
  }

  seq_len(n_survived)

}

#' Create character vector of interval labels
#'
#' @inheritParams split_info2
#' @param tstart Vector of interval start times.
#' @param tend Vector of interval end times.
#' @keywords internal
paste_intervals <- function(tstart, tend, right = TRUE) {

  if(right) {
    left_bracket <- "("
    right_bracket <- "]"
  } else {
    left_bracket <- "["
    right_bracket <- ")"
  }
  paste0(left_bracket, tstart, ",", tend, right_bracket)

}


#' Transform standard time-to-event data to interval data
#'
#' Given a data set in standard format with one row per observation unit,
#' transform the data set into interval data, where intervals are specified
#' via breaks. Each observation unit will have as many rows as interval
#' start points that it survived.
#'
#' @param data Data set from which time and status variables will be extracted.
#' @param time_var The name of the variable storing event times.
#' @param status_var The name of the variable storing the event indicator.
#' @param breaks The time points of the interval borders.
#' @param right Logical. If \code{TRUE} (default), intervals are assumed right
#' closed and left open. If \code{FALSE} left closed and right open.
#' @param max_end logical. Should the last interval span until the last
#' observed censoring or event time (if larger than the largest specified
#' cut point).
#' @import checkmate dplyr purrr tibble
split_info2 <- function(
  data,
  time_var   = "time",
  status_var = "status",
  breaks     = NULL,
  right      = TRUE,
  max_end    = FALSE) {

  assert_data_frame(data, min.rows = 2, min.cols = 2)
  assert_numeric(breaks, lower = 0, finite = TRUE, any.missing = FALSE,
    min.len = 1, null.ok = TRUE)
  assert_flag(right)
  assert_flag(max_end)
  assert_subset(c(time_var, status_var), names(data))


  if (is.null(breaks)) {
    breaks <- unique(data[[time_var]][data[[status_var]] == 1])
  }
  max_time <- max(max(data[[time_var]]), max(breaks))
  # sort interval break points in case they are not (so that interval factor
  # variables will be in correct ordering)
  breaks <- sort(breaks)

  # add last observation to breaks if necessary
  if (max_end & (max_time > max(breaks))) {
    breaks <- c(breaks, max_time)
  }

  n_survived <- map(data[[time_var]],
    ~survived_breaks(., breaks = breaks, right = right))
  last                 <- map_dbl(n_survived, max)
  n_survived           <- flatten_dbl(n_survived)
  status               <- rep(0L, length(n_survived))
  status[cumsum(last)] <- data[[status_var]]

  split_df <- tibble(
    id     = rep(seq_len(nrow(data)), times = last),
    tstart = breaks[n_survived],
    tend   = breaks[n_survived + 1L],
    status = status) %>%
  mutate(
    interval = paste_intervals(tstart, tend, right = right),
    intlen   = tend - tstart)

  attr(split_df, "breaks") <- breaks

  split_df

}
