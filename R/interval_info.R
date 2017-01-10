# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-12 16:11:27
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2017-01-10 12:19:13

#' Given breaks, create start/end times and interval information
#'
#' @param brks numeric. A vector of cut point in which the follow-up should be
#' partitioned in.
#' @param min.time numeric. Only intervals that have lower boarders larger than
#' this value will be included in the resulting data frame.
#' @return data.frame. A data frame containing the start and end times of the
#' intervals specified by the \code{brks} argument. Additionally the interval
#' length, interval mid-point and a factor variable of the intervals themselfs.
#' @import checkmate dplyr
#' @export
int_info <- function(
  brks,
  min.time = 0L) {

  # check inputs
  assert_numeric(brks, lower = 0, any.missing = FALSE)
  assert_numeric(min.time, lower  = 0L)

  # sort brks and add origin if necessary
  if(is.unsorted(brks)) {
    brks <- sort(brks)
  }
  if(min(brks!=0)) {
    brks <- c(0, brks)
  }

  intlen <- diff(brks)
  tstart <- brks[-length(brks)]
  tend   <- tstart + intlen

  tdf <- data.frame(
    tstart = tstart,
    tend   = tend,
    intlen = intlen) %>% 
    mutate(
      intmid = tstart + intlen/2, 
      interval = paste0("(", tstart, ",", tend, "]"),
      interval = factor(interval, levels=interval))

  filter(tdf, tstart >= min.time)

}



#' Given breaks, return intervals in which times vector falls
#' 
#' @inheritParams int_info
#' @param x Vector of values for which interval information should be returned.
#' @param ... Further arguments passed to \code{\link[base]{findInterval}}.
#' @import dplyr
#' @return A \code{data.frame} containing information on intervals in which 
#' values of x fall
#' @examples
#' set.seed(111018)
#' brks <- c(0, 4.5, 5, 10, 30)
#' int_info(brks)
#' x <- runif(3, 0, 30)
#' get_intervals(brks, x, left.open=TRUE)
#' @export
#' @seealso findInterval int_info

get_intervals <- function(brks, x, ...) {

  # check inputs
  assert_numeric(brks, lower = 0, any.missing = FALSE)
  assert_numeric(x, finite = TRUE, all.missing = FALSE)

  int.df <- int_info(brks)
  int <- findInterval(x, union(int.df$tstart, int.df$tend), ...)

  int.df %>% 
    slice(int)      %>%
    mutate(x = x)   %>%
    arrange(tstart) %>%
    select(x, everything())

}