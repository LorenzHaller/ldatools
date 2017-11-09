#' Implementation of the actuarial life table method.
#'
#' @inherit split_info
#' @examples
#' data("Melanoma", package="MASS")
#' Melanoma$status <- ifelse(Melanoma$status == 1, 1, 0)
#' lifetable(Melanoma, breaks = seq(0, 6000, 1500))
#' @return A \code{\link[tibble]{tibble}} containing riskset information as well
#' as hazard and survival function calculated by using the actuarial life table
#' method.
#' @seealso split_info
#' @export
lifetable <- function(
  data,
  breaks       = NULL,
  time_var     = "time",
  status_var   = "status",
  right_closed = FALSE,
  max_end      = FALSE) {

  time_var   <- enquo(time_var)
  status_var <- enquo(status_var)

  split_df <- split_info(data = data, breaks = breaks,
    !!time_var, !!status_var, right_closed = right_closed,
    max_end = max_end) %>%
    select(id:tend, interval, status) %>%
    group_by(id) %>%
    mutate(censored = 1 * (status == 0 & max(tend) == tend)) %>%
    ungroup()

  split_df %>%
    group_by(tstart, tend, interval) %>%
    summarize(
      n        = n(),
      events   = sum(status),
      dropouts = sum(censored)) %>%
    ungroup() %>%
    mutate(
      riskset  = n - dropouts / 2,
      hazard   = events / riskset,
      survival = cumprod(1 - hazard)) %>%
    arrange(tstart)

}
