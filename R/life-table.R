#' Implementation of the actuarial life table method.
#'
#' @inherit split_info
#' @import dplyr
#' @examples
#' data("Melanoma", package="MASS")
#' Melanoma$status <- ifelse(Melanoma$status == 1, 1, 0)
#' lifetable(Melanoma, Surv(time,status)~1, cut = seq(0, 6000, 1500))
#' @return A \code{\link[tibble]{tibble}} containing riskset information as well
#' as hazard and survival function calculated by using the actuarial life table
#' method.
#' @seealso split_info
#' @export
lifetable <- function(data, formula, cut = NULL) {

  split_df <- split_info(formula, data = data, cut = cut, id = "id") %>%
    select(id:tend, interval, status) %>%
    group_by(id) %>%
    mutate(censored = 1 * (status == 0 & max(tend) == tend)) %>%
    ungroup() %>%
    mutate(interval = as.character(interval))
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
