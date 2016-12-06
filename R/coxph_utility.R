#' Extract Cox-Snell residuals from a \code{coxph} object
#' 
#' @param object An object of class \code{\link[survival]{coxph}}
#' @return A vector of Cox-Snell residuals. 
#' @export
#'
get_coxsnell <- function(object) {

		## check inputs
		assert_class(object, "coxph")
		## calculate residuals as r(cox-snell)=delta - r(martingale)
		r.cs <- object$y[, "status"] - object$residuals

		return(r.cs)

}


#' Extract data needed to create Cox-Snell vs. Cumulative hazard plot
#'
#' @param object An object of class \code{\link[survival]{coxph}}.
#' @return A data frame containing (not censored) Cox-Snell residuals (\code{coxsnell})
#' and Nelson-Aalen estimate of the Cumulative Hazard (\code{Lambda}) as well as the
#' Breslow estimate for the Survival function (\code{Survivor}) for censored data.
#' @importFrom survival Surv survfit coxph
#' @import dplyr
#' @importFrom checkmate assert_class
#' @importFrom broom tidy
#' @examples
#' library(survival)
#' data("tongue", package="KMsurv")
#' cox.tongue <- coxph(Surv(time, delta)~as.factor(type), data=tongue)
#' cs.data <- get_csdata(cox.tongue)
#' head(cs.data)
#' @export
#'
get_csdata <- function(object) {

	# check inputs
	assert_class(object, "coxph")

	# extract cox snell residuals using relationship r.cs = delta - r.ma
	# whre delta is the censoring indicator from the original data and r.ma
	# are the martingale residuals
	r.cs <- get_coxsnell(object)
	# estimate the cumulative hazard of the censored sample r.cs
	sfit <- survfit(coxph(Surv(r.cs, object$y[, "status"]) ~1, method="breslow"), type="aalen") %>%
		tidy() %>%
		mutate(Lambda = -log(estimate)) %>%
		rename(coxsnell = time, Survivor=estimate) %>%
		select(coxsnell, Lambda, Survivor)


	return(sfit)

}
