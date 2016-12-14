#' Extract Cox-Snell residuals from a \code{coxph} object
#' 
#' @inheritParams survival::cox.zph
#' @return A vector of Cox-Snell residuals. 
#' @importFrom checkmate assert_class
#' @export
#'
get_coxsnell <- function(fit) {

		## check inputs
		assert_class(fit, "coxph")
		## calculate residuals as r(cox-snell)=delta - r(martingale)
		r.cs <- fit$y[, "status"] - fit$residuals

		return(r.cs)

}


#' Extract data needed to create Cox-Snell vs. Cumulative hazard plot
#'
#' @inheritParams survival::cox.zph
#' @return A data frame containing (not censored) Cox-Snell residuals 
#' (\code{coxsnell}) and Nelson-Aalen estimate of the Cumulative Hazard 
#' (\code{Lambda}) as well as the Breslow estimate for the Survival 
#' function (\code{Survivor}) for censored data.
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
get_csdata <- function(fit) {

	# check inputs
	assert_class(fit, "coxph")

	# extract cox snell residuals using relationship r.cs = delta - r.ma
	# whre delta is the censoring indicator from the original data and r.ma
	# are the martingale residuals
	r.cs <- get_coxsnell(fit)
	# estimate the cumulative hazard of the censored sample r.cs
	sfit <- survfit(coxph(Surv(r.cs, fit$y[, "status"]) ~1, method="breslow"), 
			type="aalen") %>%
		tidy() %>%
		mutate(Lambda = -log(estimate)) %>%
		rename(coxsnell = time, Survivor=estimate) %>%
		select(coxsnell, Lambda, Survivor)


	return(sfit)

}



#' Extract scaled Schoenfeld residuals.
#' 
#' Extract scaled Schoenfeld residuals from
#' \code{\link[survival]{coxph}} object in tidy format.
#' 
#' @inheritParams survival::cox.zph
#' @importFrom survival cox.zph
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @return A tidy data frame containing the (transformed) time and scaled 
#' Schoenfeld residuals for the variables used in \code{fit}
#' @export
get_scaledsch <- function(fit, transform="km") {

	# call zph functions that calculates scaled schoenfeld residuals (given 
	# transformation of time)
	zph <- do.call(cox.zph, as.list(environment()))

	## extract scaled schoenfeld residuals, return in tidy format
	as_tibble(zph$y) %>% 
		cbind(
			time      = zph$x,
			transform = zph$transform) %>% 
		gather(variable, residual, -time, -transform)

}
