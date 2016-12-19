#' Extract Cox-Snell residuals from a \code{coxph} object
#'
#' @inheritParams survival::cox.zph
#' @return A vector of Cox-Snell residuals.
#' @importFrom checkmate assert_class
#' @export
#'
get_csvec <- function(fit) {

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
#' cs.data <- get_coxsnell(cox.tongue)
#' head(cs.data)
#' @export
#'
get_coxsnell <- function(fit) {

	# check inputs
	assert_class(fit, "coxph")

	# extract cox snell residuals using relationship r.cs = delta - r.ma
	# whre delta is the censoring indicator from the original data and r.ma
	# are the martingale residuals
	r.cs <- get_csvec(fit)
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


#' Extract partial effects for specified model terms
#'
#' @inheritParams survival::cox.zph
#' @param data Any data frame containing variables used to fit the model. Only
#' first row will be used.
#' @param term The (non-linear) model term of interest.
#' @param ... Currently ignored.
#' @import magrittr dplyr
#' @importFrom stats predict setNames
#'
get_term <- function(fit, data, term, ...) {

	col.term <- grep(term, colnames(data), value=TRUE)
	if(length(col.term) > 1) {
		stop(paste0("More than one column in data that contain term: ", term, "."))
	}

	range.term <- range(data[[col.term]])
	seq.term <- seq(range.term[1], range.term[2], length.out = 100)

	newdf <- data[1, ]
	rm(data)
	gc()

	newdf <- newdf[rep(1, length(seq.term)), ]
	newdf[[col.term]] <- seq.term
	pred.term <- predict(fit, newdata=newdf, type="terms", se.fit=TRUE)
	ind.term <- grep(term, colnames(pred.term$fit), value=TRUE)
	newdf %<>% mutate(
		term = col.term,
		eff = as.numeric(pred.term$fit[, ind.term]),
		se = as.numeric(pred.term$se.fit[, ind.term]),
		ci.lower = eff - 2*se,
		ci.upper = eff + 2*se) %>%
	select_(.dots=c("term", col.term, "eff", "se", "ci.lower", "ci.upper")) %>%
	rename_(.dots=setNames(col.term, "x"))

	return(newdf)

}

#' Extract the partial effects of non-linear model terms
#'
#' @inheritParams get_term
#' @param terms A character vector (can be length one). Specifies the terms
#' for which partial effects will be returned
#' @import checkmate survival
#' @return A data frame with 5 columns.
#' @seealso \code{\link[survival]{coxph}}
#' @examples
#' fit <- coxph(Surv(time, status) ~ pspline(karno, df=4), data=veteran)
#' term.karno <- get_terms(fit, veteran, terms="karno")
#' @export
get_terms <- function(fit, data, terms, ...) {

  # check inputs
  assert_class(fit, "coxph")
  assert_class(data, "data.frame")
  assert_character(terms, min.len=1, unique=TRUE)

  # apply get_term to each element of terms
	term.list <- lapply(terms, function(z) {
		get_term(fit=fit, data=data, term=z, ...)
	})

	bind_rows(term.list)

}
