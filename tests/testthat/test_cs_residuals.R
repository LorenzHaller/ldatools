context("Cox-Snell residuals")
data("tongue", package="KMsurv")
cox.tongue <- coxph(Surv(time, delta)~type, data=tongue)

test_that("Dimensions and names correct", {
	expect_equal(length(get_coxsnell(cox.tongue)), nrow(tongue))
	expect_equal(dim(get_csdata(cox.tongue)), c(57, 3))
	expect_equal(names(get_csdata(cox.tongue)), c("coxsnell", "Lambda", "Survivor"))
})


test_that("Error thrown on misspecification", {
	expect_error(get_coxsnell())
	expect_error(get_coxsnell(iris))
	expect_error(get_csdata(iris))
})


test_that("Cox-Snell plots work", {
	expect_is(gg_coxsnell(cox.tongue, type="cumuhazard"), c("gg", "ggplot"))
	expect_is(gg_coxsnell(cox.tongue, type="cdf"), c("gg", "ggplot"))
})


test_that("Cox-Snell throws error on misspecification", {
	expect_error(gg_coxsnell(iris, type="cumuhazard"))
})
