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
})
