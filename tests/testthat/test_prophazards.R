context("Test functions for assessing PH assumption") 

data("veteran", package="survival")

fit.vet <- coxph(Surv(time, status) ~ trt + celltype + karno+ 
	diagtime + age + prior, data=veteran)

zph.vet <- cox.zph(fit.vet, transform="identity", global=FALSE)

scaledsch <- get_scaledsch(fit.vet, transform="identity")


test_that("Scaled Schoenfeld residuals obtained correctly", {
	expect_equal(nrow(scaledsch), nrow(zph.vet$y)*ncol(zph.vet$y))
	expect_equal(dim(scaledsch), c(1024, 4))
	expect_equal(names(scaledsch), c("time", "transform", "variable", "residual"))

})