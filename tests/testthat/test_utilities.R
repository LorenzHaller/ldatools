context("Utility functions")
fit <- coxph(Surv(time, status)~pspline(karno, df=4), data=veteran)
term.karno <- get_terms(fit, veteran, terms="karno")


test_that("Interval infos correct", {
	expect_equal(nrow(int_info(1:2)), 2)
	expect_equal(ncol(int_info(1:2)), 5)
	expect_equal(names(int_info(1:2)), c("tstart", "tend", "intlen", "intmid", "interval"))
	expect_equal(levels(int_info(1:2)$interval), c("(0,1]", "(1,2]"))
})


test_that("Termplot extraction works correctly", {
  expect_equal(nrow(term.karno), 100)
  expect_equal(ncol(term.karno), 6)
  expect_equal(names(term.karno), c("term", "x", "eff", "se", "ci.lower", "ci.upper"))
})


test_that("Throws error on wrong input", {
  expect_error(get_terms(fit, veteran, terms = c("a")))
})
