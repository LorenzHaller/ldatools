context("Utility functions")

test_that("Interval infos correct", {
	expect_equal(nrow(int_info(1:2)), 2)
	expect_equal(ncol(int_info(1:2)), 5)
	expect_equal(names(int_info(1:2)), c("tstart", "tend", "intlen", "intmid", "interval"))
	expect_equal(levels(int_info(1:2)$interval), c("(0,1]", "(1,2]"))
})
