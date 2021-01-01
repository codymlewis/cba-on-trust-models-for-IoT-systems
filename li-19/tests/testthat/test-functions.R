test_that("Euclidean distance works", {
        expect_equal(sqrt(2), euc_dist(c(0, 0), c(1, 1)))
        expect_equal(2, euc_dist(c(0, 0), c(1, sqrt(3))))
})


test_that("compute gap works", {
        r <- 5
        expect_equal(round(sqrt(2 * r**2) * params$gap_factor), compute_gap(r))
})
