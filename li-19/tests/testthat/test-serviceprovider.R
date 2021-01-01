test_that("provide service works", {
        sp <- ServiceProvider$new()
        expect_equal(TRUSTED, sp$provide_service())
})
