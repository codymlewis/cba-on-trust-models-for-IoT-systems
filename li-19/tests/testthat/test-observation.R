test_that("fields works", {
    context <- c(5, 70, 30, 5)
    trust <- 1
    id_sender <- 1
    obs <- Observation$new(context, trust, id_sender)
    expect_equal(context, obs$context)
    expect_equal(trust, obs$trust)
    expect_equal(id_sender, obs$id_sender)
})
