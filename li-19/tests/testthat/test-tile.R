test_that("terrain assignment works", {
    expect_equal(Tile$new(LAND)$terrain, LAND)
})

test_that("device storing works", {
    params$number_nodes <<- 200
    sp <- ServiceProvider$new()
    d <- Device$new(1, sp, NULL)
    t <- Tile$new(LAND)
    t$add_device(d)
    expect_equal(t$objects[[d$id]]$id, d$id)
    t$rm_device(d$id)
    expect_false(t$has_devices())
})

test_that("base station storing works", {
    b <- BaseStation$new(1, 1)
    t <- Tile$new(LAND)
    t$add_base_station(b)
    expect_equal(c(1, 1), t$get_base_station()$location)
})

test_that("signal works", {
    b <- BaseStation$new(1, 1)
    t <- Tile$new(LAND)
    t$add_signal(b, F)
    expect_equal(c(1, 1), t$signals[[1]]$location)
})
