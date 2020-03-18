test_that("creation works", {
    params$map_width <<- 3
    params$map_height <<- 3
    map_data <- data.frame(
        rep(LAND, 3), rep(LAND, 3), rep(WATER, 3)
    )
    f <- Field$new(map_data)
    expect_equal(f$shape(), c(3, 3))
    params$map_width <<- 500
    params$map_height <<- 500
})
