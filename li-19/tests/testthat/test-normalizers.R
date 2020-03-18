test_that("time normalizer works", {
    time <- 5
    expect_that(
        time,
        equals(normalize_time(time))
    )
})


test_that("capability normalizer works", {
    cap <- 70
    expect_that(
        1 - (cap / params$max_capability),
        equals(normalize_capability(cap))
    )
})


test_that("location normalizer works", {
    loc <- 30
    expect_that(
        1 - (loc / sqrt(params$map_width**2 + params$map_height**2)),
        equals(normalize_location(loc))
    )
})


test_that("velocity normalizer works", {
    vel <- 5
    expect_that(
        1 - (vel / params$max_velocity),
        equals(normalize_velocity(vel))
    )
})


test_that("context normalizer works", {
    time <- 5
    cap <- 70
    loc <- 30
    vel <- 5
    expect_that(
        c(
            time,
            1 - (cap / params$max_capability),
            1 - (loc / sqrt(params$map_width**2 + params$map_height**2)),
            1 - (vel / params$max_velocity)
        ),
        equals(normalize(c(time, cap, loc, vel)))
    )
})
