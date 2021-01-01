is_equal <- function(...) {
        return(isTRUE(all.equal(...)))
}

params$map_width <<- 3
params$map_height <<- 3
params$number_nodes <<- 3
map_data <- data.frame(
        rep(LAND, 3), rep(LAND, 3), rep(WATER, 3)
)
f <- Field$new(map_data)
sp <- ServiceProvider$new()
d <- Device$new(1, sp, f, c(1, 1))

test_that("device creation works", {
        expect_equal(d$id, 1)
        expect_equal(d$time_last_moved, params$time_now - 1)
        expect_equal(d$trust, rep(0, params$number_nodes))
        expect_equal(d$distrust, rep(0, params$number_nodes))
        expect_equal(d$unknown, rep(0, params$number_nodes))
})


test_that("device copying works", {
        cp <- Device$new(1, sp, copy = d)
        expect_equal(cp$id, d$id)
        expect_equal(cp$time_last_moved, d$time_last_moved)
        expect_equal(cp$trust, d$trust)
        expect_equal(cp$distrust, d$distrust)
        expect_equal(cp$unknown, d$unknown)
})


test_that("trust update works", {
        d$sp_trust_increment()
        expect_equal(d$sp_trust[[1]], 1)
})


test_that("distrust update works", {
        d$sp_distrust_increment()
        expect_equal(d$sp_distrust[[1]], 1)
})


test_that("unknown update works", {
        d$sp_unknown_increment()
        expect_equal(d$sp_unknown[[1]], 1)
})


test_that("recieving observations works", {
        context <- c(1, 1, 1, 1)
        obs <- Observation$new(context, 1, 1)
        d$recieve_observation(obs)
        expect_equal(tail(d$contexts[[1]], 4), obs$context)
})


test_that("has signal works", {
        expect_equal(d$has_signal(), T)
})


test_that("goal setting works", {
        expect_true(all(d$current_goal >= c(1, 1)))
        expect_true(all(d$current_goal <= f$shape()))
        d$new_goal()
        expect_true(all(d$current_goal >= c(1, 1)))
        expect_true(all(d$current_goal <= f$shape()))
})


test_that("moving works", {
        init_goal <- c(3, 2)
        init_velocity <- 1
        init_basestation_id <- d$get_signals()[[1]]$location
        init_tile_node_count <- length(f$get_tile(c(1, 1))[[1]]$objects)
        init_next_tile_node_count <- length(f$get_tile(c(2, 2))[[1]]$objects)
        d$current_goal <- init_goal
        d$velocity <- init_velocity
        d$move()
        expect_equal(d$location, c(2, 2))
        expect_equal(d$time_last_moved, params$time_now)
        expect_equal(init_basestation_id, d$get_signals()[[1]]$location)
        expect_false(is_equal(d$velocity, init_velocity))
        params$increment_time()
        d$velocity <- init_velocity
        d$move()
        expect_equal(d$location, c(3, 2))
        expect_equal(d$time_last_moved, params$time_now)
        expect_equal(init_basestation_id, d$get_signals()[[1]]$location)
        expect_false(is_equal(d$current_goal, init_goal))
        expect_false(is_equal(d$velocity, init_velocity))
})

test_that("adding contacts works", {
        d2 <- Device$new(2, sp, f, c(2, 2))
        devs <- c(d, d2)
        d$add_contact(2, devs)
        expect_equal(d$contacts[[1]], 2)
        expect_equal(d2$contacts[[1]], 1)
})
