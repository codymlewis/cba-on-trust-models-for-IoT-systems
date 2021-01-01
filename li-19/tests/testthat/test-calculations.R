test_that("probability works", {
        a <- 5
        b <- 3
        c <- 2
        expect_that(compute_probability(a, b, c), equals(6 / 13))
})

test_that("entropy unit works", {
        probability <- 1:5
        expect_that(
                ent_unit(probability),
                equals(
                        c(0, 1.2618595071429148, 3, 5.047438028571659, 7.3248676035896345)
                )
        )
})

test_that("entropy works", {
        probability <- 1:5
        expect_that(compute_entropy(probability), equals(-16.63416513930421))
})

test_that("entropy unit divided works", {
        probability <- 1:5
        expect_that(
                ent_unit_div(probability, 2),
                equals(
                        c(
                                -0.6309297535714574,
                                0,
                                1.1072107392856276,
                                2.5237190142858297,
                                4.170218835732348
                        )
                )
        )
})

test_that("compute trust works", {
        a <- 5
        b <- 3
        c <- 2
        expect_that(compute_trust(a, b, c), equals(0.03203451826247661))
        expect_that(compute_trust(b, a, c), equals(-0.03203451826247661))
        expect_that(compute_trust(b, c, a), equals(-0.0013648071755567592))
        expect_that(compute_trust(a, c, b), equals(0.03705298))
        expect_that(compute_trust(c, a, b), equals(-0.03705298))
})

test_that("weighted average context works", {
        expect_that(
                weighted_avg_context(c(0.2, 0.3, 0.1, 0.5, 0.6)),
                equals(0.34769794209818133)
        )
})

test_that("context distance works", {
        expect_that(
                context_distance(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1)
                ),
                equals(0.3563705936241092)
        )
})

test_that("estimate trust works", {
        expect_that(
                estimate_trust(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        0.1
                ),
                equals(0.09330239773694914)
        )
        expect_that(
                estimate_trust(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        -0.1
                ),
                equals(-0.1167438402791883)
        )
})

test_that("delta works", {
        expect_that(
                delta(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        T
                ),
                equals(c(0.029999999999999992, 0.04000000000000001, 0.2, 0.04))
        )
})

test_that("weighted trust works", {
        a <- 5
        b <- 3
        c <- 2
        expect_that(
                weighted_trust(0.1, a, b, c),
                equals(0.03)
        )
        expect_that(
                weighted_trust(0.1, b, c, a),
                equals(0.03)
        )
        expect_that(
                weighted_trust(0.1, c, a, a),
                equals(0.08)
        )
})

test_that("direct trust works", {
        expect_that(
                direct_trust(
                        0.1,
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1)
                ),
                equals(0.09774097906001301)
        )
})

test_that("indirect trust works", {
        expect_that(
                indirect_trust(
                        0.1,
                        0.1,
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        c(0.3, 0.5, 0.6, 0.1)
                ),
                equals(0.009774097906001301)
        )
})

test_that("omega works", {
        expect_that(
                omega(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1)
                ),
                equals(0.9774097906001301)
        )
})

test_that("trend of trust works", {
        expect_that(
                trend_of_trust(
                        0.1,
                        0.1,
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1)
                ),
                equals(0.0022590209399869915)
        )
})

test_that("reputation combination works", {
        expect_that(
                reputation_combination(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        c(0.3, 0.5, 0.6, 0.1),
                        0.1,
                        0.1
                ),
                equals(0.1775880558)
        )
        expect_that(
                reputation_combination(
                        c(0.2, 0.3, 0.1, 0.5),
                        c(0.3, 0.5, 0.6, 0.1),
                        c(0.3, 0.5, 0.6, 0.1),
                        0.1,
                        -0.1
                ),
                equals(0.08521704)
        )
})
