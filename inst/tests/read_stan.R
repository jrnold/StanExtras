context("McmcLongStan")

csvfiles <- dir(system.file("line/", package="mcmc4"), pattern="chain_[0-9]+\\.csv",
                full.name=TRUE)

test_that("read_stan_csv function works", {
    line1 <- StanExtras:::read_stan_csv(csvfiles[1])
    expect_is(line1, "McmcLongStan")
    expect_equal(nrow(line1@samples), 1200)
})

test_that("c,McmcLongStan-method works", {
    line1 <- StanExtras:::read_stan_csv(csvfiles[1])
    line2 <- StanExtras:::read_stan_csv(csvfiles[2])
    lineall <- c(line1, line2)
    expect_is(lineall, "McmcLongStan")
    expect_equal(nrow(lineall@samples), 2400)
})

test_that("McmcLongStan function works", {
    lineall <- McmcLongStan(csvfiles)
    expect_is(lineall, "McmcLongStan")
    expect_equal(nrow(lineall@samples), 2400)
})



