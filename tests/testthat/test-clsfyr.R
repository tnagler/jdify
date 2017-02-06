context("clsfyr functions")

dat <- data.frame(
    cl = c("A", "B")[rbinom(20, 1, 0.3) + 1],
    x = rnorm(20),
    z = rbinom(20, 1, 0.3)
)

test_that("performance measures work", {
    jd_fit <- jdify(cl ~ ., dat)
    prob1 <- predict(jd_fit, dat, "cprobs")[, 1]
    expect_error(clsfyr_performance(cbind(prob1, prob1), dat[, 1]))

    threshold <- seq(0, 1, 0.1)
    ms <- clsfyr_performance(prob1, dat[, 1], threshold)
    expect_equal(ncol(ms), length(threshold))
})

test_that("ROC plot works", {
    jd_fit <- jdify(cl ~ ., dat)
    prob <- predict(jd_fit, dat, "cprobs")[, 1]

    threshold <- seq(0, 1, 0.1)
    perf <- clsfyr_performance(prob, dat[, 1], threshold)

    expect_length(clsfyr_rocplot(perf, dat[, 1], threshold), 1)
    expect_length(clsfyr_rocplot(list(perf, perf), dat[, 1], threshold), 2)
})
