context("clsfyr functions")

dat <- data.frame(
    cl = c("A", "B")[rbinom(20, 1, 0.3) + 1],
    x = rnorm(20),
    z = rbinom(20, 1, 0.3)
)

jd_fit <- jdify(cl ~ ., dat)
probs <- predict(jd_fit, dat, "probs")

test_that("performance measures work", {
    perf <- assess_clsfyr(probs[, 1], dat[, 1] == 1)
    expect_error(assess_clsfyr(probs, dat[, 1]))
    expect_equal(nrow(perf), 11)
    expect_equal(names(perf), c("threshold", "measure", "value"))
})


test_that("AUC works", {
    perf <- assess_clsfyr(probs[, 1], dat[, 1] == 1, measure = "AUC")
    expect_error(assess_clsfyr(probs, dat[, 1]))
    expect_equal(nrow(perf), 1)
    expect_equal(names(perf), c("threshold", "measure", "value"))
})
