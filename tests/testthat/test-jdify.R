context("Fitting, predicting, and evaluating a 'jdify' object")

dat <- data.frame(
    cl = c("A", "B")[rbinom(20, 1, 0.3) + 1],
    x = rnorm(20),
    z = rbinom(20, 1, 0.3)
)

test_that("jdify works", {
    expect_is(jdify(cl ~ ., dat), "jdify")
    # only works with binary classification for now
    levels(dat$cl) <- c(levels(dat$cl), "C")
    expect_error(jdify(cl ~ ., dat))
})

test_that("predict works", {
    jd_fit <- jdify(cl ~ ., dat)

    # class prediction
    threshold <- seq(0, 1, 0.1)
    preds <- predict(jd_fit, dat, threshold = threshold)
    expect_equal(ncol(preds), length(threshold))
    expect_equal(nrow(preds), nrow(dat))
    expect_true(all(levels(preds) %in% levels(jd_fit$model_frame[, 1])))

    # probability prediction
    cprobs <- predict(jd_fit, dat, what = "probs")
    expect_gte(min(cprobs), 0)
    expect_lte(max(cprobs), 1)

    # sanity checks
    expect_error(predict(jd_fit, dat, what = "nothing"))
    expect_error(predict(jd_fit, dat, threshold = NA))
})


test_that("cross validation works", {
    cv <- cv_jdify(cl ~ ., dat)
    expect_length(cv, 12)
    expect_length(cv[[1]], 4)
})
