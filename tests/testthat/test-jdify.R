context("Fitting, predicting, and evaluating a 'jdify' object")

dat <- data.frame(
    cl = c("A", "B")[rbinom(20, 1, 0.3) + 1],
    x = rnorm(20)
)

## use joint density estimator of cckde package
fit_fun <- function(x) cctools::cckde(x)
eval_fun <- function(object, newdata) cctools::dcckde(newdata, object)

test_that("jdify works", {
    expect_error(jdify(cl ~ x, dat, fit_fun, eval_fun = function(x) x))
    jd_fit <- jdify(cl ~ x, dat, fit_fun, eval_fun = eval_fun)
    expect_is(jd_fit, "jdify")
    levels(dat$cl) <- c(levels(dat$cl), "C")
    expect_error(jdify(cl ~ x, dat, fit_fun, eval_fun = eval_fun))
})

test_that("predict works", {
    jd_fit <- jdify(cl ~ x, dat, fit_fun, eval_fun, cc = FALSE)

    # class prediction
    threshold <- seq(0, 1, 0.1)
    preds <- predict(jd_fit, dat, threshold = threshold)
    expect_equal(ncol(preds), length(threshold))
    expect_equal(nrow(preds), nrow(dat))
    expect_true(all(levels(preds) %in% levels(jd_fit$model_frame[, 1])))

    # probability prediction
    cprobs <- predict(jd_fit, dat, what = "cprobs")
    expect_gte(min(cprobs), 0)
    expect_lte(max(cprobs), 1)

    # sanity checks
    expect_error(predict(jd_fit, dat, what = "nothing"))
    expect_error(predict(jd_fit, dat, threshold = NA))
})

test_that("performance measures work", {
    jd_fit <- jdify(cl ~ x, dat, fit_fun, eval_fun, cc = FALSE)
    prob1 <- predict(jd_fit, dat, "cprobs")[, 1]
    expect_error(clsfy_performance(cbind(prob1, prob1), dat[, 1]))

    threshold <- seq(0, 1, 0.1)
    ms <- clsfy_performance(prob1, dat[, 1], threshold)
    expect_equal(ncol(ms), length(threshold))
})

test_that("cross validation works", {
    jd_fit <- jdify(cl ~ x, dat, fit_fun, eval_fun, cc = FALSE)
    cv <- cv_jdify(cl ~ x, dat, fit_fun, eval_fun)
    expect_length(cv, 12)
    expect_length(cv[[1]], 4)
})
