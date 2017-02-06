context("Joint density estimation methods")

test_that("Default methods work", {
    expect_length(jd_method("cctools"), 4)
    expect_length(jd_method("kdevine"), 4)
    expect_length(jd_method("np"), 4)
})

test_that("Custom building of methods works", {
    require(kdevine)
    fit_kdevine <- function(x, ...)
        kdevine::kdevine(x, mult.1d = 2)
    eval_kdevine <- function(object, newdata, ...)
        kdevine::dkdevine(newdata, object)
    jd_method(fit_fun = fit_kdevine, eval_fun = eval_kdevine, cc = FALSE)
    expect_error(jd_method(fit_fun = function(x, ...) x, eval_fun = function(x) x))
})
