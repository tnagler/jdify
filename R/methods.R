## cctools::cckde --------------------------------
fit_cctools <- function(x, ...)
    cctools::cckde(x, ...)
eval_cctools <- function(object, newdata, ...)
    cctools::dcckde(newdata, object)


## kdevine::kdevine --------------------------------
fit_kdevine <- function(x, ...)
    kdevine::kdevine(x, ...)
eval_kdevine <- function(object, newdata, ...)
    kdevine::dkdevine(newdata, object)


## np::npudens() --------------------------------
fit_np <- function(x, ...) {
    # the following options are usually set on package startup, but not by
    # loading namespace (as done in jd_api)
    options("np.messages" = FALSE)
    options(np.tree = FALSE)
    bw <- np::npudensbw(dat = x, nmulti = 1, ckertype = "epanechnikov")
    np::npudens(bws = bw)
}
eval_np <- function(object, newdata, ...) {
    # evaluate density
    loadNamespace("np")
    predict(object, FALSE, newdata)
}

