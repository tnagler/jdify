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
prep_data_np <- function(x, mf = NULL) {
    # bring variables into right order
    if (!is.null(mf))
        x <- x[, names(mf)]
    # find out which variables are integer valued
    ints <- which(sapply(x, function(y) all(y == round(y))))
    if (length(ints) > 0) {
        for (i in ints) {
            ran_x <- if (is.null(mf)) range(x[, i]) else range(mf[, i])
            # extend range for non-binary variables to avoid out-of-sample errors
            is_binary <- (diff(ran_x) == 1)
            if (is_binary) {
                lvls <- ran_x
            } else {
                # extend range by 1/3 in both directions
                ran_xtnd <- round(ran_x + diff(ran_x) * c(-1, 1) / 3)
                lvls <- seq(from = ran_xtnd[1], to = ran_xtnd[2], by = 1)
            }
            # set as "ordered"
            x[, i] <- ordered(x[, i], levels = lvls)
        }
    }

    x
}
fit_np <- function(x, ...) {
    # the following options are usually set on package startup, but not by
    # loading namespace (as done in jd_api)
    options("np.messages" = FALSE)
    options(np.tree = FALSE)

    x <- prep_data_np(x)
    bw <- np::npudensbw(dat = x, nmulti = 1, ckertype = "epanechnikov")
    np::npudens(bws = bw)
}
eval_np <- function(object, newdata, ...) {
    # order variables as in training data
    newdata <- newdata[, object$xnames]

    # ordered variables with same levels as training data
    for (i in which(object$bws$iord))
        newdata[, i] <- ordered(newdata[, i], levels(object$eval[[i]]))

    # evaluate density
    loadNamespace("np")
    predict(object, FALSE, newdata)
}

