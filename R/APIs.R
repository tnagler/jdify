## np::npudens() --------------------------------

prep_data_np <- function(x, mf = NULL) {
    # bring variables into right order
    if (!is.null(mf))
        x <- x[, names(mf)]
    # find out which variables are integer valued
    ints <- which(apply(x, 2, function(y) all(y == round(y))))
    for (i in ints) {
        ran_x <- if (is.null(mf)) range(x[, i]) else range(mf[, i])
        # extend range for non-binary variables to avoid out-of-sample errors
        is_binary <- (diff(ran_x) == 1)
        if (is_binary) {
            lvls <- ran_x
        } else {
            ran_xtnd <- round(ran_x * 1.5)
            lvls <- seq(from = ran_xtnd[1], to = ran_xtnd[2], by = 1)
        }
        # set as "ordered"
        x[, i] <- ordered(x[, i], levels = lvls)
    }
    x
}

fit_np <- function(x, ...) {
    np::npudensbw(prep_data_np(x), nmulti = 1, ckertype = "epanechnikov")
}

eval_np <- function(object, newdata, ...) {
    # order variables as in training data
    newdata[, object$xnames]
    # ordered variables with same levels as training data
    for (i in which(object$iord))
        newdata[, i] <- ordered(newdata[, i], object$dati$x$all.dlev[[i]])
    # evaluate density
    fitted(np::npudens(bws = object, edat = newdata))
}


## cctools::cckde --------------------------------

fit_cctools <- function(x, ...)
    cctools::cckde(x, ...)

eval_cctools <- function(object, newdata, ...)
    cctools::dcckde(newdata, object)


## kdevine::kdevine --------------------------------

# multiplier for classification as in Nagler & Czado (2016)
fit_kdevine <- function(x, ...)
    kdevine::kdevine(x, mult.1d = 2)

eval_kdevine <- function(object, newdata, ...) {
    kdevine::dkdevine(newdata, object)
}
