#' Joint density classification
#'
#' Fits a generic joint density model for the predictors and class variables.
#'
#' For predicting the class or conditional probabilities, see,
#' [predict.jdify()].
#'
#' @param formula an object of class "formula"; same as [stats::lm()].
#' @param data matrix, data frame, list or environment (or object coercible by
#'   [base::as.data.frame()]) containing the variables in the model.
#' @param jd_method an object of class `"jd_method"` or a character string
#'   for built-in methods, see [jd_method()].
#' @param ... additional parameters passed to `fit_fun`.
#'
#' @return An object of class `jdify`.
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat$cl <- c("A", "B")[round(pnorm(dat$x1 + dat$x2)) + 1]
#' dat_test <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat_test$cl <- c("A", "B")[round(pnorm(dat_test$x1 + dat_test$x2)) + 1]
#'
#' model <- jdify(cl ~ x1 + x2, dat)              # fitted joint density
#' pred <- predict(model, dat)                    # class predictions
#' probs <- predict(model, dat, what = "cprobs")  # conditional probabilities
#'
#' # in-sample performance
#' clsfy_performance(probs[, 1], dat[, 1], threshold = seq(0, 1, by = 0.05))
#'
#' @seealso [predict.jdify()], [clsfy_performance()]
#'
#' @import cctools
#' @importFrom stats model.frame predict formula
#' @export
jdify <- function(formula, data, jd_method = "cctools", ...) {
    if (is.character(jd_method) == 1)
        jd_method <- jd_method(jd_method)

    # reduce data to used variables, expand factors
    mm <- build_model(formula, data)

    # fit density estimator
    args <- list(x = mm$df)
    args <- modifyList(args, jd_method$.dots)
    args <- modifyList(args, list(...))
    if (jd_method$cc)
        args$x <- cont_conv(args$x, ...)
    f_hat <- do.call(jd_method$fit_fun, args)

    # create jdify object
    structure(
        list(formula = formula,
             model = mm,
             f_hat = f_hat,
             jd_method = jd_method),
        class = "jdify"
    )
}

build_model <- function(formula, data, fit = NULL) {
    mf <- model.frame(formula, data)  # discard unused variables
    if (length(levels(as.factor(mf[, 1]))) > 2)
        stop("Only binary classification implemented so far.")

    ## expand factors if necessary
    typs <- sapply(mf, function(x) class(x)[1])
    lvls <- lapply(mf, levels)
    nms <- names(mf)
    nms_ord <- nms[typs == "ordered"]

    ## set class variable as ordered
    cls_num <- (0:1)[as.factor(mf[, 1])]
    mf[, 1] <- ordered(cls_num, 0:1)

    ## expand factors (keep class variable as ordered)
    mf <- ordered_as_int(mf)  # ordered variables should not be expanded
    mf <- as.data.frame(cbind(mf[, 1], model.matrix(formula, mf)[, -1]))
    names(mf)[1] <- nms[1]

    ## go back to ordered for class, initially ordered, and expanded variables
    nms_xpnd <- setdiff(names(mf), nms)
    mf <- back_to_ordered(mf, nms_ord, c(nms[1], nms_xpnd), lvls)

    out <- list(df = mf, types = typs, names = nms, levels = lvls)
    ## check if data is appropriate
    if (!is.null(fit)) {
        if (!all.equal(fit[2:4], out[2:4]))
            stop("data does not match fitted model")
    }

    out
}

with_num_class <- function(mf) {
    mf_num <- mf
    mf_num[[1]] <- as.numeric(mf[[1]])
    mf_num
}

ordered_as_int <- function(mf) {
    vars <- lapply(mf, function(x) if (is.ordered(x)) as.integer(x) - 1 else x)
    as.data.frame(vars)
}

back_to_ordered <- function(df, nms_ordered, nms_fct, lvls) {
    for (nm in nms_ordered)
        df[, nm] <- ordered(lvls[[nm]][df[, nm] + 1], lvls[[nm]])
    for (nm in nms_fct)
        df[, nm] <- ordered(df[, nm], 0:1)
    df
}
