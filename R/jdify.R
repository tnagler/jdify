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
#' dat <- data.frame(
#'     cl = as.factor(rbinom(10, 1, 0.5)),
#'     x1 = rnorm(10),
#'     x2 = ordered(rbinom(10, 1, 0.3), 0:1)
#' )
#'
#' model <- jdify(cl ~ x1 + x2, dat)              # fitted joint density
#' pred <- predict(model, dat)                    # class predictions
#' probs <- predict(model, dat, what = "probs")   # conditional probabilities
#'
#' # in-sample performance
#' assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#'
#' @seealso [predict.jdify()], [assess_clsfyr()]
#'
#' @importFrom cctools cont_conv
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
        args$x <- cctools::cont_conv(args$x, ...)
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

#' @importFrom stats model.matrix
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
    mf[, 1] <- ordered((0:1)[as.factor(mf[, 1])], 0:1)

    ## expand factors (keep class variable as ordered)
    mf <- ordered_as_int(mf)  # ordered variables should not be expanded
    mf <- as.data.frame(cbind(mf[, 1], model.matrix(formula, mf)[, -1]))
    names(mf) <- gsub("`", "", names(mf))
    names(mf)[1] <- nms[1]

    ## go back to ordered for class, initially ordered, and expanded variables
    nms_xpnd <- setdiff(names(mf), nms)
    mf <- back_to_ordered(mf, nms_ord, c(nms[1], nms_xpnd), lvls)

    out <- list(formula = formula, df = mf, types = typs, names = nms, levels = lvls)
    ## check if data is appropriate
    if (!is.null(fit)) {
        if (!all.equal(fit[3:5], out[3:5]))
            stop("data does not match fitted model")
    }

    out
}

ordered_as_int <- function(mf) {
    vars <- lapply(mf, function(x) if (is.ordered(x)) as.integer(x) - 1 else x)
    df <- as.data.frame(vars)
    names(df) <- names(vars)
    df
}

back_to_ordered <- function(df, nms_ordered, nms_fct, lvls) {
    for (nm in nms_ordered)
        df[, nm] <- ordered(lvls[[nm]][df[, nm] + 1], lvls[[nm]])
    for (nm in nms_fct)
        df[, nm] <- ordered(df[, nm], 0:1)
    df
}
