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
#'     x2 = rbinom(10, 1, 0.3)
#' )
#'
#' model <- jdify(cl ~ x1 + x2, dat)              # fitted joint density
#' pred <- predict(model, dat)                    # class predictions
#' probs <- predict(model, dat, what = "cprobs")  # conditional probabilities
#'
#' # in-sample performance
#' clsfyr_performance(probs[, 1], dat[, 1], threshold = seq(0, 1, by = 0.05))
#'
#' @seealso [predict.jdify()], [clsfyr_performance()]
#'
#' @importFrom cctools cont_conv
#' @importFrom stats model.frame predict formula
#' @export
jdify <- function(formula, data, jd_method = "cctools", ...) {
    if (is.character(jd_method) == 1)
        jd_method <- jd_method(jd_method)

    # reduce data to used variables, expand factors
    mf <- prepare_model_frame(formula, data)

    # fit density estimator
    args <- modifyList(list(x = mf), jd_method$.dots)
    args <- modifyList(args, list(...))
    if (jd_method$cc)
        args$x <- cont_conv(args$x, ...)
    f_hat <- do.call(jd_method$fit_fun, args)

    # create jdify object
    structure(
        list(formula = formula,
             model_frame = mf,
             f_hat = f_hat,
             jd_method = jd_method),
        class = "jdify"
    )
}

prepare_model_frame <- function(formula, data) {
    mf <- model.frame(formula, data)
    if (length(levels(mf[, 1])) > 2)
        stop("Only binary classification implemented so far.")
    mf[, 1] <- ordered((0:1)[as.numeric(mf[, 1])], 0:1)
    mf
}
