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
    mf <- make_model_frame(formula, data)

    # fit density estimator
    args <- modifyList(list(x = prep_for_fit(mf, jd_method)), jd_method$.dots)
    args <- modifyList(args, list(...))
    f_hat <- do.call(jd_method$fit_fun, args)

    # create jdify object
    structure(
        list(model_frame = mf,
             f_hat = f_hat,
             jd_method = jd_method),
        class = "jdify"
    )
}

make_model_frame <- function(formula, data) {
    ## data preproccessing
    data <- as.data.frame(data)
    # ordered variables must be numeric, otherwise they are expanded as factors
    i_ordered <- which(sapply(data, function(x) is.ordered(x)))
    if (length(i_ordered) > 0) {
        for (i in i_ordered)
            data[, i] <- as.numeric(data[, i])
    }
    # create model frame (incl. dummy coding of factor variables)
    mf <- model.frame(formula = formula, data = data)
    if (length(levels(as.factor(mf[, 1]))) > 2)
        stop("Only binary classification implemented so far.")

    # convert all ordered variables to integers
    ordered_as_int(mf)
}

prep_for_fit <- function(model_frame, jd_method) {
    if (jd_method$cc) {
        return(cont_conv(with_num_class(model_frame)))
    } else {
        return(with_num_class(model_frame))
    }
}

with_num_class <- function(mf) {
    mf_num <- mf
    mf_num[[1]] <- as.numeric(mf[[1]])
    mf_num
}

ordered_as_int <- function(mf) {
    vars <- lapply(mf, function(x) if (is.ordered(x)) as.integer(x) else x)
    as.data.frame(vars)
}

#' Predict method for jdify objects
#'
#' @method predict jdify
#'
#' @param object an object of class `jdify`.
#' @param newdata a `data.frame` containing the predictors.
#' @param what either `"class"` for class predictions or `"probs"` for condition
#'   probability estimates.
#' @param threshold threshold parameter for class prediction (`what = "class"`);
#'   predicts the first element of `levels(class)` if `prob1 > threshold`, the
#'   second otherwise.
#' @param ... unused.
#'
#' @return A data frame containing the predictions.
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat$cl <- c("A", "B")[round(pnorm(dat$x1 + dat$x2)) + 1]
#' dat_test <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat_test$cl <- c("A", "B")[round(pnorm(dat_test$x1 + dat_test$x2)) + 1]
#'
#' # fit density with cctools package
#' model <- jdify(cl ~ x1 + x2, data = dat, jd_method("cctools"))
#' pred <- predict(model, dat)                    # class predictions
#' probs <- predict(model, dat, what = "cprobs")  # conditional probabilities
#'
#'
#' @importFrom stats model.frame predict
#' @export
predict.jdify <- function(object, newdata, what = "class", threshold = 0.5, ...) {
    stopifnot(what %in% c("class", "cprobs"))
    stopifnot(all((threshold >= 0) & (threshold <= 1)))
    newdata <- ordered_as_int(newdata)
    switch(what,
           "class"  = jdify_pred(object, newdata, threshold),
           "cprobs" = jdify_cprobs(object, newdata))
}


jdify_cprobs <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    stopifnot(all(names(object$model_frame)[-1] %in% names(newdata)))
    newdata <- newdata[, names(object$model_frame)[-1], drop = FALSE]  # only covariates

    # joint density at class = 1
    newdata <- cbind(data.frame(cl = 1), newdata)
    names(newdata)[1] <- names(object$model_frame)[1]
    f1 <- object$jd_method$eval_fun(object$f_hat, newdata)
    newdata[, 1] <- 2
    # joint density at class = 2
    f2 <- object$jd_method$eval_fun(object$f_hat, newdata)

    # joint density of predictors
    fx <- f1 + f2

    # conditional probabilities
    data.frame(prob1 = f1 / fx, prob2 = f2 / fx)
}

jdify_pred <- function(object, newdata, threshold = 0.5) {
    probs <- jdify_cprobs(object, newdata)
    cls <- levels(object$model_frame[, 1])
    res <- lapply(threshold,
                  function(t) factor(ifelse(probs[, 1] > t, cls[1], cls[2]), cls))
    res <- do.call(data.frame, res)
    names(res) <- paste0("thr", seq.int(threshold))
    attr(res, "threshold") <- threshold
    res
}
