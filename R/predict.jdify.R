
#' Predict method for jdify objects
#'
#' @method predict jdify
#'
#' @param object an object of class `jdify`.
#' @param newdata a `data.frame` containing the predictors.
#' @param what either `"class"` for class predictions or `"probs"` for condition
#'   probability estimates.
#' @param threshold threshold parameter for class prediction (`what = "class"`);
#'   predicts class \eqn{C} if \eqn{Pr(C) \ge threshold}.
#' @param ... unused.
#'
#' @return A data frame containing the predictions.
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(
#'     cl = as.factor(rbinom(10, 1, 0.5)),
#'     x1 = rnorm(10),
#'     x2 =  ordered(rbinom(10, 1, 0.3), 0:1)
#' )
#'
#' # fit density with cctools package
#' model <- jdify(cl ~ x1 + x2, data = dat, jd_method("cctools"))
#' pred <- predict(model, dat)                   # class predictions
#' probs <- predict(model, dat, what = "probs")  # conditional probabilities
#'
#' @importFrom stats model.frame predict
#' @importFrom cctools expand_as_numeric
#' @export
predict.jdify <- function(object, newdata, what = "class", threshold = 0.5, ...) {
    stopifnot(what %in% c("class", "probs"))
    stopifnot(all((threshold >= 0) & (threshold <= 1)))

    # extract predictors
    nms <- names(object$model$df)
    newdata <- newdata[, nms[-1]]
    # add class dummy
    cls_levels <- object$model$levels[[nms[1]]]
    cls_type <- object$model$types[nms[1]]
    cls <- factor(cls_levels[1], cls_levels, ordered = (cls_type == "ordered"))
    newdata <- cbind(cls, newdata)
    names(newdata)[1] <- nms[1]

    # build model matrix as during fitting
    model <- build_model(object$formula, newdata, object$model)
    if (object$jd_method$cc)
        model$df <- cctools::expand_as_numeric(model$df)

    switch(what,
           "class" = jdify_pred(object, model$df, threshold),
           "probs" = jdify_probs(object, model$df))
}

jdify_probs <- function(object, newdata) {
    cls <- object$model$levels[[1]]
    # joint density at class = cls[1]
    newdata[, 1] <- ordered(cls[1], cls)
    f1 <- object$jd_method$eval_fun(object$f_hat, newdata)
    newdata[, 1] <- ordered(cls[2], cls)
    # joint density at class = cls[2]
    f2 <- object$jd_method$eval_fun(object$f_hat, newdata)
    # joint density of predictors
    fx <- f1 + f2

    # sets conditional probabilities to 0.5 if f1 = f2 = 0
    f1 <- ifelse(fx == 0, 0.5, f1)
    f2 <- ifelse(fx == 0, 0.5, f2)
    fx <- f1 + f2

    # conditional probabilities
    probs <- cbind(f1 / fx, f2 / fx)
    colnames(probs) <- levels(object$model$df[, 1])
    as.data.frame(probs)
}

jdify_pred <- function(object, newdata, threshold = 0.5) {
    probs <- jdify_probs(object, newdata)
    cls <- object$model$levels[[1]]
    res <- lapply(threshold,
                  function(t) factor(ifelse(probs[, 1] > t, cls[1], cls[2]), cls))
    res <- do.call(data.frame, res)
    names(res) <- paste0("thr", seq.int(threshold))
    attr(res, "threshold") <- threshold
    res
}
