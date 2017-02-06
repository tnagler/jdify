
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

    model <- build_model(object$formula, newdata, object$model)

    switch(what,
           "class"  = jdify_pred(object, model$df, threshold),
           "cprobs" = jdify_cprobs(object, model$df))
}

jdify_cprobs <- function(object, newdata) {
    # joint density at class = 0
    newdata[, 1] <- 0
    f0 <- object$jd_method$eval_fun(object$f_hat, newdata)
    newdata[, 1] <- 1
    # joint density at class = 1
    f1 <- object$jd_method$eval_fun(object$f_hat, newdata)

    # joint density of predictors
    fx <- f0 + f1
    # set prob to 0.5 if f1 = f2 = 0
    prob0 <- ifelse(fx == 0, 0.5, f0 / fx)
    prob1 <- ifelse(fx == 0, 0.5, f1 / fx)

    # conditional probabilities
    data.frame(prob0 = prob0, prob1 = prob1)
}

jdify_pred <- function(object, newdata, threshold = 0.5) {
    probs <- jdify_cprobs(object, newdata)
    cls <- levels(object$model$df[, 1])
    res <- lapply(threshold,
                  function(t) factor(ifelse(probs[, 1] > t, cls[1], cls[2]), cls))
    res <- do.call(data.frame, res)
    names(res) <- paste0("thr", seq.int(threshold))
    attr(res, "threshold") <- threshold
    res
}
