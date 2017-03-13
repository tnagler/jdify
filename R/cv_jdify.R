#' Cross validation for joint density classification
#'
#' Performs k-fold cross validation for a `jdify` object.
#'
#' @param formula an object of class "formula"; same as [stats::lm()].
#' @param data matrix, data frame, list or environment (or object coercible by
#'   [base::as.data.frame()]) containing the variables in the model.
#' @param jd_method an object of class `"jd_method"` defining the method for joint
#' density estimation, see [jd_method()].
#' @param folds number of folds.
#' @param cores number of cores for parallelized cross validation (based on
#'   [foreach::foreach()]).
#' @param ... further arguments passed to `fit_fun()`.
#'
#' @return A list with elements
#' \itemize{
#'   \item{`folds1``, ..., `foldsk`: }{for each fold: the fitted model `$fit`, estimated
#' conditional probabilities (`$probs`), and indexes for training and test data
#' (`$train_index`, `$test_index`).}
#'
#'   \item{`cv_probs`: }{aggragated out-of-sample `probs` in same order as original
#' data.}
#' }
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(
#'     cl = as.factor(rbinom(100, 1, 0.5)),
#'     x1 = rnorm(100),
#'     x2 =  ordered(rbinom(10, 1, 0.3), 0:1)
#' )
#'
#' cv <- cv_jdify(cl ~ x1 + x2, dat)
#' probs <- cv$cv_probs
#' assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#'
#' @importFrom foreach foreach  %dopar% %do%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @export
cv_jdify <- function(formula, data, jd_method = "cctools", folds  = 10,
                     cores = 1,  ...) {
    # preprocessing
    stopifnot(folds > 1)
    folds <- round(folds)
    data <- as.data.frame(data)
    model <- build_model(formula, data)
    k <- 0  # CRAN check complains if undefined
    #  test indices for each fold
    test_indexes <- suppressWarnings(split(seq.int(nrow(model$df)), seq.int(folds)))
    if (cores > 1) {
        # CV with parallelization
        cl <- makeCluster(min(cores, folds[1]))
        registerDoParallel(cl)
        on.exit(stopCluster(cl))
        res_folds <- foreach(k = 1:folds[1]) %dopar%
            fit_fold(k, model, test_indexes, jd_method, ...)
    } else {
        # without
        res_folds <- foreach(k = 1:folds[1]) %do%
            fit_fold(k, model, test_indexes, jd_method, ...)
    }

    # gather results
    res_folds$model_frame <- model$df
    cv_probs <- as.data.frame(matrix(NA, nrow(model$df), 2))
    names(cv_probs) <- names(res_folds[[1]]$probs)

    # combine out-of-sample predictions (same positions as original data)
    for (fold in seq.int(folds)) {
        i <- res_folds[[fold]]$test_index
        cv_probs[i, ] <- res_folds[[fold]]$probs
    }

    # finalize
    res_folds$cv_probs <- as.data.frame(cv_probs)
    names(res_folds)[seq.int(folds)] <- paste0("fold", seq.int(folds))
    res_folds
}

fit_fold <- function(fold, model, test_indexes, jd_method, ...) {
    test_index <- test_indexes[[fold]]

    # fit on training data
    fit <- jdify(model$formula, data = model$df[-test_index, ], jd_method, ...)
    # evaluate on test data
    probs <- predict(fit, model$df[test_index, ], what = "probs")

    list(fit = fit,
         probs = probs,
         train_index = setdiff(seq.int(nrow(model$df)), test_index),
         test_index = test_index)
}
