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
#' conditional probabilities (`$cprobs`), and indexes for training and test data
#' (`$train_index`, `$test_index`).}
#'
#'   \item{`cv_cprobs`: }{aggragated out-of-sample `cprobs` in same order as original
#' data.}
#' }
#'
#'
#' @importFrom foreach foreach  %dopar% %do%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @export
cv_jdify <- function(formula, data, jd_method = "cctools", folds  = 10,
                     cores = 1,  ...) {
    # preprocessing
    stopifnot(folds >= 1)
    folds <- round(folds)
    data <- as.data.frame(data)
    mf <- model.frame(formula = formula, data = data)
    if (length(levels(as.factor(mf[, 1]))) != 2)
        stop("Only binary classification implemented so far.")
    k <- 0  # CRAN check complains if undefined

    #  test indices for each fold
    test_indexes <- suppressWarnings(split(seq.int(nrow(mf)), seq.int(folds)))
    if (cores > 1) {
        # CV with parallelization
        cl <- makeCluster(min(cores, folds[1]))
        registerDoParallel(cl)
        on.exit(stopCluster(cl))
        res_folds <- foreach(
            k = 1:folds[1],
            .export = c("fit_fold", "prep_data_np")
        ) %dopar%
            fit_fold(k, mf, test_indexes, jd_method, ...)
    } else {
        # without
        res_folds <- foreach(
            k = 1:folds[1],
            .export = c("fit_fold", "prep_data_np")
        ) %do%
            fit_fold(k, mf, test_indexes, jd_method, ...)
    }

    res_folds$model_frame <- mf
    cv_cprobs <- matrix(NA, nrow(mf), 2)
    for (fold in seq.int(folds)) {
        i <- res_folds[[fold]]$test_index
        cv_cprobs[i, ] <- as.matrix(res_folds[[fold]]$cprobs)
    }
    res_folds$cv_cprobs <- as.data.frame(cv_cprobs)
    names(res_folds)[seq.int(folds)] <- paste0("fold", seq.int(folds))
    res_folds
}

fit_fold <- function(fold, mf, test_indexes, jd_method, ...) {
    test_index <- test_indexes[[fold]]
    # fit on training data
    fit <- jdify(formula(mf), data = mf[-test_index, ], jd_method, ...)
    # evaluate on test data
    cprobs <- predict(fit, mf[test_index, ], what = "cprobs")

    list(fit = fit,
         cprobs = cprobs,
         train_index = setdiff(seq.int(nrow(mf)), test_index),
         test_index = test_index)
}
