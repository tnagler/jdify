#' Measures of classification performance
#'
#' Calculates performance measures for a calssifier Assumes there are two
#' classes, and the first of `level(true_cl)` is to be predicted (the
#' "positive").
#'
#' @param score probabilities or scores for the target class 1 ("positive");
#'   scores are assumed to be in \eqn{[0, 1]} and high scores correspond to high
#'   probability.
#' @param true_cls vector of indicators for the target class: `TRUE` or `1` if
#'   true class is the target class, `FALSE` or `0` else.
#' @param measure a character vector of performance measures to be calculated,
#' see *Details*.
#' @param threshold threshold for prediction, see [predict.jdify()].
#'
#' @return A data.frame where each column corresponds to one value of
#'   `threshold`. The corresponding values can be found with `attr(result,
#'   "threshold")`.
#'
#' @details Valid options for `measure` are
#' \itemize{
#' \item{\code{"TP"}: }{number of true positives,}
#' \item{\code{"FP"}: }{number of false positive,}
#' \item{\code{"TN"}: }{number of true negatives,}
#' \item{\code{"FN"}: }{number of false negatives,}
#' \item{\code{"TPR"}, \code{"sensitivity"}, \code{"recall"}: }{true positive rate (\eqn{TP / P}),}
#' \item{\code{"FPR"}, \code{"fall-out"}: }{false positive rate (\eqn{FP / N}),}
#' \item{\code{"TNR"}, \code{"specificity"}: }{true negative rate (\eqn{TN / N}),}
#' \item{\code{"FNR"}: }{false negative rate (\eqn{FN / P}),}
#' \item{\code{"PRC"}, \code{"PPV"}: }{precision/positive predictive value (\eqn{TP / (TP + FP)},}
#' \item{\code{"NPV"}: }{negative predictive value (\eqn{TN / (TN + FN)}),}
#' \item{\code{"FDR"}: }{false discovery rate (\eqn{FP / (TP + FP)}),}
#' \item{\code{"ACC"}, \code{"accuracy"}: }{accuracy (\eqn{(TP + TN) / (P + N)}),}
#' \item{\code{"F1"}: }{F1 score (\eqn{2 * TP / (2 * TP + FP + FN)}),}
#' \item{\code{"MCC"}: }{Matthews correlation coefficient
#' \deqn{\frac{(TP * TN - FP * FN)}{[(TP + FP) * (TP + FN) * (TN + FP) *
#'  (TN + FN)]^(-1/2)},}}
#' \item{\code{"informedness"}: }{informedness (\eqn{TP / P + TN / N - 1}),}
#' \item{\code{"markedness"}: }{markedness (\eqn{TP / (TP + FP) + TN / (TN + FN) - 1}),}
#' \item{\code{"AUC"}: }{area under the curve (must be in first position)}
#' }
#' where `P` and `N` are the number of positives and negatives, respectively.
#'
#' @seealso [get_auc()]
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(
#'     cl = as.factor(rbinom(10, 1, 0.5)),
#'     x1 = rnorm(10),
#'     x2 = rbinom(10, 1, 0.3)
#' )
#'
#' model <- jdify(cl ~ x1 + x2, data = dat)      # joint density fit
#' probs <- predict(model, dat, what = "probs")  # conditional probabilities
#'
#' # calculate performance measures
#' assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#'
#' # calculate area under the curve
#' FPR <- assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("FPR"))$value
#' TPR <- assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("TPR"))$value
#' get_auc(data.frame(FPR = FPR, TPR = TPR))
#'
#' @export
assess_clsfyr <- function(score, true_cls, measure = "ACC",
                          threshold = seq(0, 1, by = 0.1)) {
    stopifnot(is.logical(true_cls) | all(true_cls %in% c(0, 1)))
    stopifnot(NCOL(score) == 1)

    if (measure[1] == "AUC") {
        stopifnot(all(measure == "AUC"))
        # ROC curve
        roc <- data.frame(
            TPR = assess_clsfyr(score, true_cls, "TPR", threshold)$value,
            FPR = assess_clsfyr(score, true_cls, "FPR", threshold)$value
        )
        return(data.frame(threshold = NA, measure = "AUC", value = get_auc(roc)))
    }
    # expand all combinations of measure and threshold
    threshold <- sort(threshold)
    grid <- expand.grid(
        threshold = threshold,
        measure = measure,
        stringsAsFactors = FALSE
    )

    # calculate performance measure for each combination
    grid$value <- vapply(
        split(grid, seq.int(nrow(grid))),
        function(x) get_measure(x["measure"], score, x["threshold"], true_cls),
        numeric(1)
    )

    grid
}

get_measure <- function(measure, score, threshold, true_cls) {
    pred <- (score >= as.numeric(threshold))

    # total number of positives and negatives
    P <- sum(true_cls == 1, na.rm = TRUE)
    N <- sum(true_cls == 0, na.rm = TRUE)

    # number o ftrue/false positive/negatives
    TP <- sum((pred == 1) & (true_cls == 1), na.rm = TRUE)
    FP <- sum((pred == 1) & (true_cls == 0), na.rm = TRUE)
    TN <- sum((pred == 0) & (true_cls == 0), na.rm = TRUE)
    FN <- sum((pred == 0) & (true_cls == 1), na.rm = TRUE)

    switch(
        as.character(measure),
        "TP" = TP,
        "FP" = FP,
        "TN" = TN,
        "FN" = FN,
        "TPR"         = TP / P,
        "sensitivity" = TP / P,
        "recall"      = TP / P,
        "TNR"         = TN / N,
        "specificity" = TN / N,
        "FPR"      = FP / N,
        "fall-out" = FP / N,
        "FNR" = FN / P,
        "PPV"       = TP / (TP + FP),
        "precision" = TP / (TP + FP),
        "NPV" = TN / (TN + FN),
        "FDR" = FP / (TP + FP),
        "ACC"      = (TP + TN) / (P + N),
        "accuracy" = (TP + TN) / (P + N),
        "F1"  = 2 * TP / (2 * TP + FP + FN),
        "MCC" = (TP * TN - FP * FN) *
            (TP + FP)^(-1/2) *
            (TP + FN)^(-1/2) *
            (TN + FP)^(-1/2) *
            (TN + FN)^(-1/2),
        "informedness" = TP / P + TN / N - 1,
        "markedness" = TP / (TP + FP) + TN / (TN + FN) - 1,
        stop("measure not implemented")
    )
}


#' Calculate the area under the curve (AUC)
#'
#' @param roc a matrix or data frame with columns `"FPR"` and `"TPR"` containing
#'   pairs of false and true positive rates.#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(
#'     cl = as.factor(rbinom(10, 1, 0.5)),
#'     x1 = rnorm(10),
#'     x2 = rbinom(10, 1, 0.3)
#' )
#'
#' model <- jdify(cl ~ x1 + x2, data = dat)      # joint density fit
#' probs <- predict(model, dat, what = "probs")  # conditional probabilities
#'
#' # calculate performance measures
#' assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#'
#' # calculate area under the curve
#' FPR <- assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("FPR"))$value
#' TPR <- assess_clsfyr(probs[, 1], dat[, 1] == 0, measure = c("TPR"))$value
#' get_auc(data.frame(FPR = FPR, TPR = TPR))
#'
#' @export
get_auc <- function(roc) {
    if (!is.data.frame(roc))
        roc <- as.data.frame(roc)
    stopifnot(all(names(roc) %in% c("FPR", "TPR")))
    roc <- roc[, c("FPR", "TPR")]
    ord <- order(roc[, 1])
    roc <- roc[ord, ]
    idx <- seq_along(roc[, 1])[-1]
    (roc[idx, 1] - roc[idx - 1, 1]) %*% (roc[idx, 2] + roc[idx - 1, 2]) / 2
}
