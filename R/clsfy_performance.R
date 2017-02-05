#' Classification performance measures
#'
#' Calculates performance measures for a calssifier Assumes there are two
#' classes, and the first of `level(true_cl)` is to be predicted (the
#' "positive").
#'
#' @param scores probabilities or scores for class 1 (1 - prob of class 2);
#' scores are assumed to be in \eqn{[0, 1]} and high scores correspond to high
#' probability.
#' @param true_cl vector of true class labels.
#' @param threshold threshold for prediction, see [jdify::predict.jdify()].
#'
#' @return A data.frame where each column corresponds to one value of `threshold`.
#' The corresponding values can be found with `attr(result, "threshold")`.
#'
#' @examples
#' # simulate training and test data
#' dat <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat$cl <- c("A", "B")[round(pnorm(dat$x1 + dat$x2)) + 1]
#' dat_test <- data.frame(x1 = rnorm(10), x2 = rbinom(10, 1, 0.3))
#' dat_test$cl <- c("A", "B")[round(pnorm(dat_test$x1 + dat_test$x2)) + 1]
#'
#' model <- jdify(cl ~ x1 + x2, data = dat)       # joint density fit
#' probs <- predict(model, dat, what = "cprobs")  # conditional probabilities
#'
#' # calculate performance measures
#' t <- seq(0, 1, by = 0.05)
#' perf <- clsfy_performance(scores = probs[, 1], dat[, 1], threshold = t)
#'
#' # ROC plot
#' plot(t(perf[c("TPR", "FPR"), ]), type = "l", xlim = c(0, 1), ylim = c(0, 1))
#' abline(0, 1)
#'
#' @export
clsfy_performance <- function(scores, true_cl, threshold = 0.5) {
    true_cl <-  as.numeric(as.factor(true_cl))
    stopifnot(NCOL(scores) == 1)


    # calculate performance for each prediction
    preds <- sapply(threshold, function(t) (1:2)[1 + (scores < t)])
    out <- apply(preds, 2, get_one_measure, true_cl = true_cl)

    # add thresholds
    out <- rbind(out, threshold)

    ## return results with names
    rownames(out) <- c("TPR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR",
                       "ACC", "F1", "MCC", "Informedness", "Markedness",
                       "threshold")
    colnames(out) <- paste0("thr", 1:length(threshold))
    class(out) <- "clsfy_performance"
    out
}

get_one_measure <- function(x, true_cl) {
    # total number of positives and negatives
    P <- sum(true_cl == 1, na.rm = TRUE)
    N <- sum(true_cl == 2, na.rm = TRUE)

    # number o ftrue/false positive/negatives
    TP <- sum((x == 1) & (true_cl == 1), na.rm = TRUE)
    FP <- sum((x == 1) & (true_cl == 2), na.rm = TRUE)
    TN <- sum((x == 2) & (true_cl == 2), na.rm = TRUE)
    FN <- sum((x == 2) & (true_cl == 1), na.rm = TRUE)

    # metrics
    c(TP / P,                               # TPR
      TN / N,                               # SPC
      TP / (TP + FP),                       # PPC
      TN / (TN + FN),                       # NPV
      FP / N,                               # FPR
      FP / (TP + FP),                       # FDR
      FN / P,                               # FNR
      (TP + TN) / (P + N),                  # ACC
      2 * TP / (2 * TP + FP + FN),          # F1
      (TP * TN - FP * FN) *
          (TP + FP)^(-1/2) *
          (TP + FN)^(-1/2) *
          (TN + FP)^(-1/2) *
          (TN + FN)^(-1/2),                 # MCC
      TP / P + TN / N - 1,                  # Informedness
      TP / (TP + FP) + TN / (TN + FN) - 1)  # Markedness
}
