library(jdify)

dat <- foreign::read.arff(
    "/home/n5/research/datasets/retinopathy/messidor_features.arff"
)
dat <- dat[dat[, 1] == 1, ]  # only use pictures with sufficient quality
dat <- dat[, -1]  # remove quality column
## in sample ----------------------
dat <- dat[1:400, ]
## fit models
model_np <- jdify(Class ~ ., data = dat, "np")
model_cctools <- jdify(Class ~ ., data = dat, "cctools")
model_kdevine <- jdify(Class ~ ., data = dat, "kdevine")

## predict conditional class probabilities
probs_np <- predict(model_np, dat, "cprobs")
probs_cctools <- predict(model_cctools, dat, "cprobs")
probs_kdevine <- predict(model_kdevine, dat, "cprobs")

## calculate performance measures
threshold <- seq(0, 1, l = 1000)
perf_np <- clsfyr_performance(probs_np[, 1], dat$Class, threshold)
perf_cctools <- clsfyr_performance(probs_cctools[, 1], dat$Class, threshold)
perf_kdevine <- clsfyr_performance(probs_kdevine[, 1], dat$Class, threshold)

## ROC plot
clsfyr_rocplot(list(perf_np, perf_cctools, perf_kdevine))

## out of sample ------------------------

## fit models
cv_np <- cv_jdify(Class ~ ., data = dat, "np",
                  cores = 3, folds = 10)
cv_cctools <- cv_jdify(Class ~ ., data = dat, "cctools",
                       cores = 3, folds = 10)
cv_kdevine <- cv_jdify(Class ~ ., data = dat, "kdevine",
                       cores = 3, folds = 10,
                       test.level = 0.05)

probs_np <- cv_np$cv_cprobs
probs_cctools <- cv_cctools$cv_cprobs
probs_kdevine <- cv_kdevine$cv_cprobs

## calculate performance measures
threshold <- seq(0, 1, l = 1000)
perf_np <- clsfyr_performance(probs_np[, 1], dat$Class, threshold)
perf_cctools <- clsfyr_performance(probs_cctools[, 1], dat$Class, threshold)
perf_kdevine <- clsfyr_performance(probs_kdevine[, 1], dat$Class, threshold)

## ROC plot
clsfyr_rocplot(list(perf_np, perf_cctools, perf_kdevine))
beepr::beep(5)
