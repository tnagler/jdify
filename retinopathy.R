library(kdevine)
library(kdecopula)
library(VineCopula)
library(contex)
library(np)
library(doParallel)

dat <- foreign::read.arff(
    "/home/n5/research/datasets/retinopathy/messidor_features.arff"
)
dat <- dat[dat[, 1] == 1, ]  # only use pictures with sufficient quality
dat <- dat[, -1]  # remove quality column
dat <- dat[1:100, 15:19]
## in sample ----------------------

## fit models
model_np <- jdify(Class ~ ., data = dat, jd_api("np"))
model_cctools <- jdify(Class ~ ., data = dat, jd_api("cctools"))
model_kdevine <- jdify(Class ~ ., data = dat, jd_api("kdevine"))

## predict conditional class probabilities
probs_np <- predict(model_np, dat, "cprobs")
probs_cctools <- predict(model_cctools, dat, "cprobs")
probs_kdevine <- predict(model_kdevine, dat, "cprobs")

## calculate performance measures
threshold <- seq(0, 1, l = 1000)
perf_np <- clsfy_performance(probs_np[, 1], dat$Class, threshold)
perf_cctools <- clsfy_performance(probs_cctools[, 1], dat$Class, threshold)
perf_kdevine <- clsfy_performance(probs_kdevine[, 1], dat$Class, threshold)

## ROC plots
plot(t(perf_np[c("FPR", "TPR"), ]), type = "l")
lines(t(perf_cctools[c("FPR", "TPR"), ]), col = 2)
lines(t(perf_kdevine[c("FPR", "TPR"), ]), col = 3)

## out of sample ------------------------

## fit models
cv_np <- cv_jdify(Class ~ ., data = dat, jd_api("np"), cores = 3)
cv_cctools <- cv_jdify(Class ~ ., data = dat, jd_api("cctools"), cores = 3)
cv_kdevine <- cv_jdify(Class ~ ., data = dat, jd_api("kdevine"), cores = 3)

probs_np <- cv_np$cv_cprobs
probs_cctools <- cv_cctools$cv_cprobs
probs_kdevine <- cv_kdevine$cv_cprobs

## calculate performance measures
threshold <- seq(0, 1, l = 1000)
perf_np <- clsfy_performance(probs_np[, 1], dat$Class, threshold)
perf_cctools <- clsfy_performance(probs_cctools[, 1], dat$Class, threshold)
perf_kdevine <- clsfy_performance(probs_kdevine[, 1], dat$Class, threshold)

## ROC plots
plot(t(perf_np[c("FPR", "TPR"), ]), type = "l")
lines(t(perf_cctools[c("FPR", "TPR"), ]), col = 2)
lines(t(perf_kdevine[c("FPR", "TPR"), ]), col = 3)
