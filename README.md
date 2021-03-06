
jdify
=====

[![Build status Linux](https://travis-ci.org/tnagler/jdify.svg?branch=master)](https://travis-ci.org/tnagler/jdify) [![Build status Windows](https://ci.appveyor.com/api/projects/status/github/tnagler/jdify?branch=master&svg=true)](https://ci.appveyor.com/project/tnagler/jdify) [![codecov.io](https://codecov.io/github/tnagler/jdify/coverage.svg?branch=master)](https://codecov.io/github/tnagler/jdify?branch=master) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

jdify is an R package implementing classifiers based on the joint density of the predictors and the class variable. Several methods for joint density estimation can be used.

To install, open R and type

``` r
devtools::install_github("tnagler/jdify")
```

### Functionality

The core functionality is illustrated below and in this [code snippet](https://gist.github.com/tnagler/843f5c658e1139ff669d33614cc727e6). For a detailed description of all functions and their arguments, see the [API documentation](https://tnagler.github.io/jdify/reference/index.html).

#### Classification modeling

The core function in this package is `jdify()` which builds a classification model for a given data set. It estimates the joint density of the predictors and the class variable and derives conditional class probabilities from it.

``` r
dat <- data.frame(
    cl = as.factor(rbinom(10, 1, 0.5)),
    x1 = rnorm(10),
    x2 = ordered(rbinom(10, 5, 0.3))
)
model <- jdify(cl ~ x1 + x2, data = dat, jd_method = "cctools")
probs <- predict(model, dat, what = "probs")  # conditional probabilities
```

`jdify()` can handle discrete predictors. They have to be declared as `ordered` or `factor` (for unordered categorical variables). All other variables are treated as continuous.

#### Methods for joint density estimation

You can choose from three built-in methods for: `"cctools"` (default), `"kdevine"`, `"np"`. The method name indicates the package that is used for joint density estimation.

You can also create custom functions for density estimation by `jd_method()`. The following is another implementation of the method `"kdevine"`.

``` r
my_fit <- function(x, ...)
   kdevine::kdevine(x, ...)
my_eval <- function(object, newdata, ...)
   kdevine::dkdevine(newdata, object)
my_method <- jd_method(fit_fun = my_fit, eval_fun = my_eval, cc = TRUE)
#> matrix is NA. Selecting structure...
model <- jdify(cl ~ x1 + x2, data = dat, jd_method = my_method)
#> matrix is NA. Selecting structure...
```

The option `cc = TRUE` indicates that the method does not naturally handle discrete data. In this case, `jdify` automatically invokes the continuous convolution trick (see, [Nagler, 2017](https://arxiv.org/abs/1704.07457)).

#### Cross validation and performance assessment

`cv_jdify()` is a convenience function that does k-fold cross validation for you. It splits the data, fits joint density models and evaluates the conditional class probabilities on the hold-out samples.

``` r
cv <- cv_jdify(cl ~ x1 + x2, data = dat, folds = 3)
cv$cv_probs
#>             0         1
#> 1  0.26024545 0.7397545
#> 2  0.69628962 0.3037104
#> 3  1.00000000 0.0000000
#> 4  0.34235926 0.6576407
#> 5  0.65840274 0.3415973
#> 6  0.50000000 0.5000000
#> 7  0.09671546 0.9032845
#> 8  0.39558049 0.6044195
#> 9  1.00000000 0.0000000
#> 10 0.25069108 0.7493089
```

The function `assess_clsfyr()` allows to calculate several performance measures from the conditional class probabilities. Its first argument is the probability of the class, the second is a class indicator.

``` r
assess_clsfyr(cv$cv_probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#>    threshold measure     value
#> 1        0.0     ACC 0.4000000
#> 2        0.1     ACC 0.5000000
#> 3        0.2     ACC 0.5000000
#> 4        0.3     ACC 0.3000000
#> 5        0.4     ACC 0.3000000
#> 6        0.5     ACC 0.3000000
#> 7        0.6     ACC 0.4000000
#> 8        0.7     ACC 0.4000000
#> 9        0.8     ACC 0.4000000
#> 10       0.9     ACC 0.4000000
#> 11       1.0     ACC 0.4000000
#> 12       0.0      F1 0.5714286
#> 13       0.1      F1 0.6153846
#> 14       0.2      F1 0.6153846
#> 15       0.3      F1 0.3636364
#> 16       0.4      F1 0.2222222
#> 17       0.5      F1 0.2222222
#> 18       0.6      F1 0.2500000
#> 19       0.7      F1 0.0000000
#> 20       0.8      F1 0.0000000
#> 21       0.9      F1 0.0000000
#> 22       1.0      F1 0.0000000
```

### References

Nagler, T. (2017). *A generic approach to nonparametric function estimation with mixed data.* [arXiv:1704.07457](https://arxiv.org/abs/1704.07457)
