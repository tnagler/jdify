jdify
=====

jdify is an R package implementing classifiers based on the joint density of the predictors and the class variable. Several methods for joint density estimation can be used.

To install, open R and type

``` r
devtools::install_github("tnagler/jdify")
```

### Functionality

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
model <- jdify(cl ~ x1 + x2, data = dat, jd_method = my_method)
```

The option `cc = TRUE` indicates that the method does not naturally handle discrete data. In this case, `jdify` automatically invokes the continuous convolution trick (see, Nagler, 2017).

#### Cross validation and performance assessment

`cv_jdify()` is a convenience function that does k-fold cross validation for you. It splits the data, fits joint density models and evaluates the conditional class probabilities on the hold-out samples.

``` r
cv <- cv_jdify(cl ~ x1 + x2, data = dat, folds = 3)
cv$cv_probs
#>            0         1
#> 1  0.5091734 0.4908266
#> 2  0.6318730 0.3681270
#> 3  0.6338442 0.3661558
#> 4  0.4712739 0.5287261
#> 5  0.8846899 0.1153101
#> 6  0.5000000 0.5000000
#> 7  0.5335339 0.4664661
#> 8  0.6672026 0.3327974
#> 9  0.5000000 0.5000000
#> 10 0.4949965 0.5050035
```

The function `assess_clsfyr()` allows to calculate several performance measures from the conditional class probabilities. Its first argument is the probability of the class, the second is a class indicator.

``` r
assess_clsfyr(cv$cv_probs[, 1], dat[, 1] == 0, measure = c("ACC", "F1"))
#>    threshold measure     value
#> 1        0.0     ACC 0.4000000
#> 2        0.1     ACC 0.4000000
#> 3        0.2     ACC 0.4000000
#> 4        0.3     ACC 0.4000000
#> 5        0.4     ACC 0.4000000
#> 6        0.5     ACC 0.2000000
#> 7        0.6     ACC 0.6000000
#> 8        0.7     ACC 0.5000000
#> 9        0.8     ACC 0.5000000
#> 10       0.9     ACC 0.6000000
#> 11       1.0     ACC 0.6000000
#> 12       0.0      F1 0.5714286
#> 13       0.1      F1 0.5714286
#> 14       0.2      F1 0.5714286
#> 15       0.3      F1 0.5714286
#> 16       0.4      F1 0.5714286
#> 17       0.5      F1 0.3333333
#> 18       0.6      F1 0.5000000
#> 19       0.7      F1 0.0000000
#> 20       0.8      F1 0.0000000
#> 21       0.9      F1 0.0000000
#> 22       1.0      F1 0.0000000
```

### References

Nagler, T. (2017). *Nonparametric density estimation with discrete variables.* Unpublished manuscript.
