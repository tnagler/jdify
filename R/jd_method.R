#' Joint density estimation methods
#'
#' Provides pre-implemented APIs to packages for joint density
#' estimation. Optionally, custom fit and evaluation functions can be provided.
#'
#' @param package character string of the package name; so far, only
#'   `"cctools"`, `"kdevine"`, `"np"` are implemented.
#' @param fit_fun only used if `package = NULL`; a function of type `function(x,
#'   ...)` that fits a joint density model on a data matrix `x`. The `...` can
#'   be used for passing additional parameters.
#' @param eval_fun only used if `package = NULL`; a function of type
#'   `function(object, newdata, ...)` that takes an object fitted by `fit_fun`
#'   and evaluates the density estimate on `newdata`.
#' @param cc  only used if `package = NULL`; if `TRUE`, discrete variables (and
#'   the class indicator) are made continuous with [cctools::cont_conv()]; only
#'   use `FALSE` when your `fit_fun` can handle discrete variables.
#' @param ... additional parameters passed to `fit_fun`.
#'
#' @return An object of class `"jd_method"`.
#'
#' @examples
#' # pre-implemented APIs
#' cctools_method <- jd_method("cctools")
#' kdevine_method <- jd_method("kdevine")
#' np_method <- jd_method("np")
#'
#' # custom API (reimplementation of kdevine method)
#' require(kdevine)
#' fit_kdevine <- function(x, ...)
#'    kdevine::kdevine(x, ...)
#' eval_kdevine <- function(object, newdata, ...)
#'    kdevine::dkdevine(newdata, object)
#' my_method <- jd_method(fit_fun = fit_kdevine, eval_fun = eval_kdevine, cc = FALSE)
#'
#' @importFrom stats rbinom rnorm
#' @importFrom utils capture.output modifyList
#' @export
jd_method <- function(package = NULL, fit_fun = NULL, eval_fun = NULL,
                      cc = TRUE, ...) {
    if (is.null(package) & (is.null(fit_fun) | is.null(fit_fun)))
        stop("Must either specify a package or a fit_fun/eval_fun combination")
    if (is.null(package)) {
        # custom method
        method <- list(fit_fun = fit_fun,
                       eval_fun = eval_fun,
                       cc = cc)
    } else {
        stopifnot(is.character(package))
        loadNamespace(package)
        method <- switch(
            package,
            "cctools" = list(fit_fun = fit_cctools,
                             eval_fun = eval_cctools,
                             cc = FALSE),
            "kdevine" = list(fit_fun = fit_kdevine,
                             eval_fun = eval_kdevine,
                             cc = FALSE),
            "np"      = list(fit_fun = fit_np,
                             eval_fun = eval_np,
                             cc = FALSE),
            stop(paste("There is no method for", package, "yet;",
                       "create your own by providing fit_fun/eval_fun."))
        )
    }
    method$.dots <- list(...)
    class(method) <- "jd_method"
    suppressWarnings(check_jd_method(method))
    method
}

#' Check if `jd_method` works
#'
#' Throws an error when it doesn't.
#'
#' @param method an object created from [jd_method()].
#'
#' @noRd
check_jd_method <- function(method) {
    # do easy checks first to safe time in case of error
    stopifnot(inherits(method, "jd_method"))
    stopifnot(is.logical(method$cc))

    # dummy data
    set.seed(1)
    dat <- data.frame(
        cl = c("A", "B")[rbinom(20, 1, 0.5) + 1],
        x1 = rnorm(20),
        z1 = ordered(rbinom(20, 3, 0.5), 0:3),
        f1 = as.factor(rbinom(20, 3, 0.5))
    )

    # prepare for fitting
    model <- build_model(cl ~ ., dat)
    args <- list(x = model$df)
    args <- modifyList(args, method$.dots)
    if (method$cc)
        args$x <- cont_conv(args$x)

    # check fit_fun and eval_fun
    tryCatch({
        f_hat <- do.call(method$fit_fun, args)
    }, error = function(e) stop(paste("fit_fun doesn't work:", e)))

    if (method$cc)
        model$df <- expand_as_numeric(model$df)
    tryCatch({
        method$eval_fun(f_hat, newdata = model$df)
    }, error = function(e) stop(paste("eval_fun doesn't work:", e)))

    TRUE
}
