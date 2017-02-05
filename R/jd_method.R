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
#' @param suppl optional; a named list of supplementary functions or objects
#'   required to evaluate `fit_fun`/`eval_fun`.
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
#' # custom API (reimplementation of kdevine_method)
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
                      cc = TRUE, suppl = list(), ...) {
    if (is.null(package) & (is.null(fit_fun) | is.null(fit_fun)))
        stop("Must either specify a package or a fit_fun/eval_fun combination")
    if (is.null(package)) {
        # custom method
        method <- list(fit_fun = fit_fun,
                       eval_fun = eval_fun,
                       cc = cc,
                       suppl = suppl)
    } else {
        stopifnot(is.character(package))
        loadNamespace(package)
        method <- switch(
            package,
            "cctools" = list(fit_fun = fit_cctools,
                             eval_fun = eval_cctools,
                             cc = FALSE,
                             suppl = list()),
            "kdevine" = list(fit_fun = fit_kdevine,
                             eval_fun = eval_kdevine,
                             cc = FALSE,
                             suppl = list()),
            "np"      = list(fit_fun = fit_np,
                             eval_fun = eval_np,
                             cc = FALSE,
                             suppl = list(prep_data_np = prep_data_np)),
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
    stopifnot(is.list(method$suppl))
    list2env(method)

    # dummy data
    dat <- data.frame(
        cl = c("A", "B")[rbinom(10, 1, 0.3) + 1],
        x1 = rnorm(10),
        z1 = ordered(rbinom(10, 3, 0.3), 0:3)
    )
    mf <- make_model_frame(cl ~ ., dat)

    # check fit_fun and eval_fun
    args <- modifyList(list(x = prep_for_fit(mf, method)), method$.dots)

    tryCatch({
        f_hat <- do.call(method$fit_fun, args)
    }, error = function(e) stop(paste("fit_fun doesn't work:", e)))

    tryCatch({
        method$eval_fun(f_hat, newdata = with_num_class(mf[1, ]))
    }, error = function(e) stop(paste("eval_fun doesn't work:", e)))

    TRUE
}

run_silently <- function(expr) {
    suppressMessages(capture.output(eval(parse(text = expr))))
}
