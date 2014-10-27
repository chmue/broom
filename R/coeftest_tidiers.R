#' Tidy method for coeftest objects
#' 
#' TODO
#'
#' @return All tidying methods return a \code{data.frame} without rownames.
#' The structure depends on the method chosen.
#'
#' @seealso \code{\link{coeftest}} \code{\link{lm_tidiers}}
#'
#' @name coeftest_tidiers
#' 
#' @param x coeftest object
#'
#' @examples
#'
#' # TODO
NULL


#' @rdname coeftest_tidiers
#' 
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if
#' \code{conf.int=TRUE}
#' 
#' @details If \code{conf.int=TRUE}, the confidence interval is computed with
#' the \code{\link{confint}} function.
#' 
#' @return \code{tidy.coeftest} returns one row for each coefficient, with five columns:
#'   \item{term}{The term in the linear model being estimated and tested}
#'   \item{estimate}{The estimated coefficient}
#'   \item{std.error}{The corrected (robust) standard error}
#'   \item{statistic}{t- or z-statistic}
#'   \item{p.value}{two-sided p-value}
#' 
#' If \code{cont.int=TRUE}, it also includes columns for \code{conf.low} and
#' \code{conf.high}, computed with \code{\link{confint}}.
#' 
#' @export
tidy.coeftest <- function(x, conf.int=FALSE, conf.level=.95, ...) {
    nn <- c("estimate", "std.error", "statistic", "p.value")
    ret <- fix_data_frame(x[, ], nn)

    if (conf.int) {
        # avoid "Waiting for profiling to be done..." message
        CI <- suppressMessages(confint(x, level = conf.level))
        colnames(CI) = c("conf.low", "conf.high")
        ret <- cbind(ret, trans(unrowname(CI)))
    }

    ret
}

