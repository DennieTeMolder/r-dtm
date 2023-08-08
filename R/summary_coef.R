##' @export
summary_coef <- function(x, intercept = FALSE) {
  UseMethod("summary_coef", x)
}

# Stripped-down version of `summary.lm()` that only computes summary$coefficients
##' @export
summary_coef.lm <- function (x, intercept = FALSE) {
  stopifnot(x$rank > intercept)
  if (is.null(x$qr))
    stop("lm x does not have a proper 'qr' component.\n Use lm(.., qr=TRUE).")


  p <- x$rank
  r <- x$residuals
  rdf <- x$df.residual
  Qr <- x$qr
  p1 <- if (intercept) 1L:p else 2L:p

  w <- x$weights
  if (is.null(w)) {
    rss <- sum(r^2)
  } else {
    rss <- sum(w * r^2)
  }

  resvar <- rss / rdf
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- x$coefficients[Qr$pivot[p1]]

  tval <- est / se
  pval <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
  est <- round(est, digits = 5)

  list(
    effect = mat2tibble(est),
    p_value = mat2tibble(pval)
  )
}

# Author: Mark Sterken
# Quicker version of `summary.mlm()` based on matrix algebra, only computes summary$coefficients
##' @export
summary_coef.mlm <- function(x, intercept = FALSE) {
  stopifnot(x$rank > intercept)
  stopifnot(is.null(x$weights))
  if (is.null(x$qr))
    stop("mlm x does not have a proper 'qr' component.\n Use lm(.., qr=TRUE).")

  p <- x$rank
  r <- x$residuals
  rdf <- x$df.residual
  Qr <- x$qr
  p1 <- if (intercept) 1L:p else 2L:p
  rss <- colSums(r^2)

  resvar <- rss / rdf
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(rep(diag(R), each = length(resvar)) * rep(resvar, times=length(diag(R))))
  est <- x$coefficients[Qr$pivot[p1], , drop = FALSE]

  tval <- t(t(est) / se)
  pval <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
  est <- round(est, digits = 5)

  est <- t(est)
  pval <- t(pval)
  list(
    effect = mat2tibble(est),
    p_value = mat2tibble(pval)
  )
}
