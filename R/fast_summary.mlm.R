##' @export
# Author: Mark Sterken
# Quicker, stripped-down version of `summary.mlm()` that only computes summary$coefficients
fast_summary.mlm <- function(mlm) {
  stopifnot(inherits(mlm, "mlm"))
  stopifnot(mlm$rank > 0)

  p <- mlm$rank
  r <- mlm$residuals
  rdf <- mlm$df.residual
  Qr <- mlm$qr
  p1 <- 1L:p
  res2 <- colSums(r^2)

  resvar <- res2 / rdf
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(rep(diag(R), each = length(resvar)) * rep(resvar, times=length(diag(R))))
  est <- mlm$coefficients[Qr$pivot[p1], ]

  tval <- t(t(est) / se)
  pvals <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)

  tibble::tibble(
    Pvalue = pvals[2, ],
    Effect = round(est[2, ], digits = 5)
  )
}
