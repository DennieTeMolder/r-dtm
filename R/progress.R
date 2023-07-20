##' @export
# Simple progress message that tracks running time and status, see also cli::cli_progress_done()
progress_msg <- function(msg = NULL, unit = "units", only = TRUE, .envir = parent.frame(), ...) {
  if (!is.null(msg))
    stopifnot(is.character(msg))
  if (!is.null(unit)) {
    stopifnot(is.character(unit))
    unit <- paste0(unit, " ")
  }

  # Make this the only progress bar
  if (only) cli::cli_progress_cleanup()

  id <- cli::cli_progress_step(
    msg = paste0(
      msg,
      "... {cli::pb_extra$frmt(cli::pb_current)} ",
      unit,
      "done @{cli::pb_rate} [{cli::pb_elapsed}]"
    ),
    msg_done = paste0(
      msg,
      " finished, completed a total of {cli::pb_extra$frmt(cli::pb_current)} ",
      unit,
      "@{cli::pb_rate}"
    ),
    msg_failed = paste0(
      msg,
      " failed after {cli::pb_extra$frmt(cli::pb_current)} ",
      unit,
      "@{cli::pb_rate}"
    ),
    extra = list(
      frmt = function(x) format(x, big.mark = ",", scientific = FALSE)
    ),
    .envir = .envir
  )

  # Force instant display, also start at 0 not 1
  cli::cli_progress_update(id = id, set = 0, force = TRUE)
  invisible(id)
}

##' @export
# Convenience wrapper
progress_inc <- function(amount, force = TRUE, .envir = parent.frame()) {
  cli::cli_progress_update(inc = amount, force = force, .envir = .envir)
}
