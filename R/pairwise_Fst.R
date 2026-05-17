# Parallel version of `BEDASSLE::calculate.all.pairwise.Fst` with minor changes:
# 1) Returns a named matrix. 2) If ALT_DEPTH is fractional, TOTAL_DEPTH can be a
# single value with which the fractions are multiplied
##' @export
pairwise_Fst <- function(alt_depth, total_depth = 100L, cores = 1L, silent = FALSE) {
  rlang::check_installed("carrier")
  rlang::check_installed("BEDASSLE")

  stopifnot(is.matrix(alt_depth))
  if (is.vector(total_depth) && length(total_depth) == 1L) {
    if (max(alt_depth) <= 1)
      stop("When providing 'total_depth' as a single value, 'alt_depth' <= 1!")
    alt_depth <- round(alt_depth * total_depth)
    storage.mode(alt_depth) <- "integer"
  } else {
    stopifnot(is.matrix(total_depth), dim(total_depth) == dim(alt_depth))
  }
  cores <- as.integer(cores)

  # Compute comparisons
  n_row <- nrow(alt_depth)
  rnames <- rownames(alt_depth)
  combs <- utils::combn(n_row, m = 2, simplify = FALSE)
  n_combs <- length(combs)

  if (!silent) {
    msg <- paste("Computing", formatC(n_combs, big.mark = ","), "pairwise Fst values")
    cli::cli_progress_step(paste0(msg, " <", format(Sys.time(), "%a %H:%M"), ">"), msg)
  }

  # Simple case if no parallelisation is required
  if (cores < 2L) {
    if (!is.matrix(total_depth))
      total_depth <- matrix(total_depth, nrow(alt_depth), ncol(alt_depth))

    fst <- BEDASSLE::calculate.all.pairwise.Fst(
      allele.counts = alt_depth,
      sample.sizes = total_depth
    )
    rownames(fst) <- colnames(fst) <- rnames
    return(fst)
  }

  # Create a list with the combinations of rows to compare per core
  combs <- split(combs, (seq_len(length(combs)) - 1L) %/% (length(combs) / cores))
  idx_used <- lapply(combs, function(x) unique(unlist(x, use.names = FALSE)))

  # Gather data required by each core
  alt_depth <- asplit(alt_depth, 1L)
  alt_depth <- lapply(idx_used, function(idx) {
    alt_used <- alt_depth
    alt_used[-idx] <- list(NULL)
    alt_used
  })

  if (is.matrix(total_depth)) {
    total_depth <- asplit(total_depth, 1L)
    total_depth <- lapply(idx_used, function(idx) {
      total_used <- total_depth
      total_used[-idx] <- list(NULL)
      total_used
    })
  } else {
    total_depth <- as.list(rep(total_depth, cores))
  }

  # Setup mapper function in an isolated environment
  .pairwise_Fst_internal <- carrier::crate(
    function(alt_list, total_list, comb_list) {
      # If total list is a single value we pre-compute a static matrix
      if (!is.list(total_list)) {
        n <- max(sapply(alt_list, length))
        total_list <- matrix(total_list, nrow = 2L, ncol = n)
      }

      # Loop over all the requested combinations
      vapply(comb_list, FUN.VALUE = double(1), function(idx) {
        # Construct the 2 row comparison matrices
        allele_counts <- do.call(rbind, alt_list[idx])
        if (is.matrix(total_list)) {
          sample_sizes <- total_list
        } else {
          sample_sizes <- do.call(rbind, total_list[idx])
        }

        BEDASSLE::calculate.pairwise.Fst(allele.counts = allele_counts, sample.sizes = sample_sizes)
      })
    }
  )

  # Setup cluster
  cl <- parallel::makeCluster(cores, type = "PSOCK")
  on.exit(tryCatch(parallel::stopCluster(cl), error = function(e) NULL), add = TRUE)

  fst <- parallel::clusterMap(cl, .pairwise_Fst_internal, alt_depth, total_depth, combs)
  parallel::stopCluster(cl)

  # Collect results into a matrix
  fst <- unlist(fst, recursive = FALSE, use.names = FALSE)
  results <- matrix(0, n_row, n_row)
  rownames(results) <- colnames(results) <- rnames

  combs <- unlist(combs, recursive = FALSE, use.names = FALSE)
  combs <- do.call(rbind, combs)
  combs <- rbind(combs, combs[, 2:1])
  results[combs] <- rep(fst, 2)

  results
}
