# Annotate combinations of CHROM and POS based on non-overlapping regions described in ANNOT.
##' @export
annotate_single <- function(chrom, pos, annot, na = NA) {
  stopifnot(is.vector(chrom), is.vector(pos))
  stopifnot(is.vector(na), length(na) == 1L)
  stopifnot(is.data.frame(annot), c("chrom", "start", "end", "value") %in% colnames(annot))
  stopifnot(length(chrom) == length(pos), !anyDuplicated(rle(chrom)$values))

  results <- purrr::map(rlang::set_names(unique(chrom)), function(current) {
    chr_annot <- annot[annot$chrom == current, c("start", "end", "value")]

    bounds <- rbind(chr_annot$start, chr_annot$end)
    if (is.unsorted(bounds, strictly = TRUE))
      stop("Annotations are not coordinate sorted or are overlapping! Consider using merge_overlaps().")

    # Increment the CHR_ANNOT$end bounds to make them inclusive
    bounds <- bounds + rep(0:1, length.out = length(bounds))

    intervals <- findInterval(pos[chrom == current], vec = bounds)
    # Even intervals are out of bounds
    intervals[intervals %% 2L == 0L] <- NA
    # Convert odd intervals to index positions
    intervals <- as.integer((intervals + 1L) / 2L)

    chr_annot$value[intervals]
  })
  results <- purrr::list_c(results)

  if (!is.na(na) && anyNA(results))
    results[is.na(results)] <- na

  results
}

# Annotate combinations of CHROM and POS using annotate_single() for each ANNOT$type.
##' @export
annotate_multi <- function(chrom, pos, annot) {
  stopifnot(is.data.frame(annot), c("chrom", "start", "end", "value") %in% colnames(annot))
  if (!"type" %in% colnames(annot))
    annot$type <- annot$value

  # Add delimiter
  annot$value <- paste0(";", annot$value)

  res <- purrr::map(split(annot, ~type), function(current) {
    annotate_single(chrom, pos, annot = current, na = "")
  })

  # Paste results and remove leading delimiters
  res <- do.call(paste0, args = res)
  stringr::str_remove(res, "^;")
}
