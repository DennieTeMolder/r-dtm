# Return TRUE for each element in X that has private allele. X should be a
# single row of calls for a single variant and multiple samples following
# FORMAT/GT VCF spec.
##' @export
is_private <- function(x) {
  stopifnot(is.vector(x) || nrow(x) == 1, is.character(x))

  # Split genotype fields
  splits <- strsplit(x, "[/|]")
  alleles <- unique(unlist(splits, use.names = FALSE))
  alleles <- alleles[alleles != "."]

  if (nchar(alleles) > 1)
    stop("The current function only works for single digit genotypes!")

  # Private alleles only occur once
  is_private <- rep(FALSE, length(x))
  for (allele in alleles) {
    has_allele <- stringr::str_detect(x, stringr::fixed(allele))
    if (sum(has_allele) == 1)
      is_private <- is_private | has_allele
  }

  is_private
}
