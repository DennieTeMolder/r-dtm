# Specifically designed for WN metadata xlsx files
##' @export
get_oldest_parent <- function(df, codes = NULL) {
  stopifnot(is.data.frame(df))
  stopifnot(c("population_code", "parental_population_code") %in% colnames(df))
  if (is.null(codes)) {
    stopifnot(!anyNA(df$population_code), nchar(df$population_code) > 0L, df$population_code != "NA")
  } else {
    stopifnot(!anyNA(codes), nchar(codes) > 0L, codes != "NA")
  }

  if (is.null(codes)) {
    codes <- df$population_code
    parents <- df$parental_population_code
  } else {
    idx <- match(codes, df$population_code)
    if (any(is.na(idx)))
      stop("Values not found in the 'population_code' column: ", .flatten(codes[is.na(idx)]))
    parents <- df$parental_population_code[idx]
  }

  # If there is no parent we keep the current code as oldest
  is_na <- is.na(parents)
  if (any(is_na))
    parents[is_na] <- codes[is_na]

  # Recurse until oldest parent is found for each entry
  if (any(parents != codes))
    parents <- get_oldest_parent(df = df, codes = parents)

  parents
}

##' @export
get_oldest_parent_info <- function(df, codes = NULL) {
  parents <- get_oldest_parent(df = df, codes = codes)
  semi_join_reorder(df, parents, by = "population_code", allow_duplicate = TRUE)
}
