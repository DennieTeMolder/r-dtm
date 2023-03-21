# Specifically designed for WN metadata xlsx files
##' @export
get_oldest_parent <- function(df, codes = NULL) {
  stopifnot(is.data.frame(df))
  stopifnot(c("population_code", "parental_population_code") %in% colnames(df))

  if (is.null(codes))
    codes <- df$population_code

  idx <- match(codes, df$population_code)
  if (any(is.na(idx)))
    stop("Cannot find population codes:", .collapse(codes[is.na(idx)]))


  # Fetch all parental codes, when missing use current code
  parents <- df$parental_population_code[idx]
  parents <- ifelse(is.na(parents), codes, parents)

  # Recurse until oldest parent is found
  if (any(parents != codes))
    parents <- get_oldest_parent(df = df, codes = parents)

  parents
}

##' @export
get_oldest_parent_info <- function(df, codes = NULL) {
  parents <- get_oldest_parent(df = df, codes = codes)
  semi_join_reorder(df, parents, by = "population_code")
}
