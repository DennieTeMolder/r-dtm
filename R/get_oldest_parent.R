# Specifically designed for WN metadata xlsx files
##' @export
get_oldest_parent <- function(df, codes = NULL) {
  stopifnot(c("population_code", "parental_population_code") %in% colnames(df))

  if (is.null(codes))
    codes <- df$population_code

  idx <- match(codes, df$population_code)
  if (any(is.na(idx))) {
    missing <- paste(codes[is.na(idx)], collapse = ", ")
    stop(paste("Cannot find population codes:", missing))
  }

  # Fetch all parental codes, when missing use current code
  parents <- df$parental_population_code[idx]
  parents <- ifelse(is.na(parents), codes, parents)

  # Recurse until oldest parent is found
  if (any(parents != codes))
    parents <- get_oldest_parent(df = df, codes = parents)

  parents
}

get_oldest_parent_info <- function(df, codes = NULL) {
  parents <- get_oldest_parent(df = df, codes = codes)
  idx <- match(parents, df$population_code)
  return(df[idx,])
}
