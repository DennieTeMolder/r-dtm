# See: https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md
##' @export
read_gff3 <- function(file, check = TRUE) {
  first_line <- readr::read_lines(file, n_max = 1)
  if (check && !stringr::str_starts(first_line, "##gff-version 3"))
    stop("File header does not appear to be GFF3!")

  gff_cols <- c("seqid", "source", "type", "start", "end", "score", "strand",
                "phase", "attributes")
  readr::read_tsv(file,
    col_names = gff_cols,
    col_types = "fffiidfic",
    comment = "#",
    na = c(".", "NA")
  )
}
