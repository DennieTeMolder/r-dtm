##' @export
kinship <- function(mat) {
   names <- rownames(mat)
   kin_mat <- rrBLUP::A.mat(mat)
   rownames(kin_mat) <- colnames(kin_mat) <- names
   kin_mat
}
