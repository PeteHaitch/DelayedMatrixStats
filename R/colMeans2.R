### ============================================================================
### colMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, ..., useNames = TRUE) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colMeans2,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colMeans2
#' @importMethodsFrom DelayedArray seed
#' @rdname colMeans2
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @export
#' @template example_dm_matrix
#' @template example_dm_Rle
#' @author Peter Hickey
#' @examples
#'
#' colMeans2(dm_matrix)
setMethod("colMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colMeans2,
                                   blockfun = .DelayedMatrix_block_colMeans2,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importMethodsFrom Matrix colMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   ..., useNames = TRUE) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            val <- colMeans(x = x, na.rm = na.rm)
            if (!useNames) {
              val <- unname(val)
            }
            val
          }
)

#' @importMethodsFrom IRanges Views viewMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   ..., useNames = TRUE) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- Views(subject = x@rle, start = unlist(irl))
            val <- viewMeans(x = views, na.rm = na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              if (useNames) {
                nms <- colnames(x)
                if (!is.null(cols)) {
                  nms <- nms[cols]
                }
                names(val) <- nms
              }
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            val <- unlist(
              lapply(X = split(val, IDX), FUN = mean, na.rm = na.rm))
            if (useNames) {
              nms <- colnames(x)
              if (!is.null(cols)) {
                nms <- nms[cols]
              }
              names(val) <- nms
            }
            val
          }
)
